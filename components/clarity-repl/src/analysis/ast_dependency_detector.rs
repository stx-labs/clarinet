#![allow(unused_variables)]

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use std::sync::LazyLock;

use clarinet_defaults::{DEFAULT_CLARITY_VERSION, DEFAULT_EPOCH};
use clarity::types::StacksEpochId;
pub use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::analysis::RuntimeCheckErrorKind;
use clarity::vm::ast::ContractAST;
use clarity::vm::representations::{SymbolicExpression, TraitDefinition};
use clarity::vm::types::{FunctionSignature, TypeSignatureExt};
use clarity::vm::{ClarityName, ClarityVersion, SymbolicExpressionType};
use clarity_types::types::signatures::CallableSubtype;
use clarity_types::types::{
    PrincipalData, QualifiedContractIdentifier, SequenceSubtype, TraitIdentifier, TypeSignature,
    Value,
};

use super::ast_visitor::{LetBinding, TypedVar};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};

/// Contract exists but was deployed at a lower epoch than its dependency,
/// meaning the dependency contract wasn't yet available when this contract
/// was deployed.
#[derive(Debug)]
pub struct IncorrectContractHeight {
    pub contract_id: String,
    pub contract_epoch: StacksEpochId,
    pub dep_contract_id: String,
    pub dep_epoch: StacksEpochId,
}

impl std::fmt::Display for IncorrectContractHeight {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Contract '{}' is deployed at epoch {}, but dependency '{}' requires epoch {}.\n The dependency contract was deployed at a later epoch than this contract.",
            self.contract_id, self.contract_epoch, self.dep_contract_id, self.dep_epoch
        )
    }
}

/// Extended runtime check error type that wraps stacks-core's `RuntimeCheckErrorKind`
/// and adds clarinet-specific error variants.
///
/// This allows clarinet to provide more specific error messages for issues that
/// arise from epoch mismatches between contracts and their dependencies.
#[derive(Debug)]
pub enum ClarinetRuntimeCheckErrorKind {
    /// Wraps the original stacks-core error variants.
    FromStacksCore(RuntimeCheckErrorKind),
    /// Contract exists but was deployed at a lower epoch than its dependency.
    IncorrectContractHeight(IncorrectContractHeight),
}

impl std::fmt::Display for ClarinetRuntimeCheckErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ClarinetRuntimeCheckErrorKind::FromStacksCore(e) => write!(f, "{e}"),
            ClarinetRuntimeCheckErrorKind::IncorrectContractHeight(e) => write!(f, "{e}"),
        }
    }
}

impl std::error::Error for ClarinetRuntimeCheckErrorKind {}

impl From<RuntimeCheckErrorKind> for ClarinetRuntimeCheckErrorKind {
    fn from(e: RuntimeCheckErrorKind) -> Self {
        ClarinetRuntimeCheckErrorKind::FromStacksCore(e)
    }
}

pub static DEFAULT_NAME: LazyLock<ClarityName> =
    LazyLock::new(|| ClarityName::from_literal("placeholder"));

pub struct ASTDependencyDetector<'a> {
    dependencies: BTreeMap<QualifiedContractIdentifier, DependencySet>,
    current_clarity_version: Option<&'a ClarityVersion>,
    current_contract: Option<&'a QualifiedContractIdentifier>,
    defined_functions:
        BTreeMap<(&'a QualifiedContractIdentifier, &'a ClarityName), Vec<TypeSignature>>,
    defined_traits: BTreeMap<
        (&'a QualifiedContractIdentifier, &'a ClarityName),
        BTreeMap<ClarityName, FunctionSignature>,
    >,
    defined_contract_constants: BTreeMap<
        (&'a QualifiedContractIdentifier, &'a ClarityName),
        &'a QualifiedContractIdentifier,
    >,
    pending_function_checks: BTreeMap<
        // function identifier whose type is not yet defined
        (&'a QualifiedContractIdentifier, &'a ClarityName),
        // list of call sites that need to be checked once this function is
        // defined, together with the associated args
        Vec<(CallSite<'a>, &'a [SymbolicExpression])>,
    >,
    pending_trait_checks: BTreeMap<
        // trait that is not yet defined
        &'a TraitIdentifier,
        // list of call sites that need to be checked once this trait is
        // defined, together with the function called and the associated args.
        Vec<(CallSite<'a>, &'a ClarityName, &'a [SymbolicExpression])>,
    >,
    /// Trait-argument references that may be plain data, settled once every
    /// contract has been visited.
    speculative_references: Vec<SpeculativeReference<'a>>,
    /// `let` bindings in scope, outermost first.
    let_bindings: Vec<(&'a ClarityName, &'a SymbolicExpression)>,
    params: Option<Vec<TypedVar<'a>>>,
    top_level: bool,
    preloaded: &'a BTreeMap<QualifiedContractIdentifier, (ClarityVersion, ContractAST)>,
}

#[derive(Clone, Debug, Eq)]
pub struct Dependency {
    pub contract_id: QualifiedContractIdentifier,
    pub required_before_publish: bool,
    /// Found inside an expression passed as a trait argument rather than as
    /// the argument itself, so it may be plain data, like `.owner` in
    /// `(get impl { impl: .impl, owner: .owner })`. Ordering drops it rather
    /// than report a cycle.
    pub speculative: bool,
}

impl PartialEq for Dependency {
    fn eq(&self, other: &Self) -> bool {
        self.contract_id == other.contract_id
    }
}

#[allow(clippy::non_canonical_partial_ord_impl)]
impl PartialOrd for Dependency {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.contract_id.partial_cmp(&other.contract_id)
    }
}

impl Ord for Dependency {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.contract_id.cmp(&other.contract_id)
    }
}

/// Where a callee's arguments are resolved: the calling contract, whose
/// constants they may name, and the `let` bindings in scope at the call.
#[derive(Clone)]
struct CallSite<'a> {
    caller: &'a QualifiedContractIdentifier,
    let_bindings: Vec<(&'a ClarityName, &'a SymbolicExpression)>,
}

impl<'a> CallSite<'a> {
    fn let_binding(&self, name: &ClarityName) -> Option<&'a SymbolicExpression> {
        self.let_bindings
            .iter()
            .rev()
            .find_map(|(bound, value)| (*bound == name).then_some(*value))
    }
}

/// How a callee's argument refers to a contract.
#[derive(Clone)]
enum ArgReference {
    /// The argument itself names the contract.
    Definite,
    /// Found inside the argument's expression, so it may be plain data (see
    /// [`Dependency::speculative`]). Only kept if the contract can implement
    /// the argument's trait.
    Speculative(TraitIdentifier),
}

/// Contracts referenced by a callee's arguments.
type ArgDependencies<'a> = BTreeMap<&'a QualifiedContractIdentifier, ArgReference>;

struct SpeculativeReference<'a> {
    from: &'a QualifiedContractIdentifier,
    to: &'a QualifiedContractIdentifier,
    trait_identifier: TraitIdentifier,
    top_level: bool,
}

#[derive(Debug, Clone, Default)]
pub struct DependencySet {
    pub set: BTreeSet<Dependency>,
}

impl DependencySet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_dependency(
        &mut self,
        contract_id: QualifiedContractIdentifier,
        required_before_publish: bool,
    ) {
        self.insert(Dependency {
            contract_id,
            required_before_publish,
            speculative: false,
        });
    }

    pub fn add_speculative_dependency(
        &mut self,
        contract_id: QualifiedContractIdentifier,
        required_before_publish: bool,
    ) {
        self.insert(Dependency {
            contract_id,
            required_before_publish,
            speculative: true,
        });
    }

    /// A required-before-publish or definite reference to a contract
    /// overrides a deferred or speculative one.
    fn insert(&mut self, mut dependency: Dependency) {
        if let Some(existing) = self.set.take(&dependency) {
            dependency.required_before_publish |= existing.required_before_publish;
            dependency.speculative &= existing.speculative;
        }
        self.set.insert(dependency);
    }

    pub fn has_dependency(&self, contract_id: &QualifiedContractIdentifier) -> Option<bool> {
        self.set
            .get(&Dependency {
                contract_id: contract_id.clone(),
                required_before_publish: false,
                speculative: false,
            })
            .map(|dep| dep.required_before_publish)
    }
}

impl Deref for DependencySet {
    type Target = BTreeSet<Dependency>;

    fn deref(&self) -> &Self::Target {
        &self.set
    }
}

impl DerefMut for DependencySet {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.set
    }
}

impl<'a> ASTDependencyDetector<'a> {
    pub fn detect_dependencies(
        contract_asts: &'a BTreeMap<QualifiedContractIdentifier, (ClarityVersion, ContractAST)>,
        preloaded: &'a BTreeMap<QualifiedContractIdentifier, (ClarityVersion, ContractAST)>,
    ) -> Result<
        BTreeMap<QualifiedContractIdentifier, DependencySet>,
        (
            // Dependencies detected
            BTreeMap<QualifiedContractIdentifier, DependencySet>,
            // Unresolved dependencies detected
            Vec<QualifiedContractIdentifier>,
        ),
    > {
        let mut detector = Self {
            dependencies: BTreeMap::new(),
            current_clarity_version: None,
            current_contract: None,
            defined_functions: BTreeMap::new(),
            defined_traits: BTreeMap::new(),
            defined_contract_constants: BTreeMap::new(),
            pending_function_checks: BTreeMap::new(),
            pending_trait_checks: BTreeMap::new(),
            speculative_references: Vec::new(),
            let_bindings: Vec::new(),
            params: None,
            top_level: true,
            preloaded,
        };

        let mut preloaded_visitor = PreloadedVisitor {
            detector: &mut detector,
            current_clarity_version: None,
            current_contract: None,
        };

        for (contract_identifier, (clarity_version, ast)) in preloaded {
            preloaded_visitor.current_clarity_version = Some(clarity_version);
            preloaded_visitor.current_contract = Some(contract_identifier);
            traverse(&mut preloaded_visitor, &ast.expressions);
        }

        for (contract_identifier, (clarity_version, ast)) in contract_asts {
            detector
                .dependencies
                .insert(contract_identifier.clone(), DependencySet::new());
            detector.current_clarity_version = Some(clarity_version);
            detector.current_contract = Some(contract_identifier);
            traverse(&mut detector, &ast.expressions);
        }

        // Every contract has been visited, so whether a referenced contract
        // defines a trait's functions is now known if it ever will be.
        for reference in std::mem::take(&mut detector.speculative_references) {
            if detector.lacks_trait_functions(
                reference.to,
                &reference.trait_identifier,
                contract_asts,
            ) {
                continue;
            }
            detector.top_level = reference.top_level;
            detector.insert_dependency(reference.from, reference.to, true);
        }
        detector.top_level = true;

        // Anything remaining in the pending_ maps indicates an unresolved dependency
        let mut unresolved: Vec<QualifiedContractIdentifier> = detector
            .pending_function_checks
            .into_keys()
            .map(|(contract_id, _)| contract_id.clone())
            .collect();
        unresolved.append(
            &mut detector
                .pending_trait_checks
                .into_keys()
                .map(|trait_id| trait_id.contract_identifier.clone())
                .collect(),
        );
        if unresolved.is_empty() {
            Ok(detector.dependencies)
        } else {
            Err((detector.dependencies, unresolved))
        }
    }

    pub fn order_contracts<'deps>(
        dependencies: &'deps BTreeMap<QualifiedContractIdentifier, DependencySet>,
        contract_epochs: &HashMap<QualifiedContractIdentifier, StacksEpochId>,
    ) -> Result<Vec<&'deps QualifiedContractIdentifier>, ClarinetRuntimeCheckErrorKind> {
        let mut lookup = BTreeMap::new();
        let mut reverse_lookup = Vec::new();

        if dependencies.is_empty() {
            return Ok(vec![]);
        }

        for (index, (contract, _)) in dependencies.iter().enumerate() {
            lookup.insert(contract, index);
            reverse_lookup.push(contract);
        }

        let mut graph = Graph::new();
        let mut speculative_edges = Vec::new();
        for (contract, contract_dependencies) in dependencies {
            let contract_id = lookup.get(contract).unwrap();
            // Boot contracts will not be in the contract_epochs map, so default to Epoch20
            let contract_epoch = contract_epochs
                .get(contract)
                .unwrap_or(&StacksEpochId::Epoch20);
            graph.add_node(*contract_id);
            for dep in contract_dependencies.iter() {
                let dep_epoch = contract_epochs
                    .get(&dep.contract_id)
                    .unwrap_or(&StacksEpochId::Epoch20);
                if contract_epoch < dep_epoch {
                    return Err(ClarinetRuntimeCheckErrorKind::IncorrectContractHeight(
                        IncorrectContractHeight {
                            contract_id: contract.to_string(),
                            contract_epoch: *contract_epoch,
                            dep_contract_id: dep.contract_id.to_string(),
                            dep_epoch: *dep_epoch,
                        },
                    ));
                }
                let Some(dep_id) = lookup.get(&dep.contract_id) else {
                    // No need to report an error here, it will be caught
                    // and reported with proper location information by the
                    // later analyses. Just skip it.
                    continue;
                };
                if dep.speculative {
                    speculative_edges.push((*contract_id, *dep_id));
                } else {
                    graph.add_directed_edge(*contract_id, *dep_id);
                }
            }
        }

        // A trait implementation is deployed before its caller, so it can't
        // close a cycle: a speculative edge that does is plain data.
        for (contract_id, dep_id) in speculative_edges {
            if !graph.reaches(dep_id, contract_id) {
                graph.add_directed_edge(contract_id, dep_id);
            }
        }

        let mut walker = GraphWalker::new();
        let sorted_indexes = walker.get_sorted_dependencies(&graph);

        let cyclic_deps = walker.get_cycling_dependencies(&graph, &sorted_indexes);
        if let Some(deps) = cyclic_deps {
            let mut contracts = vec![];
            for index in deps.iter() {
                let contract = reverse_lookup[*index];
                contracts.push(contract.name.to_string());
            }
            return Err(ClarinetRuntimeCheckErrorKind::FromStacksCore(
                RuntimeCheckErrorKind::CircularReference(contracts),
            ));
        }

        Ok(sorted_indexes
            .iter()
            .map(|index| reverse_lookup[*index])
            .collect())
    }

    fn add_dependency(
        &mut self,
        from: &QualifiedContractIdentifier,
        to: &QualifiedContractIdentifier,
    ) {
        self.insert_dependency(from, to, false);
    }

    fn add_arg_dependencies(
        &mut self,
        from: &'a QualifiedContractIdentifier,
        dependencies: ArgDependencies<'a>,
    ) {
        for (to, reference) in dependencies {
            match reference {
                ArgReference::Definite => self.add_dependency(from, to),
                ArgReference::Speculative(trait_identifier) => {
                    self.speculative_references.push(SpeculativeReference {
                        from,
                        to,
                        trait_identifier,
                        top_level: self.top_level,
                    })
                }
            }
        }
    }

    /// Whether `contract` is known not to define every function of the
    /// trait. A contract or trait that isn't loaded yet may still implement it.
    fn lacks_trait_functions(
        &self,
        contract: &QualifiedContractIdentifier,
        trait_identifier: &TraitIdentifier,
        contract_asts: &BTreeMap<QualifiedContractIdentifier, (ClarityVersion, ContractAST)>,
    ) -> bool {
        if !self.preloaded.contains_key(contract) && !contract_asts.contains_key(contract) {
            return false;
        }
        let Some(trait_definition) = self.defined_traits.get(&(
            &trait_identifier.contract_identifier,
            &trait_identifier.name,
        )) else {
            return false;
        };
        trait_definition
            .keys()
            .any(|function| !self.defined_functions.contains_key(&(contract, function)))
    }

    fn call_site(&self) -> CallSite<'a> {
        CallSite {
            caller: self.current_contract.unwrap(),
            let_bindings: self.let_bindings.clone(),
        }
    }

    fn insert_dependency(
        &mut self,
        from: &QualifiedContractIdentifier,
        to: &QualifiedContractIdentifier,
        speculative: bool,
    ) {
        if self.preloaded.contains_key(from) {
            return;
        }

        // Ignore the placeholder contract.
        if to.name.starts_with("__") {
            return;
        }

        // Ignore self-references.
        if from == to {
            return;
        }

        let set = self.dependencies.entry(from.clone()).or_default();
        if speculative {
            set.add_speculative_dependency(to.clone(), self.top_level);
        } else {
            set.add_dependency(to.clone(), self.top_level);
        }
    }

    fn add_defined_function(
        &mut self,
        contract_identifier: &'a QualifiedContractIdentifier,
        name: &'a ClarityName,
        param_types: Vec<TypeSignature>,
    ) {
        if let Some(pending) = self
            .pending_function_checks
            .remove(&(contract_identifier, name))
        {
            for (site, args) in pending {
                let dependencies = self.check_callee_type(&site, &param_types, args);
                self.add_arg_dependencies(site.caller, dependencies);
            }
        }

        self.defined_functions
            .insert((contract_identifier, name), param_types);
    }

    fn add_pending_function_check(
        &mut self,
        callee: (&'a QualifiedContractIdentifier, &'a ClarityName),
        args: &'a [SymbolicExpression],
    ) {
        let site = self.call_site();
        self.pending_function_checks
            .entry(callee)
            .or_default()
            .push((site, args));
    }

    fn add_defined_trait(
        &mut self,
        contract_identifier: &'a QualifiedContractIdentifier,
        name: &'a ClarityName,
        trait_definition: BTreeMap<ClarityName, FunctionSignature>,
    ) {
        if let Some(pending) = self.pending_trait_checks.remove(&TraitIdentifier {
            name: name.clone(),
            contract_identifier: contract_identifier.clone(),
        }) {
            for (site, function, args) in pending {
                let dependencies =
                    self.check_trait_dependencies(&site, &trait_definition, function, args);
                self.add_arg_dependencies(site.caller, dependencies);
            }
        }

        self.defined_traits
            .insert((contract_identifier, name), trait_definition);
    }

    fn add_defined_contract_constant(
        &mut self,
        contract_identifier: &'a QualifiedContractIdentifier,
        name: &'a ClarityName,
        target_contrat_identifier: &'a QualifiedContractIdentifier,
    ) {
        self.defined_contract_constants
            .insert((contract_identifier, name), target_contrat_identifier);
    }

    fn add_pending_trait_check(
        &mut self,
        callee: &'a TraitIdentifier,
        function: &'a ClarityName,
        args: &'a [SymbolicExpression],
    ) {
        let site = self.call_site();
        self.pending_trait_checks
            .entry(callee)
            .or_default()
            .push((site, function, args));
    }

    /// The contract `expr` names: a contract principal literal, a contract
    /// constant of the caller, or a `let` binding of either.
    fn contract_reference(
        &self,
        site: &CallSite<'a>,
        expr: &'a SymbolicExpression,
    ) -> Option<&'a QualifiedContractIdentifier> {
        let mut seen = HashSet::new();
        let mut expr = expr;
        loop {
            if let Some(Value::Principal(PrincipalData::Contract(contract_id))) =
                expr.match_literal_value()
            {
                return Some(contract_id);
            }
            let name = expr.match_atom()?;
            if let Some(contract_id) = self.defined_contract_constants.get(&(site.caller, name)) {
                return Some(contract_id);
            }
            // `seen` stops a binding that names itself, in unchecked code.
            if !seen.insert(name) {
                return None;
            }
            expr = site.let_binding(name)?;
        }
    }

    /// Record the contracts a trait argument may evaluate to. A contract found
    /// inside a wrapping expression (`begin`, `match`, `get`...) is speculative,
    /// so the forms that produce a value don't need to be enumerated.
    fn trait_arg_references(
        &self,
        site: &CallSite<'a>,
        trait_identifier: &TraitIdentifier,
        expr: &'a SymbolicExpression,
        speculative: bool,
        seen: &mut HashSet<&'a ClarityName>,
        dependencies: &mut ArgDependencies<'a>,
    ) {
        if let Some(contract_id) = self.contract_reference(site, expr) {
            if speculative {
                dependencies
                    .entry(contract_id)
                    .or_insert_with(|| ArgReference::Speculative(trait_identifier.clone()));
            } else {
                dependencies.insert(contract_id, ArgReference::Definite);
            }
        } else if let Some(value) = expr
            .match_atom()
            .filter(|name| seen.insert(name))
            .and_then(|name| site.let_binding(name))
        {
            self.trait_arg_references(site, trait_identifier, value, true, seen, dependencies);
        } else if let Some(children) = expr.match_list() {
            for child in children {
                self.trait_arg_references(site, trait_identifier, child, true, seen, dependencies);
            }
        }
    }

    fn deep_check_callee_type(
        &self,
        site: &CallSite<'a>,
        arg_type: &TypeSignature,
        expr: &'a SymbolicExpression,
        dependencies: &mut ArgDependencies<'a>,
    ) {
        match arg_type {
            TypeSignature::CallableType(CallableSubtype::Trait(trait_identifier))
            | TypeSignature::TraitReferenceType(trait_identifier) => self.trait_arg_references(
                site,
                trait_identifier,
                expr,
                false,
                &mut HashSet::new(),
                dependencies,
            ),
            TypeSignature::OptionalType(inner_type) => {
                if let Some(expr) = expr.match_list().and_then(|l| l.get(1)) {
                    self.deep_check_callee_type(site, inner_type, expr, dependencies);
                }
            }
            TypeSignature::ResponseType(inner_type) => {
                // Select the success or error type from the constructor name, then
                // recurse into element 1.
                if let Some(list) = expr.match_list() {
                    let constructor = list.first().and_then(|e| e.match_atom());
                    let payload = list.get(1);
                    if let (Some(constructor), Some(payload)) = (constructor, payload) {
                        let arg_type = if constructor.as_str() == "err" {
                            &inner_type.1
                        } else {
                            &inner_type.0
                        };
                        self.deep_check_callee_type(site, arg_type, payload, dependencies);
                    }
                }
            }
            TypeSignature::TupleType(inner_type) => {
                let type_map = inner_type.get_type_map();
                if let Some(tuple) = expr.match_list() {
                    for key_value in tuple.iter().skip(1) {
                        if let Some((arg_type, expr)) = key_value.match_list().and_then(|kv| {
                            Some((type_map.get(kv.first()?.match_atom()?)?, kv.get(1)?))
                        }) {
                            self.deep_check_callee_type(site, arg_type, expr, dependencies);
                        }
                    }
                }
            }
            TypeSignature::SequenceType(SequenceSubtype::ListType(inner_type)) => {
                let item_type = inner_type.get_list_item_type();
                if let Some(list) = expr.match_list() {
                    for item in list.iter().skip(1) {
                        self.deep_check_callee_type(site, item_type, item, dependencies);
                    }
                }
            }
            _ => (),
        }
    }

    /// Contracts passed as trait arguments at `site`. Names resolve at the
    /// call site: a deferred check runs while visiting the callee.
    fn check_callee_type(
        &self,
        site: &CallSite<'a>,
        arg_types: &[TypeSignature],
        args: &'a [SymbolicExpression],
    ) -> ArgDependencies<'a> {
        let mut dependencies = ArgDependencies::new();
        for (arg_type, expr) in arg_types.iter().zip(args) {
            self.deep_check_callee_type(site, arg_type, expr, &mut dependencies);
        }
        dependencies
    }

    fn check_trait_dependencies(
        &self,
        site: &CallSite<'a>,
        trait_definition: &BTreeMap<ClarityName, FunctionSignature>,
        function_name: &ClarityName,
        args: &'a [SymbolicExpression],
    ) -> ArgDependencies<'a> {
        // Since this may run before checkers, the function may not be valid.
        // If the key does not exist, just return an empty set and the error
        // will be reported elsewhere.
        let Some(function_signature) = trait_definition.get(function_name) else {
            return ArgDependencies::new();
        };
        self.check_callee_type(site, &function_signature.args, args)
    }

    // A trait can only come from a parameter (cannot be a let binding or a return value), so
    // find the corresponding parameter and return it.
    fn get_param_trait(&self, name: &ClarityName) -> Option<&'a TraitIdentifier> {
        let Some(params) = &self.params else {
            return None;
        };
        for param in params {
            if param.name == name {
                if let SymbolicExpressionType::TraitReference(_, trait_def) = &param.type_expr.expr
                {
                    return match trait_def {
                        TraitDefinition::Defined(identifier) => Some(identifier),
                        TraitDefinition::Imported(identifier) => Some(identifier),
                    };
                } else {
                    return None;
                }
            }
        }
        None
    }

    fn get_contract_constant(
        &self,
        name: &'a ClarityName,
    ) -> Option<&'a QualifiedContractIdentifier> {
        self.defined_contract_constants
            .get(&(self.current_contract.unwrap(), name))
            .copied()
    }
}

impl<'a> ASTVisitor<'a> for ASTDependencyDetector<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        self.current_clarity_version
            .unwrap_or(&DEFAULT_CLARITY_VERSION)
    }

    // For the following traverse_define_* functions, we just want to store a
    // map of the parameter types, to be used to extract the trait type in a
    // dynamic contract call.
    fn traverse_define_private(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.params.clone_from(&parameters);
        self.top_level = false;
        let res =
            self.traverse_expr(body) && self.visit_define_private(expr, name, parameters, body);
        self.params = None;
        self.top_level = true;
        res
    }

    fn visit_define_private(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        let param_types = match parameters {
            Some(parameters) => parameters
                .iter()
                .map(|typed_var| {
                    TypeSignature::parse_type_repr(DEFAULT_EPOCH, typed_var.type_expr, &mut ())
                        .unwrap_or(TypeSignature::BoolType)
                })
                .collect(),
            None => Vec::new(),
        };

        self.add_defined_function(self.current_contract.unwrap(), name, param_types);
        true
    }

    fn traverse_define_read_only(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.params.clone_from(&parameters);
        self.top_level = false;
        let res =
            self.traverse_expr(body) && self.visit_define_read_only(expr, name, parameters, body);
        self.params = None;
        self.top_level = true;
        res
    }

    fn visit_define_read_only(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        let param_types = match parameters {
            Some(parameters) => parameters
                .iter()
                .map(|typed_var| {
                    TypeSignature::parse_type_repr(DEFAULT_EPOCH, typed_var.type_expr, &mut ())
                        .unwrap_or(TypeSignature::BoolType)
                })
                .collect(),
            None => Vec::new(),
        };

        self.add_defined_function(self.current_contract.unwrap(), name, param_types);
        true
    }

    fn traverse_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.params.clone_from(&parameters);
        self.top_level = false;
        let res =
            self.traverse_expr(body) && self.visit_define_public(expr, name, parameters, body);
        self.params = None;
        self.top_level = true;
        res
    }

    fn visit_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        let param_types = match parameters {
            Some(parameters) => parameters
                .iter()
                .map(|typed_var| {
                    TypeSignature::parse_type_repr(DEFAULT_EPOCH, typed_var.type_expr, &mut ())
                        .unwrap_or(TypeSignature::BoolType)
                })
                .collect(),
            None => Vec::new(),
        };

        self.add_defined_function(self.current_contract.unwrap(), name, param_types);
        true
    }

    fn visit_define_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        functions: &'a [SymbolicExpression],
    ) -> bool {
        if let Ok(trait_definition) = TypeSignature::parse_trait_type_repr(
            functions,
            &mut (),
            DEFAULT_EPOCH,
            *self.current_clarity_version.unwrap(),
        ) {
            self.add_defined_trait(self.current_contract.unwrap(), name, trait_definition);
        }
        true
    }

    fn visit_static_contract_call(
        &mut self,
        expr: &'a SymbolicExpression,
        contract_identifier: &'a QualifiedContractIdentifier,
        function_name: &'a ClarityName,
        args: &'a [SymbolicExpression],
    ) -> bool {
        let site = self.call_site();
        self.add_dependency(site.caller, contract_identifier);
        let dependencies = if let Some(arg_types) = self
            .defined_functions
            .get(&(contract_identifier, function_name))
        {
            // If we know the type of this function, check the parameters for traits
            self.check_callee_type(&site, arg_types, args)
        } else {
            // If we do not yet know the type of this function, record it to re-analyze later
            self.add_pending_function_check((contract_identifier, function_name), args);
            return true;
        };
        self.add_arg_dependencies(site.caller, dependencies);
        true
    }

    fn visit_dynamic_contract_call(
        &mut self,
        expr: &'a SymbolicExpression,
        callable_expr: &'a SymbolicExpression,
        function_name: &'a ClarityName,
        args: &'a [SymbolicExpression],
    ) -> bool {
        let site = self.call_site();
        let callable = callable_expr.match_atom().unwrap_or(&DEFAULT_NAME);
        if let Some(trait_identifier) = self.get_param_trait(callable) {
            let dependencies = if let Some(trait_definition) = self.defined_traits.get(&(
                &trait_identifier.contract_identifier,
                &trait_identifier.name,
            )) {
                self.check_trait_dependencies(&site, trait_definition, function_name, args)
            } else {
                self.add_pending_trait_check(trait_identifier, function_name, args);
                return true;
            };

            self.add_arg_dependencies(site.caller, dependencies);
        } else if let Some(contract_constant) = self.get_contract_constant(callable) {
            self.add_dependency(site.caller, contract_constant);
            // Also detect trait-typed argument dependencies when the callee's
            // function type is known. Skip when there are no arguments, since
            // there is nothing to check for trait types.
            if !args.is_empty() {
                let dependencies = if let Some(arg_types) = self
                    .defined_functions
                    .get(&(contract_constant, function_name))
                {
                    self.check_callee_type(&site, arg_types, args)
                } else {
                    self.add_pending_function_check((contract_constant, function_name), args);
                    ArgDependencies::new()
                };
                self.add_arg_dependencies(site.caller, dependencies);
            }
        }
        true
    }

    fn visit_call_user_defined(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        args: &'a [SymbolicExpression],
    ) -> bool {
        let site = self.call_site();
        if let Some(arg_types) = self.defined_functions.get(&(site.caller, name)) {
            let dependencies = self.check_callee_type(&site, arg_types, args);
            self.add_arg_dependencies(site.caller, dependencies);
        }

        true
    }

    fn traverse_let(
        &mut self,
        expr: &'a SymbolicExpression,
        bindings: &HashMap<&'a ClarityName, LetBinding<'a>>,
        body: &'a [SymbolicExpression],
    ) -> bool {
        // Bindings stay in scope for the trait arguments of calls in the body
        // (and in later bindings, which `let` evaluates in sequence).
        let outer_scope = self.let_bindings.len();
        self.let_bindings.extend(
            bindings
                .iter()
                .map(|(name, binding)| (*name, binding.value)),
        );
        let res = bindings
            .values()
            .all(|binding| self.traverse_expr(binding.value))
            && body.iter().all(|expr| self.traverse_expr(expr))
            && self.visit_let(expr, bindings, body);
        self.let_bindings.truncate(outer_scope);
        res
    }

    fn visit_contract_hash(
        &mut self,
        _expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        // `contract-hash?` reads the referenced contract's stored hash, so the
        // contract must be published before the caller.
        let site = self.call_site();
        if let Some(contract) = self.contract_reference(&site, input) {
            self.add_dependency(site.caller, contract);
        }
        true
    }

    fn visit_use_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        trait_identifier: &TraitIdentifier,
    ) -> bool {
        self.add_dependency(
            self.current_contract.unwrap(),
            &trait_identifier.contract_identifier,
        );
        true
    }

    fn visit_impl_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        trait_identifier: &TraitIdentifier,
    ) -> bool {
        self.add_dependency(
            self.current_contract.unwrap(),
            &trait_identifier.contract_identifier,
        );
        true
    }

    fn visit_define_constant(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        value: &'a SymbolicExpression,
    ) -> bool {
        if let Some(Value::Principal(PrincipalData::Contract(contract_principal))) =
            value.match_literal_value()
        {
            self.add_defined_contract_constant(
                self.current_contract.unwrap(),
                name,
                contract_principal,
            );
        }
        true
    }
}

// Traverses the preloaded contracts and saves function signatures only

struct PreloadedVisitor<'a, 'b> {
    detector: &'b mut ASTDependencyDetector<'a>,
    current_clarity_version: Option<&'a ClarityVersion>,
    current_contract: Option<&'a QualifiedContractIdentifier>,
}
impl<'a> ASTVisitor<'a> for PreloadedVisitor<'a, '_> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        self.current_clarity_version
            .unwrap_or(&DEFAULT_CLARITY_VERSION)
    }

    fn traverse_define_read_only(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        let param_types = match parameters {
            Some(parameters) => parameters
                .iter()
                .map(|typed_var| {
                    TypeSignature::parse_type_repr(DEFAULT_EPOCH, typed_var.type_expr, &mut ())
                        .unwrap_or(TypeSignature::BoolType)
                })
                .collect(),
            None => Vec::new(),
        };

        self.detector
            .add_defined_function(self.current_contract.unwrap(), name, param_types);
        true
    }

    fn traverse_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        let param_types = match parameters {
            Some(parameters) => parameters
                .iter()
                .map(|typed_var| {
                    TypeSignature::parse_type_repr(DEFAULT_EPOCH, typed_var.type_expr, &mut ())
                        .unwrap_or(TypeSignature::BoolType)
                })
                .collect(),
            None => Vec::new(),
        };

        self.detector
            .add_defined_function(self.current_contract.unwrap(), name, param_types);
        true
    }

    fn traverse_define_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        functions: &'a [SymbolicExpression],
    ) -> bool {
        if let Ok(trait_definition) = TypeSignature::parse_trait_type_repr(
            functions,
            &mut (),
            DEFAULT_EPOCH,
            *self.current_clarity_version.unwrap(),
        ) {
            self.detector
                .add_defined_trait(self.current_contract.unwrap(), name, trait_definition);
        }
        true
    }
}

struct Graph {
    pub adjacency_list: Vec<Vec<usize>>,
}

impl Graph {
    fn new() -> Self {
        Self {
            adjacency_list: Vec::new(),
        }
    }

    fn add_node(&mut self, _expr_index: usize) {
        self.adjacency_list.push(vec![]);
    }

    fn add_directed_edge(&mut self, src_expr_index: usize, dst_expr_index: usize) {
        let list = self.adjacency_list.get_mut(src_expr_index).unwrap();
        list.push(dst_expr_index);
    }

    fn get_node_descendants(&self, expr_index: usize) -> Vec<usize> {
        self.adjacency_list[expr_index].clone()
    }

    fn has_node_descendants(&self, expr_index: usize) -> bool {
        !self.adjacency_list[expr_index].is_empty()
    }

    fn nodes_count(&self) -> usize {
        self.adjacency_list.len()
    }

    fn reaches(&self, from: usize, to: usize) -> bool {
        let mut seen = HashSet::new();
        let mut stack = vec![from];
        while let Some(node) = stack.pop() {
            if node == to {
                return true;
            }
            if seen.insert(node) {
                stack.extend(&self.adjacency_list[node]);
            }
        }
        false
    }
}

struct GraphWalker {
    seen: HashSet<usize>,
}

impl GraphWalker {
    fn new() -> Self {
        Self {
            seen: HashSet::new(),
        }
    }

    /// Depth-first search producing a post-order sort
    fn get_sorted_dependencies(&mut self, graph: &Graph) -> Vec<usize> {
        let mut sorted_indexes = Vec::<usize>::new();
        for expr_index in 0..graph.nodes_count() {
            self.sort_dependencies_recursion(expr_index, graph, &mut sorted_indexes);
        }

        sorted_indexes
    }

    fn sort_dependencies_recursion(
        &mut self,
        tle_index: usize,
        graph: &Graph,
        branch: &mut Vec<usize>,
    ) {
        if self.seen.contains(&tle_index) {
            return;
        }

        self.seen.insert(tle_index);
        if let Some(list) = graph.adjacency_list.get(tle_index) {
            for neighbor in list.iter() {
                self.sort_dependencies_recursion(*neighbor, graph, branch);
            }
        }
        branch.push(tle_index);
    }

    fn get_cycling_dependencies(
        &self,
        graph: &Graph,
        sorted_indexes: &[usize],
    ) -> Option<Vec<usize>> {
        let mut tainted: HashSet<usize> = HashSet::new();

        for node in sorted_indexes.iter() {
            let mut tainted_descendants_count = 0;
            let descendants = graph.get_node_descendants(*node);
            for descendant in descendants.iter() {
                if !graph.has_node_descendants(*descendant) || tainted.contains(descendant) {
                    tainted.insert(*descendant);
                    tainted_descendants_count += 1;
                }
            }
            if tainted_descendants_count == descendants.len() {
                tainted.insert(*node);
            }
        }

        if tainted.len() == sorted_indexes.len() {
            return None;
        }

        let nodes = HashSet::from_iter(sorted_indexes.iter().cloned());
        let deps = nodes.difference(&tainted).copied().collect();
        Some(deps)
    }
}

#[cfg(test)]
mod tests {
    use ::clarity::vm::diagnostic::Diagnostic;
    use clarinet_defaults::{DEFAULT_CLARITY_VERSION, DEFAULT_EPOCH};
    use indoc::indoc;

    use super::*;
    use crate::repl::session::Session;
    use crate::repl::{
        ClarityCodeSource, ClarityContract, ContractDeployer, Epoch, SessionSettings,
    };

    fn build_ast(
        session: &Session,
        snippet: &str,
        name: Option<&str>,
    ) -> Result<(QualifiedContractIdentifier, ContractAST, Vec<Diagnostic>), String> {
        let contract = ClarityContract {
            code_source: ClarityCodeSource::ContractInMemory(snippet.to_string()),
            name: name.unwrap_or("contract").to_string(),
            deployer: ContractDeployer::Transient,
            clarity_version: DEFAULT_CLARITY_VERSION,
            epoch: Epoch::Specific(DEFAULT_EPOCH),
            skip_analysis: false,
        };
        let (ast, diags, _) = session.interpreter.build_ast(&contract);
        Ok((
            contract.expect_resolved_contract_identifier(None),
            ast,
            diags,
        ))
    }

    fn deploy_snippet(
        session: &Session,
        snippet: &str,
        name: Option<&str>,
        contracts: &mut BTreeMap<QualifiedContractIdentifier, (ClarityVersion, ContractAST)>,
    ) -> QualifiedContractIdentifier {
        match build_ast(session, snippet, name) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        }
    }

    #[test]
    fn no_deps() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (hello)
                (ok (print \"hello\"))
            )
        ").to_string();
        match build_ast(&session, &snippet, None) {
            Ok((contract_identifier, ast, _)) => {
                let mut contracts = BTreeMap::new();
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                let dependencies =
                    ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new())
                        .unwrap();
                assert_eq!(dependencies[&contract_identifier].len(), 0);
            }
            Err(_) => panic!("expected success"),
        }
    }

    #[test]
    fn contract_call() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let snippet1 = indoc!("
            (define-public (hello (a int))
                (ok u0)
            )
        ").to_string();
        let foo = match build_ast(&session, &snippet1, Some("foo")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (call-foo)
                (contract-call? .foo hello 4)
            )
        ").to_string();
        let test_identifier = match build_ast(&session, &snippet, Some("test")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();
        assert_eq!(dependencies[&test_identifier].len(), 1);
        assert!(!dependencies[&test_identifier].has_dependency(&foo).unwrap());
    }

    #[test]
    fn dynamic_contract_call_local_trait() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let snippet1 = indoc!("
            (define-public (hello (a int))
                (ok u0)
            )
        ").to_string();
        let bar = match build_ast(&session, &snippet1, Some("bar")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        #[rustfmt::skip]
        let snippet = indoc!("
            (define-trait my-trait
                ((hello (int) (response uint uint)))
            )
            (define-trait dyn-trait
                ((call-hello (<my-trait>) (response uint uint)))
            )
            (define-public (call-dyn (dt <dyn-trait>))
                (contract-call? dt call-hello .bar)
            )
        ").to_string();
        let test_identifier = match build_ast(&session, &snippet, Some("test")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();
        assert_eq!(dependencies[&test_identifier].len(), 1);
        assert!(!dependencies[&test_identifier].has_dependency(&bar).unwrap());
    }

    #[test]
    fn dynamic_contract_call_remote_trait() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let snippet1 = indoc!("
            (define-trait my-trait
                ((hello (int) (response uint uint)))
            )
            (define-public (hello (a int))
                (ok u0)
            )
        ").to_string();
        let bar = match build_ast(&session, &snippet1, Some("bar")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait my-trait .bar.my-trait)
            (define-trait dyn-trait
                ((call-hello (<my-trait>) (response uint uint)))
            )
            (define-public (call-dyn (dt <dyn-trait>))
                (contract-call? dt call-hello .bar)
            )
        ").to_string();
        let test_identifier = match build_ast(&session, &snippet, Some("test")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();
        assert_eq!(dependencies[&test_identifier].len(), 1);
        assert!(dependencies[&test_identifier].has_dependency(&bar).unwrap());
    }

    #[test]
    fn pass_contract_local() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let snippet1 = indoc!("
            (define-public (hello (a int))
                (ok u0)
            )
        ").to_string();
        let bar = match build_ast(&session, &snippet1, Some("bar")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        #[rustfmt::skip]
        let snippet2 = indoc!("
            (define-trait my-trait
                ((hello (int) (response uint uint)))
            )
        ").to_string();
        let my_trait = match build_ast(&session, &snippet2, Some("my-trait")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait my-trait .my-trait.my-trait)
            (define-private (pass-trait (a <my-trait>))
                (print a)
            )
            (define-public (call-it)
                (ok (pass-trait .bar))
            )
        ").to_string();
        let test_identifier = match build_ast(&session, &snippet, Some("test")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();

        assert_eq!(
            dependencies[&test_identifier].has_dependency(&my_trait),
            Some(true)
        );
        assert_eq!(
            dependencies[&test_identifier].has_dependency(&bar),
            Some(false)
        );
        assert_eq!(dependencies[&test_identifier].len(), 2);
    }

    #[test]
    fn nested_trait_in_optional_type() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let trait_snippet = indoc!("
            (define-trait my-trait ((hello () (response bool uint))))
            (define-public (hello) (ok true))
        ").to_string();
        let my_trait = deploy_snippet(&session, &trait_snippet, Some("my_trait"), &mut contracts);

        #[rustfmt::skip]
        let callee_snippet = indoc!("
            (use-trait my-trait .my_trait.my-trait)
            (define-public (call-mt (mt (optional <my-trait>))) (ok true))
        ").to_string();
        let _ = deploy_snippet(&session, &callee_snippet, Some("callee"), &mut contracts);

        let caller_snippet =
            "(define-public (call) (contract-call? .callee call-mt (some .my_trait)))".to_string();
        let caller = deploy_snippet(&session, &caller_snippet, Some("caller"), &mut contracts);

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();

        assert_eq!(dependencies[&caller].len(), 2);
        assert_eq!(dependencies[&caller].has_dependency(&my_trait), Some(false));
    }

    #[test]
    fn nested_trait_in_response_type() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let trait_snippet = indoc!("
            (define-trait my-trait ((hello () (response bool uint))))
            (define-public (hello) (ok true))
        ").to_string();
        let my_trait = deploy_snippet(&session, &trait_snippet, Some("my_trait"), &mut contracts);

        #[rustfmt::skip]
        let callee_snippet = indoc!("
            (use-trait my-trait .my_trait.my-trait)
            (define-public (call-mt (mt (response <my-trait> uint))) (ok true))
        ").to_string();
        let _ = deploy_snippet(&session, &callee_snippet, Some("callee"), &mut contracts);

        let caller_snippet =
            "(define-public (call) (contract-call? .callee call-mt (ok .my_trait)))".to_string();
        let caller = deploy_snippet(&session, &caller_snippet, Some("caller"), &mut contracts);

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();

        assert_eq!(dependencies[&caller].len(), 2);
        assert_eq!(dependencies[&caller].has_dependency(&my_trait), Some(false));
    }

    #[test]
    fn nested_trait_in_tuple_type() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let trait_snippet = indoc!("
            (define-trait my-trait ((hello () (response bool uint))))
            (define-public (hello) (ok true))
        ").to_string();
        let my_trait = deploy_snippet(&session, &trait_snippet, Some("my_trait"), &mut contracts);

        #[rustfmt::skip]
        let callee_snippet = indoc!("
            (use-trait my-trait .my_trait.my-trait)
            (define-public (call-mt (mt { t: <my-trait> })) (ok true))
        ").to_string();
        let _ = deploy_snippet(&session, &callee_snippet, Some("callee"), &mut contracts);

        let caller_snippet =
            "(define-public (call) (contract-call? .callee call-mt { t: .my_trait }))".to_string();
        let caller = deploy_snippet(&session, &caller_snippet, Some("caller"), &mut contracts);

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();

        assert_eq!(dependencies[&caller].len(), 2);
        assert_eq!(dependencies[&caller].has_dependency(&my_trait), Some(false));
    }

    #[test]
    fn nested_trait_in_list_type() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let trait_snippet = indoc!("
            (define-trait my-trait ((hello () (response bool uint))))
            (define-public (hello) (ok true))
        ").to_string();
        let my_trait = deploy_snippet(&session, &trait_snippet, Some("my_trait"), &mut contracts);

        #[rustfmt::skip]
        let callee_snippet = indoc!("
            (use-trait my-trait .my_trait.my-trait)
            (define-public (call-mt (mt (list 4 <my-trait>))) (ok true))
        ").to_string();
        let _ = deploy_snippet(&session, &callee_snippet, Some("callee"), &mut contracts);

        let caller_snippet =
            "(define-public (call) (contract-call? .callee call-mt (list .my_trait .my_trait)))"
                .to_string();
        let caller = deploy_snippet(&session, &caller_snippet, Some("caller"), &mut contracts);

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();

        assert_eq!(dependencies[&caller].len(), 2);
        assert_eq!(dependencies[&caller].has_dependency(&my_trait), Some(false));
    }

    #[test]
    fn nested_trait_in_composite_type() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let trait_snippet = indoc!("
            (define-trait my-trait ((hello () (response bool uint))))
            (define-public (hello) (ok true))
        ").to_string();
        let my_trait = deploy_snippet(&session, &trait_snippet, Some("my_trait"), &mut contracts);

        #[rustfmt::skip]
        let callee_snippet = indoc!("
            (use-trait my-trait .my_trait.my-trait)
            (define-public (call-mt (mt (response { t: (optional <my-trait>) } uint))) (ok true))
        ").to_string();
        let _ = deploy_snippet(&session, &callee_snippet, Some("callee"), &mut contracts);

        let caller_snippet =
            "(define-public (call) (contract-call? .callee call-mt (ok { t: (some .my_trait) })))"
                .to_string();
        let caller = deploy_snippet(&session, &caller_snippet, Some("caller"), &mut contracts);

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();

        assert_eq!(dependencies[&caller].len(), 2);
        assert_eq!(dependencies[&caller].has_dependency(&my_trait), Some(false));
    }

    #[test]
    fn impl_trait() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let snippet1 = indoc!("
            (define-trait something
                ((hello (int) (response uint uint)))
            )
        ").to_string();
        let other = match build_ast(&session, &snippet1, Some("other")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        #[rustfmt::skip]
        let snippet = indoc!("
            (impl-trait .other.something)
            (define-public (hello (a int))
                (ok u0)
            )
        ").to_string();
        let test_identifier = match build_ast(&session, &snippet, Some("test")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();
        assert_eq!(dependencies[&test_identifier].len(), 1);
        assert!(dependencies[&test_identifier]
            .has_dependency(&other)
            .unwrap());
    }

    #[test]
    fn use_trait() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let snippet1 = indoc!("
            (define-trait something
                ((hello (int) (response uint uint)))
            )
        ").to_string();
        let other = match build_ast(&session, &snippet1, Some("other")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait my-trait .other.something)
        ").to_string();
        let test_identifier = match build_ast(&session, &snippet, Some("test")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();
        assert_eq!(dependencies[&test_identifier].len(), 1);
        assert!(dependencies[&test_identifier]
            .has_dependency(&other)
            .unwrap());
    }

    #[test]
    fn unresolved_contract_call() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (call-foo)
                (contract-call? .foo hello 4)
            )
        ").to_string();
        let _ = match build_ast(&session, &snippet, Some("test")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        match ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()) {
            Ok(_) => panic!("expected unresolved error"),
            Err((_, unresolved)) => assert_eq!(unresolved[0].name.as_str(), "foo"),
        }
    }

    #[test]
    fn dynamic_contract_call_unresolved_trait() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait my-trait .bar.my-trait)

            (define-public (call-dyn (dt <my-trait>))
                (contract-call? dt call-hello .bar)
            )
        ").to_string();
        let _ = match build_ast(&session, &snippet, Some("test")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        match ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()) {
            Ok(_) => panic!("expected unresolved error"),
            Err((_, unresolved)) => assert_eq!(unresolved[0].name.as_str(), "bar"),
        }
    }

    #[test]
    fn contract_call_top_level() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let snippet1 = indoc!("
            (define-public (hello (a int))
                (ok u0)
            )
        ").to_string();
        let foo = match build_ast(&session, &snippet1, Some("foo")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        let snippet = "(contract-call? .foo hello 4)".to_string();
        let test_identifier = match build_ast(&session, &snippet, Some("test")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();
        assert_eq!(dependencies[&test_identifier].len(), 1);
        assert!(dependencies[&test_identifier].has_dependency(&foo).unwrap());
    }

    #[test]
    fn avoid_bad_type() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let snippet1 = indoc!("
            (define-public (hello (a (list principal)))
                (ok u0)
            )
        ").to_string();
        let foo = match build_ast(&session, &snippet1, Some("foo")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        let snippet = "(contract-call? .foo hello 4)".to_string();
        let test_identifier = match build_ast(&session, &snippet, Some("test")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();
        assert_eq!(dependencies[&test_identifier].len(), 1);
        assert!(dependencies[&test_identifier].has_dependency(&foo).unwrap());
    }

    #[test]
    fn contract_stored_in_constant() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let snippet1 = indoc!("
            (define-public (hello (a int))
                (ok u0)
            )
        ").to_string();
        let foo = match build_ast(&session, &snippet1, Some("foo")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant foo-contract .foo)
            (contract-call? foo-contract .test)
        ").to_string();
        let test_identifier = match build_ast(&session, &snippet, Some("test")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();
        assert_eq!(dependencies[&test_identifier].len(), 1);
        assert!(dependencies[&test_identifier].has_dependency(&foo).unwrap());
    }

    #[test]
    fn order_contracts_returns_incorrect_contract_height_on_epoch_mismatch() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();

        // Dependency contract (deployed at a higher epoch)
        #[rustfmt::skip]
        let snippet_dep = indoc!("
            (define-public (hello (a int))
                (ok u0)
            )
        ").to_string();
        let foo = match build_ast(&session, &snippet_dep, Some("foo")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        // Depending contract (deployed at a lower epoch)
        #[rustfmt::skip]
        let snippet = indoc!("
            (contract-call? .foo hello 4)
        ").to_string();
        let test_identifier = match build_ast(&session, &snippet, Some("test")) {
            Ok((contract_identifier, ast, _)) => {
                contracts.insert(contract_identifier.clone(), (DEFAULT_CLARITY_VERSION, ast));
                contract_identifier
            }
            Err(_) => panic!("expected success"),
        };

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();

        let mut contract_epochs = HashMap::new();
        contract_epochs.insert(test_identifier.clone(), StacksEpochId::Epoch21);
        contract_epochs.insert(foo.clone(), StacksEpochId::Epoch24);

        let result = ASTDependencyDetector::order_contracts(&dependencies, &contract_epochs);

        match result {
            Err(ClarinetRuntimeCheckErrorKind::IncorrectContractHeight(e)) => {
                assert_eq!(e.contract_id, test_identifier.to_string());
                assert_eq!(e.contract_epoch, StacksEpochId::Epoch21);
                assert_eq!(e.dep_contract_id, foo.to_string());
                assert_eq!(e.dep_epoch, StacksEpochId::Epoch24);
                let msg = e.to_string();
                assert!(
                    msg.contains(&test_identifier.to_string()),
                    "error message should contain the contract id"
                );
                assert!(
                    msg.contains(&foo.to_string()),
                    "error message should contain the dependency contract id"
                );
                assert!(
                    msg.contains("2.1"),
                    "error message should contain the contract epoch"
                );
                assert!(
                    msg.contains("2.4"),
                    "error message should contain the dependency epoch"
                );
            }
            other => panic!("expected IncorrectContractHeight, got {other:?}"),
        }
    }

    // Helpers shared by the trait-arg-in-expression tests below.
    fn setup_trait_callee(
        session: &Session,
        contracts: &mut BTreeMap<QualifiedContractIdentifier, (ClarityVersion, ContractAST)>,
    ) -> (QualifiedContractIdentifier, QualifiedContractIdentifier) {
        #[rustfmt::skip]
        let callee_snippet = indoc!("
            (define-trait reader ((get-one () (response uint uint))))
            (define-public (take (target <reader>))
              (contract-call? target get-one))
        ").to_string();
        let callee = deploy_snippet(session, &callee_snippet, Some("callee"), contracts);

        #[rustfmt::skip]
        let impl_snippet = indoc!("
            (define-public (get-one) (ok u1))
        ").to_string();
        let implementation =
            deploy_snippet(session, &impl_snippet, Some("implementation"), contracts);

        (callee, implementation)
    }

    fn assert_impl_dep(
        contracts: &BTreeMap<QualifiedContractIdentifier, (ClarityVersion, ContractAST)>,
        caller: &QualifiedContractIdentifier,
        implementation: &QualifiedContractIdentifier,
    ) {
        let dependencies =
            ASTDependencyDetector::detect_dependencies(contracts, &BTreeMap::new()).unwrap();
        assert!(
            dependencies[caller]
                .has_dependency(implementation)
                .is_some(),
            "expected .implementation to be detected as a dependency of .caller"
        );
    }

    #[test]
    fn trait_arg_constant() {
        // (define-constant target .implementation)
        // (contract-call? .callee take target)
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        let (_, implementation) = setup_trait_callee(&session, &mut contracts);

        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant target .implementation)
            (define-public (go)
              (contract-call? .callee take target))
        ").to_string();
        let caller = deploy_snippet(&session, &snippet, Some("caller"), &mut contracts);

        assert_impl_dep(&contracts, &caller, &implementation);
    }

    #[test]
    fn trait_arg_begin() {
        // (contract-call? .callee take (begin .implementation))
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        let (_, implementation) = setup_trait_callee(&session, &mut contracts);

        let snippet = "(define-public (go) (contract-call? .callee take (begin .implementation)))"
            .to_string();
        let caller = deploy_snippet(&session, &snippet, Some("caller"), &mut contracts);

        assert_impl_dep(&contracts, &caller, &implementation);
    }

    #[test]
    fn trait_arg_unwrap_panic() {
        // (contract-call? .callee take (unwrap-panic (some .implementation)))
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        let (_, implementation) = setup_trait_callee(&session, &mut contracts);

        let snippet = "(define-public (go) (contract-call? .callee take (unwrap-panic (some .implementation))))".to_string();
        let caller = deploy_snippet(&session, &snippet, Some("caller"), &mut contracts);

        assert_impl_dep(&contracts, &caller, &implementation);
    }

    #[test]
    fn trait_arg_default_to() {
        // (contract-call? .callee take (default-to .implementation (some .implementation)))
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        let (_, implementation) = setup_trait_callee(&session, &mut contracts);

        let snippet = "(define-public (go) (contract-call? .callee take (default-to .implementation (some .implementation))))".to_string();
        let caller = deploy_snippet(&session, &snippet, Some("caller"), &mut contracts);

        assert_impl_dep(&contracts, &caller, &implementation);
    }

    #[test]
    fn trait_arg_match() {
        // (contract-call? .callee take (match (some .implementation) x x .implementation))
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        let (_, implementation) = setup_trait_callee(&session, &mut contracts);

        let snippet = "(define-public (go) (contract-call? .callee take (match (some .implementation) x x .implementation)))".to_string();
        let caller = deploy_snippet(&session, &snippet, Some("caller"), &mut contracts);

        assert_impl_dep(&contracts, &caller, &implementation);
    }

    #[test]
    fn trait_arg_tuple_get() {
        // (contract-call? .callee take (get target { target: .implementation }))
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        let (_, implementation) = setup_trait_callee(&session, &mut contracts);

        let snippet = "(define-public (go) (contract-call? .callee take (get target { target: .implementation })))".to_string();
        let caller = deploy_snippet(&session, &snippet, Some("caller"), &mut contracts);

        assert_impl_dep(&contracts, &caller, &implementation);
    }

    #[test]
    fn trait_arg_constant_callee() {
        // (define-constant c .callee)
        // (contract-call? c take .implementation)
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        let (_, implementation) = setup_trait_callee(&session, &mut contracts);

        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant c .callee)
            (define-public (go)
              (contract-call? c take .implementation))
        ").to_string();
        let caller = deploy_snippet(&session, &snippet, Some("caller"), &mut contracts);

        assert_impl_dep(&contracts, &caller, &implementation);
    }

    #[test]
    fn contract_hash_self_reference() {
        // (contract-hash? .self-contract) inside self-contract must not register
        // a self-dependency — add_dependency already guards against from == to.
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        let snippet = "(define-read-only (get-hash) (contract-hash? .self-contract))".to_string();
        let self_contract =
            deploy_snippet(&session, &snippet, Some("self-contract"), &mut contracts);

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();
        assert_eq!(
            dependencies
                .get(&self_contract)
                .map(|d| d.len())
                .unwrap_or(0),
            0,
            "contract-hash? on self must not register a self-dependency"
        );
    }

    #[test]
    fn trait_arg_constant_with_callee_visited_later() {
        // `zcallee` sorts after `caller`, so the trait check is deferred until
        // `zcallee` is visited; `target` must still resolve in `caller`.
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        #[rustfmt::skip]
        let callee_snippet = indoc!("
            (define-trait reader ((get-one () (response uint uint))))
            (define-public (take (target <reader>))
              (contract-call? target get-one))
        ").to_string();
        deploy_snippet(&session, &callee_snippet, Some("zcallee"), &mut contracts);
        let implementation = deploy_snippet(
            &session,
            "(define-public (get-one) (ok u1))",
            Some("implementation"),
            &mut contracts,
        );

        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant target .implementation)
            (define-public (go)
              (contract-call? .zcallee take target))
        ").to_string();
        let caller = deploy_snippet(&session, &snippet, Some("caller"), &mut contracts);

        assert_impl_dep(&contracts, &caller, &implementation);
    }

    #[test]
    fn trait_arg_constant_in_expression() {
        // (define-constant target .implementation)
        // (contract-call? .callee take (begin target))
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        let (_, implementation) = setup_trait_callee(&session, &mut contracts);

        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant target .implementation)
            (define-public (go)
              (contract-call? .callee take (begin target)))
        ").to_string();
        let caller = deploy_snippet(&session, &snippet, Some("caller"), &mut contracts);

        assert_impl_dep(&contracts, &caller, &implementation);
    }

    #[test]
    fn contract_hash_constant() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        let target = deploy_snippet(
            &session,
            "(define-read-only (get-one) u1)",
            Some("target"),
            &mut contracts,
        );
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant c .target)
            (define-read-only (get-hash) (contract-hash? c))
        ").to_string();
        let caller = deploy_snippet(&session, &snippet, Some("caller"), &mut contracts);

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();
        assert!(dependencies[&caller].has_dependency(&target).is_some());
    }

    #[test]
    fn data_principal_in_trait_arg_does_not_create_cycle() {
        // `.user` is plain data in `caller`'s trait argument, and `user` calls
        // `caller`. `user` defines the trait's function, so it isn't filtered
        // out as a non-implementer. Ordering must not report a cycle, and must
        // still deploy the implementation before `caller`.
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        let (_, implementation) = setup_trait_callee(&session, &mut contracts);

        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (go)
              (contract-call? .callee take
                (get impl { impl: .implementation, owner: .user })))
        ").to_string();
        let caller = deploy_snippet(&session, &snippet, Some("caller"), &mut contracts);
        #[rustfmt::skip]
        let user_snippet = indoc!("
            (define-public (get-one) (ok u2))
            (define-public (run) (contract-call? .caller go))
        ").to_string();
        let user = deploy_snippet(&session, &user_snippet, Some("user"), &mut contracts);

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();
        let ordered = ASTDependencyDetector::order_contracts(&dependencies, &HashMap::new())
            .expect("a data principal must not be reported as a cycle");
        let position = |id| ordered.iter().position(|c| *c == id).unwrap();
        assert!(position(&implementation) < position(&caller));
        assert!(position(&caller) < position(&user));
    }

    #[test]
    fn trait_arg_let_outside_call() {
        // (let ((t .implementation)) (contract-call? .callee take t))
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        let (_, implementation) = setup_trait_callee(&session, &mut contracts);

        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (go)
              (let ((t .implementation))
                (contract-call? .callee take t)))
        ").to_string();
        let caller = deploy_snippet(&session, &snippet, Some("caller"), &mut contracts);

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();
        let dependency = dependencies[&caller]
            .get(&Dependency {
                contract_id: implementation,
                required_before_publish: false,
                speculative: false,
            })
            .expect("expected .implementation to be detected as a dependency of .caller");
        assert!(!dependency.speculative);
    }

    #[test]
    fn data_principal_not_implementing_trait_is_dropped() {
        // `.other` is in the trait argument's expression but doesn't define
        // the trait's functions, so it can't be what is passed.
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        let (_, implementation) = setup_trait_callee(&session, &mut contracts);
        let other = deploy_snippet(
            &session,
            "(define-read-only (get-two) u2)",
            Some("other"),
            &mut contracts,
        );

        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (go)
              (contract-call? .callee take
                (get impl { impl: .implementation, owner: .other })))
        ").to_string();
        let caller = deploy_snippet(&session, &snippet, Some("caller"), &mut contracts);

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();
        assert!(dependencies[&caller]
            .has_dependency(&implementation)
            .is_some());
        assert!(dependencies[&caller].has_dependency(&other).is_none());
    }

    #[test]
    fn speculative_dependency_on_later_epoch_is_reported() {
        let session = Session::new_without_boot_contracts(SessionSettings::default());
        let mut contracts = BTreeMap::new();
        let (_, implementation) = setup_trait_callee(&session, &mut contracts);

        let snippet = "(define-public (go) (contract-call? .callee take (begin .implementation)))";
        let caller = deploy_snippet(&session, snippet, Some("caller"), &mut contracts);

        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contracts, &BTreeMap::new()).unwrap();
        let contract_epochs = HashMap::from([
            (caller.clone(), StacksEpochId::Epoch21),
            (implementation.clone(), StacksEpochId::Epoch24),
        ]);

        match ASTDependencyDetector::order_contracts(&dependencies, &contract_epochs) {
            Err(ClarinetRuntimeCheckErrorKind::IncorrectContractHeight(e)) => {
                assert_eq!(e.contract_id, caller.to_string());
                assert_eq!(e.dep_contract_id, implementation.to_string());
            }
            other => panic!("expected IncorrectContractHeight, got {other:?}"),
        }
    }
}
