#![allow(unused_variables)]

use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{DiagnosableError, Diagnostic, Level};
use clarity::vm::functions::define::DefineFunctions;
use clarity::vm::functions::NativeFunctions;
use clarity::vm::representations::Span;
use clarity::vm::representations::SymbolicExpressionType::*;
use clarity::vm::types::TypeSignatureExt;
use clarity::vm::{ClarityName, ClarityVersion, SymbolicExpression};
use clarity_types::types::TypeSignature;
#[cfg(feature = "json_schema")]
use schemars::JsonSchema;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor, TypedVar};
use crate::analysis::{self, AnalysisPass, AnalysisResult};
use crate::repl::DEFAULT_EPOCH;

#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
pub struct Settings {
    // Strict mode sets all other options to false
    strict: bool,
    // After a filter on tx-sender, trust all inputs
    trusted_sender: bool,
    // After a filter on contract-caller, trust all inputs
    trusted_caller: bool,
    // Allow filters in callee to filter caller
    callee_filter: bool,
}

#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
pub struct SettingsFile {
    // Strict mode sets all other options to false
    strict: Option<bool>,
    // After a filter on tx-sender, trust all inputs
    trusted_sender: Option<bool>,
    // After a filter on contract-caller, trust all inputs
    trusted_caller: Option<bool>,
    // Allow filters in callee to filter caller
    callee_filter: Option<bool>,
}

impl From<SettingsFile> for Settings {
    fn from(from_file: SettingsFile) -> Self {
        if from_file.strict.unwrap_or(false) {
            Settings {
                strict: true,
                trusted_sender: false,
                trusted_caller: false,
                callee_filter: false,
            }
        } else {
            Settings {
                strict: false,
                trusted_sender: from_file.trusted_sender.unwrap_or(false),
                trusted_caller: from_file.trusted_caller.unwrap_or(false),
                callee_filter: from_file.callee_filter.unwrap_or(false),
            }
        }
    }
}

pub struct CheckError;

impl DiagnosableError for CheckError {
    fn message(&self) -> String {
        "Use of potentially unchecked data".to_string()
    }
    fn suggestion(&self) -> Option<String> {
        None
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
enum Node<'a> {
    Symbol(&'a ClarityName),
    Expr(u64),
}

#[derive(Clone, Debug)]
struct TaintSource<'a> {
    span: Span,
    children: HashSet<Node<'a>>,
}

#[derive(Clone, Debug)]
struct TaintedNode<'a> {
    sources: HashSet<Node<'a>>,
}

struct FunctionInfo {
    // Parameters which are allowed to be unchecked when passed into this
    // function.
    unchecked_params: Vec<bool>,
    // Parameters which are filtered by this function using an assertion,
    // meaning that those arguments can be considered filtered in the callee
    // after this function has been called.
    filtered_params: Vec<bool>,
}

pub struct CheckChecker<'a> {
    settings: Settings,
    taint_sources: HashMap<Node<'a>, TaintSource<'a>>,
    tainted_nodes: HashMap<Node<'a>, TaintedNode<'a>>,
    // Map expression ID to a generated diagnostic
    diagnostics: HashMap<u64, Vec<Diagnostic>>,
    annotations: &'a Vec<Annotation>,
    active_annotation: Option<usize>,
    // Record all public functions defined
    public_funcs: HashSet<&'a ClarityName>,
    // For each user-defined function, record which parameters are allowed
    // to be unchecked (tainted)
    user_funcs: HashMap<&'a ClarityName, FunctionInfo>,
    // True if currently traversing within an `as-contract` node
    in_as_contract: bool,
}

impl<'a> CheckChecker<'a> {
    fn new(annotations: &'a Vec<Annotation>, settings: Settings) -> CheckChecker<'a> {
        Self {
            settings,
            taint_sources: HashMap::new(),
            tainted_nodes: HashMap::new(),
            diagnostics: HashMap::new(),
            annotations,
            active_annotation: None,
            public_funcs: HashSet::new(),
            user_funcs: HashMap::new(),
            in_as_contract: false,
        }
    }

    fn run(mut self, contract_analysis: &'a ContractAnalysis) -> AnalysisResult {
        // First traverse the entire AST
        traverse(&mut self, &contract_analysis.expressions);

        // Collect all of the vecs of diagnostics into a vector
        let mut diagnostics: Vec<Vec<Diagnostic>> = self.diagnostics.into_values().collect();
        // Order the sets by the span of the error (the first diagnostic)
        diagnostics.sort_by(|a, b| a[0].spans[0].cmp(&b[0].spans[0]));
        // Then flatten into one vector
        Ok(diagnostics.into_iter().flatten().collect())
    }

    fn add_taint_source(&mut self, node: Node<'a>, span: Span) {
        let source_node = self.taint_sources.insert(
            node,
            TaintSource {
                span,
                children: HashSet::new(),
            },
        );
        let mut sources = HashSet::new();
        sources.insert(node);
        self.tainted_nodes.insert(node, TaintedNode { sources });
    }

    fn add_taint_source_symbol(&mut self, name: &'a ClarityName, span: Span) {
        self.add_taint_source(Node::Symbol(name), span);
    }

    fn add_tainted_node_to_sources(&mut self, node: Node<'a>, sources: &HashSet<Node<'a>>) {
        for source_node in sources {
            let source = self.taint_sources.get_mut(source_node).unwrap();
            source.children.insert(node);
        }
    }

    fn add_tainted_expr(&mut self, expr: &'a SymbolicExpression, sources: HashSet<Node<'a>>) {
        let node = Node::Expr(expr.id);
        self.add_tainted_node_to_sources(node, &sources);
        self.tainted_nodes.insert(node, TaintedNode { sources });
    }

    fn add_tainted_symbol(&mut self, name: &'a ClarityName, sources: HashSet<Node<'a>>) {
        let node = Node::Symbol(name);
        self.add_tainted_node_to_sources(node, &sources);
        self.tainted_nodes.insert(node, TaintedNode { sources });
    }

    // If this expression is tainted, add a diagnostic
    fn taint_check(&mut self, expr: &'a SymbolicExpression) {
        if self.tainted_nodes.contains_key(&Node::Expr(expr.id)) {
            self.diagnostics
                .insert(expr.id, self.generate_diagnostics(expr));
        }
    }

    fn filter_source(&mut self, source_node: &Node<'a>, rollback: bool) {
        if let Some(source) = self.taint_sources.remove(source_node) {
            self.tainted_nodes.remove(source_node);
            // Remove each taint source from its children
            for child in &source.children {
                if let Some(mut child_node) = self.tainted_nodes.remove(child) {
                    child_node.sources.remove(source_node);
                    // If the child is still tainted (by another source), add it back to the set
                    if !child_node.sources.is_empty() {
                        self.tainted_nodes.insert(*child, child_node);
                    } else if rollback {
                        if let Node::Expr(id) = child {
                            // Remove any prior diagnostics for this node
                            self.diagnostics.remove(id);
                        }
                    }
                } else if rollback {
                    if let Node::Expr(id) = child {
                        // Remove any prior diagnostics for this node
                        self.diagnostics.remove(id);
                    }
                }
            }
        }
    }

    // Filter any taint sources used in this expression
    fn filter_taint(&mut self, expr: &SymbolicExpression, rollback: bool) {
        let node = Node::Expr(expr.id);
        // Remove this node from the set of tainted nodes
        if let Some(removed_node) = self.tainted_nodes.remove(&node) {
            // Remove its sources of taint
            for source_node in &removed_node.sources {
                self.filter_source(source_node, rollback);
            }
        }
    }

    fn filter_all(&mut self) {
        self.tainted_nodes.clear();
    }

    // Check for annotations that should be attached to the given span
    fn process_annotations(&mut self, span: &Span) {
        self.active_annotation = None;

        for (i, annotation) in self.annotations.iter().enumerate() {
            if annotation.span.start_line == (span.start_line - 1) {
                self.active_annotation = Some(i);
                return;
            } else if annotation.span.start_line >= span.start_line {
                // The annotations are ordered by span, so if we have passed
                // the target line, return.
                return;
            }
        }
    }

    // Check if the expression is annotated with `allow(unchecked_data)`
    fn allow_unchecked_data(&self) -> bool {
        if let Some(idx) = self.active_annotation {
            let annotation = &self.annotations[idx];
            return matches!(
                annotation.kind,
                AnnotationKind::Allow(WarningKind::UncheckedData)
            );
        }
        false
    }

    // Check if the expression is annotated with `allow(unchecked_params)`
    fn allow_unchecked_params(&self) -> bool {
        if let Some(idx) = self.active_annotation {
            let annotation = &self.annotations[idx];
            return matches!(
                annotation.kind,
                AnnotationKind::Allow(WarningKind::UncheckedParams)
            );
        }
        false
    }

    fn apply_filters(&mut self) {
        if let Some(n) = self.active_annotation {
            let params = match &self.annotations[n].kind {
                AnnotationKind::Filter(params) => params,
                &AnnotationKind::FilterAll => {
                    self.filter_all();
                    return;
                }
                _ => return,
            };
            for param in params {
                let source = Node::Symbol(param);
                self.filter_source(&source, false);
            }
        }
    }

    fn generate_diagnostics(&self, expr: &SymbolicExpression) -> Vec<Diagnostic> {
        let mut diagnostics: Vec<Diagnostic> = Vec::new();
        let diagnostic = Diagnostic {
            level: Level::Warning,
            message: "use of potentially unchecked data".to_string(),
            spans: vec![expr.span.clone()],
            suggestion: None,
        };
        diagnostics.push(diagnostic);

        let tainted = &self.tainted_nodes[&Node::Expr(expr.id)];
        // Add a note for each source, ordered by span
        let mut source_spans = vec![];
        for source in &tainted.sources {
            let span = self.taint_sources[source].span.clone();
            let pos = source_spans.binary_search(&span).unwrap_or_else(|e| e);
            source_spans.insert(pos, span);
        }
        for span in source_spans {
            let diagnostic = Diagnostic {
                level: Level::Note,
                message: "source of untrusted input here".to_string(),
                spans: vec![span],
                suggestion: None,
            };
            diagnostics.push(diagnostic);
        }
        diagnostics
    }
}

impl<'a> ASTVisitor<'a> for CheckChecker<'a> {
    fn traverse_expr(&mut self, expr: &'a SymbolicExpression) -> bool {
        self.process_annotations(&expr.span);
        // If this expression is annotated to allow unchecked data, no need to
        // traverse it.
        if self.allow_unchecked_data() {
            return true;
        }
        let result = match &expr.expr {
            AtomValue(value) => self.visit_atom_value(expr, value),
            Atom(name) => self.visit_atom(expr, name),
            List(exprs) => self.traverse_list(expr, exprs),
            LiteralValue(value) => self.visit_literal_value(expr, value),
            Field(field) => self.visit_field(expr, field),
            TraitReference(name, trait_def) => self.visit_trait_reference(expr, name, trait_def),
        };

        self.apply_filters();
        result
    }

    fn traverse_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.public_funcs.insert(name);

        self.taint_sources.clear();
        self.tainted_nodes.clear();

        // Upon entering a public function, all parameters are tainted
        if let Some(params) = parameters {
            for param in params {
                if !is_param_type_excluded_from_checked_requirement(&param) {
                    self.add_taint_source(Node::Symbol(param.name), param.decl_span);
                }
            }
        }
        self.traverse_expr(body)
    }

    fn visit_define_read_only(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.public_funcs.insert(name);
        true
    }

    fn traverse_define_private(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.taint_sources.clear();
        self.tainted_nodes.clear();
        let mut info = FunctionInfo {
            unchecked_params: vec![],
            filtered_params: vec![],
        };

        // Upon entering a private function, parameters are considered checked,
        // unless the function is annotated otherwise.
        // TODO: for now, it is all or none, but later, allow to specify which
        // parameters can be unchecked
        let allow = self.allow_unchecked_params();
        if let Some(params) = &parameters {
            let mut unchecked_params = vec![false; params.len()];
            for (i, param) in params.iter().enumerate() {
                unchecked_params[i] = allow;
                if (allow || self.settings.callee_filter)
                    && !is_param_type_excluded_from_checked_requirement(param)
                {
                    self.add_taint_source(Node::Symbol(param.name), param.decl_span.clone());
                }
            }
            info.unchecked_params = unchecked_params;
        }
        self.traverse_expr(body);

        // Check that the return value is not tainted
        self.taint_check(body);

        if let Some(params) = &parameters {
            let mut filtered = vec![false; params.len()];
            if allow || self.settings.callee_filter {
                for (i, param) in params.iter().enumerate() {
                    if !self.taint_sources.contains_key(&Node::Symbol(param.name)) {
                        filtered[i] = true;
                    }
                }
            }
            info.filtered_params = filtered;
        }

        self.user_funcs.insert(name, info);
        true
    }

    fn traverse_if(
        &mut self,
        expr: &'a SymbolicExpression,
        cond: &'a SymbolicExpression,
        then_expr: &'a SymbolicExpression,
        else_expr: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(cond);
        self.filter_taint(cond, false);

        self.traverse_expr(then_expr);
        self.traverse_expr(else_expr);
        true
    }

    fn traverse_lazy_logical(
        &mut self,
        expr: &'a SymbolicExpression,
        function: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        for operand in operands {
            self.traverse_expr(operand);
            self.filter_taint(operand, false);
        }
        true
    }

    fn traverse_let(
        &mut self,
        expr: &'a SymbolicExpression,
        bindings: &HashMap<&'a ClarityName, &'a SymbolicExpression>,
        body: &'a [SymbolicExpression],
    ) -> bool {
        for (name, val) in bindings {
            if !self.traverse_expr(val) {
                return false;
            }
            if let Some(tainted) = self.tainted_nodes.get(&Node::Expr(val.id)) {
                let sources = tainted.sources.clone();
                // If the expression is tainted, add it to the map
                self.add_taint_source_symbol(name, expr.span.clone());
                self.add_tainted_symbol(name, sources);
            }
        }

        for expr in body {
            if !self.traverse_expr(expr) {
                return false;
            }
        }

        // The let expression returns the value of the last body expression,
        // so use that to determine if the let itself is tainted.
        if let Some(last_expr) = body.last() {
            if let Some(tainted) = self.tainted_nodes.get(&Node::Expr(last_expr.id)) {
                let sources = tainted.sources.clone();
                self.add_tainted_expr(expr, sources);
            }
        }

        for (name, val) in bindings {
            // Outside the scope of the let, remove this name
            let node = Node::Symbol(name);
            self.taint_sources.remove(&node);
            self.tainted_nodes.remove(&node);
        }
        true
    }

    fn traverse_begin(
        &mut self,
        expr: &'a SymbolicExpression,
        statements: &'a [SymbolicExpression],
    ) -> bool {
        for stmt in statements {
            if !self.traverse_expr(stmt) {
                return false;
            }
        }

        // the value of the begin is determined by the last expression
        if let Some(tainted) = &self.tainted_nodes.get(&Node::Expr(expr.id)) {
            let sources = tainted.sources.clone();
            self.add_tainted_expr(expr, sources);
        }

        true
    }

    fn traverse_as_contract(
        &mut self,
        expr: &'a SymbolicExpression,
        inner: &'a SymbolicExpression,
    ) -> bool {
        self.in_as_contract = true;
        let res = self.traverse_expr(inner) && self.visit_as_contract(expr, inner);
        self.in_as_contract = false;
        res
    }

    fn visit_asserts(
        &mut self,
        expr: &'a SymbolicExpression,
        cond: &'a SymbolicExpression,
        thrown: &'a SymbolicExpression,
    ) -> bool {
        self.filter_taint(cond, true);
        true
    }

    fn visit_atom(&mut self, expr: &'a SymbolicExpression, atom: &'a ClarityName) -> bool {
        if let Some(tainted) = self.tainted_nodes.get(&Node::Symbol(atom)) {
            let sources = tainted.sources.clone();
            self.add_tainted_expr(expr, sources);
        }
        true
    }

    fn visit_list(&mut self, expr: &'a SymbolicExpression, list: &[SymbolicExpression]) -> bool {
        let mut sources = HashSet::new();

        // For expressions with unique properties, tainted-ness is handled
        // inside the traverse_* method.
        if let Some((function_name, args)) = list.split_first() {
            if let Some(function_name) = function_name.match_atom() {
                if let Some(define_function) = DefineFunctions::lookup_by_name(function_name) {
                    return true;
                } else if let Some(native_function) = NativeFunctions::lookup_by_name_at_version(
                    function_name,
                    &ClarityVersion::latest(),
                ) {
                    use clarity::vm::functions::NativeFunctions::*;
                    match native_function {
                        Let => return true,
                        Begin => return true,
                        _ => {}
                    }
                }
            }
        }

        // For other nodes, if any of the children are tainted, the node is
        // tainted.
        for child in list {
            if let Some(tainted) = self.tainted_nodes.get(&Node::Expr(child.id)) {
                sources.extend(tainted.sources.clone());
            }
        }
        if !sources.is_empty() {
            self.add_tainted_expr(expr, sources);
        }
        true
    }

    fn visit_stx_burn(
        &mut self,
        expr: &'a SymbolicExpression,
        amount: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
    ) -> bool {
        // Input from the sender can be used un-checked to interact with the
        // sender's assets. The sender is protected by post-conditions.
        if match_tx_sender(sender) && !self.in_as_contract {
            return true;
        }
        self.taint_check(amount);
        self.taint_check(sender);
        true
    }

    fn visit_stx_transfer(
        &mut self,
        expr: &'a SymbolicExpression,
        amount: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
        recipient: &'a SymbolicExpression,
        memo: Option<&'a SymbolicExpression>,
    ) -> bool {
        // Input from the sender can be used un-checked to interact with the
        // sender's assets. The sender is protected by post-conditions.
        if match_tx_sender(sender) && !self.in_as_contract {
            return true;
        }
        self.taint_check(amount);
        self.taint_check(sender);
        self.taint_check(recipient);
        true
    }

    fn visit_ft_burn(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        amount: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
    ) -> bool {
        // Input from the sender can be used un-checked to interact with the
        // sender's assets. The sender is protected by post-conditions.
        if match_tx_sender(sender) && !self.in_as_contract {
            return true;
        }
        self.taint_check(amount);
        self.taint_check(sender);
        true
    }

    fn visit_ft_transfer(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        amount: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
        recipient: &'a SymbolicExpression,
    ) -> bool {
        // Input from the sender can be used un-checked to interact with the
        // sender's assets. The sender is protected by post-conditions.
        if match_tx_sender(sender) && !self.in_as_contract {
            return true;
        }
        self.taint_check(amount);
        self.taint_check(sender);
        self.taint_check(recipient);
        true
    }

    fn visit_ft_mint(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        amount: &'a SymbolicExpression,
        recipient: &'a SymbolicExpression,
    ) -> bool {
        self.taint_check(amount);
        self.taint_check(recipient);
        true
    }

    fn visit_nft_burn(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        identifier: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
    ) -> bool {
        // Input from the sender can be used un-checked to interact with the
        // sender's assets. The sender is protected by post-conditions.
        if match_tx_sender(sender) && !self.in_as_contract {
            return true;
        }
        self.taint_check(identifier);
        self.taint_check(sender);
        true
    }

    fn visit_nft_transfer(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        identifier: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
        recipient: &'a SymbolicExpression,
    ) -> bool {
        // Input from the sender can be used un-checked to interact with the
        // sender's assets. The sender is protected by post-conditions.
        if match_tx_sender(sender) && !self.in_as_contract {
            return true;
        }
        self.taint_check(identifier);
        self.taint_check(sender);
        self.taint_check(recipient);
        true
    }

    fn visit_nft_mint(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        identifier: &'a SymbolicExpression,
        recipient: &'a SymbolicExpression,
    ) -> bool {
        self.taint_check(identifier);
        self.taint_check(recipient);
        true
    }

    fn visit_var_set(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        value: &'a SymbolicExpression,
    ) -> bool {
        self.taint_check(value);
        true
    }

    fn visit_map_set(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
        value: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        for key_val in key.values() {
            self.taint_check(key_val);
        }
        for val_val in value.values() {
            self.taint_check(val_val);
        }
        true
    }

    fn visit_map_insert(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
        value: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        for key_val in key.values() {
            self.taint_check(key_val);
        }
        for val_val in value.values() {
            self.taint_check(val_val);
        }
        true
    }

    fn visit_map_delete(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        for val in key.values() {
            self.taint_check(val);
        }
        true
    }

    fn visit_dynamic_contract_call(
        &mut self,
        expr: &'a SymbolicExpression,
        trait_ref: &'a SymbolicExpression,
        function_name: &'a ClarityName,
        args: &'a [SymbolicExpression],
    ) -> bool {
        self.taint_check(trait_ref);
        true
    }

    fn visit_call_user_defined(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        args: &'a [SymbolicExpression],
    ) -> bool {
        if !args.is_empty() {
            let default = vec![false; args.len()];
            if let Some(info) = self.user_funcs.get(name) {
                let unchecked_args = &info.unchecked_params.clone();
                let filtered_params = &info.filtered_params.clone();
                for (i, arg) in args.iter().enumerate() {
                    if filtered_params[i] {
                        if self.settings.callee_filter {
                            self.filter_taint(arg, false);
                        }
                    } else if !unchecked_args[i] {
                        self.taint_check(arg);
                    }
                }
            }
        }
        true
    }

    fn visit_comparison(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        if func != NativeFunctions::Equals {
            return true;
        }

        if (self.settings.trusted_sender
            && ((match_tx_sender(&operands[0])
                && !self.tainted_nodes.contains_key(&Node::Expr(operands[1].id)))
                || (match_tx_sender(&operands[1])
                    && !self.tainted_nodes.contains_key(&Node::Expr(operands[0].id)))))
            || (self.settings.trusted_caller
                && ((match_contract_caller(&operands[0])
                    && !self.tainted_nodes.contains_key(&Node::Expr(operands[1].id)))
                    || (match_contract_caller(&operands[1])
                        && !self.tainted_nodes.contains_key(&Node::Expr(operands[0].id)))))
        {
            // Save all of the current taint sources before clearing them.
            let sources = self.taint_sources.keys().cloned().collect();
            self.filter_all();
            // Set this expression to be tainted by all sources so that if it
            // is inside of an assert, it will clear warnings from earlier
            // statements as well.
            self.tainted_nodes
                .insert(Node::Expr(expr.id), TaintedNode { sources });
        }
        true
    }
}

fn is_param_type_excluded_from_checked_requirement(param: &TypedVar) -> bool {
    matches!(
        TypeSignature::parse_type_repr(DEFAULT_EPOCH, param.type_expr, &mut ()),
        Ok(TypeSignature::BoolType)
    )
}

impl AnalysisPass for CheckChecker<'_> {
    fn run_pass(
        contract_analysis: &mut ContractAnalysis,
        analysis_db: &mut AnalysisDatabase,
        annotations: &Vec<Annotation>,
        settings: &analysis::Settings,
    ) -> AnalysisResult {
        let checker = CheckChecker::new(annotations, settings.check_checker);
        checker.run(contract_analysis)
    }
}

fn match_tx_sender(expr: &SymbolicExpression) -> bool {
    if let Some(name) = expr.match_atom() {
        if name.as_str() == "tx-sender" {
            return true;
        }
    }
    false
}

fn match_contract_caller(expr: &SymbolicExpression) -> bool {
    if let Some(name) = expr.match_atom() {
        if name.as_str() == "contract-caller" {
            return true;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use crate::analysis::Pass;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    #[test]
    fn allow_unchecked_bool_in_private_function_with_unchecked_params_annotation() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-data-var p1 principal tx-sender)
(define-data-var b1 bool false)
;; #[allow(unchecked_params)]
(define-private (my-func-p (p principal) (b bool))
    (begin
        (var-set p1 p)
        (var-set b1 b)
    )
)"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, result)) => {
                assert_eq!(result.diagnostics.len(), 2);
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:7:21: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "        (var-set p1 p)");
                assert_eq!(output[2], "                    ^");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:5:29: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[4],
                    "(define-private (my-func-p (p principal) (b bool))"
                );
                assert_eq!(output[5], "                            ^");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn allow_unchecked_bool_in_public_function() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-data-var myvar bool false)
(define-public (tainted-var-set (b bool))
    (ok (var-set myvar b))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn define_public() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (tainted (amount uint))
    (stx-transfer? amount (as-contract tx-sender) tx-sender)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:3:20: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "    (stx-transfer? amount (as-contract tx-sender) tx-sender)"
                );
                assert_eq!(output[2], "                   ^~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:2:26: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (tainted (amount uint))");
                assert_eq!(output[5], "                         ^~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn expr_tainted() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (expr-tainted (amount uint))
    (stx-transfer? (+ u10 amount) (as-contract tx-sender) tx-sender)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:3:20: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "    (stx-transfer? (+ u10 amount) (as-contract tx-sender) tx-sender)"
                );
                assert_eq!(output[2], "                   ^~~~~~~~~~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:2:31: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (expr-tainted (amount uint))");
                assert_eq!(output[5], "                              ^~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn let_tainted() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (let-tainted (amount uint))
    (let ((x amount))
        (stx-transfer? x (as-contract tx-sender) tx-sender)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:4:24: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "        (stx-transfer? x (as-contract tx-sender) tx-sender)"
                );
                assert_eq!(output[2], "                       ^");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:2:30: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (let-tainted (amount uint))");
                assert_eq!(output[5], "                             ^~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn filtered() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (filtered (amount uint))
    (begin
        (asserts! (< amount u100) (err u100))
        (stx-transfer? amount (as-contract tx-sender) tx-sender)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn filtered_expr() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (filtered-expr (amount uint))
    (begin
        (asserts! (< (+ amount u10) u100) (err u100))
        (stx-transfer? amount (as-contract tx-sender) tx-sender)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn let_filtered() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (let-filtered (amount uint))
    (let ((x amount))
        (asserts! (< x u100) (err u100))
        (stx-transfer? x (as-contract tx-sender) tx-sender)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn let_filtered_parent() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (let-filtered-parent (amount uint))
    (let ((x amount))
        (asserts! (< amount u100) (err u100))
        (stx-transfer? x (as-contract tx-sender) tx-sender)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn let_tainted_twice() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (let-tainted-twice (amount1 uint) (amount2 uint))
    (let ((x (+ amount1 amount2)))
        (stx-transfer? x (as-contract tx-sender) tx-sender)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 9);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:4:24: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "        (stx-transfer? x (as-contract tx-sender) tx-sender)"
                );
                assert_eq!(output[2], "                       ^");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:2:36: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[4],
                    "(define-public (let-tainted-twice (amount1 uint) (amount2 uint))"
                );
                assert_eq!(output[5], "                                   ^~~~~~~");
                assert_eq!(
                    output[6],
                    format!(
                        "checker:2:51: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[7],
                    "(define-public (let-tainted-twice (amount1 uint) (amount2 uint))"
                );
                assert_eq!(
                    output[8],
                    "                                                  ^~~~~~~"
                );
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn let_tainted_twice_filtered_once() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (let-tainted-twice-filtered-once (amount1 uint) (amount2 uint))
    (let ((x (+ amount1 amount2)))
        (asserts! (< amount1 u100) (err u100))
        (stx-transfer? x (as-contract tx-sender) tx-sender)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:5:24: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "        (stx-transfer? x (as-contract tx-sender) tx-sender)"
                );
                assert_eq!(output[2], "                       ^");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:2:65: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (let-tainted-twice-filtered-once (amount1 uint) (amount2 uint))");
                assert_eq!(
                    output[5],
                    "                                                                ^~~~~~~"
                );
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn let_tainted_twice_filtered_twice() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (let-tainted-twice-filtered-twice (amount1 uint) (amount2 uint))
    (let ((x (+ amount1 amount2)))
        (asserts! (< amount1 u100) (err u100))
        (asserts! (< amount2 u100) (err u101))
        (stx-transfer? x (as-contract tx-sender) tx-sender)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn let_tainted_twice_filtered_together() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (let-tainted-twice-filtered-together (amount1 uint) (amount2 uint))
    (let ((x (+ amount1 amount2)))
        (asserts! (< (+ amount1 amount2) u100) (err u100))
        (stx-transfer? x (as-contract tx-sender) tx-sender)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn if_filter() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (if-filter (amount uint))
    (stx-transfer? (if (< amount u100) amount u100) (as-contract tx-sender) tx-sender)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn if_not_filtered() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (if-not-filtered (amount uint))
    (stx-transfer? (if (< u50 u100) amount u100) (as-contract tx-sender) tx-sender)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:3:20: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "    (stx-transfer? (if (< u50 u100) amount u100) (as-contract tx-sender) tx-sender)");
                assert_eq!(
                    output[2],
                    "                   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                );
                assert_eq!(
                    output[3],
                    format!(
                        "checker:2:34: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (if-not-filtered (amount uint))");
                assert_eq!(output[5], "                                 ^~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn and_tainted() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (and-tainted (amount uint))
    (ok (and
        (unwrap-panic (stx-transfer? amount (as-contract tx-sender) tx-sender))
    ))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:4:38: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "        (unwrap-panic (stx-transfer? amount (as-contract tx-sender) tx-sender))");
                assert_eq!(output[2], "                                     ^~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:2:30: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (and-tainted (amount uint))");
                assert_eq!(output[5], "                             ^~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn and_filter() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (and-filter (amount uint))
    (ok (and
        (< amount u100)
        (unwrap-panic (stx-transfer? amount (as-contract tx-sender) tx-sender))
    ))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn and_filter_after() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (and-filter-after (amount uint))
    (ok (and
        (unwrap-panic (stx-transfer? amount (as-contract tx-sender) tx-sender))
        (< amount u100)
    ))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:4:38: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "        (unwrap-panic (stx-transfer? amount (as-contract tx-sender) tx-sender))");
                assert_eq!(output[2], "                                     ^~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:2:35: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (and-filter-after (amount uint))");
                assert_eq!(output[5], "                                  ^~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn or_tainted() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (or-tainted (amount uint))
    (ok (or
        (unwrap-panic (stx-transfer? amount (as-contract tx-sender) tx-sender))
    ))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:4:38: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "        (unwrap-panic (stx-transfer? amount (as-contract tx-sender) tx-sender))");
                assert_eq!(output[2], "                                     ^~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:2:29: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (or-tainted (amount uint))");
                assert_eq!(output[5], "                            ^~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn or_filter() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (or-filter (amount uint))
    (ok (or
        (< amount u100)
        (unwrap-panic (stx-transfer? amount (as-contract tx-sender) tx-sender))
    ))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn or_filter_after() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (or-filter-after (amount uint))
    (ok (or
        (unwrap-panic (stx-transfer? amount (as-contract tx-sender) tx-sender))
        (< amount u100)
    ))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:4:38: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "        (unwrap-panic (stx-transfer? amount (as-contract tx-sender) tx-sender))");
                assert_eq!(output[2], "                                     ^~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:2:34: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (or-filter-after (amount uint))");
                assert_eq!(output[5], "                                 ^~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn stx_burn_senders() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (stx-burn-senders (amount uint))
    (stx-burn? amount tx-sender)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn tainted_stx_burn() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (tainted-stx-burn (amount uint))
    (begin
        (try! (stx-burn? amount (as-contract tx-sender)))
        (as-contract (stx-burn? amount tx-sender))
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 12);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:4:26: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "        (try! (stx-burn? amount (as-contract tx-sender)))"
                );
                assert_eq!(output[2], "                         ^~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:2:35: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (tainted-stx-burn (amount uint))");
                assert_eq!(output[5], "                                  ^~~~~~");
                assert_eq!(
                    output[6],
                    format!(
                        "checker:5:33: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn stx_transfer_senders() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (stx-transfer-senders (amount uint) (recipient principal))
    (stx-transfer? amount tx-sender recipient)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn tainted_ft_burn() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-fungible-token stackaroo)
(define-public (tainted-ft-burn (amount uint))
    (begin
        (try! (ft-burn? stackaroo amount (as-contract tx-sender)))
        (as-contract (ft-burn? stackaroo amount tx-sender))
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 12);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:5:35: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "        (try! (ft-burn? stackaroo amount (as-contract tx-sender)))"
                );
                assert_eq!(output[2], "                                  ^~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:34: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (tainted-ft-burn (amount uint))");
                assert_eq!(output[5], "                                 ^~~~~~");
                assert_eq!(
                    output[6],
                    format!(
                        "checker:6:42: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn ft_burn_senders() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-fungible-token stackaroo)
(define-public (ft-burn-senders (amount uint))
    (ft-burn? stackaroo amount tx-sender)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn tainted_ft_transfer() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-fungible-token stackaroo)
(define-public (tainted-ft-transfer (amount uint))
    (let ((sender tx-sender))
        (try! (ft-transfer? stackaroo amount (as-contract tx-sender) tx-sender))
        (as-contract (ft-transfer? stackaroo amount tx-sender sender))
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 12);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:5:39: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "        (try! (ft-transfer? stackaroo amount (as-contract tx-sender) tx-sender))"
                );
                assert_eq!(output[2], "                                      ^~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:38: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[4],
                    "(define-public (tainted-ft-transfer (amount uint))"
                );
                assert_eq!(output[5], "                                     ^~~~~~");
                assert_eq!(
                    output[6],
                    format!(
                        "checker:6:46: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn ft_transfer_senders() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-fungible-token stackaroo)
(define-public (ft-transfer-senders (amount uint) (recipient principal))
    (ft-transfer? stackaroo amount tx-sender recipient)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn tainted_ft_mint() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-fungible-token stackaroo)
(define-public (tainted-ft-mint (amount uint))
    (begin
        (try! (ft-mint? stackaroo amount (as-contract tx-sender)))
        (as-contract (ft-mint? stackaroo amount tx-sender))
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 12);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:5:35: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "        (try! (ft-mint? stackaroo amount (as-contract tx-sender)))"
                );
                assert_eq!(output[2], "                                  ^~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:34: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (tainted-ft-mint (amount uint))");
                assert_eq!(output[5], "                                 ^~~~~~");
                assert_eq!(
                    output[6],
                    format!(
                        "checker:6:42: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn tainted_nft_burn() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-non-fungible-token stackaroo uint)
(define-public (tainted-nft-burn (identifier uint))
    (begin
        (try! (nft-burn? stackaroo identifier (as-contract tx-sender)))
        (as-contract (nft-burn? stackaroo identifier tx-sender))
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 12);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:5:36: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "        (try! (nft-burn? stackaroo identifier (as-contract tx-sender)))"
                );
                assert_eq!(output[2], "                                   ^~~~~~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:35: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[4],
                    "(define-public (tainted-nft-burn (identifier uint))"
                );
                assert_eq!(output[5], "                                  ^~~~~~~~~~");
                assert_eq!(
                    output[6],
                    format!(
                        "checker:6:43: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn nft_burn_senders() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-non-fungible-token stackaroo uint)
(define-public (nft-burn-senders (identifier uint))
    (nft-burn? stackaroo identifier tx-sender)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn tainted_nft_transfer() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-non-fungible-token stackaroo uint)
(define-public (tainted-nft-transfer (identifier uint))
    (let ((sender tx-sender))
        (try! (nft-transfer? stackaroo identifier (as-contract tx-sender) tx-sender))
        (as-contract (nft-transfer? stackaroo identifier tx-sender sender))
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 12);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:5:40: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "        (try! (nft-transfer? stackaroo identifier (as-contract tx-sender) tx-sender))"
                );
                assert_eq!(
                    output[2],
                    "                                       ^~~~~~~~~~"
                );
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:39: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[4],
                    "(define-public (tainted-nft-transfer (identifier uint))"
                );
                assert_eq!(
                    output[5],
                    "                                      ^~~~~~~~~~"
                );
                assert_eq!(
                    output[6],
                    format!(
                        "checker:6:47: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn nft_transfer_senders() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-non-fungible-token stackaroo uint)
(define-public (nft-transfer-senders (identifier uint) (recipient principal))
    (nft-transfer? stackaroo identifier tx-sender recipient)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn tainted_nft_mint() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-non-fungible-token stackaroo uint)
(define-public (tainted-nft-mint (identifier uint))
    (begin
        (try! (nft-mint? stackaroo identifier (as-contract tx-sender)))
        (as-contract (nft-mint? stackaroo identifier tx-sender))
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 12);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:5:36: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "        (try! (nft-mint? stackaroo identifier (as-contract tx-sender)))"
                );
                assert_eq!(output[2], "                                   ^~~~~~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:35: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[4],
                    "(define-public (tainted-nft-mint (identifier uint))"
                );
                assert_eq!(output[5], "                                  ^~~~~~~~~~");
                assert_eq!(
                    output[6],
                    format!(
                        "checker:6:43: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn tainted_var_set() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-data-var myvar uint u0)
(define-public (tainted-var-set (amount uint))
    (ok (var-set myvar amount))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:4:24: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "    (ok (var-set myvar amount))");
                assert_eq!(output[2], "                       ^~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:34: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (tainted-var-set (amount uint))");
                assert_eq!(output[5], "                                 ^~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn tainted_map_set() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-map mymap { key-name-1: uint } { val-name-1: int })
(define-public (tainted-map-set (key uint) (value int))
    (ok (map-set mymap {key-name-1: key} {val-name-1: value}))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 12);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:4:37: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "    (ok (map-set mymap {key-name-1: key} {val-name-1: value}))"
                );
                assert_eq!(output[2], "                                    ^~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:34: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[4],
                    "(define-public (tainted-map-set (key uint) (value int))"
                );
                assert_eq!(output[5], "                                 ^~~");
                assert_eq!(
                    output[6],
                    format!(
                        "checker:4:55: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[7],
                    "    (ok (map-set mymap {key-name-1: key} {val-name-1: value}))"
                );
                assert_eq!(
                    output[8],
                    "                                                      ^~~~~"
                );
                assert_eq!(
                    output[9],
                    format!(
                        "checker:3:45: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[10],
                    "(define-public (tainted-map-set (key uint) (value int))"
                );
                assert_eq!(
                    output[11],
                    "                                            ^~~~~"
                );
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn tainted_map_set2() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-map mymap uint int)
(define-public (tainted-map-set (key uint) (value int))
    (ok (map-set mymap key value))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 12);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:4:24: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "    (ok (map-set mymap key value))");
                assert_eq!(output[2], "                       ^~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:34: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[4],
                    "(define-public (tainted-map-set (key uint) (value int))"
                );
                assert_eq!(output[5], "                                 ^~~");
                assert_eq!(
                    output[6],
                    format!(
                        "checker:4:28: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[7], "    (ok (map-set mymap key value))");
                assert_eq!(output[8], "                           ^~~~~");
                assert_eq!(
                    output[9],
                    format!(
                        "checker:3:45: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[10],
                    "(define-public (tainted-map-set (key uint) (value int))"
                );
                assert_eq!(
                    output[11],
                    "                                            ^~~~~"
                );
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn tainted_map_insert() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-map mymap { key-name-1: uint } { val-name-1: int })
(define-public (tainted-map-insert (key uint) (value int))
    (ok (map-insert mymap {key-name-1: key} {val-name-1: value}))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 12);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:4:40: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "    (ok (map-insert mymap {key-name-1: key} {val-name-1: value}))"
                );
                assert_eq!(output[2], "                                       ^~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:37: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[4],
                    "(define-public (tainted-map-insert (key uint) (value int))"
                );
                assert_eq!(output[5], "                                    ^~~");
                assert_eq!(
                    output[6],
                    format!(
                        "checker:4:58: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[7],
                    "    (ok (map-insert mymap {key-name-1: key} {val-name-1: value}))"
                );
                assert_eq!(
                    output[8],
                    "                                                         ^~~~~"
                );
                assert_eq!(
                    output[9],
                    format!(
                        "checker:3:48: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[10],
                    "(define-public (tainted-map-insert (key uint) (value int))"
                );
                assert_eq!(
                    output[11],
                    "                                               ^~~~~"
                );
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn tainted_map_insert2() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-map mymap uint int)
(define-public (tainted-map-insert (key uint) (value int))
    (ok (map-insert mymap key value))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 12);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:4:27: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "    (ok (map-insert mymap key value))");
                assert_eq!(output[2], "                          ^~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:37: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[4],
                    "(define-public (tainted-map-insert (key uint) (value int))"
                );
                assert_eq!(output[5], "                                    ^~~");
                assert_eq!(
                    output[6],
                    format!(
                        "checker:4:31: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[7], "    (ok (map-insert mymap key value))");
                assert_eq!(output[8], "                              ^~~~~");
                assert_eq!(
                    output[9],
                    format!(
                        "checker:3:48: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[10],
                    "(define-public (tainted-map-insert (key uint) (value int))"
                );
                assert_eq!(
                    output[11],
                    "                                               ^~~~~"
                );
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn tainted_map_delete() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-map mymap { key-name-1: uint } { val-name-1: int })
(define-public (tainted-map-delete (key uint))
    (ok (map-delete mymap {key-name-1: key}))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:4:40: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "    (ok (map-delete mymap {key-name-1: key}))");
                assert_eq!(output[2], "                                       ^~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:37: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (tainted-map-delete (key uint))");
                assert_eq!(output[5], "                                    ^~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn dynamic_contract_call() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-trait multiplier
    ((multiply (uint uint) (response uint uint)))
)
(define-public (my-multiply (untrusted <multiplier>) (a uint) (b uint))
    (contract-call? untrusted multiply a b)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:6:21: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "    (contract-call? untrusted multiply a b)");
                assert_eq!(output[2], "                    ^~~~~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:5:30: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[4],
                    "(define-public (my-multiply (untrusted <multiplier>) (a uint) (b uint))"
                );
                assert_eq!(output[5], "                             ^~~~~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn check_private() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        settings.repl_settings.analysis.check_checker.callee_filter = false;
        let mut session = Session::new(settings);
        let snippet = "
(define-private (my-transfer (amount uint))
    (stx-transfer? amount (as-contract tx-sender) tx-sender)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn check_private_call() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-private (my-transfer (amount uint))
    (ok true)
)
(define-public (tainted (amount uint))
    (my-transfer amount)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:6:18: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "    (my-transfer amount)");
                assert_eq!(output[2], "                 ^~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:5:26: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (tainted (amount uint))");
                assert_eq!(output[5], "                         ^~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn check_private_after() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (tainted (amount uint))
    (my-func amount)
)
(define-private (my-func (amount uint))
    (ok true)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:3:14: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "    (my-func amount)");
                assert_eq!(output[2], "             ^~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:2:26: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (tainted (amount uint))");
                assert_eq!(output[5], "                         ^~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn check_private_allow() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
;; #[allow(unchecked_params)]
(define-private (my-transfer (amount uint))
    (begin
        (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
        (ok true)
    )
)
(define-public (tainted (amount uint))
    (my-transfer amount)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:5:30: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "        (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))"
                );
                assert_eq!(output[2], "                             ^~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:31: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-private (my-transfer (amount uint))");
                assert_eq!(output[5], "                              ^~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn check_private_return() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
;; #[allow(unchecked_params)]
(define-private (my-func (amount uint))
    (ok amount)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:4:5: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "    (ok amount)");
                assert_eq!(output[2], "    ^~~~~~~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:27: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-private (my-func (amount uint))");
                assert_eq!(output[5], "                          ^~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn check_private_return_cleaned() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
;; #[allow(unchecked_params)]
(define-private (cleaner (amount uint))
    (begin
        (asserts! (< amount u10) (err 1))
        (ok amount)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn check_private_return_clean() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
;; #[allow(unchecked_params)]
(define-private (cleaner (amount uint))
    (begin
        (+ amount u1)
        (ok true)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn unchecked_params_safe() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
;; #[allow(unchecked_params)]
(define-private (my-func (amount uint))
    (ok true)
)
(define-public (tainted (amount uint))
    (my-func amount)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn unchecked_params_safe_after() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (tainted (amount uint))
    (my-func amount)
)
;; #[allow(unchecked_params)]
(define-private (my-func (amount uint))
    (ok true)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn allow_unchecked_data() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (allow_tainted (amount uint))
    ;; #[allow(unchecked_data)]
    (stx-transfer? amount (as-contract tx-sender) tx-sender)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn allow_unchecked_data_parent() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (allow_tainted (amount uint))
    ;; #[allow(unchecked_data)]
    (let ((x (+ amount u1)))
        (stx-transfer? amount (as-contract tx-sender) tx-sender)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn allow_unchecked_data_function() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
;; #[allow(unchecked_data)]
(define-public (allow_tainted (amount uint))
    (stx-transfer? amount (as-contract tx-sender) tx-sender)
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn annotate_other_expr() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (tainted (amount uint))
    (begin
        ;; #[allow(unchecked_data)]
        (+ amount u1)
        (stx-transfer? amount (as-contract tx-sender) tx-sender)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:6:24: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "        (stx-transfer? amount (as-contract tx-sender) tx-sender)"
                );
                assert_eq!(output[2], "                       ^~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:2:26: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (tainted (amount uint))");
                assert_eq!(output[5], "                         ^~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn annotate_other_expr2() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (tainted (amount uint))
    (begin
        (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
        ;; #[allow(unchecked_data)]
        (ok (+ amount u1))
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:4:30: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "        (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))"
                );
                assert_eq!(output[2], "                             ^~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:2:26: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (tainted (amount uint))");
                assert_eq!(output[5], "                         ^~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn private_filter() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        settings.repl_settings.analysis.check_checker.callee_filter = true;
        let mut session = Session::new(settings);
        let snippet = "
(define-public (tainted (amount uint))
    (begin
        (try! (my-filter amount))
        (stx-transfer? amount (as-contract tx-sender) tx-sender)
    )
)

(define-private (my-filter (amount uint))
    (begin
        (asserts! (< amount u10) (err u100))
        (ok true)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn private_filter_indirect() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        settings.repl_settings.analysis.check_checker.callee_filter = true;
        let mut session = Session::new(settings);
        let snippet = "
(define-public (tainted (amount uint))
    (begin
        (try! (my-filter amount))
        (stx-transfer? amount (as-contract tx-sender) tx-sender)
    )
)

(define-private (my-filter (amount uint))
    (my-filter-inner amount)
)

(define-private (my-filter-inner (amount uint))
    (begin
        (asserts! (< amount u10) (err u100))
        (ok true)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn filter_all() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-data-var admin principal tx-sender)
(define-public (filter_all (amount uint))
    (begin
        ;; #[filter(*)]
        (asserts! (is-eq tx-sender (var-get admin)) (err u400))
        (stx-transfer? amount (as-contract tx-sender) tx-sender)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn filter_one() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-data-var admin principal tx-sender)
(define-public (filter_one (amount uint))
    (begin
        ;; #[filter(amount)]
        (asserts! (is-eq tx-sender (var-get admin)) (err u400))
        (stx-transfer? amount (as-contract tx-sender) tx-sender)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn filter_two() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-data-var admin principal tx-sender)
(define-public (filter_two (amount1 uint) (amount2 uint))
    (begin
        ;; #[filter(amount1, amount2)]
        (asserts! (is-eq tx-sender (var-get admin)) (err u400))
        (stx-transfer? (+ amount1 amount2) (as-contract tx-sender) tx-sender)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn filter_all2() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-data-var admin principal tx-sender)
(define-public (filter_all2 (amount1 uint) (amount2 uint))
    (begin
        ;; #[filter(*)]
        (asserts! (is-eq tx-sender (var-get admin)) (err u400))
        (stx-transfer? (+ amount1 amount2) (as-contract tx-sender) tx-sender)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn filter_one_of_two() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        settings.repl_settings.analysis.check_checker.trusted_sender = false;
        let mut session = Session::new(settings);
        let snippet = "
(define-data-var admin principal tx-sender)
(define-public (filter_one (amount1 uint) (amount2 uint))
    (begin
        ;; #[filter(amount2)]
        (asserts! (is-eq tx-sender (var-get admin)) (err u400))
        (stx-transfer? (+ amount1 amount2) (as-contract tx-sender) tx-sender)
    )
)
        "
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:7:24: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(
                    output[1],
                    "        (stx-transfer? (+ amount1 amount2) (as-contract tx-sender) tx-sender)"
                );
                assert_eq!(output[2], "                       ^~~~~~~~~~~~~~~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:29: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(
                    output[4],
                    "(define-public (filter_one (amount1 uint) (amount2 uint))"
                );
                assert_eq!(output[5], "                            ^~~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn filter_trait() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-trait my-trait
    (
        (my-method (uint) (response uint uint))
    )
)

(define-data-var principal-check principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)

(define-public (deposit (trait-contract <my-trait>))
    (begin
        (asserts! (is-eq (contract-of trait-contract) (var-get principal-check)) (err u0))
        (try! (as-contract (contract-call? trait-contract my-method u1)))
        (ok u1)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn check_after() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (filtered (amount uint))
    (begin
        (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
        (asserts! (< amount u100) (err u100))
        (ok true)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn check_after_callee() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        settings.repl_settings.analysis.check_checker.callee_filter = false;
        let mut session = Session::new(settings);
        let snippet = "
(define-private (my-transfer (amount uint))
    (begin
        (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
        (ok true)
    )
)
(define-public (filtered (amount uint))
    (begin
        (try! (my-transfer amount))
        (asserts! (< amount u100) (err u100))
        (ok true)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn trusted_sender() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        settings.repl_settings.analysis.check_checker.trusted_sender = true;
        let mut session = Session::new(settings);
        let snippet = "
(define-data-var owner principal tx-sender)
(define-public (set-owner (address principal))
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) (err u1))
        (ok (var-set owner address))
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn trusted_sender_after() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        settings.repl_settings.analysis.check_checker.trusted_sender = true;
        let mut session = Session::new(settings);
        let snippet = "
(define-data-var owner principal tx-sender)
(define-public (set-owner (address principal))
    (begin
        (var-set owner address)
        (asserts! (is-eq tx-sender (var-get owner)) (err u1))
        (ok true)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn trusted_sender_disabled() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        settings.repl_settings.analysis.check_checker.trusted_sender = false;
        let mut session = Session::new(settings);
        let snippet = "
(define-data-var owner principal tx-sender)
(define-public (set-owner (address principal))
    (begin
        (asserts! (is-eq tx-sender (var-get owner)) (err u1))
        (ok (var-set owner address))
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:6:28: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "        (ok (var-set owner address))");
                assert_eq!(output[2], "                           ^~~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:28: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (set-owner (address principal))");
                assert_eq!(output[5], "                           ^~~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn trusted_caller() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        settings.repl_settings.analysis.check_checker.trusted_caller = true;
        let mut session = Session::new(settings);
        let snippet = "
(define-data-var owner principal tx-sender)
(define-public (set-owner (address principal))
    (begin
        (asserts! (is-eq contract-caller (var-get owner)) (err u1))
        (ok (var-set owner address))
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn trusted_caller_after() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        settings.repl_settings.analysis.check_checker.trusted_caller = true;
        let mut session = Session::new(settings);
        let snippet = "
(define-data-var owner principal tx-sender)
(define-public (set-owner (address principal))
    (begin
        (var-set owner address)
        (asserts! (is-eq contract-caller (var-get owner)) (err u1))
        (ok true)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn trusted_caller_disabled() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        settings.repl_settings.analysis.check_checker.trusted_caller = false;
        let mut session = Session::new(settings);
        let snippet = "
(define-data-var owner principal tx-sender)
(define-public (set-owner (address principal))
    (begin
        (asserts! (is-eq contract-caller (var-get owner)) (err u1))
        (ok (var-set owner address))
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:6:28: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "        (ok (var-set owner address))");
                assert_eq!(output[2], "                           ^~~~~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:3:28: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (set-owner (address principal))");
                assert_eq!(output[5], "                           ^~~~~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn callee_filter() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        settings.repl_settings.analysis.check_checker.callee_filter = true;
        let mut session = Session::new(settings);
        let snippet = "
(define-private (write-data (data uint))
    (begin
        (asserts! (< u10 data) (err 400))
        (ok true)
    )
)

(define-data-var saved uint u0)

(define-public (handle-one (arg1 uint))
    (begin
        (try! (write-data arg1))
        (var-set saved arg1)
        (ok true)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn callee_filter_disabled() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CheckChecker];
        settings.repl_settings.analysis.check_checker.callee_filter = false;
        let mut session = Session::new(settings);
        let snippet = "
;; #[allow(unchecked_params)]
(define-private (write-data (data uint))
    (begin
        (asserts! (< u10 data) (err 400))
        (ok true)
    )
)

(define-data-var saved uint u0)

(define-public (handle-one (arg1 uint))
    (begin
        (try! (write-data arg1))
        (var-set saved arg1)
        (ok true)
    )
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, _)) => {
                assert_eq!(output.len(), 6);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:15:24: {} use of potentially unchecked data",
                        yellow!("warning:")
                    )
                );
                assert_eq!(output[1], "        (var-set saved arg1)");
                assert_eq!(output[2], "                       ^~~~");
                assert_eq!(
                    output[3],
                    format!(
                        "checker:12:28: {} source of untrusted input here",
                        blue!("note:")
                    )
                );
                assert_eq!(output[4], "(define-public (handle-one (arg1 uint))");
                assert_eq!(output[5], "                           ^~~~");
            }
            _ => panic!("Expected successful interpretation"),
        };
    }
}
