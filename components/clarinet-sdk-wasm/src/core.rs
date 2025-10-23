use std::collections::{BTreeMap, HashMap};
use std::panic;
use std::path::PathBuf;

use clarinet_deployments::diagnostic_digest::DiagnosticsDigest;
use clarinet_deployments::types::{
    DeploymentSpecification, DeploymentSpecificationFile, EmulatedContractPublishSpecification,
    TransactionSpecification,
};
use clarinet_deployments::{
    generate_default_deployment, initiate_session_from_manifest,
    update_session_with_deployment_plan,
};
use clarinet_files::{
    FileAccessor, FileLocation, ProjectManifest, StacksNetwork, WASMFileSystemAccessor,
};
use clarity_repl::clarity::analysis::contract_interface_builder::{
    ContractInterface, ContractInterfaceFunction, ContractInterfaceFunctionAccess,
};
use clarity_repl::clarity::chainstate::StacksAddress;
use clarity_repl::clarity::vm::types::{
    PrincipalData, QualifiedContractIdentifier, StandardPrincipalData,
};
use clarity_repl::clarity::{
    Address, ClarityVersion, EvaluationResult, ExecutionResult, StacksEpochId, SymbolicExpression,
};
use clarity_repl::repl::clarity_values::{uint8_to_string, uint8_to_value};
use clarity_repl::repl::hooks::perf::CostField;
use clarity_repl::repl::session::CostsReport;
use clarity_repl::repl::settings::RemoteDataSettings;
use clarity_repl::repl::{
    clarity_values, ClarityCodeSource, ClarityContract, ContractDeployer, Epoch, Session,
    SessionSettings, DEFAULT_CLARITY_VERSION, DEFAULT_EPOCH,
};
use gloo_utils::format::JsValueSerdeExt;
use js_sys::Function as JsFunction;
use serde::{Deserialize, Serialize};
use serde_json::json;
use serde_wasm_bindgen::to_value as encode_to_js;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsValue;

use crate::utils::events::serialize_event;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(typescript_type = "Map<string, Map<string, bigint>>")]
    pub type AssetsMap;
    #[wasm_bindgen(typescript_type = "Map<string, string>")]
    pub type Accounts;
    #[wasm_bindgen(typescript_type = "EpochString")]
    pub type EpochString;
    #[wasm_bindgen(typescript_type = "ClarityVersionString")]
    pub type ClarityVersionString;
    #[wasm_bindgen(typescript_type = "IContractAST")]
    pub type IContractAST;
    #[wasm_bindgen(typescript_type = "Map<string, IContractInterface>")]
    pub type IContractInterfaces;
}

impl EpochString {
    pub fn new(obj: &str) -> Self {
        Self {
            obj: JsValue::from_str(obj),
        }
    }
}

macro_rules! log {
    ( $( $t:tt )* ) => {
        web_sys::console::log_1(&format!( $( $t )* ).into());
    }
}

#[derive(Debug, Deserialize)]
struct CallContractArgsJSON {
    contract: String,
    method: String,
    args_maps: Vec<HashMap<usize, u8>>,
    sender: String,
}

#[derive(Debug, Deserialize)]
#[wasm_bindgen]
pub struct CallFnArgs {
    contract: String,
    method: String,
    args: Vec<Vec<u8>>,
    sender: String,
}

#[wasm_bindgen]
impl CallFnArgs {
    #[wasm_bindgen(constructor)]
    pub fn new(
        contract: String,
        method: String,
        args: Vec<js_sys::Uint8Array>,
        sender: String,
    ) -> Self {
        Self {
            contract,
            method,
            args: args.into_iter().map(|a| a.to_vec()).collect(),
            sender,
        }
    }

    /*
      The mineBlock method receives an JSON Array of Txs, including ContractCalls.
      Because it's JSON, the Uint8Array arguments are passed as Map<index, value> instead of Vec<u8>.
      This method transform the Map back into a Vec.
    */
    fn from_json_args(json_args: CallContractArgsJSON) -> Self {
        let args = json_args
            .args_maps
            .into_iter()
            .map(|arg| {
                let mut parsed_arg = vec![0; arg.len()];
                for (i, v) in arg {
                    parsed_arg[i] = v;
                }
                parsed_arg
            })
            .collect();

        Self {
            contract: json_args.contract,
            method: json_args.method,
            args,
            sender: json_args.sender,
        }
    }
}

#[derive(Debug, Deserialize)]
#[wasm_bindgen]
pub struct ContractOptions {
    clarity_version: ClarityVersion,
}

#[wasm_bindgen]
impl ContractOptions {
    #[wasm_bindgen(constructor)]
    pub fn new(clarity_version: Option<u32>) -> Self {
        let clarity_version = match clarity_version {
            Some(v) => match v {
                1 => ClarityVersion::Clarity1,
                2 => ClarityVersion::Clarity2,
                3 => ClarityVersion::Clarity3,
                4 => ClarityVersion::Clarity4,
                _ => {
                    log!("Invalid clarity version {v}. Using default version.");
                    DEFAULT_CLARITY_VERSION
                }
            },
            _ => DEFAULT_CLARITY_VERSION,
        };

        Self { clarity_version }
    }
}

#[derive(Debug, Deserialize)]
#[wasm_bindgen]
pub struct DeployContractArgs {
    name: String,
    content: String,
    options: ContractOptions,
    sender: String,
}
#[wasm_bindgen]
impl DeployContractArgs {
    #[wasm_bindgen(constructor)]
    pub fn new(name: String, content: String, options: ContractOptions, sender: String) -> Self {
        Self {
            name,
            content,
            options,
            sender,
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[wasm_bindgen]
pub struct TransferSTXArgs {
    amount: u64,
    recipient: String,
    sender: String,
}

#[wasm_bindgen]
impl TransferSTXArgs {
    #[wasm_bindgen(constructor)]
    pub fn new(amount: u64, recipient: String, sender: String) -> Self {
        Self {
            amount,
            recipient,
            sender,
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
#[wasm_bindgen]
pub struct TxArgs {
    call_private_fn: Option<CallContractArgsJSON>,
    call_public_fn: Option<CallContractArgsJSON>,
    deploy_contract: Option<DeployContractArgs>,
    #[serde(rename(serialize = "transfer_stx", deserialize = "transferSTX"))]
    transfer_stx: Option<TransferSTXArgs>,
}

#[wasm_bindgen(getter_with_clone)]
#[derive(Debug, Serialize, Deserialize)]
pub struct TransactionRes {
    pub result: String,
    pub events: String,
    pub costs: String,
    pub performance: Option<String>,
}

#[derive(Serialize, Deserialize)]
pub struct TransactionResRaw {
    pub result: String,
    pub events: Vec<String>,
}

#[wasm_bindgen(getter_with_clone)]
pub struct SessionReport {
    pub coverage: String,
    pub costs: String,
}

pub fn execution_result_to_transaction_res(execution: &ExecutionResult) -> TransactionRes {
    let result = match &execution.result {
        EvaluationResult::Snippet(result) => clarity_values::to_raw_value(&result.result),
        EvaluationResult::Contract(ref contract) => match contract.result {
            Some(ref result) => clarity_values::to_raw_value(result),
            _ => "0x03".into(),
        },
    };
    let events_as_strings = execution
        .events
        .iter()
        .map(|e| json!(serialize_event(e)).to_string())
        .collect::<Vec<String>>();

    TransactionRes {
        result,
        events: json!(events_as_strings).to_string(),
        costs: json!(execution.cost).to_string(),
        performance: None,
    }
}

#[derive(Clone)]
struct ProjectCache {
    accounts: HashMap<String, String>,
    contracts_locations: HashMap<QualifiedContractIdentifier, FileLocation>,
    contracts_interfaces: HashMap<QualifiedContractIdentifier, ContractInterface>,
    session: Session,
}

#[wasm_bindgen]
pub struct SDKOptions {
    #[wasm_bindgen(js_name = trackCosts)]
    pub track_costs: bool,
    #[wasm_bindgen(js_name = trackCoverage)]
    pub track_coverage: bool,
    #[wasm_bindgen(js_name = trackPerformance)]
    pub track_performance: bool,
}

#[wasm_bindgen]
impl SDKOptions {
    #[wasm_bindgen(constructor)]
    pub fn new(track_costs: bool, track_coverage: bool, track_performance: Option<bool>) -> Self {
        Self {
            track_costs,
            track_coverage,
            track_performance: track_performance.unwrap_or(false),
        }
    }
}

#[wasm_bindgen]
pub struct SDK {
    #[wasm_bindgen(getter_with_clone)]
    pub deployer: String,
    cache: HashMap<FileLocation, ProjectCache>,
    accounts: HashMap<String, String>,
    contracts_locations: HashMap<QualifiedContractIdentifier, FileLocation>,
    contracts_interfaces: HashMap<QualifiedContractIdentifier, ContractInterface>,
    session: Option<Session>,
    file_accessor: Box<dyn FileAccessor>,
    options: SDKOptions,
    current_test_name: String,
    costs_reports: Vec<CostsReport>,
}

#[wasm_bindgen]
impl SDK {
    #[wasm_bindgen(constructor)]
    pub fn new(fs_request: JsFunction, options: Option<SDKOptions>) -> Self {
        panic::set_hook(Box::new(console_error_panic_hook::hook));

        let file_accessor = Box::new(WASMFileSystemAccessor::new(fs_request));

        let track_coverage = options.as_ref().is_some_and(|o| o.track_coverage);
        let track_costs = options.as_ref().is_some_and(|o| o.track_costs);
        let track_performance = options.as_ref().is_some_and(|o| o.track_performance);

        Self {
            deployer: String::new(),
            cache: HashMap::new(),
            accounts: HashMap::new(),
            contracts_interfaces: HashMap::new(),
            contracts_locations: HashMap::new(),
            session: None,
            file_accessor,
            options: SDKOptions {
                track_coverage,
                track_costs,
                track_performance,
            },
            current_test_name: String::new(),
            costs_reports: vec![],
        }
    }

    #[wasm_bindgen(js_name=getDefaultEpoch)]
    pub fn get_default_epoch() -> EpochString {
        EpochString {
            obj: DEFAULT_EPOCH.to_string().into(),
        }
    }

    #[wasm_bindgen(js_name=getDefaultClarityVersionForCurrentEpoch)]
    pub fn default_clarity_version_for_current_epoch(&self) -> ClarityVersionString {
        let session = self.get_session();
        let current_epoch = session.interpreter.datastore.get_current_epoch();
        ClarityVersionString {
            obj: ClarityVersion::default_for_epoch(current_epoch)
                .to_string()
                .into(),
        }
    }

    #[wasm_bindgen(js_name=initEmptySession)]
    pub async fn init_empty_session(
        &mut self,
        remote_data_settings: JsValue,
    ) -> Result<(), String> {
        let config: Option<RemoteDataSettings> =
            serde_wasm_bindgen::from_value(remote_data_settings)
                .map_err(|e| format!("Failed to parse remote data settings: {e}"))?;

        let mut settings = SessionSettings::default();
        settings.repl_settings.remote_data = config.unwrap_or_default();
        let session = Session::new(settings);

        self.session = Some(session);
        Ok(())
    }

    #[wasm_bindgen(js_name=initSession)]
    pub async fn init_session(&mut self, cwd: String, manifest_path: String) -> Result<(), String> {
        let cwd_path = PathBuf::from(cwd);
        let cwd_root = FileLocation::FileSystem { path: cwd_path };
        let manifest_location = FileLocation::try_parse(&manifest_path, Some(&cwd_root))
            .ok_or("Failed to parse manifest location")?;

        let ProjectCache {
            session,
            contracts_interfaces,
            contracts_locations,
            accounts,
        } = match self.cache.get(&manifest_location) {
            Some(cache) => cache.clone(),
            None => self.setup_session(&manifest_location).await?,
        };

        self.deployer = session.interpreter.get_tx_sender().to_string();

        self.contracts_interfaces = contracts_interfaces;
        self.contracts_locations = contracts_locations;
        self.accounts = accounts;
        self.session = Some(session);

        Ok(())
    }

    async fn setup_session(
        &mut self,
        manifest_location: &FileLocation,
    ) -> Result<ProjectCache, String> {
        let manifest =
            ProjectManifest::from_file_accessor(manifest_location, true, &*self.file_accessor)
                .await?;
        let project_root = manifest_location.get_parent_location()?;
        let deployment_plan_location =
            FileLocation::try_parse("deployments/default.simnet-plan.yaml", Some(&project_root))
                .ok_or("Failed to parse default deployment location")?;

        let (mut deployment, artifacts) = generate_default_deployment(
            &manifest,
            &StacksNetwork::Simnet,
            false,
            Some(&*self.file_accessor),
            Some(StacksEpochId::Epoch21),
        )
        .await?;

        if !artifacts.success {
            let diags_digest = DiagnosticsDigest::new(&artifacts.diags, &deployment);
            if diags_digest.errors > 0 {
                return Err(diags_digest.message);
            }
        }

        if self
            .file_accessor
            .file_exists(deployment_plan_location.to_string())
            .await?
        {
            let spec_file_content = self
                .file_accessor
                .read_file(deployment_plan_location.to_string())
                .await?;

            let mut spec_file = DeploymentSpecificationFile::from_file_content(&spec_file_content)?;

            // the contract publish txs are managed by the manifest
            // keep the user added txs and merge them with the default deployment plan
            if let Some(ref mut plan) = spec_file.plan {
                for batch in plan.batches.iter_mut() {
                    batch.remove_publish_transactions()
                }
            }

            let existing_deployment = DeploymentSpecification::from_specifications(
                &spec_file,
                &StacksNetwork::Simnet,
                &project_root,
                None,
            )?;

            deployment.merge_batches(existing_deployment.plan.batches);

            self.write_deployment_plan(
                &deployment,
                &project_root,
                &deployment_plan_location,
                Some(&spec_file_content),
            )
            .await?;
        } else {
            self.write_deployment_plan(&deployment, &project_root, &deployment_plan_location, None)
                .await?;
        }

        let mut session = initiate_session_from_manifest(&manifest);
        if self.options.track_coverage {
            session.enable_coverage_hook();
        }
        session.enable_logger_hook();
        let executed_contracts = update_session_with_deployment_plan(
            &mut session,
            &deployment,
            Some(&artifacts.asts),
            Some(DEFAULT_EPOCH),
        );

        let mut accounts = HashMap::new();
        if let Some(ref spec) = deployment.genesis {
            for wallet in spec.wallets.iter() {
                if wallet.name == "deployer" {
                    self.deployer = wallet.address.to_string();
                }
                accounts.insert(wallet.name.clone(), wallet.address.to_string());
            }
        }

        let mut contracts_interfaces = HashMap::new();

        for (contract_id, result) in session
            .boot_contracts
            .clone()
            .into_iter()
            .chain(executed_contracts.into_iter())
        {
            match result {
                Ok(execution_result) => {
                    if let EvaluationResult::Contract(ref result) = &execution_result.result {
                        let contract_id = result.contract.analysis.contract_identifier.clone();
                        if let Some(contract_interface) =
                            &result.contract.analysis.contract_interface
                        {
                            contracts_interfaces
                                .insert(contract_id.clone(), contract_interface.clone());
                        }
                    }
                }
                Err(diagnostics) => {
                    let contract_diagnostics = HashMap::from([(contract_id, diagnostics)]);
                    let diags_digest = DiagnosticsDigest::new(&contract_diagnostics, &deployment);
                    if diags_digest.errors > 0 {
                        return Err(diags_digest.message);
                    }
                }
            }
        }

        let mut contracts_locations = HashMap::new();
        for (contract_id, (_, location)) in &deployment.contracts {
            contracts_locations.insert(contract_id.clone(), location.clone());
        }

        let cache = ProjectCache {
            accounts,
            contracts_interfaces,
            contracts_locations,
            session,
        };
        self.cache.insert(manifest_location.clone(), cache.clone());
        Ok(cache)
    }

    #[wasm_bindgen(js_name=clearCache)]
    pub fn clear_cach(&mut self) {
        self.cache.clear();
    }

    async fn write_deployment_plan(
        &self,
        deployment_plan: &DeploymentSpecification,
        project_root: &FileLocation,
        deployment_plan_location: &FileLocation,
        existing_file: Option<&str>,
    ) -> Result<(), String> {
        // we must manually update the location of the contracts in the deployment plan to be relative to the project root
        // because the serialize function is not able to get project_root location in wasm, so it falls back to the full path
        // https://github.com/hirosystems/clarinet/blob/7a41c0c312148b3a5f0eee28a95bebf2766d2e8d/components/clarinet-files/src/lib.rs#L379
        let mut deployment_plan_with_relative_paths = deployment_plan.clone();
        deployment_plan_with_relative_paths
            .plan
            .batches
            .iter_mut()
            .for_each(|batch| {
                batch.transactions.iter_mut().for_each(|tx| {
                    if let TransactionSpecification::EmulatedContractPublish(
                        EmulatedContractPublishSpecification { location, .. },
                    ) = tx
                    {
                        *location = FileLocation::from_path_string(
                            &location
                                .get_relative_path_from_base(project_root)
                                .expect("failed to retrieve relative path"),
                        )
                        .expect("failed to get file location");
                    }
                });
            });

        let deployment_file = deployment_plan_with_relative_paths.to_file_content()?;

        if let Some(existing_file) = existing_file {
            if existing_file.as_bytes() == deployment_file {
                return Ok(());
            }
        }

        log!("Updated deployment plan file");

        self.file_accessor
            .write_file(deployment_plan_location.to_string(), &deployment_file)
            .await?;
        Ok(())
    }

    fn get_session(&self) -> &Session {
        self.session
            .as_ref()
            .expect("Session not initialised. Call initSession() first")
    }

    fn get_session_mut(&mut self) -> &mut Session {
        self.session
            .as_mut()
            .expect("Session not initialised. Call initSession() first")
    }

    #[wasm_bindgen(getter, js_name=blockHeight)]
    pub fn block_height(&mut self) -> u32 {
        let session = self.get_session_mut();
        session.interpreter.get_block_height()
    }

    #[wasm_bindgen(getter, js_name=stacksBlockHeight)]
    pub fn stacks_block_height(&mut self) -> u32 {
        let session = self.get_session_mut();
        session.interpreter.get_block_height()
    }

    #[wasm_bindgen(getter, js_name=burnBlockHeight)]
    pub fn burn_block_height(&mut self) -> u32 {
        let session = self.get_session_mut();
        session.interpreter.get_burn_block_height()
    }

    #[wasm_bindgen(getter, js_name=currentEpoch)]
    pub fn current_epoch(&mut self) -> String {
        let session = self.get_session_mut();
        session
            .interpreter
            .datastore
            .get_current_epoch()
            .to_string()
    }

    #[wasm_bindgen(js_name=setEpoch)]
    pub fn set_epoch(&mut self, epoch: EpochString) {
        let epoch = epoch.as_string().unwrap_or(DEFAULT_EPOCH.to_string());
        let epoch = match epoch.as_str() {
            "2.0" => StacksEpochId::Epoch20,
            "2.05" => StacksEpochId::Epoch2_05,
            "2.1" => StacksEpochId::Epoch21,
            "2.2" => StacksEpochId::Epoch22,
            "2.3" => StacksEpochId::Epoch23,
            "2.4" => StacksEpochId::Epoch24,
            "2.5" => StacksEpochId::Epoch25,
            "3.0" => StacksEpochId::Epoch30,
            "3.1" => StacksEpochId::Epoch31,
            "3.2" => StacksEpochId::Epoch32,
            "3.3" => StacksEpochId::Epoch33,
            _ => {
                log!("Invalid epoch {epoch}. Using default epoch");
                DEFAULT_EPOCH
            }
        };

        let session = self.get_session_mut();
        session.update_epoch(epoch);
    }

    #[wasm_bindgen(js_name=getContractsInterfaces)]
    pub fn get_contracts_interfaces(&self) -> Result<IContractInterfaces, JsError> {
        let contracts_interfaces: HashMap<String, ContractInterface> = self
            .contracts_interfaces
            .iter()
            .map(|(k, v)| (k.to_string(), v.clone()))
            .collect();
        Ok(encode_to_js(&contracts_interfaces)?.unchecked_into::<IContractInterfaces>())
    }

    #[wasm_bindgen(js_name=getContractSource)]
    pub fn get_contract_source(&self, contract: &str) -> Option<String> {
        let session = self.get_session();
        let contract_id = Session::desugar_contract_id(&self.deployer, contract).ok()?;
        let contract = session.contracts.get(&contract_id)?;
        Some(contract.code.clone())
    }

    #[wasm_bindgen(js_name=getContractAST)]
    pub fn get_contract_ast(&self, contract: &str) -> Result<IContractAST, String> {
        let session = self.get_session();
        let contract_id = Session::desugar_contract_id(&self.deployer, contract)?;
        let contract = session.contracts.get(&contract_id).ok_or("err")?;

        Ok(encode_to_js(&contract.ast)
            .map_err(|e| e.to_string())?
            .unchecked_into::<IContractAST>())
    }

    #[wasm_bindgen(js_name=getAssetsMap)]
    pub fn get_assets_maps(&self) -> Result<AssetsMap, JsError> {
        let session = &self.get_session();
        let assets_maps = session.get_assets_maps();
        Ok(encode_to_js(&assets_maps)?.unchecked_into::<AssetsMap>())
    }

    #[wasm_bindgen(js_name=getAccounts)]
    pub fn get_accounts(&mut self) -> Result<Accounts, JsError> {
        Ok(encode_to_js(&self.accounts)?.unchecked_into::<Accounts>())
    }

    #[wasm_bindgen(js_name=getDataVar)]
    pub fn get_data_var(&mut self, contract: &str, var_name: &str) -> Result<String, String> {
        let contract_id = Session::desugar_contract_id(&self.deployer, contract)?;
        let session = self.get_session_mut();
        session
            .interpreter
            .get_data_var(&contract_id, var_name)
            .ok_or_else(|| "value not found".into())
    }

    #[wasm_bindgen(js_name=getBlockTime)]
    pub fn get_block_time(&mut self) -> u64 {
        self.get_session_mut().interpreter.get_block_time()
    }

    #[wasm_bindgen(js_name=getMapEntry)]
    pub fn get_map_entry(
        &mut self,
        contract: &str,
        map_name: &str,
        map_key: Vec<u8>,
    ) -> Result<String, String> {
        let contract_id = Session::desugar_contract_id(&self.deployer, contract)?;
        let session = self.get_session_mut();
        session
            .interpreter
            .get_map_entry(&contract_id, map_name, &uint8_to_value(&map_key))
            .ok_or_else(|| "value not found".into())
    }

    fn get_function_interface(
        &self,
        contract: &str,
        method: &str,
    ) -> Result<&ContractInterfaceFunction, String> {
        let contract_id = Session::desugar_contract_id(&self.deployer, contract)?;
        let contract_interface = self
            .contracts_interfaces
            .get(&contract_id)
            .ok_or_else(|| format!("unable to get contract interface for {contract}"))?;
        contract_interface
            .functions
            .iter()
            .find(|func| func.name == method)
            .ok_or_else(|| format!("contract {contract} has no function {method}"))
    }

    fn call_contract_fn(
        &mut self,
        CallFnArgs {
            contract,
            method,
            args,
            sender,
        }: &CallFnArgs,
        allow_private: bool,
    ) -> Result<TransactionRes, String> {
        let test_name = self.current_test_name.clone();
        let track_costs = self.options.track_costs;
        let track_performance = self.options.track_performance;

        if PrincipalData::parse_standard_principal(sender).is_err() {
            return Err(format!("Invalid sender address '{sender}'."));
        }

        let parsed_args = args
            .iter()
            .map(|a| SymbolicExpression::atom_value(uint8_to_value(a)))
            .collect::<Vec<SymbolicExpression>>();

        let session = self.get_session_mut();
        let execution = session
            .call_contract_fn(
                contract,
                method,
                &parsed_args,
                sender,
                allow_private,
                track_costs,
            )
            .map_err(|diagnostics| {
                let mut message = format!(
                    "Call contract function error: {contract}::{method}({})",
                    args.iter()
                        .map(|a| uint8_to_string(a))
                        .collect::<Vec<String>>()
                        .join(", ")
                );
                if let Some(diag) = diagnostics.last() {
                    message = format!("{message} -> {}", diag.message);
                }
                message
            })?;

        // Collect performance data before accessing self.costs_reports
        let performance_data = if track_performance {
            session.get_performance_data()
        } else {
            None
        };

        // Release the session borrow before accessing self.costs_reports
        let _ = session;

        if track_costs {
            if let Some(ref cost) = execution.cost {
                let contract_id =
                    Session::desugar_contract_id(&self.deployer, contract)?.to_string();
                self.costs_reports.push(CostsReport {
                    test_name,
                    contract_id,
                    method: method.to_string(),
                    args: parsed_args.iter().map(|a| a.to_string()).collect(),
                    cost_result: cost.clone(),
                });
            }
        }

        let mut response = execution_result_to_transaction_res(&execution);

        if let Some(perf_data) = performance_data {
            response.performance = Some(perf_data);
        }

        Ok(response)
    }

    #[wasm_bindgen(js_name=callReadOnlyFn)]
    pub fn call_read_only_fn(&mut self, args: &CallFnArgs) -> Result<TransactionRes, String> {
        if let Ok(interface) = self.get_function_interface(&args.contract, &args.method) {
            if interface.access != ContractInterfaceFunctionAccess::read_only {
                return Err(format!("{} is not a read-only function", &args.method));
            }
        }
        self.call_contract_fn(args, false)
    }

    fn inner_call_public_fn(
        &mut self,
        args: &CallFnArgs,
        advance_chain_tip: bool,
    ) -> Result<TransactionRes, String> {
        if let Ok(interface) = self.get_function_interface(&args.contract, &args.method) {
            if interface.access != ContractInterfaceFunctionAccess::public {
                return Err(format!("{} is not a public function", &args.method));
            }
        }

        if advance_chain_tip {
            let session = self.get_session_mut();
            session.advance_chain_tip(1);
        }
        self.call_contract_fn(args, false)
    }

    fn inner_call_private_fn(
        &mut self,
        args: &CallFnArgs,
        advance_chain_tip: bool,
    ) -> Result<TransactionRes, String> {
        if let Ok(interface) = self.get_function_interface(&args.contract, &args.method) {
            if interface.access != ContractInterfaceFunctionAccess::private {
                return Err(format!("{} is not a private function", &args.method));
            }
        }
        if advance_chain_tip {
            let session = self.get_session_mut();
            session.advance_chain_tip(1);
        }
        self.call_contract_fn(args, true)
    }

    fn inner_transfer_stx(
        &mut self,
        args: &TransferSTXArgs,
        advance_chain_tip: bool,
    ) -> Result<TransactionRes, String> {
        if PrincipalData::parse_standard_principal(&args.sender).is_err() {
            return Err(format!("Invalid sender address '{}'.", args.sender));
        }

        if PrincipalData::parse(&args.recipient).is_err() {
            return Err(format!("Invalid recipient address '{}'.", args.recipient));
        }

        let session = self.get_session_mut();
        let initial_tx_sender = session.get_tx_sender();
        session.set_tx_sender(&args.sender);

        let execution = match session.stx_transfer(args.amount, &args.recipient) {
            Ok(res) => res,
            Err(diagnostics) => {
                let mut message = format!("{}: {}", "STX transfer error", args.sender);
                if let Some(diag) = diagnostics.last() {
                    message = format!("{} -> {}", message, diag.message);
                }
                return Err(message);
            }
        };

        if advance_chain_tip {
            session.advance_chain_tip(1);
        }
        session.set_tx_sender(&initial_tx_sender);
        Ok(execution_result_to_transaction_res(&execution))
    }

    fn inner_deploy_contract(
        &mut self,
        args: &DeployContractArgs,
        advance_chain_tip: bool,
    ) -> Result<TransactionRes, String> {
        if PrincipalData::parse_standard_principal(&args.sender).is_err() {
            return Err(format!("Invalid sender address '{}'.", args.sender));
        }

        let execution = {
            let session = self.get_session_mut();
            if advance_chain_tip {
                session.advance_chain_tip(1);
            }
            let current_epoch = session.interpreter.datastore.get_current_epoch();

            let contract = ClarityContract {
                code_source: ClarityCodeSource::ContractInMemory(args.content.clone()),
                name: args.name.clone(),
                deployer: ContractDeployer::Address(args.sender.to_string()),
                clarity_version: args.options.clarity_version,
                epoch: Epoch::Specific(current_epoch),
            };

            match session.deploy_contract(&contract, false, None) {
                Ok(res) => res,
                Err(diagnostics) => {
                    let mut message = format!(
                        "Contract deployment runtime error: {}.{}",
                        args.sender, args.name
                    );
                    if let Some(diag) = diagnostics.last() {
                        message = format!("{} -> {}", message, diag.message);
                    }
                    return Err(message);
                }
            }
        };

        if let EvaluationResult::Contract(ref result) = &execution.result {
            let contract_id = result.contract.analysis.contract_identifier.clone();
            if let Some(contract_interface) = &result.contract.analysis.contract_interface {
                self.contracts_interfaces
                    .insert(contract_id, contract_interface.clone());
            }
        };

        Ok(execution_result_to_transaction_res(&execution))
    }

    #[wasm_bindgen(js_name=deployContract)]
    pub fn deploy_contract(&mut self, args: &DeployContractArgs) -> Result<TransactionRes, String> {
        self.inner_deploy_contract(args, true)
    }

    #[wasm_bindgen(js_name = "transferSTX")]
    pub fn transfer_stx(&mut self, args: &TransferSTXArgs) -> Result<TransactionRes, String> {
        self.inner_transfer_stx(args, true)
    }

    #[wasm_bindgen(js_name = "callPublicFn")]
    pub fn call_public_fn(&mut self, args: &CallFnArgs) -> Result<TransactionRes, String> {
        self.inner_call_public_fn(args, true)
    }

    #[wasm_bindgen(js_name = "callPrivateFn")]
    pub fn call_private_fn(&mut self, args: &CallFnArgs) -> Result<TransactionRes, String> {
        self.inner_call_private_fn(args, true)
    }

    #[wasm_bindgen(js_name=mineBlock)]
    pub fn mine_block_js(&mut self, js_txs: js_sys::Array) -> Result<JsValue, String> {
        let mut results: Vec<TransactionRes> = vec![];

        let txs: Vec<TxArgs> = js_txs
            .into_serde()
            .map_err(|e| format!("Failed to parse js txs: {e}"))?;

        {
            let session = self.get_session_mut();
            session.advance_chain_tip(1);
        }

        for tx in txs {
            let result = if let Some(call_public) = tx.call_public_fn {
                self.inner_call_public_fn(&CallFnArgs::from_json_args(call_public), false)
            } else if let Some(call_private) = tx.call_private_fn {
                self.inner_call_private_fn(&CallFnArgs::from_json_args(call_private), false)
            } else if let Some(transfer_stx) = tx.transfer_stx {
                self.inner_transfer_stx(&transfer_stx, false)
            } else if let Some(deploy_contract) = tx.deploy_contract {
                self.inner_deploy_contract(&deploy_contract, false)
            } else {
                return Err("Invalid tx arguments".into());
            }?;
            results.push(result);
        }

        encode_to_js(&results).map_err(|e| format!("error: {e}"))
    }

    #[wasm_bindgen(js_name=mineEmptyBlock)]
    pub fn mine_empty_block(&mut self) -> u32 {
        self.mine_empty_burn_block()
    }
    #[wasm_bindgen(js_name=mineEmptyBlocks)]
    pub fn mine_empty_blocks(&mut self, count: Option<u32>) -> u32 {
        self.mine_empty_burn_blocks(count)
    }
    #[wasm_bindgen(js_name=mineEmptyStacksBlock)]
    pub fn mine_empty_stacks_block(&mut self) -> Result<u32, String> {
        let session = self.get_session_mut();
        match session.advance_stacks_chain_tip(1) {
            Ok(new_height) => Ok(new_height),
            Err(_) => Err("use mineEmptyBurnBlock in epoch lower than 3.0".to_string()),
        }
    }

    #[wasm_bindgen(js_name=mineEmptyStacksBlocks)]
    pub fn mine_empty_stacks_blocks(&mut self, count: Option<u32>) -> Result<u32, String> {
        let session = self.get_session_mut();
        match session.advance_stacks_chain_tip(count.unwrap_or(1)) {
            Ok(new_height) => Ok(new_height),
            Err(_) => Err("use mineEmptyBurnBlocks in epoch lower than 3.0".to_string()),
        }
    }

    #[wasm_bindgen(js_name=mineEmptyBurnBlock)]
    pub fn mine_empty_burn_block(&mut self) -> u32 {
        let session = self.get_session_mut();
        session.advance_burn_chain_tip(1)
    }
    #[wasm_bindgen(js_name=mineEmptyBurnBlocks)]
    pub fn mine_empty_burn_blocks(&mut self, count: Option<u32>) -> u32 {
        let session = self.get_session_mut();
        session.advance_burn_chain_tip(count.unwrap_or(1))
    }

    #[wasm_bindgen(js_name=runSnippet)]
    pub fn run_snippet(&mut self, snippet: String) -> String {
        let session = self.get_session_mut();
        match session.eval(snippet.clone(), false) {
            Ok(res) => match res.result {
                EvaluationResult::Snippet(result) => clarity_values::to_raw_value(&result.result),
                EvaluationResult::Contract(_) => unreachable!(
                    "Contract evaluation result should not be returned from eval_snippet",
                ),
            },
            Err(diagnostics) => {
                let mut message = "error:".to_string();
                diagnostics.iter().for_each(|d| {
                    message = format!("{message}\n{}", d.message);
                });
                message
            }
        }
    }

    #[wasm_bindgen(js_name=execute)]
    pub fn execute(&mut self, snippet: String) -> Result<TransactionRes, String> {
        let session = self.get_session_mut();
        match session.eval(snippet.clone(), false) {
            Ok(res) => Ok(execution_result_to_transaction_res(&res)),
            Err(diagnostics) => {
                let message = diagnostics
                    .iter()
                    .map(|d| d.message.to_string())
                    .collect::<Vec<String>>()
                    .join("\n");
                Err(format!("error: {message}"))
            }
        }
    }

    #[wasm_bindgen(js_name=executeCommand)]
    pub fn execute_command(&mut self, snippet: String) -> String {
        let session = self.get_session_mut();
        if !snippet.starts_with("::") {
            return "error: command must start with ::".to_string();
        }
        session.handle_command(&snippet)
    }

    #[wasm_bindgen(js_name=getLastContractCallTrace)]
    /// Returns the last contract call trace as a string, if available.
    pub fn get_last_contract_call_trace(&self) -> Option<String> {
        let session = self.get_session();
        session.last_contract_call_trace.clone()
    }

    #[wasm_bindgen(js_name=setLocalAccounts)]
    pub fn set_local_accounts(&mut self, addresses: Vec<String>) {
        let principals = addresses
            .into_iter()
            .filter_map(|a| {
                // Validate each address before converting
                if PrincipalData::parse_standard_principal(&a).is_err() {
                    log!(
                        "Warning: Invalid address '{}' in setLocalAccounts, skipping",
                        a
                    );
                    return None;
                }
                Some(StandardPrincipalData::from(
                    StacksAddress::from_string(&a).unwrap(),
                ))
            })
            .collect();
        let session = self.get_session_mut();
        session
            .interpreter
            .clarity_datastore
            .save_local_account(principals);
    }

    #[wasm_bindgen(js_name=mintSTX)]
    pub fn mint_stx(&mut self, recipient: String, amount: u64) -> Result<String, String> {
        if PrincipalData::parse(&recipient).is_err() {
            return Err(format!("Invalid recipient address '{recipient}'."));
        }

        let session = self.get_session_mut();

        session.interpreter.mint_stx_balance(
            PrincipalData::Standard(StandardPrincipalData::from(
                StacksAddress::from_string(&recipient).unwrap(),
            )),
            amount,
        )
    }

    #[wasm_bindgen(js_name=setCurrentTestName)]
    pub fn set_current_test_name(&mut self, test_name: String) {
        let session = self.get_session_mut();
        session.set_test_name(test_name.clone());
        self.current_test_name = test_name;
    }

    // this method empty the session costs and coverage reports
    // and returns this report
    #[wasm_bindgen(js_name=collectReport)]
    pub fn collect_report(
        &mut self,
        include_boot_contracts: bool,
        boot_contracts_path: String,
    ) -> Result<SessionReport, String> {
        let contracts_locations = self.contracts_locations.clone();
        let session = self.get_session_mut();

        let mut asts = BTreeMap::new();
        let mut contract_paths = BTreeMap::new();

        for (contract_id, contract) in session.contracts.iter() {
            asts.insert(contract_id.clone(), contract.ast.clone());
        }
        for (contract_id, contract_location) in contracts_locations.iter() {
            contract_paths.insert(contract_id.name.to_string(), contract_location.to_string());
        }

        if include_boot_contracts {
            for (contract_id, (_, ast)) in clarity_repl::repl::boot::BOOT_CONTRACTS_DATA.iter() {
                asts.insert(contract_id.clone(), ast.clone());
                contract_paths.insert(
                    contract_id.name.to_string(),
                    format!("{boot_contracts_path}/{}.clar", contract_id.name),
                );
            }
        }

        let coverage = session.collect_lcov_content(&asts, &contract_paths);

        let costs = serde_json::to_string(&self.costs_reports).map_err(|e| e.to_string())?;
        self.costs_reports.clear();

        Ok(SessionReport { coverage, costs })
    }

    #[wasm_bindgen(js_name=enablePerformance)]
    pub fn enable_performance(&mut self, cost_field: String) -> Result<(), String> {
        let session = self.get_session_mut();
        let cost_field = CostField::from(cost_field.as_str());
        session.enable_performance(cost_field);
        Ok(())
    }
}
