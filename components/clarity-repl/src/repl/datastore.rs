use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;
use std::rc::Rc;
use std::vec;

use clarity::types::chainstate::{
    BlockHeaderHash, BurnchainHeaderHash, ConsensusHash, SortitionId, StacksAddress, StacksBlockId,
    TrieHash, VRFSeed,
};
use clarity::types::StacksEpochId;
use clarity::util::hash::Sha512Trunc256Sum;
use clarity::vm::analysis::{AnalysisDatabase, ContractAnalysis};
use clarity::vm::ast::build_ast;
use clarity::vm::contracts::Contract as ContractContextResponse;
use clarity::vm::database::clarity_db::ContractDataVarName;
use clarity::vm::database::clarity_store::ContractCommitment;
use clarity::vm::database::{
    BurnStateDB, ClarityBackingStore, ClarityDatabase, ClaritySerializable, HeadersDB, StoreType,
};
use clarity::vm::errors::{InterpreterError, InterpreterResult as Result};
use clarity::vm::{ContractContext, StacksEpoch};
use clarity_types::types::{
    PrincipalData, QualifiedContractIdentifier, StandardPrincipalData, TupleData,
};
use pox_locking::handle_contract_call_special_cases;
use sha2::{Digest, Sha512_256};

use super::interpreter::BLOCK_LIMIT_MAINNET;
use super::remote_data::fs::{get_file_from_cache, write_file_to_cache};
use super::remote_data::{epoch_for_height, Block, HttpClient, Sortition};
use super::settings::RemoteNetworkInfo;
use crate::repl::remote_data::context;

// todo: export it from clarity
const CLARITY_STORAGE_BLOCK_TIME_KEY: &str = "_stx-data::clarity_storage::block_time";

const SECONDS_BETWEEN_BURN_BLOCKS: u64 = 600;
const SECONDS_BETWEEN_STACKS_BLOCKS: u64 = 10;

fn epoch_to_peer_version(epoch: StacksEpochId) -> u8 {
    use clarity::consts::*;
    match epoch {
        StacksEpochId::Epoch10 => PEER_VERSION_EPOCH_1_0,
        StacksEpochId::Epoch20 => PEER_VERSION_EPOCH_2_0,
        StacksEpochId::Epoch2_05 => PEER_VERSION_EPOCH_2_05,
        StacksEpochId::Epoch21 => PEER_VERSION_EPOCH_2_1,
        StacksEpochId::Epoch22 => PEER_VERSION_EPOCH_2_2,
        StacksEpochId::Epoch23 => PEER_VERSION_EPOCH_2_3,
        StacksEpochId::Epoch24 => PEER_VERSION_EPOCH_2_4,
        StacksEpochId::Epoch25 => PEER_VERSION_EPOCH_2_5,
        StacksEpochId::Epoch30 => PEER_VERSION_EPOCH_3_0,
        StacksEpochId::Epoch31 => PEER_VERSION_EPOCH_3_1,
        StacksEpochId::Epoch32 => PEER_VERSION_EPOCH_3_2,
        StacksEpochId::Epoch33 => PEER_VERSION_EPOCH_3_3,
    }
}

/// This stores all of the data for the Clarity VM, especially MARF values
/// It also includes the data needed for mainnet execution simulation (HTTP Client, FS Cache)
pub struct ClarityDatastore {
    open_chain_tip: StacksBlockId,
    current_chain_tip: Rc<RefCell<StacksBlockId>>,
    store: HashMap<String, BTreeMap<u32, String>>,
    metadata: HashMap<(String, String), String>,
    height_at_chain_tip: HashMap<StacksBlockId, u32>,
    chain_tip_at_height: HashMap<u32, StacksBlockId>,

    remote_network_info: Option<RemoteNetworkInfo>,
    remote_block_info_cache: Rc<RefCell<HashMap<StacksBlockId, Block>>>,
    remote_sortition_cache: Rc<RefCell<HashMap<BurnchainHeaderHash, Sortition>>>,
    fs_cache_location: PathBuf,
    local_accounts: Vec<StandardPrincipalData>,

    client: HttpClient,
}

impl Clone for ClarityDatastore {
    fn clone(&self) -> Self {
        // for performance optimization, a simnet session can be stored and cached
        // when cloning the session (and the datastore), we do not want to keep the
        // current_chain_tip RefCell value, but rather sync it with the open_chain_tip
        *self.current_chain_tip.borrow_mut() = self.open_chain_tip.clone();
        Self {
            open_chain_tip: self.open_chain_tip.clone(),
            current_chain_tip: Rc::clone(&self.current_chain_tip),
            store: self.store.clone(),
            metadata: self.metadata.clone(),
            height_at_chain_tip: self.height_at_chain_tip.clone(),
            chain_tip_at_height: self.chain_tip_at_height.clone(),
            remote_network_info: self.remote_network_info.clone(),
            remote_block_info_cache: Rc::clone(&self.remote_block_info_cache),
            remote_sortition_cache: Rc::clone(&self.remote_sortition_cache),
            fs_cache_location: self.fs_cache_location.clone(),
            local_accounts: self.local_accounts.clone(),
            client: self.client.clone(),
        }
    }
}

struct BurnBlockHashes {
    header_hash: BurnchainHeaderHash,
    consensus_hash: ConsensusHash,
    vrf_seed: VRFSeed,
    sortition_id: SortitionId,
}

#[derive(Clone, Debug)]
struct BurnBlockInfo {
    consensus_hash: ConsensusHash,
    vrf_seed: VRFSeed,
    sortition_id: SortitionId,
    burn_block_time: u64,
    burn_chain_height: u32,
}

#[derive(Clone, Debug)]
pub struct StacksBlockInfo {
    block_header_hash: BlockHeaderHash,
    burn_block_header_hash: BurnchainHeaderHash,
    stacks_block_time: u64,
}

#[derive(Clone, Debug)]
pub struct StacksConstants {
    pub burn_start_height: u32,
    pub pox_prepare_length: u32,
    pub pox_reward_cycle_length: u32,
    pub pox_rejection_fraction: u64,
}

impl Default for StacksConstants {
    fn default() -> Self {
        StacksConstants {
            burn_start_height: 0,
            pox_prepare_length: 50,
            pox_reward_cycle_length: 1050,
            pox_rejection_fraction: 0,
        }
    }
}

/// Store the Stacks and Burn blocks info
#[derive(Clone, Debug)]
pub struct Datastore {
    genesis_id: StacksBlockId,
    burn_chain_tip: BurnchainHeaderHash,
    burn_chain_height: u32,
    tenure_height: u32,
    current_chain_tip: Rc<RefCell<StacksBlockId>>,
    remote_block_info_cache: Rc<RefCell<HashMap<StacksBlockId, Block>>>,
    remote_sortition_cache: Rc<RefCell<HashMap<BurnchainHeaderHash, Sortition>>>,
    burn_blocks: HashMap<BurnchainHeaderHash, BurnBlockInfo>,
    stacks_chain_height: u32,
    stacks_blocks: HashMap<StacksBlockId, StacksBlockInfo>,
    sortition_lookup: HashMap<SortitionId, BurnchainHeaderHash>,
    tenure_height_at_stacks_height: HashMap<u32, u32>,
    stacks_height_at_tenure_height: HashMap<u32, u32>,
    consensus_hash_lookup: HashMap<ConsensusHash, SortitionId>,
    current_epoch: StacksEpochId,
    current_epoch_start_height: u32,
    constants: StacksConstants,
    remote_network_info: Option<RemoteNetworkInfo>,
    initial_burn_height: u32,
    client: HttpClient,
}

fn height_to_hashed_bytes(height: u32) -> [u8; 32] {
    let input_bytes = height.to_be_bytes();
    let mut hasher = Sha512_256::new();
    hasher.update(input_bytes);
    let hash = Sha512Trunc256Sum::from_hasher(hasher);
    hash.0
}

impl BurnBlockHashes {
    fn from_height(height: u32) -> Self {
        let bytes = height_to_hashed_bytes(height);
        let header_hash = {
            let mut buffer = bytes;
            buffer[0] = 2;
            BurnchainHeaderHash::from_bytes(&buffer[0..32]).unwrap()
        };
        let consensus_hash = {
            let mut buffer = bytes;
            buffer[0] = 3;
            ConsensusHash::from_bytes(&buffer[0..20]).unwrap()
        };
        let vrf_seed = {
            let mut buffer = bytes;
            buffer[0] = 4;
            VRFSeed(buffer)
        };
        Self {
            header_hash,
            consensus_hash,
            vrf_seed,
            sortition_id: SortitionId(bytes),
        }
    }
}

impl ClarityDatastore {
    pub fn new(remote_network_info: Option<RemoteNetworkInfo>, client: HttpClient) -> Self {
        if let Some(remote_network_info) = remote_network_info {
            return Self::new_with_remote_data(remote_network_info, client);
        }

        let height = 0;
        let id = StacksBlockId(height_to_hashed_bytes(height));
        let fs_cache_location = remote_network_info
            .and_then(|r| r.cache_location)
            .unwrap_or(PathBuf::from("./.cache"))
            .join("datastore");

        Self {
            open_chain_tip: id.clone(),
            current_chain_tip: Rc::new(RefCell::new(id.clone())),
            store: HashMap::new(),
            metadata: HashMap::new(),
            height_at_chain_tip: HashMap::from([(id.clone(), height)]),
            chain_tip_at_height: HashMap::from([(height, id.clone())]),

            remote_network_info: None,
            remote_block_info_cache: Rc::new(RefCell::new(HashMap::new())),
            remote_sortition_cache: Rc::new(RefCell::new(HashMap::new())),
            fs_cache_location,
            local_accounts: Vec::new(),

            client,
        }
    }

    fn new_with_remote_data(remote_network_info: RemoteNetworkInfo, client: HttpClient) -> Self {
        let height = remote_network_info.initial_height;
        let path = format!("/extended/v2/blocks/{height}");
        let block = client.fetch_block(&path);
        let sortition = client.fetch_sortition(block.burn_block_height);
        let block_cache = HashMap::from([(block.index_block_hash.clone(), block.clone())]);
        let sortition_cache = HashMap::from([(block.burn_block_hash, sortition)]);
        let fs_cache_location = remote_network_info
            .cache_location
            .as_ref()
            .unwrap_or(&PathBuf::from("./.cache"))
            .join("datastore");

        let id = block.index_block_hash;

        Self {
            open_chain_tip: id.clone(),
            current_chain_tip: Rc::new(RefCell::new(id.clone())),
            store: HashMap::new(),
            metadata: HashMap::new(),
            height_at_chain_tip: HashMap::from([(id.clone(), height)]),
            chain_tip_at_height: HashMap::from([(height, id.clone())]),
            remote_network_info: Some(remote_network_info),
            remote_block_info_cache: Rc::new(RefCell::new(block_cache)),
            remote_sortition_cache: Rc::new(RefCell::new(sortition_cache)),
            fs_cache_location,
            local_accounts: Vec::new(),

            client,
        }
    }

    pub fn as_analysis_db(&mut self) -> AnalysisDatabase<'_> {
        AnalysisDatabase::new(self)
    }

    /// begin, commit, rollback a save point identified by key
    ///    this is used to clean up any data from aborted blocks
    ///     (NOT aborted transactions that is handled by the clarity vm directly).
    /// The block header hash is used for identifying savepoints.
    ///     this _cannot_ be used to rollback to arbitrary prior block hash, because that
    ///     blockhash would already have committed and no longer exist in the save point stack.
    /// this is a "lower-level" rollback than the roll backs performed in
    ///   ClarityDatabase or AnalysisDatabase -- this is done at the backing store level.
    pub fn begin(&mut self, _current: &StacksBlockId, _next: &StacksBlockId) {
        // self.marf.begin(current, next)
        //     .expect(&format!("ERROR: Failed to begin new MARF block {} - {})", current, next));
        // self.chain_tip = self.marf.get_open_chain_tip()
        //     .expect("ERROR: Failed to get open MARF")
        //     .clone();
        // self.side_store.begin(&self.chain_tip);
    }
    pub fn rollback(&mut self) {
        // self.marf.drop_current();
        // self.side_store.rollback(&self.chain_tip);
        // self.chain_tip = StacksBlockId::sentinel();
    }
    // This is used by miners
    //   so that the block validation and processing logic doesn't
    //   reprocess the same data as if it were already loaded
    pub fn commit_mined_block(&mut self, _will_move_to: &StacksBlockId) {
        // rollback the side_store
        //    the side_store shouldn't commit data for blocks that won't be
        //    included in the processed chainstate (like a block constructed during mining)
        //    _if_ for some reason, we do want to be able to access that mined chain state in the future,
        //    we should probably commit the data to a different table which does not have uniqueness constraints.
        // self.side_store.rollback(&self.chain_tip);
        // self.marf.commit_mined(will_move_to)
        //     .expect("ERROR: Failed to commit MARF block");
    }

    pub fn commit_to(&mut self, _final_bhh: &StacksBlockId) {
        // println!("commit_to({})", final_bhh);
        // self.side_store.commit_metadata_to(&self.chain_tip, final_bhh);
        // self.side_store.commit(&self.chain_tip);
        // self.marf.commit_to(final_bhh)
        //     .expect("ERROR: Failed to commit MARF block");
    }

    pub fn save_local_account(&mut self, local_accounts: Vec<StandardPrincipalData>) {
        self.local_accounts = local_accounts;
    }

    fn is_key_from_local_account(&mut self, key: &str) -> bool {
        let parts: Vec<&str> = key.split("::").collect();
        if let Ok(principal) = PrincipalData::parse(parts[1]) {
            let standard_principal = match principal {
                PrincipalData::Contract(contract) => contract.issuer,
                PrincipalData::Standard(standard) => standard,
            };
            return self.local_accounts.contains(&standard_principal);
        }
        false
    }

    fn put(&mut self, key: &str, value: &str) {
        let height = self.get_current_block_height();
        self.store
            .entry(key.to_string())
            .or_default()
            .insert(height, value.to_string());
    }

    fn fetch_block(&mut self, url: &str) -> Block {
        let block = self.client.fetch_block(url);
        self.remote_block_info_cache
            .borrow_mut()
            .insert(block.index_block_hash.clone(), block.clone());
        self.height_at_chain_tip
            .insert(block.index_block_hash.clone(), block.height);
        self.chain_tip_at_height
            .insert(block.height, block.index_block_hash.clone());

        if self
            .remote_sortition_cache
            .borrow()
            .get(&block.burn_block_hash)
            .is_none()
        {
            let sortition = self.client.fetch_sortition(block.burn_block_height);
            self.remote_sortition_cache
                .borrow_mut()
                .insert(block.burn_block_hash.clone(), sortition);
        }

        block
    }

    fn get_remote_block_info_from_height(&mut self, height: u32) -> Block {
        if let Some(hash) = self.chain_tip_at_height.get(&height) {
            return self.get_remote_block_info_from_hash(&hash.clone());
        }
        self.fetch_block(&format!("/extended/v2/blocks/{height}"))
    }

    fn get_remote_block_info_from_hash(&mut self, hash: &StacksBlockId) -> Block {
        if let Some(cached) = self.remote_block_info_cache.borrow().get(hash) {
            return cached.clone();
        }
        self.fetch_block(&format!("/extended/v2/blocks/{hash}"))
    }

    fn get_remote_chaintip(&mut self) -> String {
        let initial_height = self.remote_network_info.as_ref().unwrap().initial_height;
        let height = self.get_current_block_height().min(initial_height);
        let block_info = self.get_remote_block_info_from_height(height);
        block_info.index_block_hash.to_string()
    }

    fn fetch_clarity_marf_value(&mut self, key: &str) -> Result<Option<String>> {
        let key_hash = TrieHash::from_key(key);
        let tip = self.get_remote_chaintip();
        let url = format!("/v2/clarity/marf/{key_hash}?tip={tip}&proof=false");
        self.client.fetch_clarity_data(&url)
    }

    fn populate_context_functions(
        &mut self,
        contract_id: &QualifiedContractIdentifier,
        context_str: Option<String>,
    ) -> Result<ContractContext> {
        let contract_src_key = ClarityDatabase::make_metadata_key(
            StoreType::Contract,
            ContractDataVarName::ContractSrc.as_str(),
        );
        let contract_src =
            self.get_metadata(contract_id, &contract_src_key)?
                .ok_or(InterpreterError::Expect(format!(
                    "No contract source found for contract: {contract_id}",
                )))?;

        let mut contract_context = context_str
            .ok_or(InterpreterError::Expect(format!(
                "No contract context found for contract: {contract_id}",
            )))
            .and_then(|s| {
                serde_json::from_str::<ContractContextResponse>(&s).map_err(|e| {
                    InterpreterError::Expect(format!("Failed to parse contract context: {e}"))
                })
            })?
            .contract_context;
        contract_context.functions.clear();

        let analysis = self
            .get_metadata(contract_id, AnalysisDatabase::storage_key())?
            .ok_or(InterpreterError::Expect(format!(
                "No analysis metadata found for contract: {contract_id}",
            )))
            .and_then(|s| {
                serde_json::from_str::<ContractAnalysis>(&s).map_err(|e| {
                    InterpreterError::Expect(format!("Failed to parse analysis metadata: {e}"))
                })
            })?;

        let contract_ast = build_ast(
            contract_id,
            &contract_src,
            &mut (),
            analysis.clarity_version,
            analysis.epoch,
        )?;

        context::set_functions_in_contract_context(
            &contract_ast.expressions,
            &mut contract_context,
            &analysis.epoch,
        )?;

        Ok(contract_context)
    }

    fn fetch_clarity_metadata(
        &mut self,
        contract_id: &QualifiedContractIdentifier,
        key: &str,
    ) -> Result<Option<String>> {
        let addr = contract_id.issuer.to_string();
        let contract = contract_id.name.to_string();
        let tip = self.get_remote_chaintip();

        let cache_file_path = PathBuf::from(format!(
            "{}_{}_{}_{}",
            addr,
            contract,
            key.replace(":", "_"),
            tip
        ))
        .with_extension("json");

        if let Some(cached) = get_file_from_cache(&self.fs_cache_location, &cache_file_path) {
            return Ok(Some(cached));
        }

        let url = format!("/v2/clarity/metadata/{addr}/{contract}/{key}?tip={tip}");
        let raw_response = self.client.fetch_clarity_data(&url)?;

        let contract_context_key = ClarityDatabase::make_metadata_key(
            StoreType::Contract,
            ContractDataVarName::Contract.as_str(),
        );
        let response = if key == contract_context_key {
            // the `vm-metadata::9::contract`, returns the contracts Context, including the contract AST.
            // since node don't have the `clarity-vm/developer-mode` cargo feature enabled,
            // the AST doesn't contain the spans, which are useful for most debugging purposes.
            // The solution is to fetch the source code of the contract, and parse it locally,
            let contract_context = self.populate_context_functions(contract_id, raw_response)?;
            let contract_context_response = ContractContextResponse { contract_context };
            Some(
                serde_json::to_string(&contract_context_response).map_err(|e| {
                    InterpreterError::Expect(format!("Failed to serialize contract context: {e}"))
                })?,
            )
        } else {
            raw_response
        };

        if let Some(content) = &response {
            write_file_to_cache(
                &self.fs_cache_location,
                &cache_file_path,
                content.as_bytes(),
            );
        }

        Ok(response)
    }
}

impl ClarityBackingStore for ClarityDatastore {
    fn put_all_data(&mut self, items: Vec<(String, String)>) -> Result<()> {
        for (key, value) in items {
            self.put(&key, &value);
        }
        Ok(())
    }

    /// fetch K-V out of the committed datastore
    fn get_data(&mut self, key: &str) -> Result<Option<String>> {
        let current_height = self.get_current_block_height();
        let fetch_remote_data =
            self.remote_network_info.is_some() && !self.is_key_from_local_account(key);

        let values_map = self.store.get(key);

        if fetch_remote_data {
            // if the value for the exact current_chain_tip is present, return it
            if let Some(data) = values_map.and_then(|data| data.get(&current_height)) {
                return Ok(Some(data.clone()));
            }

            let initial_height = self.remote_network_info.as_ref().unwrap().initial_height;
            if current_height > initial_height {
                if let Some((_, value)) = values_map.and_then(|data| {
                    data.iter()
                        .rev()
                        .find(|(height, _)| height > &&initial_height && height <= &&current_height)
                }) {
                    return Ok(Some(value.clone()));
                }
            }

            let data = self.fetch_clarity_marf_value(key);
            if let Ok(Some(value)) = &data {
                self.put(key, value);
            }
            return data;
        }

        Ok(values_map.and_then(|data| {
            data.iter()
                .rev()
                .find(|(height, _)| height <= &&current_height)
                .map(|(_, value)| value.clone())
        }))
    }

    fn get_data_from_path(&mut self, _hash: &TrieHash) -> Result<Option<String>> {
        unreachable!()
    }

    fn get_data_with_proof(&mut self, _key: &str) -> Result<Option<(String, Vec<u8>)>> {
        Ok(None)
    }

    fn get_data_with_proof_from_path(
        &mut self,
        _hash: &TrieHash,
    ) -> Result<Option<(String, Vec<u8>)>> {
        unreachable!()
    }

    fn has_entry(&mut self, key: &str) -> Result<bool> {
        Ok(self.get_data(key)?.is_some())
    }

    /// change the current MARF context to service reads from a different chain_tip
    ///   used to implement time-shifted evaluation.
    /// returns the previous block header hash on success
    fn set_block_hash(&mut self, bhh: StacksBlockId) -> Result<StacksBlockId> {
        let prior_tip = self.open_chain_tip.clone();
        if self.remote_network_info.is_some() {
            #[allow(clippy::map_entry)]
            if !self.height_at_chain_tip.contains_key(&bhh) {
                let block_info = self.get_remote_block_info_from_hash(&bhh);
                self.height_at_chain_tip
                    .insert(bhh.clone(), block_info.height);
                self.chain_tip_at_height
                    .insert(block_info.height, bhh.clone());
            }
        }
        *self.current_chain_tip.borrow_mut() = bhh;
        Ok(prior_tip)
    }

    fn get_block_at_height(&mut self, height: u32) -> Option<StacksBlockId> {
        if let Some(remote_network_info) = &self.remote_network_info {
            if height <= remote_network_info.initial_height {
                let block_info = self.get_remote_block_info_from_height(height);
                return Some(block_info.index_block_hash);
            }
        }
        self.chain_tip_at_height.get(&height).cloned()
    }

    /// this function returns the current block height, as viewed by this marfed-kv structure,
    ///  i.e., it changes on time-shifted evaluation. the open_chain_tip functions always
    ///   return data about the chain tip that is currently open for writing.
    fn get_current_block_height(&mut self) -> u32 {
        let current_chain_tip = self.current_chain_tip.borrow().clone();
        if let Some(&height) = self.height_at_chain_tip.get(&current_chain_tip) {
            return height;
        }

        if let Some(initial_height) = self.remote_network_info.as_ref().map(|d| d.initial_height) {
            let block_info = self.get_remote_block_info_from_hash(&current_chain_tip);
            if block_info.height <= initial_height {
                return block_info.height;
            }
        }

        u32::MAX
    }

    fn get_open_chain_tip_height(&mut self) -> u32 {
        self.height_at_chain_tip
            .get(&self.open_chain_tip)
            .copied()
            .unwrap_or(u32::MAX)
    }

    fn get_open_chain_tip(&mut self) -> StacksBlockId {
        self.open_chain_tip.clone()
    }

    fn insert_metadata(
        &mut self,
        contract: &QualifiedContractIdentifier,
        key: &str,
        value: &str,
    ) -> Result<()> {
        self.metadata
            .insert((contract.to_string(), key.to_string()), value.to_string());
        Ok(())
    }

    fn get_metadata(
        &mut self,
        contract: &QualifiedContractIdentifier,
        key: &str,
    ) -> Result<Option<String>> {
        let metadata = self.metadata.get(&(contract.to_string(), key.to_string()));
        if metadata.is_some() {
            return Ok(metadata.cloned());
        }
        if self.remote_network_info.is_some() && !self.local_accounts.contains(&contract.issuer) {
            let data = self.fetch_clarity_metadata(contract, key);
            if let Ok(Some(value)) = &data {
                self.insert_metadata(contract, key, value)?;
            }
            return data;
        }
        Ok(None)
    }

    fn get_contract_hash(
        &mut self,
        _contract: &QualifiedContractIdentifier,
    ) -> Result<(StacksBlockId, Sha512Trunc256Sum)> {
        panic!("Datastore cannot get_contract_hash")
    }

    fn get_metadata_manual(
        &mut self,
        _at_height: u32,
        _contract: &QualifiedContractIdentifier,
        _key: &str,
    ) -> Result<Option<String>> {
        panic!("Datastore cannot get_metadata_manual")
    }

    fn get_cc_special_cases_handler(&self) -> Option<clarity::vm::database::SpecialCaseHandler> {
        Some(&handle_contract_call_special_cases)
    }

    fn make_contract_commitment(&mut self, contract_hash: Sha512Trunc256Sum) -> String {
        ContractCommitment {
            hash: contract_hash,
            block_height: self.get_open_chain_tip_height(),
        }
        .serialize()
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn get_side_store(&mut self) -> &rusqlite::Connection {
        panic!("Datastore cannot get_side_store")
    }
}

impl Datastore {
    pub fn new(
        clarity_datastore: &ClarityDatastore,
        constants: StacksConstants,
        client: HttpClient,
    ) -> Self {
        if clarity_datastore.remote_network_info.is_some() {
            return Self::new_with_remote_data(clarity_datastore, constants, client);
        }

        let stacks_chain_height = 0;
        let burn_chain_height = 0;
        let bytes = height_to_hashed_bytes(stacks_chain_height);
        let id = StacksBlockId(bytes);
        let genesis_time = chrono::Utc::now().timestamp() as u64;

        let burn_block_hashes = BurnBlockHashes::from_height(burn_chain_height);
        let burn_block_header_hash = burn_block_hashes.header_hash.clone();

        let burn_block = BurnBlockInfo {
            consensus_hash: burn_block_hashes.consensus_hash,
            vrf_seed: burn_block_hashes.vrf_seed,
            sortition_id: burn_block_hashes.sortition_id,
            burn_block_time: genesis_time,
            burn_chain_height,
        };

        let stacks_block = StacksBlockInfo {
            block_header_hash: BlockHeaderHash(bytes),
            burn_block_header_hash: burn_block_hashes.header_hash.clone(),
            stacks_block_time: genesis_time + SECONDS_BETWEEN_STACKS_BLOCKS,
        };

        let sortition_lookup = HashMap::from([(
            burn_block.sortition_id.clone(),
            burn_block_header_hash.clone(),
        )]);
        let consensus_hash_lookup = HashMap::from([(
            burn_block.consensus_hash.clone(),
            burn_block.sortition_id.clone(),
        )]);
        let tenure_height_at_stacks_height = HashMap::from([(0, 0)]);
        let stacks_height_at_tenure_height = HashMap::from([(0, 0)]);
        let burn_blocks = HashMap::from([(burn_block_header_hash.clone(), burn_block.clone())]);
        let stacks_blocks = HashMap::from([(id.clone(), stacks_block)]);

        Datastore {
            genesis_id: id,
            burn_chain_tip: burn_block_header_hash,
            burn_chain_height,
            tenure_height: 0,
            current_chain_tip: Rc::clone(&clarity_datastore.current_chain_tip),
            remote_block_info_cache: Rc::clone(&clarity_datastore.remote_block_info_cache),
            remote_sortition_cache: Rc::clone(&clarity_datastore.remote_sortition_cache),
            burn_blocks,
            stacks_chain_height,
            stacks_blocks,
            sortition_lookup,
            consensus_hash_lookup,
            tenure_height_at_stacks_height,
            stacks_height_at_tenure_height,
            current_epoch: StacksEpochId::Epoch2_05,
            current_epoch_start_height: stacks_chain_height,
            constants,
            remote_network_info: None,
            initial_burn_height: 0,
            client,
        }
    }

    fn new_with_remote_data(
        clarity_datastore: &ClarityDatastore,
        constants: StacksConstants,
        client: HttpClient,
    ) -> Self {
        let current_chain_tip = clarity_datastore.current_chain_tip.borrow();
        let stacks_chain_height = clarity_datastore
            .height_at_chain_tip
            .get(&current_chain_tip)
            .unwrap();

        let block = {
            let cache = clarity_datastore.remote_block_info_cache.borrow();
            cache.get(&current_chain_tip).unwrap().clone()
        };
        let sortition = {
            let cache = clarity_datastore.remote_sortition_cache.borrow();
            cache.get(&block.burn_block_hash).unwrap().clone()
        };

        let is_mainnet = clarity_datastore
            .remote_network_info
            .as_ref()
            .unwrap()
            .is_mainnet;

        let burn_chain_height = block.burn_block_height;
        let id = block.index_block_hash.clone();
        let burn_block_header_hash = block.burn_block_hash.clone();
        let block_header_hash = block.hash;

        let sortition_id = sortition.sortition_id.clone();
        let consensus_hash = sortition.consensus_hash;

        let vrf_seed = sortition.vrf_seed.unwrap_or_else(|| {
            let bytes = height_to_hashed_bytes(burn_chain_height);
            VRFSeed(bytes)
        });

        let burn_block = BurnBlockInfo {
            consensus_hash,
            vrf_seed,
            sortition_id: sortition_id.clone(),
            burn_block_time: block.burn_block_time,
            burn_chain_height,
        };

        let stacks_block = StacksBlockInfo {
            block_header_hash,
            burn_block_header_hash: burn_block_header_hash.clone(),
            stacks_block_time: block.block_time,
        };

        let sortition_lookup =
            HashMap::from([(sortition_id.clone(), burn_block_header_hash.clone())]);
        let consensus_hash_lookup =
            HashMap::from([(burn_block.consensus_hash.clone(), sortition_id)]);
        let tenure_height_at_stacks_height =
            HashMap::from([(*stacks_chain_height, block.tenure_height)]);
        let stacks_height_at_tenure_height =
            HashMap::from([(block.tenure_height, *stacks_chain_height)]);
        let burn_blocks = HashMap::from([(burn_block_header_hash.clone(), burn_block)]);
        let stacks_blocks = HashMap::from([(id.clone(), stacks_block)]);

        Datastore {
            genesis_id: id,
            burn_chain_tip: burn_block_header_hash,
            burn_chain_height,
            tenure_height: block.tenure_height,
            current_chain_tip: Rc::clone(&clarity_datastore.current_chain_tip),
            remote_block_info_cache: Rc::clone(&clarity_datastore.remote_block_info_cache),
            remote_sortition_cache: Rc::clone(&clarity_datastore.remote_sortition_cache),
            burn_blocks,
            stacks_chain_height: *stacks_chain_height,
            stacks_blocks,
            sortition_lookup,
            consensus_hash_lookup,
            tenure_height_at_stacks_height,
            stacks_height_at_tenure_height,
            current_epoch: epoch_for_height(is_mainnet, *stacks_chain_height),
            current_epoch_start_height: *stacks_chain_height,
            constants,
            remote_network_info: clarity_datastore.remote_network_info.clone(),
            initial_burn_height: burn_chain_height,
            client,
        }
    }

    pub fn get_current_epoch(&self) -> StacksEpochId {
        self.current_epoch
    }

    pub fn get_current_stacks_block_height(&self) -> u32 {
        self.stacks_chain_height
    }

    pub fn get_current_burn_block_height(&self) -> u32 {
        self.burn_chain_height
    }

    fn get_tenure_for_burn_block_height(&self, height: u32) -> u32 {
        *self
            .tenure_height_at_stacks_height
            .get(&height)
            .unwrap_or(&0)
    }

    pub fn get_current_tenure(&self) -> u32 {
        self.get_tenure_for_burn_block_height(self.stacks_chain_height)
    }

    fn build_next_stacks_block(&self, clarity_datastore: &ClarityDatastore) -> StacksBlockInfo {
        let stacks_block_height = self.stacks_chain_height;

        let previous_stacks_block = self
            .stacks_blocks
            .get(&clarity_datastore.open_chain_tip)
            .expect("current chain tip missing in stacks block table");
        let last_burn_block = self
            .burn_blocks
            .get(&self.burn_chain_tip)
            .expect("burn block missing in burn block table");

        let last_block_time = std::cmp::max(
            previous_stacks_block.stacks_block_time,
            last_burn_block.burn_block_time,
        );

        let block_header_hash = {
            let mut buffer = height_to_hashed_bytes(stacks_block_height);
            buffer[0] = 1;
            BlockHeaderHash(buffer)
        };
        let stacks_block_time: u64 = last_block_time + SECONDS_BETWEEN_STACKS_BLOCKS;

        StacksBlockInfo {
            block_header_hash,
            burn_block_header_hash: self.burn_chain_tip.clone(),
            stacks_block_time,
        }
    }

    pub fn advance_burn_chain_tip(
        &mut self,
        clarity_datastore: &mut ClarityDatastore,
        count: u32,
    ) -> u32 {
        for _ in 1..=count {
            let next_burn_block_time = {
                let last_stacks_block = self
                    .stacks_blocks
                    .get(&clarity_datastore.open_chain_tip)
                    .unwrap_or_else(|| {
                        panic!(
                            "current chain tip missing in stacks_blocks table: {}",
                            clarity_datastore.open_chain_tip
                        )
                    });
                let last_burn_block =
                    self.burn_blocks
                        .get(&self.burn_chain_tip)
                        .unwrap_or_else(|| {
                            panic!(
                                "burn block missing in burn_blocks table: {}",
                                self.burn_chain_tip
                            )
                        });

                let mut next_burn_block_time =
                    last_burn_block.burn_block_time + SECONDS_BETWEEN_BURN_BLOCKS;
                if last_stacks_block.stacks_block_time > next_burn_block_time {
                    next_burn_block_time =
                        last_stacks_block.stacks_block_time + SECONDS_BETWEEN_STACKS_BLOCKS;
                }
                next_burn_block_time
            };

            let previous_height = self.burn_chain_height;
            let next_height = previous_height + 1;
            let burn_block_hashes = BurnBlockHashes::from_height(next_height);
            let burn_block_header_hash = burn_block_hashes.header_hash.clone();

            let burn_block = BurnBlockInfo {
                consensus_hash: burn_block_hashes.consensus_hash,
                vrf_seed: burn_block_hashes.vrf_seed,
                sortition_id: burn_block_hashes.sortition_id,
                burn_block_time: next_burn_block_time,
                burn_chain_height: next_height,
            };

            self.consensus_hash_lookup.insert(
                burn_block.consensus_hash.clone(),
                burn_block.sortition_id.clone(),
            );
            self.sortition_lookup.insert(
                burn_block.sortition_id.clone(),
                burn_block_header_hash.clone(),
            );
            self.burn_chain_tip = burn_block_header_hash.clone();
            self.burn_blocks
                .insert(burn_block_header_hash, burn_block.clone());
            self.burn_chain_height = next_height;
            self.tenure_height += 1;
            self.advance_stacks_chain_tip(clarity_datastore, 1);

            self.stacks_height_at_tenure_height
                .insert(self.tenure_height, self.stacks_chain_height);
        }

        self.burn_chain_height
    }

    pub fn advance_stacks_chain_tip(
        &mut self,
        clarity_datastore: &mut ClarityDatastore,
        count: u32,
    ) -> u32 {
        for _ in 1..=count {
            self.stacks_chain_height += 1;
            let bytes = height_to_hashed_bytes(self.stacks_chain_height);
            let id = StacksBlockId(bytes);
            let block_info = self.build_next_stacks_block(clarity_datastore);

            if self.current_epoch.uses_marfed_block_time() {
                clarity_datastore
                    .put_all_data(vec![(
                        CLARITY_STORAGE_BLOCK_TIME_KEY.to_string(),
                        format!("{}", block_info.stacks_block_time),
                    )])
                    .expect("failed to update vm-epoch::block-height");
            }
            self.stacks_blocks.insert(id.clone(), block_info);

            self.tenure_height_at_stacks_height
                .insert(self.stacks_chain_height, self.tenure_height);
            clarity_datastore
                .height_at_chain_tip
                .entry(id.clone())
                .or_insert(self.stacks_chain_height);
            clarity_datastore
                .chain_tip_at_height
                .entry(self.stacks_chain_height)
                .or_insert(id.clone());
            clarity_datastore.open_chain_tip = id.clone();
            *clarity_datastore.current_chain_tip.borrow_mut() = id;
        }

        self.stacks_chain_height
    }

    pub fn set_current_epoch(
        &mut self,
        clarity_datastore: &mut ClarityDatastore,
        epoch: StacksEpochId,
    ) {
        if epoch == self.current_epoch {
            return;
        }
        clarity_datastore.put("vm-epoch::epoch-version", &format!("{:08x}", epoch as u32));
        self.current_epoch = epoch;
        self.current_epoch_start_height = self.stacks_chain_height;
        if epoch >= StacksEpochId::Epoch30 {
            // ideally the burn chain tip should be advanced for each new epoch
            // but this would introduce breaking changes to existing 2.x tests
            self.advance_burn_chain_tip(clarity_datastore, 1);
        }
    }
}

impl HeadersDB for Datastore {
    fn get_stacks_block_header_hash_for_block(
        &self,
        id_bhh: &StacksBlockId,
        _epoch_id: &StacksEpochId,
    ) -> Option<BlockHeaderHash> {
        if let Some(hash) = self
            .stacks_blocks
            .get(id_bhh)
            .map(|id| id.block_header_hash.clone())
        {
            return Some(hash);
        };

        self.remote_block_info_cache
            .borrow()
            .get(id_bhh)
            .map(|block| block.hash.clone())
    }

    fn get_burn_header_hash_for_block(
        &self,
        id_bhh: &StacksBlockId,
    ) -> Option<BurnchainHeaderHash> {
        if let Some(hash) = self
            .stacks_blocks
            .get(id_bhh)
            .map(|block| block.burn_block_header_hash.clone())
        {
            return Some(hash);
        }

        if self.remote_network_info.is_some() {
            return self
                .remote_block_info_cache
                .borrow()
                .get(id_bhh)
                .map(|block| block.burn_block_hash.clone());
        }

        None
    }

    fn get_consensus_hash_for_block(
        &self,
        id_bhh: &StacksBlockId,
        _epoch_id: &StacksEpochId,
    ) -> Option<ConsensusHash> {
        let bbh = self.get_burn_header_hash_for_block(id_bhh)?;
        if let Some(consensus_hash) = self.burn_blocks.get(&bbh).map(|b| b.consensus_hash.clone()) {
            return Some(consensus_hash);
        }
        if self.remote_network_info.is_some() {
            return self
                .remote_sortition_cache
                .borrow()
                .get(&bbh)
                .map(|s| s.consensus_hash.clone());
        }
        None
    }

    fn get_vrf_seed_for_block(
        &self,
        id_bhh: &StacksBlockId,
        _epoch_id: &StacksEpochId,
    ) -> Option<VRFSeed> {
        let bbh = self.get_burn_header_hash_for_block(id_bhh)?;
        if let Some(vrf_seed) = self.burn_blocks.get(&bbh).map(|b| b.vrf_seed.clone()) {
            return Some(vrf_seed);
        }
        if self.remote_network_info.is_some() {
            return self
                .remote_sortition_cache
                .borrow()
                .get(&bbh)
                .and_then(|s| s.vrf_seed.clone());
        }
        None
    }

    fn get_stacks_block_time_for_block(&self, id_bhh: &StacksBlockId) -> Option<u64> {
        if let Some(time) = self
            .stacks_blocks
            .get(id_bhh)
            .map(|id| id.stacks_block_time)
        {
            return Some(time);
        };

        self.remote_block_info_cache
            .borrow()
            .get(id_bhh)
            .map(|block| block.block_time)
    }

    fn get_burn_block_time_for_block(
        &self,
        id_bhh: &StacksBlockId,
        _epoch_id: Option<&StacksEpochId>,
    ) -> Option<u64> {
        if let Some(hash) = self
            .get_burn_header_hash_for_block(id_bhh)
            .and_then(|hash| self.burn_blocks.get(&hash))
            .map(|b| b.burn_block_time)
        {
            return Some(hash);
        }

        if self.remote_network_info.is_some() {
            return self
                .remote_block_info_cache
                .borrow()
                .get(id_bhh)
                .map(|block| block.burn_block_time);
        }

        None
    }

    fn get_burn_block_height_for_block(&self, id_bhh: &StacksBlockId) -> Option<u32> {
        if let Some(height) = self
            .get_burn_header_hash_for_block(id_bhh)
            .and_then(|hash| self.burn_blocks.get(&hash))
            .map(|b| b.burn_chain_height)
        {
            return Some(height);
        }

        self.remote_block_info_cache
            .borrow()
            .get(id_bhh)
            .map(|block| block.burn_block_height)
    }

    fn get_stacks_height_for_tenure_height(
        &self,
        _id_bhh: &StacksBlockId,
        tenure_height: u32,
    ) -> Option<u32> {
        if let Some(height) = self
            .stacks_height_at_tenure_height
            .get(&tenure_height)
            .copied()
        {
            return Some(height);
        }

        // todo: if epoch < 3.0
        Some(tenure_height)
    }

    fn get_miner_address(
        &self,
        id_bhh: &StacksBlockId,
        _epoch_id: &StacksEpochId,
    ) -> Option<StacksAddress> {
        if self.get_burn_block_height_for_block(id_bhh).is_some() {
            return StacksAddress::burn_address(id_bhh != &self.genesis_id).into();
        }
        None
    }

    fn get_burnchain_tokens_spent_for_block(
        &self,
        id_bhh: &StacksBlockId,
        _epoch_id: &StacksEpochId,
    ) -> Option<u128> {
        if id_bhh == &self.genesis_id {
            return Some(0);
        };
        if self.get_burn_block_height_for_block(id_bhh).is_some() {
            return Some(2000);
        };
        None
    }

    fn get_burnchain_tokens_spent_for_winning_block(
        &self,
        id_bhh: &StacksBlockId,
        _epoch_id: &StacksEpochId,
    ) -> Option<u128> {
        if id_bhh == &self.genesis_id {
            return Some(0);
        };
        None
    }

    fn get_tokens_earned_for_block(
        &self,
        id_bhh: &StacksBlockId,
        _epoch_id: &StacksEpochId,
    ) -> Option<u128> {
        if id_bhh == &self.genesis_id {
            return Some(0);
        };
        if self.get_burn_block_height_for_block(id_bhh).is_some() {
            return Some(5000);
        }
        None
    }
}

impl BurnStateDB for Datastore {
    fn get_v1_unlock_height(&self) -> u32 {
        0
    }

    fn get_v2_unlock_height(&self) -> u32 {
        0
    }

    fn get_v3_unlock_height(&self) -> u32 {
        0
    }

    fn get_pox_3_activation_height(&self) -> u32 {
        0
    }

    fn get_pox_4_activation_height(&self) -> u32 {
        0
    }

    fn get_tip_burn_block_height(&self) -> Option<u32> {
        use StacksEpochId::*;
        match self.current_epoch {
            Epoch10 | Epoch20 | Epoch2_05 | Epoch21 | Epoch22 | Epoch23 | Epoch24 | Epoch25 => {
                let current_chain_tip = self.current_chain_tip.borrow();
                if let Some(height) = self.get_burn_block_height_for_block(&current_chain_tip) {
                    return Some(height);
                }

                self.remote_block_info_cache
                    .borrow()
                    .get(&current_chain_tip)
                    .map(|block| block.burn_block_height)
            }
            // preserve the 3.0 -> 3.3 special behavior of burn-block-height
            // https://github.com/stacks-network/stacks-core/pull/5524
            Epoch30 | Epoch31 | Epoch32 | Epoch33 => Some(self.burn_chain_height),
        }
    }

    fn get_tip_sortition_id(&self) -> Option<SortitionId> {
        let current_chain_tip = self.current_chain_tip.borrow();
        self.get_burn_header_hash_for_block(&current_chain_tip)
            .and_then(|hash| self.burn_blocks.get(&hash))
            .map(|block| block.sortition_id.clone())
    }

    /// Returns the *burnchain block height* for the `sortition_id` is associated with.
    fn get_burn_block_height(&self, sortition_id: &SortitionId) -> Option<u32> {
        self.sortition_lookup
            .get(sortition_id)
            .and_then(|hash| self.burn_blocks.get(hash))
            .map(|burn_block_info| burn_block_info.burn_chain_height)
    }

    /// Returns the height of the burnchain when the Stacks chain started running.
    fn get_burn_start_height(&self) -> u32 {
        self.constants.burn_start_height
    }

    fn get_pox_prepare_length(&self) -> u32 {
        self.constants.pox_prepare_length
    }

    fn get_pox_reward_cycle_length(&self) -> u32 {
        self.constants.pox_reward_cycle_length
    }

    fn get_pox_rejection_fraction(&self) -> u64 {
        self.constants.pox_rejection_fraction
    }

    /// Returns the burnchain header hash for the given burn block height, as queried from the given SortitionId.
    ///
    /// Returns Some if `self.get_burn_start_height() <= height < self.get_burn_block_height(sortition_id)`, and None otherwise.
    fn get_burn_header_hash(
        &self,
        burn_height: u32,
        _sortition_id: &SortitionId,
    ) -> Option<BurnchainHeaderHash> {
        if burn_height > self.burn_chain_height {
            return None;
        }

        if self.remote_network_info.is_some() && burn_height <= self.initial_burn_height {
            let sortition = self.client.fetch_sortition(burn_height);
            return Some(sortition.burn_block_hash);
        }

        // Otherwise, generate the burn block hashes locally
        let burn_block_hashes = BurnBlockHashes::from_height(burn_height);
        Some(burn_block_hashes.header_hash)
    }

    /// Lookup a `SortitionId` keyed to a `ConsensusHash`.
    ///
    /// Returns None if no block found.
    fn get_sortition_id_from_consensus_hash(
        &self,
        consensus_hash: &ConsensusHash,
    ) -> Option<SortitionId> {
        self.consensus_hash_lookup.get(consensus_hash).cloned()
    }

    /// The epoch is defined as by a start and end height. This returns
    /// the epoch enclosing `height`.
    fn get_stacks_epoch(&self, _height: u32) -> Option<StacksEpoch> {
        Some(StacksEpoch {
            epoch_id: self.current_epoch,
            start_height: self.current_epoch_start_height.into(),
            end_height: u64::MAX,
            block_limit: BLOCK_LIMIT_MAINNET,
            network_epoch: epoch_to_peer_version(self.current_epoch),
        })
    }

    fn get_stacks_epoch_by_epoch_id(&self, _epoch_id: &StacksEpochId) -> Option<StacksEpoch> {
        self.get_stacks_epoch(0)
    }

    /// Get the PoX payout addresses for a given burnchain block
    fn get_pox_payout_addrs(
        &self,
        height: u32,
        _sortition_id: &SortitionId,
    ) -> Option<(Vec<TupleData>, u128)> {
        if height <= self.burn_chain_height {
            Some((vec![], 0))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::types::StacksEpoch;

    use super::*;
    use crate::repl::settings::ApiUrl;

    fn get_datastores() -> (ClarityDatastore, Datastore) {
        let client = HttpClient::new(ApiUrl("https://api.tesnet.hiro.so".to_string()));
        let constants = StacksConstants::default();
        let clarity_datastore = ClarityDatastore::new(None, client.clone());
        let datastore = Datastore::new(&clarity_datastore, constants, client);
        (clarity_datastore, datastore)
    }

    fn get_datastores_with_remote_data() -> (ClarityDatastore, Datastore) {
        let mut server = mockito::Server::new();
        let _ = server
            .mock("GET", "/extended/v2/blocks/10")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                r#"{
                    "canonical": true,
                    "height": 10,
                    "hash": "0xaff3b535a135348ed00023ec1bdc3da9005253a9ce80a4906ade03ea6685d342",
                    "block_time": 1735934294,
                    "block_time_iso": "2025-01-03T19:58:14.000Z",
                    "tenure_height": 10,
                    "index_block_hash": "0x201cf66636e693d95998b40ddd0cbe038432806046eed11866052f15a9fa8fc5",
                    "parent_block_hash": "0x94c3d8f56ed2e1093f26089572af9cc5d5b097d461dcc184196f1ee2070de063",
                    "parent_index_block_hash": "0x1969bdddb9902162f5fdd2ff49cabb30300a9819c89bedd4c27fed82f8c9cf4b",
                    "burn_block_time": 1735451504,
                    "burn_block_time_iso": "2024-12-29T05:51:44.000Z",
                    "burn_block_hash": "0x57f3e2bd4519e4263353bf6b7614a9cee7f2d36fe61409852d42e41afe5e6cad",
                    "burn_block_height": 798,
                    "miner_txid": "0x5fb426cf9eb4577b545bd731634886d5bd5c9d40d573e2cdb95100f483913491",
                    "tx_count": 2
                }"#,
            )
            .create();
        let _ = server
            .mock("GET", "/v3/sortitions/burn_height/798")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                r#"[{
                    "burn_block_hash": "0x57f3e2bd4519e4263353bf6b7614a9cee7f2d36fe61409852d42e41afe5e6cad",
                    "burn_block_height":798,
                    "burn_header_timestamp": 1735451504,
                    "sortition_id": "0x71e332329133c0f331c6b5e9b21a415ea0c32aa300a0e94a88e7c30d4aaf78c6",
                    "parent_sortition_id": "0xd6bffd8c4cd86428d5404ef36867319976008e84047d2acbae9079fd918c4de9",
                    "consensus_hash": "0x44f6511f569d3ed78d437af619d529b6c66a4fa2"
                }]"#,
            )
            .create();
        let client = HttpClient::new(ApiUrl(server.url()));
        let constants = StacksConstants::default();
        let clarity_datastore = ClarityDatastore::new(
            Some(RemoteNetworkInfo {
                initial_height: 10,
                is_mainnet: false,
                api_url: ApiUrl(server.url()),
                network_id: 2147483648,
                cache_location: Some(PathBuf::from("./.cache")),
            }),
            client.clone(),
        );
        let datastore = Datastore::new(&clarity_datastore, constants, client);
        (clarity_datastore, datastore)
    }

    #[test]
    fn test_advance_chain_tip() {
        let (mut clarity_datastore, mut datastore) = get_datastores();
        datastore.advance_burn_chain_tip(&mut clarity_datastore, 5);
        assert_eq!(datastore.stacks_chain_height, 5);
    }

    #[test]
    fn test_set_current_epoch() {
        let (mut clarity_datastore, mut datastore) = get_datastores();
        let epoch_id = StacksEpochId::Epoch25;
        datastore.set_current_epoch(&mut clarity_datastore, epoch_id);
        assert_eq!(datastore.current_epoch, epoch_id);
    }

    #[test]
    fn test_get_v1_unlock_height() {
        let (_, datastore) = get_datastores();
        assert_eq!(datastore.get_v1_unlock_height(), 0);
    }

    #[test]
    fn test_get_v2_unlock_height() {
        let (_, datastore) = get_datastores();
        assert_eq!(datastore.get_v2_unlock_height(), 0);
    }

    #[test]
    fn test_get_v3_unlock_height() {
        let (_, datastore) = get_datastores();
        assert_eq!(datastore.get_v3_unlock_height(), 0);
    }

    #[test]
    fn test_get_pox_3_activation_height() {
        let (_, datastore) = get_datastores();
        assert_eq!(datastore.get_pox_3_activation_height(), 0);
    }

    #[test]
    fn test_get_pox_4_activation_height() {
        let (_, datastore) = get_datastores();
        assert_eq!(datastore.get_pox_4_activation_height(), 0);
    }

    #[test]
    fn test_get_tip_burn_block_height() {
        let (mut clarity_datastore, mut datastore) = get_datastores();
        let chain_height = 10;
        datastore.advance_burn_chain_tip(&mut clarity_datastore, 10);
        let tip_burn_block_height = datastore.get_tip_burn_block_height();
        assert_eq!(tip_burn_block_height, Some(chain_height));
    }

    #[test]
    fn test_get_burn_start_height() {
        let (_, datastore) = get_datastores();
        assert_eq!(datastore.get_burn_start_height(), 0);
    }

    #[test]
    fn test_get_pox_prepare_length() {
        let (_, datastore) = get_datastores();
        assert_eq!(datastore.get_pox_prepare_length(), 50);
    }

    #[test]
    fn test_get_pox_reward_cycle_length() {
        let (_, datastore) = get_datastores();
        assert_eq!(datastore.get_pox_reward_cycle_length(), 1050);
    }

    #[test]
    fn test_get_pox_rejection_fraction() {
        let (_, datastore) = get_datastores();
        assert_eq!(datastore.get_pox_rejection_fraction(), 0);
    }

    #[test]
    fn test_get_stacks_epoch() {
        let (_, datastore) = get_datastores();
        let height = 10;
        let epoch = datastore.get_stacks_epoch(height);
        assert_eq!(
            epoch,
            Some(StacksEpoch {
                epoch_id: StacksEpochId::Epoch2_05,
                start_height: 0,
                end_height: u64::MAX,
                block_limit: BLOCK_LIMIT_MAINNET,
                network_epoch: clarity::consts::PEER_VERSION_EPOCH_2_05,
            })
        );
    }

    #[test]
    fn test_get_stacks_epoch_by_epoch_id() {
        let (_, datastore) = get_datastores();
        let epoch_id = StacksEpochId::Epoch2_05;
        let epoch = datastore.get_stacks_epoch_by_epoch_id(&epoch_id);
        assert_eq!(
            epoch,
            Some(StacksEpoch {
                epoch_id: StacksEpochId::Epoch2_05,
                start_height: 0,
                end_height: u64::MAX,
                block_limit: BLOCK_LIMIT_MAINNET,
                network_epoch: clarity::consts::PEER_VERSION_EPOCH_2_05,
            })
        );
    }

    #[test]
    fn test_get_current_block_height() {
        let (mut clarity_datastore, _) = get_datastores();
        let height = clarity_datastore.get_current_block_height();
        assert_eq!(height, 0);
    }

    #[test]
    fn test_get_current_block_heigth_with_remote_data() {
        let (mut clarity_datastore, _datastore) = get_datastores_with_remote_data();
        let height = clarity_datastore.get_current_block_height();
        assert_eq!(height, 10);
    }

    // make sure that when a ClarityDatastore is clones, the current_chain_tip is reset
    #[test]
    fn test_clarity_datastore_caching() {
        let (mut clarity_datastore, mut datastore) = get_datastores();

        let initial_tip = clarity_datastore.current_chain_tip.borrow().clone();

        let cache = clarity_datastore.clone();

        datastore.advance_burn_chain_tip(&mut clarity_datastore, 10);

        let current_tip = clarity_datastore.current_chain_tip.borrow().clone();
        assert_ne!(current_tip, initial_tip);

        #[allow(clippy::redundant_clone)]
        let clarity_datastore = cache.clone();
        let current_tip = clarity_datastore.current_chain_tip.borrow().clone();
        assert_eq!(current_tip, initial_tip);
    }
}
