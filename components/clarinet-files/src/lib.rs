pub mod clarinetrc;
pub mod error;
pub mod paths;

#[cfg(not(target_arch = "wasm32"))]
pub mod devnet_diff;
mod network_manifest;
mod project_manifest;
#[cfg(feature = "json_schema")]
pub mod schema;

pub use network_manifest::{BitcoinNetwork, StacksNetwork};

#[cfg(target_arch = "wasm32")]
mod wasm_fs_accessor;
use std::collections::HashMap;
use std::future::Future;
use std::path::PathBuf;
use std::pin::Pin;

pub use network_manifest::{
    compute_addresses, AccountConfig, DevnetConfig, DevnetConfigFile, NetworkManifest,
    NetworkManifestFile, PoxStackingOrder, DEFAULT_BITCOIN_EXPLORER_IMAGE,
    DEFAULT_BITCOIN_NODE_IMAGE, DEFAULT_DERIVATION_PATH, DEFAULT_DOCKER_PLATFORM,
    DEFAULT_EPOCH_2_0, DEFAULT_EPOCH_2_05, DEFAULT_EPOCH_2_1, DEFAULT_EPOCH_2_2, DEFAULT_EPOCH_2_3,
    DEFAULT_EPOCH_2_4, DEFAULT_EPOCH_2_5, DEFAULT_EPOCH_3_0, DEFAULT_EPOCH_3_1, DEFAULT_EPOCH_3_2,
    DEFAULT_EPOCH_3_3, DEFAULT_FAUCET_MNEMONIC, DEFAULT_FIRST_BURN_HEADER_HEIGHT,
    DEFAULT_POSTGRES_IMAGE, DEFAULT_STACKER_MNEMONIC, DEFAULT_STACKS_API_IMAGE,
    DEFAULT_STACKS_EXPLORER_IMAGE, DEFAULT_STACKS_MINER_MNEMONIC, DEFAULT_STACKS_NODE_IMAGE,
    DEFAULT_STACKS_SIGNER_IMAGE,
};
pub use project_manifest::{
    ProjectManifest, ProjectManifestFile, RequirementConfig, INVALID_CLARITY_VERSION,
};
#[cfg(target_arch = "wasm32")]
pub use wasm_fs_accessor::WASMFileSystemAccessor;

pub type FileAccessorResult<T> = Pin<Box<dyn Future<Output = Result<T, String>>>>;

pub trait FileAccessor {
    fn file_exists(&self, path: String) -> FileAccessorResult<bool>;
    fn read_file(&self, path: String) -> FileAccessorResult<String>;
    fn read_files(
        &self,
        contracts_paths: Vec<String>,
    ) -> FileAccessorResult<HashMap<String, String>>;
    fn write_file(&self, path: String, content: &[u8]) -> FileAccessorResult<()>;
}

pub fn get_manifest_location(path: Option<String>) -> Option<PathBuf> {
    if let Some(path) = path {
        let manifest_path = PathBuf::from(path);
        if !manifest_path.exists() {
            return None;
        }
        Some(manifest_path)
    } else {
        let mut current_dir = std::env::current_dir().unwrap();
        loop {
            current_dir.push("Clarinet.toml");

            if current_dir.exists() {
                return Some(current_dir);
            }
            current_dir.pop();

            if !current_dir.pop() {
                return None;
            }
        }
    }
}
