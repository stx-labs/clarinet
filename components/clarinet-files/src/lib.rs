extern crate serde;

#[macro_use]
extern crate serde_derive;

pub mod clarinetrc;

#[cfg(not(target_arch = "wasm32"))]
pub mod devnet_diff;
mod network_manifest;
mod project_manifest;
#[cfg(feature = "json_schema")]
pub mod schema;

pub use network_manifest::{BitcoinNetwork, StacksNetwork};

#[cfg(target_arch = "wasm32")]
mod wasm_fs_accessor;
use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::fmt;
use std::future::Future;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::str::FromStr;

pub use network_manifest::{
    compute_addresses, AccountConfig, DevnetConfig, DevnetConfigFile, NetworkManifest,
    NetworkManifestFile, PoxStackingOrder, DEFAULT_BITCOIN_EXPLORER_IMAGE,
    DEFAULT_BITCOIN_NODE_IMAGE, DEFAULT_DERIVATION_PATH, DEFAULT_DOCKER_PLATFORM,
    DEFAULT_EPOCH_2_0, DEFAULT_EPOCH_2_05, DEFAULT_EPOCH_2_1, DEFAULT_EPOCH_2_2, DEFAULT_EPOCH_2_3,
    DEFAULT_EPOCH_2_4, DEFAULT_EPOCH_2_5, DEFAULT_EPOCH_3_0, DEFAULT_EPOCH_3_1, DEFAULT_EPOCH_3_2,
    DEFAULT_FAUCET_MNEMONIC, DEFAULT_FIRST_BURN_HEADER_HEIGHT, DEFAULT_POSTGRES_IMAGE,
    DEFAULT_STACKER_MNEMONIC, DEFAULT_STACKS_API_IMAGE, DEFAULT_STACKS_EXPLORER_IMAGE,
    DEFAULT_STACKS_MINER_MNEMONIC, DEFAULT_STACKS_NODE_IMAGE, DEFAULT_STACKS_SIGNER_IMAGE,
};
pub use project_manifest::{
    ProjectManifest, ProjectManifestFile, RequirementConfig, INVALID_CLARITY_VERSION,
};
use serde::ser::{Serialize, SerializeMap, Serializer};
use url::Url;
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

#[derive(Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
#[serde(untagged)]
pub enum FileLocation {
    FileSystem { path: PathBuf },
    Url { url: Url },
}

impl fmt::Display for FileLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FileLocation::FileSystem { path } => write!(f, "{}", path.display()),
            FileLocation::Url { url } => write!(f, "{url}"),
        }
    }
}

impl FileLocation {
    pub fn try_parse(
        location_string: &str,
        project_root_location_hint: Option<&FileLocation>,
    ) -> Option<FileLocation> {
        if let Ok(location) = FileLocation::from_url_string(location_string) {
            return Some(location);
        }
        if let Ok(FileLocation::FileSystem { path }) =
            FileLocation::from_path_string(location_string)
        {
            match (project_root_location_hint, path.is_relative()) {
                (None, true) => return None,
                (Some(hint), true) => {
                    let mut location = hint.clone();
                    location.append_path(location_string).ok()?;
                    return Some(location);
                }
                (_, false) => return Some(FileLocation::FileSystem { path }),
            }
        }
        None
    }

    pub fn from_path(path: PathBuf) -> FileLocation {
        FileLocation::FileSystem { path }
    }

    pub fn from_url(url: Url) -> FileLocation {
        FileLocation::Url { url }
    }

    pub fn from_url_string(url_string: &str) -> Result<FileLocation, String> {
        let url = Url::from_str(url_string)
            .map_err(|e| format!("unable to parse {url_string} as a url\n{e:?}"))?;

        #[cfg(not(target_arch = "wasm32"))]
        if url.scheme() == "file" {
            let path = url
                .to_file_path()
                .map_err(|_| format!("unable to conver url {url} to path"))?;
            return Ok(FileLocation::FileSystem { path });
        }

        Ok(FileLocation::Url { url })
    }

    pub fn from_path_string(path_string: &str) -> Result<FileLocation, String> {
        let path = PathBuf::from_str(path_string)
            .map_err(|e| format!("unable to parse {path_string} as a path\n{e:?}"))?;
        Ok(FileLocation::FileSystem { path })
    }

    pub fn append_path(&mut self, path_string: &str) -> Result<(), String> {
        let path_to_append = PathBuf::from_str(path_string)
            .map_err(|e| format!("unable to read relative path {path_string}\n{e:?}"))?;
        match self.borrow_mut() {
            FileLocation::FileSystem { path } => {
                path.extend(&path_to_append);
            }
            FileLocation::Url { url } => {
                let mut paths_segments = url
                    .path_segments_mut()
                    .map_err(|_| "unable to mutate url")?;
                for component in path_to_append.components() {
                    let segment = component
                        .as_os_str()
                        .to_str()
                        .ok_or(format!("unable to format component {component:?}"))?;
                    paths_segments.push(segment);
                }
            }
        }
        Ok(())
    }

    pub fn read_content_as_utf8(&self) -> Result<String, String> {
        let content = self.read_content()?;
        let contract_as_utf8 = String::from_utf8(content)
            .map_err(|e| format!("unable to read content as utf8 {self}\n{e:?}"))?;
        Ok(contract_as_utf8)
    }

    fn fs_read_content(path: &Path) -> Result<Vec<u8>, String> {
        use std::fs::File;
        use std::io::{BufReader, Read};
        let file = File::open(path)
            .map_err(|e| format!("unable to read file {}\n{:?}", path.display(), e))?;
        let mut file_reader = BufReader::new(file);
        let mut file_buffer = vec![];
        file_reader
            .read_to_end(&mut file_buffer)
            .map_err(|e| format!("unable to read file {}\n{:?}", path.display(), e))?;
        Ok(file_buffer)
    }

    fn fs_exists(path: &Path) -> bool {
        path.exists()
    }

    fn fs_write_content(file_path: &PathBuf, content: &[u8]) -> Result<(), String> {
        use std::fs::{self, File};
        use std::io::Write;
        let mut parent_directory = file_path.clone();
        parent_directory.pop();
        fs::create_dir_all(&parent_directory).map_err(|e| {
            format!(
                "unable to create parent directory {}\n{}",
                parent_directory.display(),
                e
            )
        })?;
        let mut file = File::create(file_path)
            .map_err(|e| format!("unable to open file {}\n{}", file_path.display(), e))?;
        file.write_all(content)
            .map_err(|e| format!("unable to write file {}\n{}", file_path.display(), e))?;
        Ok(())
    }

    pub async fn get_project_manifest_location(
        &self,
        file_accessor: Option<&dyn FileAccessor>,
    ) -> Result<FileLocation, String> {
        match file_accessor {
            None => {
                let mut project_root_location = self.get_project_root_location()?;
                project_root_location.append_path("Clarinet.toml")?;
                Ok(project_root_location)
            }
            Some(file_accessor) => {
                let mut manifest_location = None;
                let mut parent_location = self.get_parent_location();
                while let Ok(ref parent) = parent_location {
                    let mut candidate = parent.clone();
                    candidate.append_path("Clarinet.toml")?;

                    if let Ok(exists) = file_accessor.file_exists(candidate.to_string()).await {
                        if exists {
                            manifest_location = Some(candidate);
                            break;
                        }
                    }
                    if &parent.get_parent_location().unwrap() == parent {
                        break;
                    }
                    parent_location = parent.get_parent_location();
                }
                match manifest_location {
                    Some(manifest_location) => Ok(manifest_location),
                    None => Err(format!(
                        "No Clarinet.toml is associated to the contract {}",
                        self.get_file_name().unwrap_or_default()
                    )),
                }
            }
        }
    }

    pub fn get_project_root_location(&self) -> Result<FileLocation, String> {
        let mut project_root_location = self.clone();
        match project_root_location.borrow_mut() {
            FileLocation::FileSystem { path } => {
                let mut manifest_found = false;
                while path.pop() {
                    path.push("Clarinet.toml");
                    if FileLocation::fs_exists(path) {
                        path.pop();
                        manifest_found = true;
                        break;
                    }
                    path.pop();
                }

                match manifest_found {
                    true => Ok(project_root_location),
                    false => Err(format!("unable to find root location from {self}")),
                }
            }
            _ => {
                unimplemented!();
            }
        }
    }

    pub fn get_parent_location(&self) -> Result<FileLocation, String> {
        let mut parent_location = self.clone();
        match &mut parent_location {
            FileLocation::FileSystem { path } => {
                let mut parent = path.clone();
                parent.pop();
                if parent.to_str() == path.to_str() {
                    return Err(String::from("reached root"));
                }
                path.pop();
            }
            FileLocation::Url { url } => {
                let mut segments = url
                    .path_segments_mut()
                    .map_err(|_| "unable to mutate url".to_string())?;
                segments.pop();
            }
        }
        Ok(parent_location)
    }

    pub fn get_network_manifest_location(
        &self,
        network: &StacksNetwork,
    ) -> Result<FileLocation, String> {
        let mut network_manifest_location = self.get_project_root_location()?;
        network_manifest_location.append_path("settings")?;
        network_manifest_location.append_path(match network {
            StacksNetwork::Devnet | StacksNetwork::Simnet => "Devnet.toml",
            StacksNetwork::Testnet => "Testnet.toml",
            StacksNetwork::Mainnet => "Mainnet.toml",
        })?;
        Ok(network_manifest_location)
    }

    pub fn get_relative_path_from_base(
        &self,
        base_location: &FileLocation,
    ) -> Result<String, String> {
        let file = self.to_string();
        Ok(file[(base_location.to_string().len() + 1)..].to_string())
    }

    pub fn get_relative_location(&self) -> Result<String, String> {
        let base = self.get_project_root_location().map(|l| l.to_string())?;
        let file = self.to_string();
        Ok(file[(base.len() + 1)..].to_string())
    }

    pub fn get_file_name(&self) -> Option<String> {
        match self {
            FileLocation::FileSystem { path } => {
                path.file_name().and_then(|f| f.to_str().map(String::from))
            }
            FileLocation::Url { url } => url
                .path_segments()
                .and_then(|mut segments| segments.next_back().map(String::from)),
        }
    }

    pub fn read_content(&self) -> Result<Vec<u8>, String> {
        let bytes = match &self {
            FileLocation::FileSystem { path } => FileLocation::fs_read_content(path),
            FileLocation::Url { url } => match url.scheme() {
                #[cfg(not(target_arch = "wasm32"))]
                "file" => {
                    let path = url
                        .to_file_path()
                        .map_err(|e| format!("unable to convert url {url} to path\n{e:?}"))?;
                    FileLocation::fs_read_content(&path)
                }
                "http" | "https" => {
                    unimplemented!()
                }
                _ => {
                    unimplemented!()
                }
            },
        }?;
        Ok(bytes)
    }

    pub fn exists(&self) -> bool {
        match self {
            FileLocation::FileSystem { path } => FileLocation::fs_exists(path),
            FileLocation::Url { url: _url } => unimplemented!(),
        }
    }

    pub fn write_content(&self, content: &[u8]) -> Result<(), String> {
        match self {
            FileLocation::FileSystem { path } => FileLocation::fs_write_content(path, content),
            FileLocation::Url { url: _url } => unimplemented!(),
        }
    }

    pub fn to_url_string(&self) -> Result<String, String> {
        match self {
            #[cfg(target_arch = "wasm32")]
            FileLocation::FileSystem { .. } => {
                Err("Url::from_file_path is not supported in wasm32".to_string())
            }
            #[cfg(not(target_arch = "wasm32"))]
            FileLocation::FileSystem { path } => Url::from_file_path(path)
                .map_err(|_| format!("unable to convert path {} to url", path.display()))
                .map(String::from),
            FileLocation::Url { url } => Ok(url.to_string()),
        }
    }

    pub fn to_path_buf(&self) -> PathBuf {
        match self {
            FileLocation::FileSystem { path } => path.clone(),
            FileLocation::Url { url } => PathBuf::from(url.to_string()),
        }
    }
}

impl Serialize for FileLocation {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        match self {
            FileLocation::FileSystem { path: _ } => {
                let path = match self.get_relative_location() {
                    Ok(relative_path) => relative_path, // Use relative path if possible
                    Err(_) => self.to_string(),         // Fallback on fully qualified path
                };
                map.serialize_entry("path", &path)?;
            }
            FileLocation::Url { url } => {
                map.serialize_entry("url", &url.to_string())?;
            }
        }
        map.end()
    }
}

pub fn get_manifest_location(path: Option<String>) -> Option<FileLocation> {
    if let Some(path) = path {
        let manifest_path = PathBuf::from(path);
        if !manifest_path.exists() {
            return None;
        }
        Some(FileLocation::from_path(manifest_path))
    } else {
        let mut current_dir = std::env::current_dir().unwrap();
        loop {
            current_dir.push("Clarinet.toml");

            if current_dir.exists() {
                return Some(FileLocation::from_path(current_dir));
            }
            current_dir.pop();

            if !current_dir.pop() {
                return None;
            }
        }
    }
}
