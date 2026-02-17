use std::fs;
use std::fs::File;
use std::io::BufReader;
use std::path::{Path, PathBuf};

use chainhook_sdk::chainhooks::types::{ChainhookSpecificationNetworkMap, ChainhookStore};
use chainhook_sdk::types::{BitcoinNetwork, StacksNetwork};

pub fn parse_chainhook_full_specification(
    path: &PathBuf,
) -> Result<ChainhookSpecificationNetworkMap, String> {
    let path = File::open(path).map_err(|_| format!("unable to locate {}", path.display()))?;

    let mut hook_spec_file_reader = BufReader::new(path);
    let specification: ChainhookSpecificationNetworkMap =
        serde_json::from_reader(&mut hook_spec_file_reader)
            .map_err(|e| format!("unable to parse chainhook spec: {e}"))?;

    Ok(specification)
}

pub fn load_chainhooks(
    project_root: &Path,
    networks: &(BitcoinNetwork, StacksNetwork),
) -> Result<ChainhookStore, String> {
    let hook_files = get_chainhooks_files(project_root)?;
    let mut stacks_chainhooks = vec![];
    let mut bitcoin_chainhooks = vec![];
    for (path, relative_path) in hook_files.into_iter() {
        match parse_chainhook_full_specification(&path) {
            Ok(hook) => match hook {
                ChainhookSpecificationNetworkMap::Bitcoin(predicate) => {
                    let mut spec = predicate.into_specification_for_network(&networks.0)?;
                    spec.enabled = true;
                    bitcoin_chainhooks.push(spec)
                }
                ChainhookSpecificationNetworkMap::Stacks(predicate) => {
                    let mut spec = predicate.into_specification_for_network(&networks.1)?;
                    spec.enabled = true;
                    stacks_chainhooks.push(spec)
                }
            },
            Err(msg) => return Err(format!("{relative_path} syntax incorrect: {msg}")),
        };
    }
    Ok(ChainhookStore {
        stacks_chainhooks,
        bitcoin_chainhooks,
    })
}

fn get_chainhooks_files(project_root: &Path) -> Result<Vec<(PathBuf, String)>, String> {
    let chainhooks_dir = project_root.join("chainhooks");
    let prefix_len = chainhooks_dir.to_string_lossy().len() + 1;
    let Ok(paths) = fs::read_dir(&chainhooks_dir) else {
        return Ok(vec![]);
    };
    let mut hook_paths = vec![];
    for path in paths {
        let file = path.unwrap().path();
        let is_extension_valid = file
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| ext == "json");

        if let Some(true) = is_extension_valid {
            let relative_path = file.clone();
            let (_, relative_path) = relative_path.to_str().unwrap().split_at(prefix_len);
            hook_paths.push((file, relative_path.to_string()));
        }
    }

    Ok(hook_paths)
}
