use std::fs::{self, File};
use std::io::{BufReader, Read, Write};
use std::path::{Path, PathBuf};

use crate::{FileAccessor, StacksNetwork};

/// Parse a URI string to a PathBuf.
/// On native, only `file://` URIs are accepted and converted to filesystem paths.
/// On WASM, any URI scheme is accepted (e.g. `vscode-test-web://`, `vscode-vfs://`)
/// and the full URI string is kept as-is in the PathBuf so it can be sent back to
/// the VSCode client (which uses `Uri.parse()` on these strings).
fn path_from_file_uri(uri_string: &str) -> Option<PathBuf> {
    let url = url::Url::parse(uri_string).ok()?;
    #[cfg(not(target_arch = "wasm32"))]
    {
        if url.scheme() == "file" {
            url.to_file_path().ok()
        } else {
            None
        }
    }
    #[cfg(target_arch = "wasm32")]
    {
        // On WASM, keep the full URI string as the PathBuf value.
        // PathBuf operations (parent, join, file_name) work on URI strings
        // since they use `/` as separator, and the full URI is needed by the
        // JS client (customVFS.ts) which calls `Uri.parse()` on these strings.
        let _ = url;
        Some(PathBuf::from(uri_string))
    }
}

/// Remove `/./` segments from a path string.
/// `vscode-test-web` struggle with extra `/./` in paths
/// Uses string manipulation rather than `Path::components()` to preserve
/// URI scheme prefixes (e.g. `vscode-vfs://mount/...`) on WASM.
fn normalize_dot_segments(path: PathBuf) -> PathBuf {
    let s = path.to_string_lossy();
    if !s.contains("/./") {
        return path;
    }
    let mut s = s.into_owned();
    while s.contains("/./") {
        s = s.replace("/./", "/");
    }
    PathBuf::from(s)
}

/// Try to parse a location string as either a file:// URI or a plain path.
/// If relative and project_root is provided, resolve against it.
pub fn try_parse_path(location_string: &str, project_root: Option<&Path>) -> Option<PathBuf> {
    if let Some(path) = path_from_file_uri(location_string) {
        return Some(path);
    }
    let path = PathBuf::from(location_string);
    match (project_root, path.is_relative()) {
        (None, true) => None,
        (Some(root), true) => Some(normalize_dot_segments(root.join(&path))),
        (_, false) => Some(path),
    }
}

/// Walk up from a path to find a directory containing Clarinet.toml.
pub fn find_project_root(from: &Path) -> Result<PathBuf, String> {
    let mut path = from.to_path_buf();
    // If `from` is a file, start from its parent
    if path.is_file() {
        path.pop();
    }
    loop {
        path.push("Clarinet.toml");
        if path.exists() {
            path.pop();
            return Ok(path);
        }
        path.pop();
        if !path.pop() {
            return Err(format!(
                "unable to find root location from {}",
                from.display()
            ));
        }
    }
}

/// Async version of manifest location search using FileAccessor (for WASM).
pub async fn find_manifest_location_async(
    from: &Path,
    file_accessor: &dyn FileAccessor,
) -> Result<PathBuf, String> {
    let mut current = from.parent().map(|p| p.to_path_buf());
    while let Some(ref dir) = current {
        let candidate = dir.join("Clarinet.toml");
        if let Ok(true) = file_accessor
            .file_exists(candidate.to_string_lossy().to_string())
            .await
        {
            return Ok(candidate);
        }
        let next = dir.parent().map(|p| p.to_path_buf());
        if next.as_ref() == Some(dir) {
            break;
        }
        current = next;
    }
    Err(format!(
        "No Clarinet.toml is associated to the contract {}",
        from.file_name()
            .map(|f| f.to_string_lossy())
            .unwrap_or_default()
    ))
}

/// Find the manifest location from a path (non-async, native only).
pub fn find_manifest_location(from: &Path) -> Result<PathBuf, String> {
    let root = find_project_root(from)?;
    Ok(root.join("Clarinet.toml"))
}

/// Get the network manifest location for a given network.
pub fn get_network_manifest_path(project_root: &Path, network: &StacksNetwork) -> PathBuf {
    project_root.join("settings").join(match network {
        StacksNetwork::Devnet | StacksNetwork::Simnet => "Devnet.toml",
        StacksNetwork::Testnet => "Testnet.toml",
        StacksNetwork::Mainnet => "Mainnet.toml",
    })
}

/// Get the project root directory from a manifest (Clarinet.toml) path.
pub fn project_root_from_manifest_location(manifest_location: &Path) -> Result<PathBuf, String> {
    find_project_root(manifest_location.parent().unwrap_or(Path::new(".")))
}

/// Get the relative path from a base directory.
pub fn get_relative_path(path: &Path, base: &Path) -> Result<String, String> {
    path.strip_prefix(base)
        .map(|p| p.to_string_lossy().into_owned())
        .map_err(|_| format!("{} is not under {}", path.display(), base.display()))
}

/// Read file content as bytes.
pub fn read_content(path: &Path) -> Result<Vec<u8>, String> {
    let file =
        File::open(path).map_err(|e| format!("unable to read file {}\n{:?}", path.display(), e))?;
    let mut reader = BufReader::new(file);
    let mut buffer = vec![];
    reader
        .read_to_end(&mut buffer)
        .map_err(|e| format!("unable to read file {}\n{:?}", path.display(), e))?;
    Ok(buffer)
}

/// Read file content as a UTF-8 string.
pub fn read_content_as_utf8(path: &Path) -> Result<String, String> {
    let content = read_content(path)?;
    String::from_utf8(content)
        .map_err(|e| format!("unable to read content as utf8 {}\n{e:?}", path.display()))
}

/// Write content to a path, creating parent directories as needed.
pub fn write_content(path: &Path, content: &[u8]) -> Result<(), String> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|e| {
            format!(
                "unable to create parent directory {}\n{e}",
                parent.display()
            )
        })?;
    }
    let mut file =
        File::create(path).map_err(|e| format!("unable to open file {}\n{e}", path.display()))?;
    file.write_all(content)
        .map_err(|e| format!("unable to write file {}\n{e}", path.display()))?;
    Ok(())
}

/// Convert a PathBuf to a URL string.
/// On native, converts a filesystem path to a `file://` URI.
/// On WASM, the PathBuf already contains the full URI string, so return it as-is.
pub fn path_to_url_string(path: &Path) -> Result<String, String> {
    #[cfg(not(target_arch = "wasm32"))]
    {
        url::Url::from_file_path(path)
            .map_err(|_| format!("unable to convert path {} to url", path.display()))
            .map(String::from)
    }
    #[cfg(target_arch = "wasm32")]
    {
        Ok(path.to_string_lossy().into_owned())
    }
}
