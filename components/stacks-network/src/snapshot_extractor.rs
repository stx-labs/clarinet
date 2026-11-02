use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::sync::mpsc::Sender;
use std::{fmt, fs};

use clarinet_files::{
    DevnetConfig, DEFAULT_BITCOIN_NODE_IMAGE, DEFAULT_STACKS_NODE_IMAGE,
    DEFAULT_STACKS_SIGNER_IMAGE,
};
use flate2::read::GzDecoder;
use stacks_common::util::hash::Hash160;
use tar::Archive;

use crate::orchestrator::DEVNET_SNAPSHOT_READY_MARKER;
use crate::DevnetEvent;

const DEVNET_SNAPSHOT: &[u8] = include_bytes!("../data/devnet_default_snapshot.tar.gz");

/// The images a snapshot was built with. Written to the ready marker, since chainstate
/// from other versions may not be readable (Bitcoin Core 28+ obfuscates block files).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SnapshotImages<'a> {
    pub bitcoin_node: &'a str,
    pub stacks_node: &'a str,
    pub stacks_signer: &'a str,
}

impl SnapshotImages<'static> {
    /// The images the embedded snapshot was built with.
    pub const DEFAULT: Self = Self {
        bitcoin_node: DEFAULT_BITCOIN_NODE_IMAGE,
        stacks_node: DEFAULT_STACKS_NODE_IMAGE,
        stacks_signer: DEFAULT_STACKS_SIGNER_IMAGE,
    };
}

impl SnapshotImages<'_> {
    /// Snapshots are cached per image set, so building one doesn't evict another.
    pub fn cache_dir(&self, cache_root: &Path) -> PathBuf {
        cache_root.join(Hash160::from_data(self.to_string().as_bytes()).to_hex())
    }
}

impl<'a> From<&'a DevnetConfig> for SnapshotImages<'a> {
    fn from(config: &'a DevnetConfig) -> Self {
        Self {
            bitcoin_node: &config.bitcoin_node_image_url,
            stacks_node: &config.stacks_node_image_url,
            stacks_signer: &config.stacks_signer_image_url,
        }
    }
}

impl fmt::Display for SnapshotImages<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            bitcoin_node,
            stacks_node,
            stacks_signer,
        } = self;
        writeln!(f, "bitcoin_node_image_url = \"{bitcoin_node}\"")?;
        writeln!(f, "stacks_node_image_url = \"{stacks_node}\"")?;
        writeln!(f, "stacks_signer_image_url = \"{stacks_signer}\"")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum SnapshotPlan {
    UseCached,
    /// Replace the cache, if any, with the embedded snapshot.
    ExtractEmbedded,
    /// No snapshot matches the images. The cache is left untouched.
    RunFromGenesis,
}

impl SnapshotPlan {
    pub fn new(cached_marker: Option<&str>, images: &SnapshotImages) -> Self {
        if cached_marker == Some(images.to_string().as_str()) {
            Self::UseCached
        } else if *images == SnapshotImages::DEFAULT {
            Self::ExtractEmbedded
        } else {
            Self::RunFromGenesis
        }
    }
}

/// Before snapshots were cached per image set, the cache root held a single one.
pub fn remove_unkeyed_snapshot(cache_root: &Path) -> io::Result<()> {
    for name in ["bitcoin", "stacks", "events_export"] {
        let path = cache_root.join(name);
        if path.exists() {
            fs::remove_dir_all(path)?;
        }
    }
    let marker = cache_root.join(DEVNET_SNAPSHOT_READY_MARKER);
    if marker.exists() {
        fs::remove_file(marker)?;
    }
    Ok(())
}

/// Extract embedded devnet snapshot to the specified directory
pub fn extract_embedded_snapshot(
    snapshot_dir: &Path,
    devnet_event_tx: &Sender<DevnetEvent>,
) -> Result<(), String> {
    let _ = devnet_event_tx.send(DevnetEvent::info(
        "Extracting embedded devnet snapshot data...".to_string(),
    ));

    unpack_snapshot(GzDecoder::new(DEVNET_SNAPSHOT), snapshot_dir)?;

    let _ = devnet_event_tx.send(DevnetEvent::success(
        "Embedded devnet snapshot extracted successfully".to_string(),
    ));

    Ok(())
}

/// Unpacks the archive's `devnet` directory. The ready marker is written last,
/// so an interrupted extraction is never mistaken for a complete snapshot.
fn unpack_snapshot(tar: impl Read, snapshot_dir: &Path) -> Result<(), String> {
    if snapshot_dir.exists() {
        fs::remove_dir_all(snapshot_dir)
            .map_err(|e| format!("Failed to remove stale snapshot directory: {e}"))?;
    }
    fs::create_dir_all(snapshot_dir)
        .map_err(|e| format!("Failed to create snapshot directory: {e}"))?;

    let mut archive = Archive::new(tar);
    for entry in archive
        .entries()
        .map_err(|e| format!("Failed to read archive entries: {e}"))?
    {
        let mut entry = entry.map_err(|e| format!("Failed to read archive entry: {e}"))?;
        let path = entry
            .path()
            .map_err(|e| format!("Failed to get entry path: {e}"))?;

        let Ok(stripped_path) = path.strip_prefix("devnet") else {
            continue;
        };
        if stripped_path == Path::new(DEVNET_SNAPSHOT_READY_MARKER) {
            continue;
        }
        let dest_path = snapshot_dir.join(stripped_path);
        entry
            .unpack(&dest_path)
            .map_err(|e| format!("Failed to extract file {}: {}", dest_path.display(), e))?;
    }

    fs::write(
        snapshot_dir.join(DEVNET_SNAPSHOT_READY_MARKER),
        SnapshotImages::DEFAULT.to_string(),
    )
    .map_err(|e| format!("Failed to write snapshot marker: {e}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    const CUSTOM: SnapshotImages = SnapshotImages {
        bitcoin_node: "example/bitcoind:custom",
        ..SnapshotImages::DEFAULT
    };

    #[test]
    fn marker_lists_every_image() {
        let marker = SnapshotImages::DEFAULT.to_string();
        assert_eq!(marker.lines().count(), 3);
        assert!(marker.contains(&format!("\"{DEFAULT_BITCOIN_NODE_IMAGE}\"")));
        assert!(marker.contains(&format!("\"{DEFAULT_STACKS_NODE_IMAGE}\"")));
        assert!(marker.contains(&format!("\"{DEFAULT_STACKS_SIGNER_IMAGE}\"")));
    }

    #[test]
    fn matching_cache_is_used() {
        let default = SnapshotImages::DEFAULT;
        let plan = SnapshotPlan::new(Some(&default.to_string()), &default);
        assert_eq!(plan, SnapshotPlan::UseCached);

        let plan = SnapshotPlan::new(Some(&CUSTOM.to_string()), &CUSTOM);
        assert_eq!(plan, SnapshotPlan::UseCached);
    }

    #[test]
    fn default_images_extract_embedded_snapshot() {
        let default = SnapshotImages::DEFAULT;
        let plan = SnapshotPlan::new(None, &default);
        assert_eq!(plan, SnapshotPlan::ExtractEmbedded);

        let plan = SnapshotPlan::new(Some(""), &default);
        assert_eq!(plan, SnapshotPlan::ExtractEmbedded);

        let plan = SnapshotPlan::new(Some(&CUSTOM.to_string()), &default);
        assert_eq!(plan, SnapshotPlan::ExtractEmbedded);
    }

    #[test]
    fn custom_images_without_matching_cache_run_from_genesis() {
        let plan = SnapshotPlan::new(None, &CUSTOM);
        assert_eq!(plan, SnapshotPlan::RunFromGenesis);

        let default = SnapshotImages::DEFAULT.to_string();
        let plan = SnapshotPlan::new(Some(&default), &CUSTOM);
        assert_eq!(plan, SnapshotPlan::RunFromGenesis);
    }

    #[test]
    fn image_sets_are_cached_apart() {
        let root = Path::new("cache");
        let default_dir = SnapshotImages::DEFAULT.cache_dir(root);
        assert_eq!(default_dir.parent(), Some(root));
        assert_eq!(default_dir, SnapshotImages::DEFAULT.cache_dir(root));
        assert_ne!(default_dir, CUSTOM.cache_dir(root));
    }

    fn snapshot_tar() -> Vec<u8> {
        let mut builder = tar::Builder::new(Vec::new());
        let mut append = |path: &str, data: &[u8]| {
            let mut header = tar::Header::new_gnu();
            header.set_size(data.len() as u64);
            header.set_mode(0o644);
            builder.append_data(&mut header, path, data).unwrap();
        };
        append(&format!("devnet/{DEVNET_SNAPSHOT_READY_MARKER}"), b"stale");
        append("devnet/blocks.dat", &[1; 4096]);
        builder.into_inner().unwrap()
    }

    #[test]
    fn marker_is_written_after_every_entry() {
        let dir = tempfile::tempdir().unwrap();
        let snapshot_dir = dir.path().join("snapshot");

        unpack_snapshot(snapshot_tar().as_slice(), &snapshot_dir).unwrap();
        assert!(snapshot_dir.join("blocks.dat").exists());
        let marker = fs::read_to_string(snapshot_dir.join(DEVNET_SNAPSHOT_READY_MARKER)).unwrap();
        assert_eq!(marker, SnapshotImages::DEFAULT.to_string());
    }

    #[test]
    fn interrupted_extraction_leaves_no_marker() {
        let dir = tempfile::tempdir().unwrap();
        let snapshot_dir = dir.path().join("snapshot");

        let tar = snapshot_tar();
        let truncated = &tar[..tar.len() - 4096];
        assert!(unpack_snapshot(truncated, &snapshot_dir).is_err());
        assert!(!snapshot_dir.join(DEVNET_SNAPSHOT_READY_MARKER).exists());
    }

    #[test]
    fn removes_unkeyed_snapshot() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        let keyed = SnapshotImages::DEFAULT.cache_dir(root);
        for path in [
            root.join("bitcoin/regtest"),
            root.join("stacks"),
            keyed.clone(),
        ] {
            fs::create_dir_all(path).unwrap();
        }
        fs::write(root.join(DEVNET_SNAPSHOT_READY_MARKER), "").unwrap();

        remove_unkeyed_snapshot(root).unwrap();
        let remaining: Vec<_> = fs::read_dir(root)
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .collect();
        assert_eq!(remaining, [keyed]);
    }

    #[test]
    fn embedded_snapshot_was_built_with_default_images() {
        let mut archive = Archive::new(GzDecoder::new(DEVNET_SNAPSHOT));
        let mut marker = archive
            .entries()
            .unwrap()
            .map(Result::unwrap)
            .find(|entry| {
                entry.path().unwrap() == Path::new("devnet").join(DEVNET_SNAPSHOT_READY_MARKER)
            })
            .expect("The embedded snapshot should contain a ready marker");
        let mut images = String::new();
        marker.read_to_string(&mut images).unwrap();
        assert_eq!(
            images,
            SnapshotImages::DEFAULT.to_string(),
            "Regenerate data/devnet_default_snapshot.tar.gz after changing the default images"
        );
    }
}
