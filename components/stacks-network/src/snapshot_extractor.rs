use std::path::Path;
use std::sync::mpsc::Sender;
use std::{fmt, fs};

use clarinet_files::{
    DevnetConfig, DEFAULT_BITCOIN_NODE_IMAGE, DEFAULT_STACKS_NODE_IMAGE,
    DEFAULT_STACKS_SIGNER_IMAGE,
};
use flate2::read::GzDecoder;
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
    ExtractEmbedded {
        stale_marker: Option<String>,
    },
    /// No snapshot matches the images. The cache is left untouched.
    RunFromGenesis {
        cached_marker: Option<String>,
    },
}

impl SnapshotPlan {
    pub fn new(cached_marker: Option<String>, images: &SnapshotImages) -> Self {
        let images = images.to_string();
        if cached_marker.as_deref() == Some(images.as_str()) {
            Self::UseCached
        } else if images == SnapshotImages::DEFAULT.to_string() {
            Self::ExtractEmbedded {
                stale_marker: cached_marker,
            }
        } else {
            Self::RunFromGenesis { cached_marker }
        }
    }
}

/// Renders a marker for logs. Markers written before snapshots were keyed by image are empty.
pub fn describe_marker(marker: &str) -> &str {
    match marker.trim() {
        "" => "unknown images (snapshot created by an older Clarinet)",
        images => images,
    }
}

/// Extract embedded devnet snapshot to the specified directory
pub fn extract_embedded_snapshot(
    snapshot_dir: &Path,
    devnet_event_tx: &Sender<DevnetEvent>,
) -> Result<bool, String> {
    let _ = devnet_event_tx.send(DevnetEvent::info(
        "Extracting embedded devnet snapshot data...".to_string(),
    ));

    if snapshot_dir.exists() {
        fs::remove_dir_all(snapshot_dir)
            .map_err(|e| format!("Failed to remove stale snapshot directory: {e}"))?;
    }

    // Create the snapshot directory if it doesn't exist
    fs::create_dir_all(snapshot_dir)
        .map_err(|e| format!("Failed to create snapshot directory: {e}"))?;

    // Decompress and extract the tar archive
    let decoder = GzDecoder::new(DEVNET_SNAPSHOT);
    let mut archive = Archive::new(decoder);

    // Extract to the snapshot directory, stripping the top-level "devnet" directory
    for entry in archive
        .entries()
        .map_err(|e| format!("Failed to read archive entries: {e}"))?
    {
        let mut entry = entry.map_err(|e| format!("Failed to read archive entry: {e}"))?;
        let path = entry
            .path()
            .map_err(|e| format!("Failed to get entry path: {e}"))?;

        // Strip the leading "devnet/" from the path
        if let Ok(stripped_path) = path.strip_prefix("devnet") {
            let dest_path = snapshot_dir.join(stripped_path);
            entry
                .unpack(&dest_path)
                .map_err(|e| format!("Failed to extract file {}: {}", dest_path.display(), e))?;
        }
    }

    // A test checks that the archive's own marker matches these images
    let marker_path = snapshot_dir.join(DEVNET_SNAPSHOT_READY_MARKER);
    fs::write(&marker_path, SnapshotImages::DEFAULT.to_string())
        .map_err(|e| format!("Failed to create snapshot marker: {e}"))?;

    let _ = devnet_event_tx.send(DevnetEvent::success(
        "Embedded devnet snapshot extracted successfully".to_string(),
    ));

    Ok(true)
}

#[cfg(test)]
mod tests {
    use std::io::Read;

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
        let plan = SnapshotPlan::new(Some(default.to_string()), &default);
        assert_eq!(plan, SnapshotPlan::UseCached);

        let plan = SnapshotPlan::new(Some(CUSTOM.to_string()), &CUSTOM);
        assert_eq!(plan, SnapshotPlan::UseCached);
    }

    #[test]
    fn default_images_extract_embedded_snapshot() {
        let default = SnapshotImages::DEFAULT;
        let plan = SnapshotPlan::new(None, &default);
        assert_eq!(plan, SnapshotPlan::ExtractEmbedded { stale_marker: None });

        // Created by an older Clarinet, before markers were keyed
        let plan = SnapshotPlan::new(Some(String::new()), &default);
        let stale_marker = Some(String::new());
        assert_eq!(plan, SnapshotPlan::ExtractEmbedded { stale_marker });

        let plan = SnapshotPlan::new(Some(CUSTOM.to_string()), &default);
        let stale_marker = Some(CUSTOM.to_string());
        assert_eq!(plan, SnapshotPlan::ExtractEmbedded { stale_marker });
    }

    #[test]
    fn custom_images_without_matching_cache_run_from_genesis() {
        let plan = SnapshotPlan::new(None, &CUSTOM);
        assert_eq!(
            plan,
            SnapshotPlan::RunFromGenesis {
                cached_marker: None
            }
        );

        let default = SnapshotImages::DEFAULT.to_string();
        let plan = SnapshotPlan::new(Some(default.clone()), &CUSTOM);
        let cached_marker = Some(default);
        assert_eq!(plan, SnapshotPlan::RunFromGenesis { cached_marker });
    }

    #[test]
    fn describes_unkeyed_marker() {
        assert!(describe_marker("").contains("older Clarinet"));
        let marker = SnapshotImages::DEFAULT.to_string();
        assert_eq!(describe_marker(&marker), marker.trim());
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
