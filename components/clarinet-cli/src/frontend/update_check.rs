use std::time::Duration;

use clarinet_files::clarinetrc::ClarinetRC;

const GITHUB_RELEASES_URL: &str = "https://api.github.com/repos/stx-labs/clarinet/releases/latest";
const REQUEST_TIMEOUT: Duration = Duration::from_secs(3);
const CHECK_INTERVAL: Duration = Duration::from_secs(72 * 60 * 60);
const VERSION_CACHE_FILE: &str = ".latest_version";

/// Check if a newer version of clarinet is available and print a warning if so.
/// Any failure is silently ignored.
///
/// If a cached latest version exists and is fresh (<72h), it is used directly.
/// Otherwise, the latest version is fetched from GitHub (with a short timeout),
/// cached to disk, and compared.
pub fn check_for_update() {
    let current_version = env!("CARGO_PKG_VERSION");

    let Some(config_dir) = ClarinetRC::get_config_dir() else {
        return;
    };

    let cache_path = config_dir.join(VERSION_CACHE_FILE);

    let latest = if should_check(&cache_path) {
        // Cache is missing or stale — fetch from GitHub (only happens every 72h)
        let fetched = fetch_latest_version();
        if let Some(ref version) = fetched {
            let _ = std::fs::write(&cache_path, version);
        }
        fetched
    } else {
        read_cached_version(&cache_path)
    };

    if let Some(latest) = latest {
        if is_newer(&latest, current_version) {
            eprintln!(
                "\nA new release of clarinet is available: {} -> {}",
                current_version, latest,
            );
        }
    }
}

fn should_check(cache_path: &std::path::Path) -> bool {
    let Ok(metadata) = std::fs::metadata(cache_path) else {
        return true;
    };
    let Ok(modified) = metadata.modified() else {
        return true;
    };
    let Ok(elapsed) = modified.elapsed() else {
        return true;
    };
    elapsed >= CHECK_INTERVAL
}

fn read_cached_version(path: &std::path::Path) -> Option<String> {
    let content = std::fs::read_to_string(path).ok()?;
    let version = content.trim().to_string();
    if version.is_empty() {
        None
    } else {
        Some(version)
    }
}

fn fetch_latest_version() -> Option<String> {
    let client = reqwest::blocking::Client::builder()
        .timeout(REQUEST_TIMEOUT)
        .user_agent("clarinet-update-check")
        .build()
        .ok()?;

    let resp = client.get(GITHUB_RELEASES_URL).send().ok()?;
    let json: serde_json::Value = resp.json().ok()?;
    let tag = json.get("tag_name")?.as_str()?;

    // Strip leading 'v'
    let version = tag.strip_prefix('v').unwrap_or(tag);
    Some(version.to_string())
}

fn is_newer(latest: &str, current: &str) -> bool {
    let Ok(latest) = semver::Version::parse(latest) else {
        return false;
    };
    let Ok(current) = semver::Version::parse(current) else {
        return false;
    };
    latest > current
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_newer() {
        assert!(is_newer("3.15.0", "3.14.1"));
        assert!(is_newer("4.0.0", "3.14.1"));
        assert!(is_newer("3.14.2", "3.14.1"));
        assert!(!is_newer("3.14.1", "3.14.1"));
        assert!(!is_newer("3.14.0", "3.14.1"));
        assert!(!is_newer("2.0.0", "3.14.1"));
    }

    #[test]
    fn test_is_newer_with_prerelease() {
        assert!(is_newer("3.15.0", "3.14.1-rc.1"));
        assert!(!is_newer("3.14.1-rc.1", "3.14.1"));
    }

    #[test]
    fn test_should_check_missing_file() {
        let path = std::path::Path::new("/tmp/nonexistent_update_check_test");
        assert!(should_check(path));
    }

    #[test]
    fn test_should_check_recent_cache() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(VERSION_CACHE_FILE);
        std::fs::write(&path, "3.15.0").unwrap();
        assert!(!should_check(&path));
    }

    #[test]
    fn test_read_cached_version() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(VERSION_CACHE_FILE);

        assert!(read_cached_version(&path).is_none());

        std::fs::write(&path, "3.15.0").unwrap();
        assert_eq!(read_cached_version(&path).as_deref(), Some("3.15.0"));

        std::fs::write(&path, "  3.15.0\n").unwrap();
        assert_eq!(read_cached_version(&path).as_deref(), Some("3.15.0"));

        std::fs::write(&path, "").unwrap();
        assert!(read_cached_version(&path).is_none());
    }
}
