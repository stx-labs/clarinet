use std::time::Duration;

use clarinet_files::clarinetrc::ClarinetRC;

const GITHUB_RELEASES_URL: &str = "https://api.github.com/repos/stx-labs/clarinet/releases/latest";
const REQUEST_TIMEOUT: Duration = Duration::from_secs(3);
const CHECK_INTERVAL: Duration = Duration::from_secs(72 * 60 * 60);
const TIMESTAMP_FILE: &str = ".last_update_check";

/// Check if a newer version of clarinet is available and print a warning if so.
/// any failure is silently ignored.
pub fn check_for_update() {
    let current_version = env!("CARGO_PKG_VERSION");

    std::thread::spawn(move || {
        let Some(config_dir) = ClarinetRC::get_config_dir() else {
            return;
        };

        let timestamp_path = config_dir.join(TIMESTAMP_FILE);
        if !should_check(&timestamp_path) {
            return;
        }

        if let Some(latest) = fetch_latest_version() {
            // Touch the file to update its mtime after a successful fetch
            let _ = touch(&timestamp_path);

            if is_newer(&latest, current_version) {
                eprintln!(
                    "\nA new release of clarinet is available: {} -> {}",
                    current_version, latest,
                );
            }
        }
    });
}

fn should_check(timestamp_path: &std::path::Path) -> bool {
    let Ok(metadata) = std::fs::metadata(timestamp_path) else {
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

fn touch(path: &std::path::Path) -> std::io::Result<()> {
    std::fs::write(path, [])
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
    fn test_should_check_recent_timestamp() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(TIMESTAMP_FILE);
        touch(&path).unwrap();
        assert!(!should_check(&path));
    }
}
