use std::sync::mpsc;
use std::time::Duration;

use chrono::{DateTime, Utc};
use clarinet_files::clarinetrc::ClarinetRC;

const GITHUB_RELEASES_URL: &str = "https://api.github.com/repos/stx-labs/clarinet/releases/latest";
// if GH take longer than this, return a result without fetching version
const REQUEST_TIMEOUT: Duration = Duration::from_secs(1);
// will only fetch versionfrom GH every 72 hours
const CHECK_INTERVAL: Duration = Duration::from_hours(72);
// after a release, wait before checking for updates again
const RELEASE_COOLDOWN: Duration = Duration::from_hours(1);
// file to cache the latest version
const VERSION_CACHE_FILE: &str = ".latest_version";
// time to wait for the gh fetch to complete before returning a result
const UPDATE_CHECK_WAIT: Duration = Duration::from_millis(100);

pub struct UpdateHandle(mpsc::Receiver<Option<String>>);

/// Spawn the update check on a background thread so it doesn't block CLI startup.
pub fn check_for_update_async() -> UpdateHandle {
    let (tx, rx) = mpsc::channel();
    std::thread::spawn(move || {
        let _ = tx.send(check_for_update());
    });
    UpdateHandle(rx)
}

/// Wait up to 1 second for the background update check to finish,
/// then print the result to stderr.
pub fn print_update_message(handle: UpdateHandle) {
    if let Ok(Some(message)) = handle.0.recv_timeout(UPDATE_CHECK_WAIT) {
        eprintln!("{message}");
    }
}

fn check_for_update() -> Option<String> {
    let current_version = env!("CARGO_PKG_VERSION");

    let config_dir = ClarinetRC::get_config_dir()?;
    let cache_path = config_dir.join(VERSION_CACHE_FILE);

    let (latest, published_at) = if should_check(&cache_path) {
        // Cache is missing or stale — fetch from GitHub (only happens every 72h)
        let fetched = fetch_latest_version();
        if let Some((ref version, ref published)) = fetched {
            let cache_content = format!("{version}\n{published}");
            let _ = std::fs::write(&cache_path, cache_content);
        }
        fetched
            .map(|(v, p)| (Some(v), Some(p)))
            .unwrap_or((None, None))
    } else {
        read_cached_version(&cache_path)
    };

    let latest = latest?;
    if is_newer(&latest, current_version) && !is_within_cooldown(published_at.as_deref()) {
        Some(format!(
            "\nA new release of clarinet is available: {current_version} -> {latest}",
        ))
    } else {
        None
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

fn read_cached_version(path: &std::path::Path) -> (Option<String>, Option<String>) {
    let Ok(content) = std::fs::read_to_string(path) else {
        return (None, None);
    };
    let mut lines = content.lines();
    let version = lines
        .next()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty());
    let published_at = lines
        .next()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty());
    (version, published_at)
}

fn fetch_latest_version() -> Option<(String, String)> {
    let client = reqwest::blocking::Client::builder()
        .timeout(REQUEST_TIMEOUT)
        .user_agent("clarinet-update-check")
        .build()
        .ok()?;

    let resp = client.get(GITHUB_RELEASES_URL).send().ok()?;
    let json: serde_json::Value = resp.json().ok()?;
    let tag = json.get("tag_name")?.as_str()?;
    let published_at = json.get("published_at")?.as_str()?;

    // Strip leading 'v'
    let version = tag.strip_prefix('v').unwrap_or(tag);
    Some((version.to_string(), published_at.to_string()))
}

fn is_within_cooldown(published_at: Option<&str>) -> bool {
    let Some(published_at) = published_at else {
        return false;
    };
    let Ok(published) = published_at.parse::<DateTime<Utc>>() else {
        return false;
    };
    let elapsed = Utc::now().signed_duration_since(published);
    elapsed < chrono::Duration::from_std(RELEASE_COOLDOWN).unwrap_or_default()
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

        assert_eq!(read_cached_version(&path), (None, None));

        std::fs::write(&path, "3.15.0\n2025-01-01T00:00:00Z").unwrap();
        assert_eq!(
            read_cached_version(&path),
            (
                Some("3.15.0".to_string()),
                Some("2025-01-01T00:00:00Z".to_string())
            )
        );

        std::fs::write(&path, "  3.15.0\n").unwrap();
        assert_eq!(
            read_cached_version(&path),
            (Some("3.15.0".to_string()), None)
        );

        std::fs::write(&path, "").unwrap();
        assert_eq!(read_cached_version(&path), (None, None));
    }

    #[test]
    fn test_is_within_cooldown() {
        // No published_at — no cooldown
        assert!(!is_within_cooldown(None));

        // Published long ago — no cooldown
        assert!(!is_within_cooldown(Some("2020-01-01T00:00:00Z")));

        // Published just now — within cooldown
        let now = Utc::now().to_rfc3339();
        assert!(is_within_cooldown(Some(&now)));

        // Invalid date — no cooldown
        assert!(!is_within_cooldown(Some("not-a-date")));
    }
}
