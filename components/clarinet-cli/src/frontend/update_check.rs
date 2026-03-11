use std::time::Duration;

use clarinet_files::clarinetrc::ClarinetRC;

const GITHUB_RELEASES_URL: &str = "https://api.github.com/repos/stx-labs/clarinet/releases/latest";
const REQUEST_TIMEOUT: Duration = Duration::from_secs(5);

/// Check if a newer version of clarinet is available and print a warning if so.
/// any failure is silently ignored.
pub fn check_for_update() {
    let current_version = env!("CARGO_PKG_VERSION");

    std::thread::spawn(move || {
        if let Some(latest) = fetch_latest_version() {
            if is_newer(&latest, current_version) {
                eprintln!(
                    "\nA new release of clarinet is available: {} -> {}\nTo disable this warning, set `ignore_version_warning = true` in {}",
                    current_version,
                    latest,
                    ClarinetRC::get_settings_file_path(),
                );
            }
        }
    });
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
}
