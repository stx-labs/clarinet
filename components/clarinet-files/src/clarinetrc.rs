use std::env;
use std::path::PathBuf;

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct ClarinetRC {
    pub enable_hints: Option<bool>,
}

impl ClarinetRC {
    pub fn get_config_dir() -> Option<PathBuf> {
        env::home_dir().map(|h| h.join(".clarinet"))
    }

    pub fn get_settings_file_path() -> Option<PathBuf> {
        Self::get_config_dir().map(|d| d.join("clarinetrc.toml"))
    }

    pub fn from_rc_file() -> Self {
        let mut rc = Self::read_rc_file().unwrap_or(Self { enable_hints: None });
        let disable_hints = env::var("CLARINET_DISABLE_HINTS").ok();
        rc.enable_hints = resolve_enable_hints(rc.enable_hints, disable_hints.as_deref());
        rc
    }

    fn read_rc_file() -> Option<Self> {
        let path = Self::get_settings_file_path().filter(|path| path.exists())?;
        let content = std::fs::read_to_string(&path)
            .inspect_err(|_| clarity_repl::ueprint!("unable to read file {}", path.display()))
            .ok()?;
        toml::from_str(&content)
            .inspect_err(|_| clarity_repl::ueprint!("unable to parse {}", path.display()))
            .ok()
    }
}

/// The rc file wins. When it doesn't set hints, they are disabled only by
/// `CLARINET_DISABLE_HINTS=1`, as before the setting moved to `clarinetrc.toml`
fn resolve_enable_hints(from_file: Option<bool>, disable_hints_env: Option<&str>) -> Option<bool> {
    from_file.or_else(|| disable_hints_env.map(|v| v != "1"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn legacy_telemetry_setting_is_ignored() {
        let rc: ClarinetRC =
            toml::from_str("enable_hints = false\nenable_telemetry = true\n").unwrap();
        assert_eq!(rc.enable_hints, Some(false));
    }

    #[test]
    fn disable_hints_env_var() {
        assert_eq!(resolve_enable_hints(None, None), None);
        assert_eq!(resolve_enable_hints(None, Some("1")), Some(false));
        assert_eq!(resolve_enable_hints(None, Some("0")), Some(true));
        assert_eq!(resolve_enable_hints(None, Some("")), Some(true));
    }

    #[test]
    fn rc_file_hints_setting_wins_over_env_var() {
        assert_eq!(resolve_enable_hints(Some(true), Some("1")), Some(true));
        assert_eq!(resolve_enable_hints(Some(false), Some("0")), Some(false));
        assert_eq!(resolve_enable_hints(Some(false), None), Some(false));
    }
}
