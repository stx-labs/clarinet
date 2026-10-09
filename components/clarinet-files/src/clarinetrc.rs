use std::env;
use std::path::PathBuf;

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Default, Debug)]
pub struct ClarinetRC {
    pub enable_hints: Option<bool>,
    pub enable_telemetry: Option<bool>,
}

impl ClarinetRC {
    pub fn get_config_dir() -> Option<PathBuf> {
        dirs::home_dir().map(|h| h.join(".clarinet"))
    }

    pub fn get_settings_file_path() -> Option<PathBuf> {
        Self::get_config_dir().map(|d| d.join("clarinetrc.toml"))
    }

    pub fn from_rc_file() -> Self {
        if let Some(path) = Self::get_settings_file_path() {
            if path.exists() {
                match std::fs::read_to_string(&path) {
                    Ok(content) => match toml::from_str::<ClarinetRC>(&content) {
                        Ok(res) => return res,
                        Err(_) => {
                            println!("unable to parse {}", path.display());
                        }
                    },
                    Err(_) => {
                        println!("unable to read file {}", path.display());
                    }
                }
            }
        }

        // Keep backwards compatibility with ENV var
        let disable_hints = env::var("CLARINET_DISABLE_HINTS").ok();
        Self {
            enable_hints: enable_hints_from_env(disable_hints.as_deref()),
            ..Default::default()
        }
    }
}

/// Hints are disabled only by `CLARINET_DISABLE_HINTS=1`, as before the setting moved to `clarinetrc.toml`
fn enable_hints_from_env(disable_hints: Option<&str>) -> Option<bool> {
    disable_hints.map(|v| v != "1")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn disable_hints_env_var() {
        assert_eq!(enable_hints_from_env(None), None);
        assert_eq!(enable_hints_from_env(Some("1")), Some(false));
        assert_eq!(enable_hints_from_env(Some("0")), Some(true));
        assert_eq!(enable_hints_from_env(Some("")), Some(true));
    }
}
