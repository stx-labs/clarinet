use std::env;

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Default, Debug)]
pub struct ClarinetRC {
    pub enable_hints: Option<bool>,
    pub enable_telemetry: Option<bool>,
}

impl ClarinetRC {
    pub fn get_settings_file_path() -> &'static str {
        "~/.clarinet/clarinetrc.toml"
    }

    pub fn from_rc_file() -> Self {
        let home_dir = dirs::home_dir();

        if let Some(path) = home_dir.map(|home_dir| home_dir.join(".clarinet/clarinetrc.toml")) {
            if path.exists() {
                match std::fs::read_to_string(path) {
                    Ok(content) => match toml::from_str::<ClarinetRC>(&content) {
                        Ok(res) => return res,
                        Err(_) => {
                            println!("unable to parse {}", Self::get_settings_file_path());
                        }
                    },
                    Err(_) => {
                        println!("unable to read file {}", Self::get_settings_file_path());
                    }
                }
            }
        };

        // Keep backwards compatibility with ENV var
        let enable_hints = match env::var("CLARINET_DISABLE_HINTS") {
            Ok(v) => Some(v == "1"),
            Err(_) => None,
        };
        Self {
            enable_hints,
            ..Default::default()
        }
    }
}
