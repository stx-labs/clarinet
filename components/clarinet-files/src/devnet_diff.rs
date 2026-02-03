use bitcoin::hex::DisplayHex;
use hashbrown::HashMap;

pub use crate::DevnetConfig;

/// Config which fields to check for differences
pub struct DevnetDiffConfig {
    /// Fields to check for differences
    significant_fields: Vec<SignificantField>,
}

/// Represents a field that should be checked for differences
pub struct SignificantField {
    /// Name of the field for display purposes
    pub name: String,
    /// Function to extract the value from a DevnetConfig
    pub extractor: Extractor,
}
type Extractor = Box<dyn Fn(&DevnetConfig) -> String>;

/// Creates an extractor for any field that implements ToString
fn make_extractor<T: ToString + 'static>(accessor: fn(&DevnetConfig) -> T) -> Extractor {
    Box::new(move |config| accessor(config).to_string())
}

impl DevnetDiffConfig {
    /// Create a new diff configuration with default significant fields
    pub fn new() -> Self {
        Self {
            significant_fields: Self::default_significant_fields(),
        }
    }

    /// Create a diff configuration with custom fields
    pub fn with_fields(fields: Vec<(&'static str, Extractor)>) -> Self {
        Self {
            significant_fields: fields
                .into_iter()
                .map(|(name, extractor)| SignificantField {
                    name: name.to_string(),
                    extractor,
                })
                .collect(),
        }
    }

    /// Get the default set of significant fields
    fn default_significant_fields() -> Vec<SignificantField> {
        vec![
            // Epoch configurations
            SignificantField {
                name: "epoch_2_0".to_string(),
                extractor: make_extractor(|config| config.epoch_2_0),
            },
            SignificantField {
                name: "epoch_2_05".to_string(),
                extractor: make_extractor(|config| config.epoch_2_05),
            },
            SignificantField {
                name: "epoch_2_1".to_string(),
                extractor: make_extractor(|config| config.epoch_2_1),
            },
            SignificantField {
                name: "epoch_2_2".to_string(),
                extractor: make_extractor(|config| config.epoch_2_2),
            },
            SignificantField {
                name: "epoch_2_3".to_string(),
                extractor: make_extractor(|config| config.epoch_2_3),
            },
            SignificantField {
                name: "epoch_2_4".to_string(),
                extractor: make_extractor(|config| config.epoch_2_4),
            },
            SignificantField {
                name: "epoch_2_5".to_string(),
                extractor: make_extractor(|config| config.epoch_2_5),
            },
            SignificantField {
                name: "epoch_3_0".to_string(),
                extractor: make_extractor(|config| config.epoch_3_0),
            },
            // With the default snapshot, the devnet starts at epoch 3.0
            // So users can set custom higher epoch heights

            // Signers
            SignificantField {
                name: "stacks_signers_keys".to_string(),
                extractor: make_extractor(|config| {
                    let mut keys: Vec<String> = config
                        .stacks_signers_keys
                        .iter()
                        .map(|key| clarity::types::PrivateKey::to_bytes(key).to_lower_hex_string())
                        .collect();
                    keys.sort();
                    keys.join(",")
                }),
            },
            // Stacking orders
            SignificantField {
                name: "pox_stacking_orders".to_string(),
                extractor: make_extractor(|config| {
                    let mut orders = config.pox_stacking_orders.clone();
                    orders.sort_by(|a, b| a.wallet.cmp(&b.wallet));
                    orders
                        .into_iter()
                        .map(|pso| {
                            format!(
                                "{}-{}-{}-{}-{}",
                                pso.start_at_cycle,
                                pso.duration,
                                pso.wallet,
                                pso.slots,
                                pso.btc_address
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(",")
                }),
            },
        ]
    }

    /// Check if any significant fields are different
    pub fn is_compatible(
        &self,
        user_config: &DevnetConfig,
    ) -> Result<bool, Vec<(String, String, String)>> {
        let default_config = DevnetConfig::default();
        let mut errors = Vec::new();
        for field in &self.significant_fields {
            let default_value = (field.extractor)(&default_config);
            let user_value = (field.extractor)(user_config);

            if default_value != user_value {
                if field.name == "pox_stacking_orders" {
                    let stacking_errors = self.clean_stacking_orders(user_value, default_value);
                    for (field_name, user_val, default_val) in stacking_errors {
                        errors.push((field_name, user_val, default_val));
                    }
                } else {
                    errors.push((field.name.clone(), user_value, default_value))
                }
            }
        }
        if errors.is_empty() {
            Ok(true)
        } else {
            Err(errors)
        }
    }

    fn clean_stacking_orders(
        &self,
        user_value: String,
        default_value: String,
    ) -> Vec<(String, String, String)> {
        let user_orders: Vec<String> = user_value.split(",").map(String::from).collect();
        let default_orders: Vec<String> = default_value.split(",").map(String::from).collect();
        let constructed_user_orders = construct_stacking_orders(&user_orders);
        let constructed_default_orders = construct_stacking_orders(&default_orders);

        if constructed_user_orders == constructed_default_orders {
            return Vec::new();
        }

        constructed_default_orders
            .into_iter()
            .flat_map(|(btc_address, default_order)| {
                let user_order = constructed_user_orders.get(&btc_address);

                match user_order {
                    Some(user_order) => {
                        // Compare fields between user and default orders
                        default_order
                            .into_iter()
                            .filter_map(|(field, default_val)| {
                                let user_val = user_order
                                    .iter()
                                    .find(|(f, _)| *f == field)
                                    .map(|(_, v)| v.clone());

                                match user_val {
                                    Some(val) if val != default_val => {
                                        Some((field, val, default_val))
                                    }
                                    None => Some((field, String::new(), default_val)),
                                    _ => None,
                                }
                            })
                            .collect::<Vec<_>>()
                    }
                    None => {
                        // If no user order exists, add all default fields
                        default_order
                            .into_iter()
                            .map(|(field, default_val)| (field, String::new(), default_val))
                            .collect::<Vec<_>>()
                    }
                }
            })
            .collect()
    }
}

fn construct_stacking_orders(orders: &[String]) -> HashMap<String, Vec<(String, String)>> {
    let mut result = HashMap::new();
    for order in orders {
        let vals: Vec<_> = order
            .split("-")
            .map(String::from)
            .filter(|s| !s.is_empty())
            .collect();
        if !vals.is_empty() {
            let btc_address = vals[4].to_owned();
            let fields = [
                "start_at_cycle",
                "duration",
                "wallet",
                "slots",
                "btc_address",
            ];
            let order_vals: Vec<_> = fields.map(String::from).into_iter().zip(vals).collect();
            result.insert(btc_address, order_vals);
        }
    }
    result
}

impl Default for DevnetDiffConfig {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_differences() {
        let config1 = DevnetConfig::default();

        let differ = DevnetDiffConfig::new();
        assert!(differ.is_compatible(&config1).is_ok());
    }

    #[test]
    fn test_with_differences() {
        let default_config = DevnetConfig::default();
        let mut user_config = default_config;
        user_config.epoch_3_0 = 150; // Different from default
        user_config.pox_stacking_orders = vec![];

        let differ = DevnetDiffConfig::new();
        let different_fields = differ.is_compatible(&user_config);
        assert!(different_fields.is_err());

        assert!(different_fields.is_err());
        let incompatibles = different_fields.unwrap_err();
        assert_eq!(incompatibles[0].0, "epoch_3_0");
        assert_eq!(incompatibles[1].0, "start_at_cycle");
    }

    #[test]
    fn test_custom_fields() {
        let custom_fields = vec![("epoch_3_0", make_extractor(|c| c.epoch_3_0.to_string()))];

        let differ = DevnetDiffConfig::with_fields(custom_fields);

        let default_config = DevnetConfig::default();
        let mut user_config = default_config;
        user_config.epoch_3_0 = 145;

        let different_fields = differ.is_compatible(&user_config);
        assert!(different_fields.is_err());
        let incompatibles = different_fields.unwrap_err();
        assert_eq!(incompatibles.len(), 1);
        assert_eq!(incompatibles[0].0, "epoch_3_0");
    }
}
