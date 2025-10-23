use clarity_repl::repl;
use schemars::{schema_for, JsonSchema};
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[cfg(feature = "json_schema")]
use crate::project_manifest::ProjectManifestFile;

/// Generates JSON Schema for Clarinet.toml manifest file.
pub fn generate_clarinet_manifest_schema() -> Value {
    serde_json::to_value(schema_for!(ProjectManifestFile))
        .expect("Schema generation should never fail(report bug)")
}

/// Schema definition for contract configuration
#[derive(Serialize, Deserialize, Debug, Clone)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
pub(crate) struct ContractConfig {
    /// Relative path to the contract file from project root
    path: String,
    /// Deployer identifier
    #[serde(skip_serializing_if = "Option::is_none")]
    deployer: Option<String>,
    /// Clarity language version to use
    #[serde(skip_serializing_if = "Option::is_none")]
    #[cfg_attr(
        feature = "json_schema",
        schemars(schema_with = "clarity_version_schema")
    )]
    clarity_version: Option<u8>,
    /// Stacks blockchain epoch
    #[serde(skip_serializing_if = "Option::is_none")]
    epoch: Option<EpochValue>,
}

/// Epoch can be specified as string or number
#[derive(Serialize, Deserialize, Debug, Clone)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
#[serde(untagged)]
pub(crate) enum EpochValue {
    /// String epoch value
    #[cfg_attr(feature = "json_schema", schemars(schema_with = "epoch_string_schema"))]
    String(String),
    /// Numeric epoch value
    #[cfg_attr(feature = "json_schema", schemars(schema_with = "epoch_number_schema"))]
    Number(f64),
}

pub(crate) fn clarity_version_schema(_gen: &mut schemars::SchemaGenerator) -> schemars::Schema {
    use schemars::json_schema;

    json_schema!({
        "type": "integer",
        "enum": [1, 2, 3],
        "description": "Clarity language version (1, 2, or 3)"
    })
}

pub(crate) fn epoch_string_schema(_gen: &mut schemars::SchemaGenerator) -> schemars::Schema {
    use schemars::json_schema;

    json_schema!({
        "type": "string",
        "enum": [
            "2.0", "2.05", "2.1", "2.2", "2.3", "2.4", "2.5",
            "3.0", "3.1", "3.2", "latest"
        ]
    })
}

pub(crate) fn epoch_number_schema(_gen: &mut schemars::SchemaGenerator) -> schemars::Schema {
    use schemars::json_schema;

    json_schema!({
        "type": "number",
        "enum": [2.0, 2.05, 2.1, 2.2, 2.3, 2.4, 2.5, 3.0, 3.1, 3.2]
    })
}

pub(crate) fn requirements_schema(gen: &mut schemars::SchemaGenerator) -> schemars::Schema {
    use schemars::json_schema;

    let item_schema = gen.subschema_for::<crate::project_manifest::RequirementConfig>();

    json_schema!({
        "type": "array",
        "items": item_schema,
        "description": "External contract dependencies"
    })
}

pub(crate) fn contracts_schema(gen: &mut schemars::SchemaGenerator) -> schemars::Schema {
    use schemars::json_schema;

    let contract_schema = gen.subschema_for::<ContractConfig>();

    json_schema!({
        "type": "object",
        "additionalProperties": contract_schema,
        "description": "Contract definitions for the project"
    })
}
pub(crate) fn repl_schema(gen: &mut schemars::SchemaGenerator) -> schemars::Schema {
    gen.subschema_for::<repl::SettingsFile>()
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

    const SCHEMA_FILENAME: &str = "clarinet-manifest.schema.json";

    fn get_schema_path() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(SCHEMA_FILENAME)
    }

    #[test]
    fn test_schema_is_valid_json() {
        let schema = generate_clarinet_manifest_schema();
        assert!(schema.is_object());

        assert_eq!(
            schema["$schema"],
            "https://json-schema.org/draft/2020-12/schema"
        );
        assert_eq!(schema["title"], "ProjectManifestFile");
    }

    #[test]
    fn test_schema_has_required_structure() {
        let schema = generate_clarinet_manifest_schema();

        assert!(schema["properties"]["project"]["$ref"]
            .as_str()
            .unwrap()
            .contains("ProjectConfigFile"));

        // Verify other properties exist
        assert!(schema["properties"]["contracts"].is_object());
        assert!(schema["properties"]["repl"].is_object());

        // Verify required fields
        let required = schema["required"].as_array().unwrap();
        assert_eq!(required.len(), 1);
        assert!(required.contains(&serde_json::json!("project")));
    }

    /// Generates the schema file for IDE consumption.
    /// Run with: cargo test --package clarinet-files generate_schema_file -- --ignored --nocapture
    #[test]
    #[ignore]
    fn generate_schema_file() {
        let schema = generate_clarinet_manifest_schema();
        let schema_json = serde_json::to_string_pretty(&schema).unwrap();
        let schema_path = get_schema_path();

        std::fs::write(&schema_path, schema_json).expect("Failed to write schema file");
        println!("Schema file generated at: {}", schema_path.display());
    }
}
