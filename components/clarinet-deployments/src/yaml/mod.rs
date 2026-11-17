use serde::de::DeserializeOwned;
use serde::Serialize;

/// Read YAML with Clarinet's boolean, merge-key, and binary-string policies.
/// These options do not implement full YAML 1.2 Core or version-based schema selection.
#[allow(clippy::disallowed_methods)]
pub fn from_yaml<T: DeserializeOwned>(content: impl AsRef<[u8]>) -> Result<T, serde_saphyr::Error> {
    let options = serde_saphyr::options! {
        strict_booleans: true,
        merge_keys: serde_saphyr::MergeKeyPolicy::AsOrdinary,
        ignore_binary_tag_for_string: true,
    };
    serde_saphyr::from_slice_with_options(content.as_ref(), options)
}

/// Write YAML using 1.2 quoting rules without a version directive or leading document marker.
pub fn to_yaml<T: Serialize>(value: &T) -> Result<String, serde_saphyr::SerializeError> {
    let options = serde_saphyr::ser_options! {
        yaml_12: true,
        no_lang_directive: true,
    };
    serde_saphyr::to_string_with_options(value, options)
}

#[cfg(test)]
mod yaml_behavior;

#[cfg(test)]
mod known_spec_deviations;
