use serde::de::DeserializeOwned;

/// Read YAML with Clarinet's boolean, merge-key, and binary-string policies.
/// These options do not implement full YAML 1.2 Core or version-based schema selection.
#[allow(clippy::disallowed_methods)]
pub fn from_yaml<T: DeserializeOwned>(content: &[u8]) -> Result<T, serde_saphyr::Error> {
    let options = serde_saphyr::options! {
        strict_booleans: true,
        merge_keys: serde_saphyr::MergeKeyPolicy::AsOrdinary,
        ignore_binary_tag_for_string: true,
    };
    serde_saphyr::from_slice_with_options(content, options)
}

#[cfg(test)]
mod tests;
