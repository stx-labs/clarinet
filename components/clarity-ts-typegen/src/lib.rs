mod naming;
mod ts_writer;

use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

use clarity::vm::analysis::contract_interface_builder::ContractInterface;

/// Generate TypeScript type files for the given contracts.
///
/// Accepts a map of contract name -> ContractInterface.
/// Writes one `.ts` file per contract and an `index.ts` into `output_dir`.
/// Overwrites existing files (these are generated artifacts).
pub fn generate(
    contracts: &BTreeMap<String, &ContractInterface>,
    output_dir: &Path,
) -> Result<usize, String> {
    fs::create_dir_all(output_dir)
        .map_err(|e| format!("failed to create output directory: {e}"))?;

    let mut generated = Vec::new();

    for (contract_name, interface) in contracts {
        let content = ts_writer::generate_contract_types(contract_name, interface);
        let file_path = output_dir.join(format!("{contract_name}.ts"));

        fs::write(&file_path, &content).map_err(|e| {
            format!(
                "failed to write {}: {e}",
                file_path.to_string_lossy()
            )
        })?;

        generated.push(contract_name.as_str());
    }

    // BTreeMap iteration is already sorted, but let's be explicit
    let index_content = ts_writer::generate_index(&generated);
    let index_path = output_dir.join("index.ts");
    fs::write(&index_path, &index_content)
        .map_err(|e| format!("failed to write index.ts: {e}"))?;

    Ok(generated.len())
}
