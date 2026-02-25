use std::fs;
use std::hint::black_box;
use std::path::Path;

use clarinet_format::formatter::{ClarityFormatter, Settings};
use divan::Bencher;

fn get_test_files() -> Vec<String> {
    let golden_dir = Path::new("tests/golden");
    fs::read_dir(golden_dir)
        .expect("Failed to read golden directory")
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension()? == "clar" {
                Some(path.to_string_lossy().into_owned())
            } else {
                None
            }
        })
        .collect()
}

#[divan::bench_group]
mod format_benches {
    use super::*;

    #[divan::bench(sample_count = 10)]
    fn format_all_contracts(bencher: Bencher) {
        let files = get_test_files();
        let formatter = ClarityFormatter::new(Settings::default());

        bencher.bench_local(|| {
            for file_path in &files {
                let source = fs::read_to_string(file_path).expect("Failed to read test file");
                black_box(formatter.format(black_box(&source), None));
            }
        });
    }

    // Benchmark a specific large contract as a representative sample
    #[divan::bench(sample_count = 10)]
    fn format_clarity_bitcoin(bencher: Bencher) {
        let formatter = ClarityFormatter::new(Settings::default());
        let source = fs::read_to_string("tests/golden/clarity-bitcoin.clar")
            .expect("Failed to read test file");

        bencher.bench_local(|| black_box(formatter.format(black_box(&source), None)));
    }
}

fn main() {
    divan::main();
}
