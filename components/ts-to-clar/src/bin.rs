use ts_to_clar::transpile;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let file_name = args[1].clone();
    let file_path = std::path::Path::new(&file_name);
    let extension = file_path.extension().unwrap_or_default();
    assert_eq!(extension, "ts");
    assert!(file_path.is_file());
    assert!(file_name.ends_with(".clar.ts"));

    let output_path = file_name.strip_suffix(".ts").unwrap();

    let src = std::fs::read_to_string(file_path).unwrap();
    let clarity_code = transpile(&file_name, &src).unwrap();
    std::fs::write(output_path, clarity_code).unwrap();
    println!("Transpiled {} to {}", file_name, output_path);
}
