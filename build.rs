use std::ffi::OsStr;
use std::io::Write;
use std::{fs, path::Path};

fn main() {
    let output_path = Path::new(&std::env::var("OUT_DIR").unwrap()).join("generated_tests.rs");
    let mut output_file = fs::File::create(output_path).unwrap();

    writeln!(output_file, "#[cfg(test)]").unwrap();
    writeln!(output_file, "mod generated_tests {{").unwrap();

    for file in fs::read_dir(Path::new(env!("CARGO_MANIFEST_DIR")).join("programs")).unwrap() {
        let file = file.unwrap();
        if !file.metadata().unwrap().is_file() {
            println!("{} isn't a file", file.path().display());
            continue;
        }
        if file.path().extension() == Some(OsStr::new("json")) {
            println!("{} is a json file, skip", file.path().display());
            continue;
        }

        let path = file.path();

        writeln!(
            output_file,
            "    #[test]
    fn {}() {{
        super::compile_and_run_program(\"{}\");
    }}",
            path.file_stem().unwrap().to_str().unwrap(),
            path.to_str().unwrap()
        )
        .unwrap();
    }

    writeln!(output_file, "}}").unwrap();

    println!("cargo:rerun-if-changed=programs/");
}
