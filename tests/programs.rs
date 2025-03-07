use serde::Deserialize;
use std::{
    fs::{self, Permissions},
    io::Write,
    path::Path,
    process::{Command, Stdio},
};

include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));

#[derive(Deserialize)]
struct Json {
    stdout: Option<String>,
    stdin: Option<String>,
    error: Option<bool>,
}

fn compile_and_run_program(path: impl AsRef<Path>) {
    let path = path.as_ref();
    let config = compilers_project::Config { verbose: true };
    println!("-------- TESTING {} --------", path.display());

    // Load config for the program test case
    let conf = fs::read_to_string(path.with_extension("json")).unwrap();
    let json: Json = serde_json::from_str(&conf).unwrap();

    // Compile and check status
    let code = fs::read_to_string(path).unwrap();
    let elf = match (
        compilers_project::compile(&code, &config),
        json.error.unwrap_or(false),
    ) {
        (Ok(elf), false) => elf,
        (Ok(_), true) => panic!("Program compiled, but error was expected"),
        (Err(e), false) => panic!("Unexpected compiler error: {e}"),
        (Err(_), true) => return,
    };

    // Create temp binary to run
    let mut tempfile = tempfile::NamedTempFile::new().unwrap();
    #[cfg(target_family = "unix")]
    {
        use std::os::unix::fs::PermissionsExt;
        let file = tempfile.as_file();
        let mut mode = file.metadata().unwrap().permissions().mode();
        mode |= 0o111;
        file.set_permissions(Permissions::from_mode(mode)).unwrap();
    }
    tempfile.write_all(&elf).unwrap();
    tempfile.flush().unwrap();
    let (_, binary) = tempfile.keep().unwrap();

    // Run and feed stdin
    let mut process = Command::new(binary)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    if let (Some(input), Some(stdin)) = (&json.stdin, &mut process.stdin) {
        stdin.write_all(input.as_bytes()).unwrap();
    }

    // Check the output
    let output = process.wait_with_output().unwrap();
    if let Some(stdout) = json.stdout {
        if output.stdout != stdout.as_bytes() {
            eprintln!("Expected: {stdout}");
            eprintln!("Output: {}", String::from_utf8_lossy(&output.stdout));
            eprintln!("In file: {}", path.display());
            panic!("Executable produced output which does not match expected");
        }
    }
}
