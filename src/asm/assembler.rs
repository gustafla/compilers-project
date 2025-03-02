use crate::config;
use std::{
    ffi::{OsStr, OsString},
    fmt::Display,
    fs::{self, File},
    io::{self, Read},
    path::{Path, PathBuf},
    process,
};
use thiserror::Error;

static STDLIB_ASM_CODE: &str = include_str!("stdlib.s");

#[derive(Debug, Error)]
pub enum Error {
    #[error("Cannot create temporary directory")]
    TmpDirCreate(#[source] io::Error),
    #[error("Cannot write to {0}")]
    TmpDirWrite(PathBuf, #[source] io::Error),
    #[error("Cannot read from {0}")]
    TmpDirRead(PathBuf, #[source] io::Error),
    #[error("Cannot invoke {0} for {1}")]
    InvokeBinutil(String, PathBuf, #[source] io::Error),
    #[error("Failed to {verb} {output}\nstdout: {stdout}\nstderr: {stderr}")]
    RunBinutil {
        verb: String,
        output: PathBuf,
        stdout: String,
        stderr: String,
    },
}

pub fn assemble(
    assembly_code: &str,
    link_with_c: bool,
    extra_libraries: &[&str],
) -> Result<Vec<u8>, Error> {
    let tempdir = tempfile::tempdir().map_err(Error::TmpDirCreate)?;
    let path = tempdir.path();

    let res = assemble_impl(
        assembly_code,
        path,
        "program",
        link_with_c,
        extra_libraries,
        |path| {
            let mut buf = Vec::new();
            let mut file = File::open(&path).map_err(|e| Error::TmpDirRead(path.clone(), e))?;
            file.read_to_end(&mut buf)
                .map_err(|e| Error::TmpDirRead(path, e))?;
            Ok(buf)
        },
    );
    if config::verbose() {
        eprintln!("Assembled and linked successfully");
    }
    res
}

fn assemble_impl<T>(
    assembly_code: &str,
    workdir: impl AsRef<Path>,
    tempfile_basename: impl AsRef<Path>,
    link_with_c: bool,
    extra_libraries: &[&str],
    take_output: impl Fn(PathBuf) -> Result<T, Error>,
) -> Result<T, Error> {
    let workdir = workdir.as_ref();
    let tempfile_basename = tempfile_basename.as_ref();

    let stdlib_asm = workdir.join("stdlib.s");
    let stdlib_obj = workdir.join("stdlib.o");
    let program_asm = workdir.join(tempfile_basename.with_extension("s"));
    let program_obj = workdir.join(tempfile_basename.with_extension("o"));
    let output_file = workdir.join("a.out");

    if crate::config::verbose() {
        eprintln!("Writing stdlib code to: {}", stdlib_asm.display());
        eprintln!("Writing stdlib obj to: {}", stdlib_obj.display());
        eprintln!("Writing program code to: {}", program_asm.display());
        eprintln!("Writing program obj to: {}", program_obj.display());
        eprintln!("Writing executable to: {}", output_file.display());
    }

    let final_stdlib_asm_code = if link_with_c {
        drop_start_symbol(STDLIB_ASM_CODE)
    } else {
        STDLIB_ASM_CODE.into()
    };

    fs::write(&stdlib_asm, final_stdlib_asm_code)
        .map_err(|e| Error::TmpDirWrite(stdlib_asm.clone(), e))?;
    fs::write(&program_asm, assembly_code)
        .map_err(|e| Error::TmpDirWrite(program_asm.clone(), e))?;

    let asm = Binutil::assembler().flags(["-g"]);
    asm.run(&stdlib_obj, &[stdlib_asm])?;
    asm.run(&program_obj, &[program_asm])?;

    let mut linker_flags = vec![String::from("-static")];
    linker_flags.extend(extra_libraries.iter().map(|lib| format!("-l{lib}")));
    if link_with_c {
        Binutil::c_compiler()
    } else {
        Binutil::linker()
    }
    .flags(linker_flags)
    .run(&output_file, &[stdlib_obj, program_obj])?;

    take_output(output_file)
}

fn drop_start_symbol(code: &str) -> String {
    match code
        .split_once("# BEGIN START")
        .map(|(l, _)| String::from(l))
        .and_then(|mut asm| {
            code.split_once("# END START").map(|(_, r)| {
                asm.push_str(r);
                asm
            })
        }) {
        Some(asm) => asm,
        None => String::from(code),
    }
}

#[derive(Debug)]
struct Binutil {
    program: &'static str,
    flags: Vec<OsString>,
}

impl Display for Binutil {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.program {
            "as" => write!(f, "assemble"),
            "cc" => write!(f, "link"),
            "ld" => write!(f, "link"),
            _ => write!(f, "run {}", self.program),
        }
    }
}

impl Binutil {
    fn new(program: &'static str) -> Self {
        Self {
            program,
            flags: Vec::new(),
        }
    }

    fn assembler() -> Self {
        Self::new("as")
    }

    fn c_compiler() -> Self {
        Self::new("cc")
    }

    fn linker() -> Self {
        Self::new("ld")
    }

    fn flags<I: IntoIterator<Item = F>, F: AsRef<OsStr>>(mut self, flags: I) -> Self {
        self.flags
            .extend(flags.into_iter().map(|f| f.as_ref().into()));
        self
    }

    fn run(&self, output: impl AsRef<OsStr>, input: &[impl AsRef<OsStr>]) -> Result<(), Error> {
        let output = output.as_ref();

        let mut cmd = process::Command::new(self.program);
        let run = cmd.arg("-o").arg(output).args(&self.flags).args(input);

        if config::verbose() {
            eprintln!("Invoking {run:?}");
        }

        let run = run
            .output()
            .map_err(|e| Error::InvokeBinutil(self.program.into(), output.into(), e))?;
        if !run.status.success() {
            return Err(Error::RunBinutil {
                verb: self.to_string(),
                output: output.into(),
                stdout: String::from_utf8_lossy(&run.stdout).into_owned(),
                stderr: String::from_utf8_lossy(&run.stderr).into_owned(),
            });
        }
        Ok(())
    }
}
