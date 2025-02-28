use clap::Parser;
use compilers_project as compiler;
use std::{
    error::Error,
    ffi::OsStr,
    fs::{self, OpenOptions, Permissions},
    io::{self, Write},
    path::{Path, PathBuf},
};

macro_rules! err {
    ($e: expr) => {{
        ::compilers_project::print_error($e);
        ::std::process::exit(1);
    }};
}

#[derive(Parser, Debug)]
#[command(version, about = "Compilers Course Project - Standalone Compiler")]
struct Cli {
    #[command(flatten)]
    config: compiler::Config,
    #[arg(short, long)]
    output: Option<PathBuf>,
    #[arg(long, help = "Print IR")]
    ir: bool,
    #[arg(long, help = "Print assembly")]
    asm: bool,
    #[clap(required = true)]
    files: Vec<PathBuf>,
}

fn output_to_file(path: impl AsRef<Path>, data: impl AsRef<[u8]>) -> io::Result<()> {
    let mut file = OpenOptions::new().create_new(true).write(true).open(path)?;
    file.write_all(data.as_ref())?;
    #[cfg(target_family = "unix")]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut mode = file.metadata()?.permissions().mode();
        mode |= 0o111;
        file.set_permissions(Permissions::from_mode(mode))?;
    }
    Ok(())
}

fn print_ir(code: &str, config: &compiler::Config) {
    let ir = match compiler::generate_ir(code, config) {
        Ok(ir) => ir,
        Err(ref e) => err!(e),
    };
    for ins in ir {
        if let compiler::ir::Op::Label(_) = ins.op {
            println!();
        }
        println!("{}", ins.op);
    }
}

fn compile(code: &str, cli: Cli, input: &Path) {
    let program = match compiler::compile(code, &cli.config) {
        Ok(program) => program,
        Err(ref e) => err!(e),
    };

    let output = cli.output.unwrap_or_else(|| {
        input
            .file_stem()
            .filter(|_| input != Path::new("-"))
            .unwrap_or_else(|| OsStr::new("a.out"))
            .into()
    });

    if cli.config.verbose {
        println!("Writing compiler output to {}", output.display());
    }

    if let Err(ref e) = output_to_file(&output, program) {
        eprintln!("Can't write compiler output to {}", output.display());
        err!(e);
    }
}

fn main() {
    let mut cli = Cli::parse();

    let input = {
        if cli.files.len() > 1 {
            let error: Box<dyn Error> = "Only one source file is currently supported".into();
            err!(error.as_ref());
        }
        #[allow(clippy::unwrap_used, reason = "Required argument can't be empty")]
        cli.files.pop().unwrap()
    };

    let code = match if input == Path::new("-") {
        io::read_to_string(io::stdin().lock())
    } else {
        fs::read_to_string(&input)
    } {
        Ok(code) => code,
        Err(ref e) => err!(e),
    };

    if cli.ir {
        print_ir(&code, &cli.config);
    } else if cli.asm {
        match compiler::generate_assembly(&code, &cli.config) {
            Ok(asm) => println!("{asm}"),
            Err(ref e) => err!(e),
        }
    } else {
        compile(&code, cli, &input);
    }
}
