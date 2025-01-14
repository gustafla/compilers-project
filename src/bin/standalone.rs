use clap::Parser;
use compilers_project as compiler;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(version, about = "Compilers Course Project - Standalone Compiler")]
struct Cli {
    #[command(flatten)]
    config: compiler::Config,
    #[arg(short, long)]
    output: Option<PathBuf>,
    #[clap(required = true)]
    files: Vec<PathBuf>,
}

fn main() {
    let cli = Cli::parse();

    println!("{cli:?}");
    unimplemented!();
}
