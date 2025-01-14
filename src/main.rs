use clap::{Args, Parser, Subcommand};
use std::{net::IpAddr, path::PathBuf};

#[derive(Parser, Debug)]
#[command(version, about = "Compilers Course Project", infer_subcommands = true)]
struct Cli {
    #[arg(short, long, global = true)]
    verbose: bool,
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    Server(ServerArgs),
    Compile(CompileArgs),
}

#[derive(Args, Debug)]
struct ServerArgs {
    #[arg(short, long, default_value_t = 3000, value_parser = clap::value_parser!(u16).range(1..))]
    port: u16,
    #[arg(short, long, default_value = "::")]
    address: IpAddr,
}

#[derive(Args, Debug)]
struct CompileArgs {
    #[arg(short, long)]
    output: Option<PathBuf>,
    #[clap(required = true)]
    files: Vec<PathBuf>,
}

fn main() {
    let cli = Cli::parse();

    println!("{cli:?}");
}
