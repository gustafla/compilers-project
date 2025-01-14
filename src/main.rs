use base64::prelude::*;
use clap::{Args, Parser, Subcommand};
use serde::Deserialize;
use serde_json::json;
use std::{
    error::Error,
    io::{self, BufReader, Read, Write},
    net::{IpAddr, SocketAddr, TcpListener, TcpStream},
    path::PathBuf,
};

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
    #[arg(default_value = "::", short = 'a', long = "addr", value_name = "ADDR")]
    ip: IpAddr,
    #[arg(default_value = "3000", short, long, value_parser = clap::value_parser!(u16).range(1..))]
    port: u16,
}

#[derive(Args, Debug)]
struct CompileArgs {
    #[arg(short, long)]
    output: Option<PathBuf>,
    #[clap(required = true)]
    files: Vec<PathBuf>,
}

#[derive(Deserialize)]
#[serde(tag = "command", rename_all = "lowercase")]
enum Request {
    Ping,
    Compile { code: String },
}

fn print_error(mut error: &dyn Error) {
    eprintln!("\x1b[93m{error}\x1b[0m");
    while let Some(source) = error.source() {
        eprintln!("Caused by: \x1b[35m{source}\x1b[0m");
        error = source;
    }
}

#[expect(unused_variables, reason = "Unimplemented")]
fn compile(code: String) -> Result<Vec<u8>, Box<dyn Error>> {
    Ok(b"Hello!\n".to_vec())
}

fn handle_connection(stream: Result<TcpStream, io::Error>) -> Result<(), Box<dyn Error>> {
    let mut stream = stream?;
    let request: Request = serde_json::from_reader(&stream)?;
    let response = match request {
        Request::Ping => json!({}),
        Request::Compile { code } => match compile(code) {
            Ok(program) => json!({
                "program": BASE64_STANDARD.encode(program)
            }),
            Err(e) => json!({"error": e.to_string()}),
        },
    };
    stream.write_all(response.to_string().as_bytes())?;
    Ok(())
}

fn run_server(args: ServerArgs) -> Result<(), Box<dyn Error>> {
    let addr = SocketAddr::new(args.ip, args.port);
    let listener = TcpListener::bind(addr)?;

    for stream in listener.incoming() {
        if let Err(e) = handle_connection(stream) {
            print_error(e.as_ref());
        }
    }

    Ok(())
}

fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Command::Server(server_args) => run_server(server_args),
        #[expect(unused_variables, reason = "Unimplemented")]
        Command::Compile(compile_args) => unimplemented!(),
    };

    if let Err(e) = result {
        print_error(e.as_ref());
        std::process::exit(1);
    }
}
