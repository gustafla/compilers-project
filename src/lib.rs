mod tokenizer;

use clap::Args;
use thiserror::Error;

#[derive(Args, Debug)]
pub struct Config {
    #[arg(short, long, global = true)]
    pub verbose: bool,
}

#[derive(Error, Debug)]
pub enum Error {}

#[expect(unused_variables, reason = "Unimplemented")]
pub fn compile(code: &str, config: &Config) -> Result<Vec<u8>, Error> {
    Ok(b"Hello!\n".to_vec())
}

pub fn print_error(mut error: &dyn std::error::Error) {
    eprintln!("\x1b[93m{error}\x1b[0m");
    while let Some(source) = error.source() {
        eprintln!("Caused by: \x1b[35m{source}\x1b[0m");
        error = source;
    }
}
