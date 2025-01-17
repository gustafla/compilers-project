mod location;
mod parser;
mod tokenizer;

use clap::Args;
pub use location::Location;
use thiserror::Error;

#[derive(Args, Debug)]
pub struct Config {
    #[arg(short, long, global = true)]
    pub verbose: bool,
}

#[cfg(test)]
impl Default for Config {
    fn default() -> Self {
        Self { verbose: true }
    }
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Could not tokenize input")]
    Tokenize(#[from] tokenizer::Error),
    #[error("Could not parse token list")]
    Parse(#[from] parser::Error),
}

pub fn print_error(mut error: &dyn std::error::Error) {
    eprintln!("\x1b[93m{error}\x1b[0m");
    while let Some(source) = error.source() {
        eprintln!("Caused by: \x1b[35m{source}\x1b[0m");
        error = source;
    }
}

#[expect(unused_variables, reason = "Unimplemented")]
pub fn compile(code: &str, config: &Config) -> Result<Vec<u8>, Error> {
    let tokens = tokenizer::tokenize(code, config)?;
    let ast = parser::parse(&tokens, config)?;
    todo!()
}
