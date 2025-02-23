pub mod ast;
mod config;
mod location;
pub mod parser;
pub mod tokenizer;
mod trace;
mod typecheck;

pub use config::Config;
pub use location::Location;
pub use typecheck::Type;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Could not tokenize input")]
    Tokenize(#[from] tokenizer::Error),
    #[error("Could not parse token list")]
    Parse(#[from] parser::Error),
    #[error("Type checker rejected the program")]
    TypeCheck(#[from] typecheck::Error),
}

pub fn print_error(mut error: &dyn std::error::Error) {
    eprintln!("\x1b[93m{error}\x1b[0m");
    while let Some(source) = error.source() {
        eprintln!("Caused by: \x1b[35m{source}\x1b[0m");
        error = source;
    }
}

pub fn compile(code: &str, config: &Config) -> Result<Vec<u8>, Error> {
    config::configure(config.clone()); // TODO: this thread_local "global" stinks
    let tokens = tokenizer::tokenize(code)?;
    let ast = parser::parse(&tokens)?;
    dbg!(typecheck::typecheck(&ast)?);
    todo!()
}

pub fn parse<'a>(code: &'a str, config: &Config) -> Result<ast::Ast<'a>, Error> {
    config::configure(config.clone()); // TODO: this thread_local "global" stinks
    let tokens = tokenizer::tokenize(code)?;
    let ast = parser::parse(&tokens)?;
    Ok(ast)
}
