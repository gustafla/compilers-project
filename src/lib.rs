pub mod ast;
mod config;
mod ir;
mod location;
pub mod parser;
mod symtab;
pub mod tokenizer;
mod trace;
mod typecheck;

pub use config::Config;
pub use location::Location;
pub use symtab::SymbolTable;
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

macro_rules! fun {
    (($($par: expr),*$(,)?) => $res: expr) => {
        Type::Fun {
            parameters: vec![$($par),*],
            result: Box::new($res),
        }
    };
}

pub fn compile(code: &str, config: &Config) -> Result<Vec<u8>, Error> {
    config::configure(config.clone()); // TODO: this thread_local "global" stinks
    let tokens = tokenizer::tokenize(code)?;
    let mut ast = parser::parse(&tokens)?;

    let root_types = &[
        ("print_int", fun!((Type::Int) => Type::Unit)),
        ("print_bool", fun!((Type::Bool) => Type::Unit)),
        ("read_int", fun!(() => Type::Int)),
        ("binary_add", fun!((Type::Int, Type::Int) => Type::Int)),
        ("binary_sub", fun!((Type::Int, Type::Int) => Type::Int)),
        ("unary_sub", fun!((Type::Int) => Type::Int)),
        ("binary_mul", fun!((Type::Int, Type::Int) => Type::Int)),
        ("binary_div", fun!((Type::Int, Type::Int) => Type::Int)),
        ("binary_rem", fun!((Type::Int, Type::Int) => Type::Int)),
        ("binary_lt", fun!((Type::Int, Type::Int) => Type::Bool)),
        ("binary_leq", fun!((Type::Int, Type::Int) => Type::Bool)),
        ("binary_gt", fun!((Type::Int, Type::Int) => Type::Bool)),
        ("binary_geq", fun!((Type::Int, Type::Int) => Type::Bool)),
        ("binary_and", fun!((Type::Bool, Type::Bool) => Type::Bool)),
        ("binary_or", fun!((Type::Bool, Type::Bool) => Type::Bool)),
        ("unary_not", fun!((Type::Bool) => Type::Bool)),
    ];

    typecheck::typecheck(&mut ast, root_types)?;
    dbg!(&ast);
    let ir = ir::generate_ir(&ast, root_types);
    todo!()
}

pub fn parse<'a>(code: &'a str, config: &Config) -> Result<ast::Ast<'a>, Error> {
    config::configure(config.clone()); // TODO: this thread_local "global" stinks
    let tokens = tokenizer::tokenize(code)?;
    let ast = parser::parse(&tokens)?;
    Ok(ast)
}
