pub mod ast;
mod config;
mod ir;
mod location;
pub mod parser;
mod symtab;
pub mod tokenizer;
mod trace;
mod typecheck;

use ast::{Operator, op::Ary};
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
        (
            Operator::Add.function_name(Ary::Binary),
            fun!((Type::Int, Type::Int) => Type::Int),
        ),
        (
            Operator::Sub.function_name(Ary::Binary),
            fun!((Type::Int, Type::Int) => Type::Int),
        ),
        (
            Operator::Sub.function_name(Ary::Unary),
            fun!((Type::Int) => Type::Int),
        ),
        (
            Operator::Mul.function_name(Ary::Binary),
            fun!((Type::Int, Type::Int) => Type::Int),
        ),
        (
            Operator::Div.function_name(Ary::Binary),
            fun!((Type::Int, Type::Int) => Type::Int),
        ),
        (
            Operator::Rem.function_name(Ary::Binary),
            fun!((Type::Int, Type::Int) => Type::Int),
        ),
        (
            Operator::Lt.function_name(Ary::Binary),
            fun!((Type::Int, Type::Int) => Type::Bool),
        ),
        (
            Operator::Leq.function_name(Ary::Binary),
            fun!((Type::Int, Type::Int) => Type::Bool),
        ),
        (
            Operator::Gt.function_name(Ary::Binary),
            fun!((Type::Int, Type::Int) => Type::Bool),
        ),
        (
            Operator::Geq.function_name(Ary::Binary),
            fun!((Type::Int, Type::Int) => Type::Bool),
        ),
        (
            Operator::And.function_name(Ary::Binary),
            fun!((Type::Bool, Type::Bool) => Type::Bool),
        ),
        (
            Operator::Or.function_name(Ary::Binary),
            fun!((Type::Bool, Type::Bool) => Type::Bool),
        ),
        (
            Operator::Not.function_name(Ary::Unary),
            fun!((Type::Bool) => Type::Bool),
        ),
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
