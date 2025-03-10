mod asm;
pub mod ast;
mod config;
pub mod ir;
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
    #[error("Failed to generate IR")]
    IrGen(#[from] ir::Error),
    #[error("Failed to assemble and link")]
    Assembler(#[from] asm::Error),
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

type RootTypes<'a> = Vec<(&'a str, Type)>;

pub fn compile(code: &str, config: &Config) -> Result<Vec<u8>, Error> {
    let assembly_code = generate_assembly(code, config)?;
    let x86_64_elf = asm::assemble(&assembly_code, false, &[])?;
    Ok(x86_64_elf)
}

pub fn generate_assembly(code: &str, config: &Config) -> Result<String, Error> {
    let ins = generate_ir(code, config)?;
    let asm = asm::generate_assembly(&ins);
    Ok(asm)
}

pub fn generate_ir<'a>(
    code: &'a str,
    config: &Config,
) -> Result<Vec<(ir::Function<'a>, Vec<ir::Instruction<'a>>)>, Error> {
    let (ast, root_types) = typecheck_impl(code, config)?;
    let ins = ir::generate_ir(&ast, &root_types)?;
    Ok(ins)
}

pub fn typecheck<'a>(code: &'a str, config: &Config) -> Result<ast::Module<'a>, Error> {
    let (module, _) = typecheck_impl(code, config)?;
    Ok(module)
}

fn typecheck_impl<'a>(
    code: &'a str,
    config: &Config,
) -> Result<(ast::Module<'a>, RootTypes<'a>), Error> {
    let mut module = parse(code, config)?;

    let mut root_types = vec![
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
    root_types.extend(
        module
            .functions
            .iter()
            .map(|fun| (fun.identifier.name, fun.as_type())),
    );

    typecheck::typecheck(&mut module, &root_types)?;
    Ok((module, root_types))
}

pub fn parse<'a>(code: &'a str, config: &Config) -> Result<ast::Module<'a>, Error> {
    let tokens = tokenize(code, config)?;
    let ast = parser::parse(&tokens)?;
    Ok(ast)
}

pub fn tokenize<'a>(code: &'a str, config: &Config) -> Result<tokenizer::Tokens<'a>, Error> {
    config::configure(config.clone());
    let tokens = tokenizer::tokenize(code)?;
    Ok(tokens)
}
