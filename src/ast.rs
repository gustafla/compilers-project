pub mod macros;
pub mod op;

use std::fmt::Display;

use crate::{Location, Type};
pub use op::Op;

#[derive(Debug)]
pub struct Ast<'a> {
    pub location: Location,
    pub ty: Option<Type>,
    pub tree: Box<Expression<'a>>,
}

#[derive(Debug)]
pub enum Expression<'a> {
    Literal(Literal<'a>),
    Identifier(Identifier<'a>),
    Conditional(Conditional<'a>),
    FnCall(FnCall<'a>),
    Block(Block<'a>),
    Var(Var<'a>),
    BinaryOp(BinaryOp<'a>),
    UnaryOp(UnaryOp<'a>),
    While(While<'a>),
}

impl Display for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expression::Literal(_) => "literal",
                Expression::Identifier(_) => "identifier",
                Expression::Conditional(_) => "conditional",
                Expression::FnCall(_) => "function call",
                Expression::Block(_) => "block",
                Expression::Var(_) => "variable declaration",
                Expression::BinaryOp(_) => "binary operation",
                Expression::UnaryOp(_) => "unary operation",
                Expression::While(_) => "while loop",
            }
        )
    }
}

pub type Int = i64;

#[derive(Debug, PartialEq)]
pub enum Literal<'a> {
    Int(Int),
    Bool(bool),
    Str(&'a str),
}

#[derive(Debug, PartialEq)]
pub struct Identifier<'a> {
    pub name: &'a str,
}

#[derive(Debug)]
pub struct Conditional<'a> {
    pub condition: Ast<'a>,
    pub then_expr: Ast<'a>,
    pub else_expr: Option<Ast<'a>>,
}

#[derive(Debug)]
pub struct FnCall<'a> {
    pub function: Ast<'a>,
    pub arguments: Vec<Ast<'a>>,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub expressions: Vec<Ast<'a>>,
    pub result: Option<Ast<'a>>,
}

#[derive(Debug)]
pub struct Var<'a> {
    pub id: Ast<'a>,
    pub init: Ast<'a>,
}

#[derive(Debug)]
pub struct BinaryOp<'a> {
    pub left: Ast<'a>,
    pub op: Op,
    pub right: Ast<'a>,
}

#[derive(Debug)]
pub struct UnaryOp<'a> {
    pub op: Op,
    pub right: Ast<'a>,
}

#[derive(Debug)]
pub struct While<'a> {
    pub condition: Ast<'a>,
    pub do_expr: Ast<'a>,
}
