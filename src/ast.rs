pub mod macros;
pub mod op;

use crate::Location;
pub use op::Op;

#[derive(Debug)]
pub struct Ast<'a> {
    pub location: Location,
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
