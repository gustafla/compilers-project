use crate::{
    tokenizer::{Kind, Token, Tokens},
    Config,
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {}

type Int = i64;

pub enum Literal<'a> {
    Int(Int),
    Bool(bool),
    Str(&'a str),
}

impl<'a> Literal<'a> {
    pub fn parse(token: &Token, code: &'a str) -> Option<Self> {
        match token.kind() {
            Kind::Identifier => match token.as_str(code).parse::<bool>() {
                Ok(boolean) => Some(Literal::Bool(boolean)),
                Err(..) => None,
            },
            Kind::Integer => match token.as_str(code).parse::<Int>() {
                Ok(i) => Some(Literal::Int(i)),
                Err(..) => None,
            },
            Kind::StrLiteral => {
                let unquoted = &token.as_str(code)[1..token.len() - 2];
                Some(Literal::Str(unquoted))
            }
            _ => None,
        }
    }
}

pub struct Identifer<'a> {
    name: &'a str,
}

impl<'a> Identifer<'a> {
    pub fn parse(token: &Token, code: &'a str) -> Option<Self> {
        match token.kind() {
            Kind::Identifier => Some(Self {
                name: token.as_str(code),
            }),
            _ => None,
        }
    }
}

pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl Op {
    pub fn parse(token: &Token, code: &str) -> Option<Self> {
        match token.kind() {
            Kind::Operator => match token.as_str(code) {
                "+" => Some(Op::Add),
                "-" => Some(Op::Sub),
                "*" => Some(Op::Mul),
                "/" => Some(Op::Div),
                _ => None,
            },
            _ => None,
        }
    }
}

pub struct BinaryOp<'a> {
    left: Box<Expression<'a>>,
    op: Op,
    right: Box<Expression<'a>>,
}

fn parse_term<'a>(token: &Token, code: &'a str) -> Option<Expression<'a>> {
    if let Some(lit) = Literal::parse(token, code) {
        Some(Expression::Literal(lit))
    } else if let Some(id) = Identifer::parse(token, code) {
        Some(Expression::Identifier(id))
    } else {
        None
    }
}

impl<'a> BinaryOp<'a> {
    pub fn parse(tokens: &[Token], code: &'a str) -> Option<(Self, usize)> {
        match tokens {
            [left, op, right, ..] => {
                let left = Box::new(parse_term(left, code)?);
                let op = Op::parse(op, code)?;
                let right = Box::new(parse_term(right, code)?);
                Some((Self { left, op, right }, 3))
            }
            _ => None,
        }
    }
}

pub enum Expression<'a> {
    Literal(Literal<'a>),
    Identifier(Identifer<'a>),
    BinaryOp(BinaryOp<'a>),
}

impl<'a> Expression<'a> {
    pub fn parse_left(tokens: &[Token], code: &'a str) -> Option<Self> {
        let Some(token) = tokens.first() else {
            return None;
        };

        let mut left = parse_term(token, code)?;
        let mut n = 1;
        while let Some(token) = tokens.get(n) {
            let Some(op) = Op::parse(token, code) else {
                break;
            };
            n += 1;

            let Some(right) = tokens.get(n).and_then(|token| parse_term(token, code)) else {
                break;
            };
            n += 1;

            left = Expression::BinaryOp(BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            });
        }

        Some(left)
    }
}

pub struct Ast<'a> {
    root: Expression<'a>,
}

pub fn parse<'a>(tokens: &'a Tokens, config: &Config) -> Result<Ast<'a>, Error> {
    todo!()
}
