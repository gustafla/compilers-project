mod tests;

use crate::{
    tokenizer::{Kind, Token, Tokens},
    trace::{end_trace, start_trace, trace},
};
use std::{fmt::Display, num::ParseIntError, str::ParseBoolError};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Expected one of {of:?}, but encountered {token:?}")]
    ExpectedOneOf { of: Vec<String>, token: String },
    #[error("Expected a literal, but encountered {0:?}")]
    ExpectedLiteral(Kind),
    #[error("Expected an identifier, but encountered {0:?}")]
    ExpectedIdentifier(Kind),
    #[error("Expected a literal or an identifier, but encountered {0:?}")]
    ExpectedTerm(Kind),
    #[error("Expected an operator, but encountered {0:?}")]
    ExpectedOp(Kind),
    #[error("Cannot parse {token:?} as a boolean")]
    Bool {
        token: String,
        source: ParseBoolError,
    },
    #[error("Cannot parse {token:?} as an integer")]
    Int {
        token: String,
        source: ParseIntError,
    },
}

type Int = i64;

#[derive(Debug)]
pub enum Literal<'a> {
    Int(Int),
    Bool(bool),
    Str(&'a str),
}

impl<'a> Literal<'a> {
    pub fn parse(tokens: &'a Tokens, at: &mut usize) -> Result<Self, Error> {
        let code = tokens.code();
        let token = tokens.peek(*at);
        match token.kind() {
            Kind::Identifier | Kind::Integer | Kind::StrLiteral => {
                let token = tokens.consume(at);
                match token.kind() {
                    Kind::Identifier => match token.as_str(code).parse::<bool>() {
                        Ok(boolean) => Ok(Literal::Bool(boolean)),
                        Err(source) => Err(Error::Bool {
                            token: token.as_str(code).into(),
                            source,
                        }),
                    },
                    Kind::Integer => match token.as_str(code).parse::<Int>() {
                        Ok(i) => Ok(Literal::Int(i)),
                        Err(source) => Err(Error::Int {
                            token: token.as_str(code).into(),
                            source,
                        }),
                    },
                    Kind::StrLiteral => {
                        let unquoted = &token.as_str(code)[1..token.len() - 2];
                        Ok(Literal::Str(unquoted))
                    }
                    _ => unreachable!(),
                }
            }
            kind => Err(Error::ExpectedLiteral(kind)),
        }
    }
}

#[derive(Debug)]
pub struct Identifer<'a> {
    name: &'a str,
}

impl<'a> Identifer<'a> {
    pub fn parse(tokens: &'a Tokens, at: &mut usize) -> Result<Self, Error> {
        let code = tokens.code();
        let token = tokens.peek(*at);
        if token.kind() != Kind::Identifier {
            return Err(Error::ExpectedIdentifier(token.kind()));
        }

        let token = tokens.consume(at);
        Ok(Self {
            name: &token.as_str(code),
        })
    }
}

#[derive(Debug)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl Op {
    pub fn parse(tokens: &Tokens, at: &mut usize) -> Result<Self, Error> {
        let code = tokens.code();
        let token = tokens.peek(*at);
        if token.kind() != Kind::Operator {
            return Err(Error::ExpectedOp(token.kind()));
        }

        let token = tokens.consume(at);
        match token.as_str(code) {
            "+" => Ok(Op::Add),
            "-" => Ok(Op::Sub),
            "*" => Ok(Op::Mul),
            "/" => Ok(Op::Div),
            str => Err(Error::ExpectedOneOf {
                of: vec!["+".into(), "-".into(), "*".into(), "/".into()],
                token: str.into(),
            }),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Add => '+',
                Op::Sub => '-',
                Op::Mul => '*',
                Op::Div => '/',
            }
        )
    }
}

#[derive(Debug)]
pub struct BinaryOp<'a> {
    left: Box<Expression<'a>>,
    op: Op,
    right: Box<Expression<'a>>,
}

#[derive(Debug)]
pub enum Expression<'a> {
    Literal(Literal<'a>),
    Identifier(Identifer<'a>),
    BinaryOp(BinaryOp<'a>),
}

fn parse_factor<'a>(tokens: &'a Tokens, at: &mut usize) -> Result<Expression<'a>, Error> {
    if let Ok(lit) = Literal::parse(tokens, at) {
        Ok(Expression::Literal(lit))
    } else if let Ok(id) = Identifer::parse(tokens, at) {
        Ok(Expression::Identifier(id))
    } else {
        Err(Error::ExpectedTerm(tokens.peek(*at).kind()))
    }
}

macro_rules! token_str {
    ($tokens: expr, $at: expr) => {
        $tokens.peek($at).as_str($tokens.code())
    };
}

fn parse_expression_left<'a>(
    tokens: &'a Tokens<'_>,
    at: &mut usize,
    depth: usize,
    ops: &[&str],
    parse_term_fn: impl Fn(&'a Tokens, &mut usize, usize) -> Result<Expression<'a>, Error>,
) -> Result<Expression<'a>, Error> {
    let code = tokens.code();

    start_trace!("Parser", depth);
    trace!(token_str!(tokens, *at));

    // Accumulate tree into left
    let mut left = parse_term_fn(tokens, at, depth + 1)?;

    let result = loop {
        if !ops.contains(&tokens.peek(*at).as_str(code)) {
            break Ok(left);
        }

        trace!(token_str!(tokens, *at));

        let op = match Op::parse(tokens, at) {
            Ok(op) => op,
            Err(e) => break Err(e),
        };

        trace!(token_str!(tokens, *at));

        let right = match parse_term_fn(tokens, at, depth + 1) {
            Ok(expr) => expr,
            Err(e) => break Err(e),
        };

        left = Expression::BinaryOp(BinaryOp {
            left: Box::new(left),
            op,
            right: Box::new(right),
        });
    };

    end_trace!();

    result
}

fn parse_expression<'a>(tokens: &'a Tokens<'_>) -> Result<Expression<'a>, Error> {
    let mut at = 0;
    parse_expression_left(
        tokens,
        &mut at,
        0,
        &["+", "-"],
        |tokens, at, depth| {
            parse_expression_left(tokens, at, depth, &["*", "/"], |tokens, at, _| {
                parse_factor(tokens, at)
            })
        },
        // TODO: why is depth wrong in trace?
    )
}

pub struct Ast<'a> {
    root: Expression<'a>,
}

impl Tokens<'_> {
    fn consume(&self, at: &mut usize) -> Token {
        let token = self.peek(*at);
        *at += 1;
        token
    }

    fn consume_one_of(&self, at: &mut usize, expected: &[&str]) -> Result<Token, Error> {
        let token = self.peek(*at);
        let fragment = token.as_str(self.code());
        if !expected.contains(&fragment) {
            return Err(Error::ExpectedOneOf {
                of: expected.iter().map(|&s| String::from(s)).collect(),
                token: String::from(fragment),
            });
        }
        *at += 1;
        Ok(token)
    }
}

pub fn parse<'a>(tokens: &'a Tokens) -> Result<Ast<'a>, Error> {
    let root = parse_expression(tokens)?;
    dbg!(&root);
    Ok(Ast { root })
}
