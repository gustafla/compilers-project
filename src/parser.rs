#![cfg_attr(not(test), expect(dead_code, reason = "Work in progress"))]

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
    #[error("Expected keyword `{0}`, but encountered {1:?}")]
    ExpectedKeyword(String, Kind),
    #[error("Unexpected {0:?} (expected EOF)")]
    ExpectedEnd(Kind),
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

#[derive(Debug, PartialEq)]
pub enum Literal<'a> {
    Int(Int),
    Bool(bool),
    Str(&'a str),
}

impl<'a> Literal<'a> {
    pub fn parse(tokens: &'a Tokens, at: &mut usize) -> Option<Result<Self, Error>> {
        let code = tokens.code();
        let token = tokens.peek(*at);

        let result = match token.kind() {
            Kind::Identifier => match token.as_str(code).parse::<bool>() {
                Ok(boolean) => Ok(Literal::Bool(boolean)),
                Err(..) => return None, // No error, there are other identifiers than true and false
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
                // TODO: Process escape syntax like \"\" and \\
                Ok(Literal::Str(unquoted))
            }
            _ => return None,
        };
        let _ = tokens.consume(at);
        Some(result)
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifer<'a> {
    name: &'a str,
}

impl<'a> Identifer<'a> {
    pub fn parse(tokens: &'a Tokens, at: &mut usize) -> Option<Self> {
        let code = tokens.code();
        let token = tokens.peek(*at);
        if token.kind() != Kind::Identifier {
            return None;
        }

        let token = tokens.consume(at);
        Some(Self {
            name: token.as_str(code),
        })
    }
}

#[derive(Debug, PartialEq)]
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
    left: Ast<'a>,
    op: Op,
    right: Ast<'a>,
}

#[derive(Debug)]
pub struct Conditional<'a> {
    condition: Ast<'a>,
    then_expr: Ast<'a>,
    else_expr: Option<Ast<'a>>,
}

impl<'a> Conditional<'a> {
    fn parse(tokens: &'a Tokens, at: &mut usize) -> Option<Result<Self, Error>> {
        let code = tokens.code();

        // If
        let token = tokens.peek(*at);
        if token.kind() != Kind::Identifier || token.as_str(code) != "if" {
            return None;
        }
        tokens.consume(at);

        // <condition>
        let condition = match parse_expression(tokens, at) {
            Ok(expr) => Ast {
                tree: Box::new(expr),
            },
            Err(e) => return Some(Err(e)),
        };

        // Then
        let token = tokens.consume(at);
        if token.kind() != Kind::Identifier || token.as_str(code) != "then" {
            return Some(Err(Error::ExpectedKeyword("then".into(), token.kind())));
        }

        // <then>
        let then_expr = match parse_expression(tokens, at) {
            Ok(expr) => Ast {
                tree: Box::new(expr),
            },
            Err(e) => return Some(Err(e)),
        };

        // Else
        let token = tokens.peek(*at);
        let else_expr = if token.kind() == Kind::Identifier || token.as_str(code) == "else" {
            // <else>
            tokens.consume(at);
            match parse_expression(tokens, at) {
                Ok(expr) => Some(Ast {
                    tree: Box::new(expr),
                }),
                Err(e) => return Some(Err(e)),
            }
        } else {
            None
        };

        Some(Ok(Conditional {
            condition,
            then_expr,
            else_expr,
        }))
    }
}

#[derive(Debug)]
pub enum Expression<'a> {
    Literal(Literal<'a>),
    Identifier(Identifer<'a>),
    BinaryOp(BinaryOp<'a>),
    Conditional(Conditional<'a>),
}

#[derive(Debug)]
pub struct Ast<'a> {
    // location: Location,
    tree: Box<Expression<'a>>,
}

fn parse_factor<'a>(tokens: &'a Tokens, at: &mut usize) -> Result<Expression<'a>, Error> {
    if tokens.peek(*at).as_str(tokens.code()) == "(" {
        parse_parenthesized(tokens, at)
    } else if let Some(res) = Literal::parse(tokens, at) {
        Ok(Expression::Literal(res?))
    } else if let Some(id) = Identifer::parse(tokens, at) {
        Ok(Expression::Identifier(id))
    } else if let Some(res) = Conditional::parse(tokens, at) {
        Ok(Expression::Conditional(res?))
    } else {
        Err(Error::ExpectedTerm(tokens.peek(*at).kind()))
    }
}

fn parse_parenthesized<'a>(tokens: &'a Tokens, at: &mut usize) -> Result<Expression<'a>, Error> {
    tokens.consume_one_of(at, &["("])?;
    let expr = parse_expression(tokens, at);
    tokens.consume_one_of(at, &[")"])?;
    expr
}

macro_rules! token_str {
    ($tokens: expr, $at: expr) => {
        $tokens.peek($at).as_str($tokens.code())
    };
}

fn parse_expression_left<'a>(
    tokens: &'a Tokens<'_>,
    at: &mut usize,
    ops: &[&str],
    parse_term_fn: impl Fn(&'a Tokens, &mut usize) -> Result<Expression<'a>, Error>,
) -> Result<Expression<'a>, Error> {
    let code = tokens.code();

    start_trace!("Parser");
    trace!(token_str!(tokens, *at));

    // Accumulate tree into left
    let mut left = parse_term_fn(tokens, at)?;

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

        let right = match parse_term_fn(tokens, at) {
            Ok(expr) => expr,
            Err(e) => break Err(e),
        };

        left = Expression::BinaryOp(BinaryOp {
            left: Ast {
                tree: Box::new(left),
            },
            op,
            right: Ast {
                tree: Box::new(right),
            },
        });
    };

    end_trace!();

    result
}

fn parse_expression<'a>(tokens: &'a Tokens<'_>, at: &mut usize) -> Result<Expression<'a>, Error> {
    parse_expression_left(tokens, at, &["+", "-"], |tokens, at| {
        parse_expression_left(tokens, at, &["*", "/"], parse_factor)
    })
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
    let mut at = 0;
    let root = parse_expression(tokens, &mut at)?;

    let kind = tokens.peek(at).kind();
    if kind != Kind::End {
        return Err(Error::ExpectedEnd(kind));
    }

    dbg!(&root);
    Ok(Ast {
        tree: Box::new(root),
    })
}
