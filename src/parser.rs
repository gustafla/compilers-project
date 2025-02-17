mod tests;

use crate::{
    ast::{
        Ast, BinaryOp, Block, Conditional, Expression, FnCall, Identifier, Int, Literal, Op,
        UnaryOp, Var,
    },
    config,
    tokenizer::{Kind, Token, Tokens},
    trace::{end_trace, start_trace, traceln},
};
use std::{num::ParseIntError, str::ParseBoolError};
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
    #[error("Expected an comma, but encountered {0:?}")]
    ExpectedComma(Kind),
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

fn parse_op(tokens: &Tokens, at: &mut usize) -> Result<Op, Error> {
    let code = tokens.code();
    let token = tokens.peek(*at);
    let s = token.as_str(code);
    traceln!("parse_op, token = {s:?}");
    let op = match s.parse::<Op>() {
        Ok(op) => op,
        Err(..) => {
            return Err(Error::ExpectedOneOf {
                of: vec![
                    "+".into(),
                    "-".into(),
                    "*".into(),
                    "/".into(),
                    "%".into(),
                    "==".into(),
                    "!=".into(),
                    "<=".into(),
                    "<".into(),
                    ">=".into(),
                    ">".into(),
                    "and".into(),
                    "or".into(),
                    "not".into(),
                    "=".into(),
                ],
                token: s.into(),
            })
        }
    };
    traceln!("consuming...");

    tokens.consume(at);
    Ok(op)
}

fn parse_literal<'a>(tokens: &'a Tokens, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
    let code = tokens.code();
    let token = tokens.peek(*at);
    traceln!("parse_literal, token = {:?}", token.as_str(code));

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
            let unquoted = &token.as_str(code)[1..token.len() - 1];
            // TODO: Process escape syntax like \"\" and \\
            Ok(Literal::Str(unquoted))
        }
        _ => return None,
    };
    let _ = tokens.consume(at);
    Some(result.map(|lit| Ast {
        location: token.location().clone(),
        tree: Box::new(Expression::Literal(lit)),
    }))
}

fn parse_identifier<'a>(tokens: &'a Tokens, at: &mut usize) -> Option<Ast<'a>> {
    let code = tokens.code();
    let token = tokens.peek(*at);
    traceln!("parse_identifier, token = {:?}", token.as_str(code));

    if token.kind() != Kind::Identifier {
        return None;
    }

    let token = tokens.consume(at);
    Some(Ast {
        location: token.location().clone(),
        tree: Box::new(Expression::Identifier(Identifier {
            name: token.as_str(code),
        })),
    })
}

fn parse_conditional<'a>(tokens: &'a Tokens, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
    let code = tokens.code();
    let token = tokens.peek(*at);
    traceln!("parse_conditional, token = {:?}", token.as_str(code));

    // If
    if token.kind() != Kind::Identifier || token.as_str(code) != "if" {
        return None;
    }
    let start = token.location().start();
    tokens.consume(at);

    // <condition>
    let condition = match parse_expression(tokens, at) {
        Ok(ast) => ast,
        Err(e) => return Some(Err(e)),
    };

    // Then
    let token = tokens.consume(at);
    if token.kind() != Kind::Identifier || token.as_str(code) != "then" {
        return Some(Err(Error::ExpectedKeyword("then".into(), token.kind())));
    }

    // <then>
    let then_expr = match parse_expression(tokens, at) {
        Ok(ast) => ast,
        Err(e) => return Some(Err(e)),
    };

    // Else
    let token = tokens.peek(*at);
    let else_expr = if token.kind() == Kind::Identifier && token.as_str(code) == "else" {
        // <else>
        tokens.consume(at);
        match parse_expression(tokens, at) {
            Ok(ast) => Some(ast),
            Err(e) => return Some(Err(e)),
        }
    } else {
        None
    };

    let end = tokens.peek(*at - 1).location().end();
    Some(Ok(Ast {
        location: (start..end).into(),
        tree: Box::new(Expression::Conditional(Conditional {
            condition,
            then_expr,
            else_expr,
        })),
    }))
}

fn parse_fn_call<'a>(tokens: &'a Tokens, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
    let code = tokens.code();
    let (t0, t1) = (tokens.peek(*at), tokens.peek(*at + 1));
    traceln!("parse_fn_call, token = {:?}", t0.as_str(code));

    // Check for identifier followed by an opening paren
    let (Kind::Identifier, "(") = (t0.kind(), t1.as_str(code)) else {
        return None;
    };
    let start = t0.location().start();
    let function = parse_identifier(tokens, at).unwrap();

    // Consume opening paren
    tokens.consume(at);

    // Parse arguments
    let mut arguments = Vec::new();
    loop {
        if tokens.peek(*at).as_str(code) == ")" {
            tokens.consume(at);
            break;
        }
        let arg = match parse_expression(tokens, at) {
            Ok(ast) => ast,
            Err(e) => return Some(Err(e)),
        };
        arguments.push(arg);
        let token = tokens.consume(at);
        if token.as_str(code) == ")" {
            break;
        }
        if token.as_str(code) != "," {
            return Some(Err(Error::ExpectedComma(token.kind())));
        }
    }

    let end = tokens.peek(*at - 1).location().end();
    Some(Ok(Ast {
        location: (start..end).into(),
        tree: Box::new(Expression::FnCall(FnCall {
            function,
            arguments,
        })),
    }))
}

fn parse_block<'a>(tokens: &'a Tokens, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
    let code = tokens.code();
    let token = tokens.peek(*at);
    let start = token.location().start();
    traceln!("parse_block, token = {:?}", token.as_str(code));

    if tokens.consume_one_of(at, &["{"]).is_err() {
        return None;
    };

    let mut expressions = Vec::new();
    let mut result = None;

    while tokens.consume_one_of(at, &["}"]).is_err() {
        // Only blocks can contain variable declarations, try them first
        let ast = match parse_var(tokens, at) {
            Some(Ok(ast)) => ast,
            Some(Err(e)) => return Some(Err(e)),
            None => match parse_expression(tokens, at) {
                Ok(ast) => ast,
                Err(e) => return Some(Err(e)),
            },
        };

        if let Err(e) = tokens.consume_one_of(at, &[";"]) {
            if tokens.consume_one_of(at, &["}"]).is_ok() {
                result = Some(ast);
                break;
            }

            if tokens.peek(*at - 1).as_str(code) != "}" {
                return Some(Err(e));
            }
        };

        expressions.push(ast);
    }

    let end = tokens.peek(*at - 1).location().end();
    Some(Ok(Ast {
        location: (start..end).into(),
        tree: Box::new(Expression::Block(Block {
            expressions,
            result,
        })),
    }))
}

fn parse_var<'a>(tokens: &'a Tokens, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
    let code = tokens.code();
    let token = tokens.peek(*at);
    traceln!("parse_var, token = {:?}", token.as_str(code));

    // var
    if token.kind() != Kind::Identifier || token.as_str(code) != "var" {
        return None;
    }
    let start = token.location().start();
    tokens.consume(at);

    // _MAYBE_ parse_assignment_expr_right could be used here,
    // but I'd have to specify its parse_term_fn and that's not trivial.
    // Also, I'd have to verify that it has at least one =

    // <id>
    let Some(id) = parse_identifier(tokens, at) else {
        return Some(Err(Error::ExpectedIdentifier(tokens.peek(*at).kind())));
    };

    // Op::Assign
    match parse_op(tokens, at) {
        Ok(Op::Assign) => {}
        Ok(op) => {
            return Some(Err(Error::ExpectedOneOf {
                of: vec!["=".to_owned()],
                token: op.to_string(),
            }))
        }
        Err(e) => return Some(Err(e)),
    };

    // <init>
    let init = match parse_expression(tokens, at) {
        Ok(ast) => ast,
        Err(e) => return Some(Err(e)),
    };

    let end = tokens.peek(*at - 1).location().end();
    Some(Ok(Ast {
        location: (start..end).into(),
        tree: Box::new(Expression::Var(Var { id, init })),
    }))
}

fn parse_factor<'a>(tokens: &'a Tokens, at: &mut usize) -> Result<Ast<'a>, Error> {
    let code = tokens.code();
    let token = tokens.peek(*at);
    let tok_str = token.as_str(code);
    traceln!("parse_factor, token = {:?}", tok_str);

    if tok_str == "(" {
        return parse_parenthesized(tokens, at);
    } else {
        for parse_fn in [parse_block, parse_literal, parse_fn_call, parse_conditional] {
            if let Some(res) = parse_fn(tokens, at) {
                return res;
            }
        }
        if let Some(id) = parse_identifier(tokens, at) {
            return Ok(id);
        }
    };

    Err(Error::ExpectedTerm(tokens.peek(*at).kind()))
}

fn parse_parenthesized<'a>(tokens: &'a Tokens, at: &mut usize) -> Result<Ast<'a>, Error> {
    let code = tokens.code();
    let token = tokens.peek(*at);
    traceln!("parse_parenthesized, token = {:?}", token.as_str(code));

    tokens.consume_one_of(at, &["("])?;
    let res = parse_expression(tokens, at);
    tokens.consume_one_of(at, &[")"])?;
    res
}

fn parse_binary_expr_left<'a>(
    tokens: &'a Tokens<'_>,
    at: &mut usize,
    ops: &[&str],
    parse_term_fn: impl Fn(&'a Tokens, &mut usize) -> Result<Ast<'a>, Error>,
) -> Result<Ast<'a>, Error> {
    let code = tokens.code();
    traceln!(
        "parse_binary_expr_left for {ops:?}, token = {:?} at {at}",
        tokens.peek(*at).as_str(code)
    );

    // Accumulate tree into left
    let mut left = parse_term_fn(tokens, at)?;
    let start = left.location.start();

    let result = loop {
        if !ops.contains(&tokens.peek(*at).as_str(code)) {
            break Ok(left);
        }

        let op = match parse_op(tokens, at) {
            Ok(op) => op,
            Err(e) => break Err(e),
        };

        let right = match parse_term_fn(tokens, at) {
            Ok(ast) => ast,
            Err(e) => break Err(e),
        };

        left = Ast {
            location: (start..right.location.end()).into(),
            tree: Box::new(Expression::BinaryOp(BinaryOp { left, op, right })),
        };
    };

    result
}

fn parse_assignment_expr_right<'a>(
    tokens: &'a Tokens<'_>,
    at: &mut usize,
    parse_term_fn: impl Fn(&'a Tokens, &mut usize) -> Result<Ast<'a>, Error>,
) -> Result<Ast<'a>, Error> {
    let code = tokens.code();
    traceln!(
        "parse_assignment_expr_right (=), token = {:?} at {at}",
        tokens.peek(*at).as_str(code)
    );

    let mut terms = Vec::new();
    terms.push(parse_term_fn(tokens, at)?);

    while tokens.peek(*at).as_str(code) == "=" {
        tokens.consume(at);
        terms.push(parse_term_fn(tokens, at)?);
    }

    let mut right = terms.pop().unwrap();

    while let Some(ast) = terms.pop() {
        right = Ast {
            location: (ast.location.start()..right.location.end()).into(),
            tree: Box::new(Expression::BinaryOp(BinaryOp {
                left: ast,
                op: Op::Assign,
                right,
            })),
        };
    }

    Ok(right)
}

fn parse_unary_expr<'a>(
    tokens: &'a Tokens<'_>,
    at: &mut usize,
    ops: &[&str],
    parse_term_fn: impl Fn(&'a Tokens, &mut usize) -> Result<Ast<'a>, Error>,
) -> Result<Ast<'a>, Error> {
    let code = tokens.code();
    traceln!(
        "parse_unary_expr for {ops:?}, token = {:?} at {at}",
        tokens.peek(*at).as_str(code)
    );

    let mut op = Vec::new();
    while ops.contains(&tokens.peek(*at).as_str(code)) {
        op.push((tokens.peek(*at).location().start(), parse_op(tokens, at)?));
    }

    let mut ast = parse_term_fn(tokens, at)?;

    while let Some((sign_start, op)) = op.pop() {
        ast = Ast {
            location: (sign_start..ast.location.end()).into(),
            tree: Box::new(Expression::UnaryOp(UnaryOp { op, right: ast })),
        };
    }

    Ok(ast)
}

fn parse_expression<'a>(tokens: &'a Tokens<'_>, at: &mut usize) -> Result<Ast<'a>, Error> {
    parse_assignment_expr_right(tokens, at, |tokens, at| {
        parse_binary_expr_left(tokens, at, &["or"], |tokens, at| {
            parse_binary_expr_left(tokens, at, &["and"], |tokens, at| {
                parse_binary_expr_left(tokens, at, &["==", "!="], |tokens, at| {
                    parse_binary_expr_left(tokens, at, &["<", "<=", ">", ">="], |tokens, at| {
                        parse_binary_expr_left(tokens, at, &["+", "-"], |tokens, at| {
                            parse_binary_expr_left(tokens, at, &["*", "/", "%"], |tokens, at| {
                                parse_unary_expr(tokens, at, &["-", "not"], |tokens, at| {
                                    parse_factor(tokens, at)
                                })
                            })
                        })
                    })
                })
            })
        })
    })
}

impl Tokens<'_> {
    fn consume(&self, at: &mut usize) -> Token {
        let token = self.peek(*at);
        traceln!(
            "Tokens::consume, at = {}, token = {:?}",
            *at,
            token.as_str(self.code())
        );
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
    start_trace!("Parser");
    let mut at = 0;
    let root = parse_expression(tokens, &mut at)?;
    end_trace!();

    if config::verbose() {
        dbg!(&root);
    }

    let kind = tokens.peek(at).kind();
    if kind != Kind::End {
        return Err(Error::ExpectedEnd(kind));
    }

    Ok(root)
}
