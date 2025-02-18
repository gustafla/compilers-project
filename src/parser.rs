mod tests;

use crate::{
    ast::{
        Ast, BinaryOp, Block, Conditional, Expression, FnCall, Identifier, Int, Literal, Op,
        UnaryOp, Var, While,
    },
    config,
    tokenizer::{Kind, Token, Tokens},
    trace::{end_trace, start_trace, traceln},
    Location,
};
use std::{num::ParseIntError, str::ParseBoolError};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Expected {of:?}, but encountered {token:?}")]
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
    let (_, fragment) = tokens.peek(at);
    traceln!("parse_op, token = {fragment:?}");
    let op = match fragment.parse::<Op>() {
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
                token: fragment.into(),
            })
        }
    };
    traceln!("consuming...");

    tokens.consume(at);
    Ok(op)
}

fn parse_literal<'a>(tokens: &'a Tokens, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
    let (token, fragment) = tokens.peek(at);
    traceln!("parse_literal, token = {:?}", fragment);

    let result = match token.kind() {
        Kind::Identifier => match fragment.parse::<bool>() {
            Ok(boolean) => Ok(Literal::Bool(boolean)),
            Err(..) => return None, // No error, there are other identifiers than true and false
        },
        Kind::Integer => match fragment.parse::<Int>() {
            Ok(i) => Ok(Literal::Int(i)),
            Err(source) => Err(Error::Int {
                token: fragment.into(),
                source,
            }),
        },
        Kind::StrLiteral => {
            let unquoted = &fragment[1..token.len() - 1];
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
    let (token, fragment) = tokens.peek(at);
    traceln!("parse_identifier, token = {:?}", fragment);

    if token.kind() != Kind::Identifier {
        return None;
    }

    tokens.consume(at);
    Some(Ast {
        location: token.location().clone(),
        tree: Box::new(Expression::Identifier(Identifier { name: fragment })),
    })
}

fn parse_conditional<'a>(tokens: &'a Tokens, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
    let (token, fragment) = tokens.peek(at);
    traceln!("parse_conditional, token = {:?}", fragment);

    // If
    if token.kind() != Kind::Identifier || fragment != "if" {
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
    let (token, fragment) = tokens.consume(at);
    if token.kind() != Kind::Identifier || fragment != "then" {
        return Some(Err(Error::ExpectedKeyword("then".into(), token.kind())));
    }

    // <then>
    let then_expr = match parse_expression(tokens, at) {
        Ok(ast) => ast,
        Err(e) => return Some(Err(e)),
    };

    // Else
    let (token, fragment) = tokens.peek(at);
    let else_expr = if token.kind() == Kind::Identifier && fragment == "else" {
        // <else>
        tokens.consume(at);
        match parse_expression(tokens, at) {
            Ok(ast) => Some(ast),
            Err(e) => return Some(Err(e)),
        }
    } else {
        None
    };

    let end = tokens.peek_behind(at).0.location().end();
    Some(Ok(Ast {
        location: (start..end).into(),
        tree: Box::new(Expression::Conditional(Conditional {
            condition,
            then_expr,
            else_expr,
        })),
    }))
}

fn parse_while<'a>(tokens: &'a Tokens, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
    let (token, fragment) = tokens.peek(at);
    traceln!("parse_while, token = {:?}", fragment);

    // While
    if token.kind() != Kind::Identifier || fragment != "while" {
        return None;
    }
    let start = token.location().start();
    tokens.consume(at);

    // <condition>
    let condition = match parse_expression(tokens, at) {
        Ok(ast) => ast,
        Err(e) => return Some(Err(e)),
    };

    // Do
    let (token, fragment) = tokens.consume(at);
    if token.kind() != Kind::Identifier || fragment != "do" {
        return Some(Err(Error::ExpectedKeyword("do".into(), token.kind())));
    }

    // <do>
    let do_expr = match parse_expression(tokens, at) {
        Ok(ast) => ast,
        Err(e) => return Some(Err(e)),
    };

    let end = tokens.peek_behind(at).0.location().end();
    Some(Ok(Ast {
        location: (start..end).into(),
        tree: Box::new(Expression::While(While { condition, do_expr })),
    }))
}

fn parse_fn_call<'a>(tokens: &'a Tokens, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
    let ((t0, s0), (_, s1)) = (tokens.peek(at), tokens.peek_ahead(at));
    traceln!("parse_fn_call, token = {s0:?}");

    // Check for identifier followed by an opening paren
    let (Kind::Identifier, "(") = (t0.kind(), s1) else {
        return None;
    };
    let start = t0.location().start();
    let function = parse_identifier(tokens, at).unwrap();

    // Consume opening paren
    tokens.consume(at);

    // Parse arguments
    let mut arguments = Vec::new();
    loop {
        if let (_, ")") = tokens.peek(at) {
            tokens.consume(at);
            break;
        }
        let arg = match parse_expression(tokens, at) {
            Ok(ast) => ast,
            Err(e) => return Some(Err(e)),
        };
        arguments.push(arg);
        let (token, fragment) = tokens.consume(at);
        if fragment == ")" {
            break;
        }
        if fragment != "," {
            return Some(Err(Error::ExpectedComma(token.kind())));
        }
    }

    let end = tokens.peek_behind(at).0.location().end();
    Some(Ok(Ast {
        location: (start..end).into(),
        tree: Box::new(Expression::FnCall(FnCall {
            function,
            arguments,
        })),
    }))
}

fn parse_block<'a>(tokens: &'a Tokens, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
    if tokens.consume_expect(at, (Kind::Punctuation, "{")).is_err() {
        return None;
    };

    Some(parse_block_contents(tokens, at, (Kind::Punctuation, "}")))
}

fn parse_block_contents<'a>(
    tokens: &'a Tokens,
    at: &mut usize,
    end: (Kind, &str),
) -> Result<Ast<'a>, Error> {
    let (token, fragment) = tokens.peek(at);
    let start = token.location().start();
    traceln!("parse_block_contents, token = {:?}", fragment);

    let mut expressions = Vec::new();
    let mut result = None;

    while tokens.consume_expect(at, end).is_err() {
        // Only blocks can contain variable declarations, try them first
        let ast = match parse_var(tokens, at) {
            Some(Ok(ast)) => ast,
            Some(Err(e)) => return Err(e),
            None => parse_expression(tokens, at)?,
        };

        if let Err(e) = tokens.consume_expect(at, (Kind::Punctuation, ";")) {
            if tokens.consume_expect(at, end).is_ok() {
                result = Some(ast);
                break;
            }

            if tokens.peek_behind(at).1 != "}" {
                return Err(e);
            }
        };

        expressions.push(ast);
    }

    let end = tokens.peek_behind(at).0.location().end();
    Ok(Ast {
        location: (start..end).into(),
        tree: Box::new(Expression::Block(Block {
            expressions,
            result,
        })),
    })
}

fn parse_var<'a>(tokens: &'a Tokens, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
    let (token, fragment) = tokens.peek(at);
    traceln!("parse_var, token = {:?}", fragment);

    // var
    if token.kind() != Kind::Identifier || fragment != "var" {
        return None;
    }
    let start = token.location().start();
    tokens.consume(at);

    // _MAYBE_ parse_assignment_expr_right could be used here,
    // but I'd have to specify its parse_term_fn and that's not trivial.
    // Also, I'd have to verify that it has at least one =

    // <id>
    let Some(id) = parse_identifier(tokens, at) else {
        return Some(Err(Error::ExpectedIdentifier(tokens.peek(at).0.kind())));
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

    let end = tokens.peek_behind(at).0.location().end();
    Some(Ok(Ast {
        location: (start..end).into(),
        tree: Box::new(Expression::Var(Var { id, init })),
    }))
}

fn parse_factor<'a>(tokens: &'a Tokens, at: &mut usize) -> Result<Ast<'a>, Error> {
    let (_, fragment) = tokens.peek(at);
    traceln!("parse_factor, token = {:?}", fragment);

    if fragment == "(" {
        return parse_parenthesized(tokens, at);
    } else {
        for parse_fn in [
            parse_block,
            parse_literal,
            parse_fn_call,
            parse_conditional,
            parse_while,
        ] {
            if let Some(res) = parse_fn(tokens, at) {
                return res;
            }
        }
        if let Some(id) = parse_identifier(tokens, at) {
            return Ok(id);
        }
    };

    Err(Error::ExpectedTerm(tokens.peek(at).0.kind()))
}

fn parse_parenthesized<'a>(tokens: &'a Tokens, at: &mut usize) -> Result<Ast<'a>, Error> {
    let (_, fragment) = tokens.peek(at);
    traceln!("parse_parenthesized, token = {:?}", fragment);

    tokens.consume_expect(at, (Kind::Punctuation, "("))?;
    let res = parse_expression(tokens, at);
    tokens.consume_expect(at, (Kind::Punctuation, ")"))?;
    res
}

fn parse_binary_expr_left<'a>(
    tokens: &'a Tokens<'_>,
    at: &mut usize,
    ops: &[&str],
    parse_term_fn: impl Fn(&'a Tokens, &mut usize) -> Result<Ast<'a>, Error>,
) -> Result<Ast<'a>, Error> {
    let (_, fragment) = tokens.peek(at);
    traceln!("parse_binary_expr_left for {ops:?}, token = {fragment:?} at {at}");

    // Accumulate tree into left
    let mut left = parse_term_fn(tokens, at)?;
    let start = left.location.start();

    let result = loop {
        if !ops.contains(&tokens.peek(at).1) {
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
    let (_, fragment) = tokens.peek(at);
    traceln!("parse_assignment_expr_right (=), token = {fragment:?} at {at}",);

    let mut terms = Vec::new();
    terms.push(parse_term_fn(tokens, at)?);

    while tokens.peek(at).1 == "=" {
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
    let (_, fragment) = tokens.peek(at);
    traceln!("parse_unary_expr for {ops:?}, token = {fragment:?} at {at}");

    let mut op = Vec::new();
    while ops.contains(&tokens.peek(at).1) {
        op.push((tokens.peek(at).0.location().start(), parse_op(tokens, at)?));
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

impl<'a> Tokens<'a> {
    fn peek_behind(&self, at: &usize) -> (Token, &'a str) {
        self.peek(&(*at - 1))
    }

    fn peek_ahead(&self, at: &usize) -> (Token, &'a str) {
        self.peek(&(*at + 1))
    }

    fn peek(&self, at: &usize) -> (Token, &'a str) {
        match self.get(*at).cloned() {
            Some(token) => {
                let fragment = token.as_str(self.code);
                (token, fragment)
            }
            None => (
                Token::end(
                    self.last()
                        .map(|last| Location::from(last.location().end()..last.location().end()))
                        .unwrap_or_default(),
                ),
                "EOF",
            ),
        }
    }

    fn consume(&self, at: &mut usize) -> (Token, &'a str) {
        let (token, fragment) = self.peek(at);
        traceln!("Tokens::consume, at = {}, token = {:?}", *at, fragment);
        *at += 1;
        (token, fragment)
    }

    fn consume_expect(
        &self,
        at: &mut usize,
        expected: (Kind, &str),
    ) -> Result<(Token, &'a str), Error> {
        let (token, fragment) = self.peek(at);
        if (token.kind(), fragment) != expected {
            return Err(Error::ExpectedOneOf {
                of: vec![String::from(expected.1)],
                token: String::from(fragment),
            });
        }
        *at += 1;
        Ok((token, fragment))
    }
}

pub fn parse<'a>(tokens: &'a Tokens) -> Result<Ast<'a>, Error> {
    start_trace!("Parser");
    let mut at = 0;
    let root = parse_block_contents(tokens, &mut at, (Kind::End, "EOF"))?;
    end_trace!();

    if config::verbose() {
        dbg!(&root);
    }

    Ok(root)
}
