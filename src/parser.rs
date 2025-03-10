mod tests;

use crate::{
    Location, Type,
    ast::{
        Ast, BinaryOp, Block, Conditional, Expression, FnCall, Function, Identifier, Int, Literal,
        Module, Operator, Parameter, UnaryOp, Var, While, macros::ast,
    },
    config,
    tokenizer::{Kind, Token, Tokens},
    trace::{end_trace, start_trace, traceln},
};
use std::{
    num::ParseIntError,
    str::{FromStr, ParseBoolError},
};
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
    #[error("Expected type, but encountered {0:?}")]
    ExpectedType(Kind),
    #[error("Can't parse type: {0}")]
    Type(String),
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
    #[error("Function {fun:?} definition is not followed by a block")]
    FunDefBlock { fun: String },
}

fn parse_op(tokens: &Tokens<'_>, at: &mut usize) -> Result<Operator, Error> {
    let (_, fragment) = tokens.peek(at);
    traceln!("parse_op, token = {fragment:?}");
    let op = match fragment.parse::<Operator>() {
        Ok(op) => op,
        Err(..) => {
            return Err(Error::ExpectedOneOf {
                of: [
                    "+", "-", "*", "/", "%", "==", "!=", "<=", "<", ">=", ">", "and", "or", "not",
                    "=",
                ]
                .into_iter()
                .map(String::from)
                .collect(),
                token: fragment.into(),
            });
        }
    };
    traceln!("consuming...");

    tokens.consume(at);
    Ok(op)
}

fn parse_literal<'a>(tokens: &Tokens<'a>, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
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
    Some(result.map(|lit| {
        ast! {
            token.location().clone() => Expression::Literal(lit)
        }
    }))
}

fn parse_identifier<'a>(tokens: &Tokens<'a>, at: &mut usize) -> Option<Ast<'a>> {
    let (token, fragment) = tokens.peek(at);
    traceln!("parse_identifier, token = {:?}", fragment);

    if token.kind() != Kind::Identifier {
        return None;
    }

    tokens.consume(at);
    Some(ast! {
        token.location().clone() => Expression::Identifier(Identifier { name: fragment })
    })
}

fn parse_conditional<'a>(tokens: &Tokens<'a>, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
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
    Some(Ok(ast! {
        (start..end).into() => Expression::Conditional(Conditional { condition, then_expr, else_expr, })
    }))
}

fn parse_while<'a>(tokens: &Tokens<'a>, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
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
    Some(Ok(ast! {
        (start..end).into() => Expression::While(While { condition, do_expr })
    }))
}

fn parse_break<'a>(tokens: &Tokens<'a>, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
    let (token, fragment) = tokens.peek(at);
    if token.kind() != Kind::Identifier || fragment != "break" {
        return None;
    }
    tokens.consume(at);
    Some(Ok(ast! {
        token.location().clone() => Expression::Break,
    }))
}

fn parse_continue<'a>(tokens: &Tokens<'a>, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
    let (token, fragment) = tokens.peek(at);
    if token.kind() != Kind::Identifier || fragment != "continue" {
        return None;
    }
    tokens.consume(at);
    Some(Ok(ast! {
        token.location().clone() => Expression::Continue,
    }))
}

fn parse_return<'a>(tokens: &Tokens<'a>, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
    let (token, fragment) = tokens.peek(at);
    if token.kind() != Kind::Identifier || fragment != "return" {
        return None;
    }
    tokens.consume(at);

    let expr = if tokens.peek(at).0.kind() != Kind::Punctuation {
        match parse_expression(tokens, at) {
            Ok(ast) => Some(ast),
            Err(e) => return Some(Err(e)),
        }
    } else {
        None
    };

    let end = expr
        .as_ref()
        .map(|ast| ast.location.end())
        .unwrap_or(token.location().end());
    Some(Ok(
        ast! {(token.location().start()..end).into() => Expression::Return(expr)},
    ))
}

fn parse_fn_call<'a>(tokens: &Tokens<'a>, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
    let ((t0, s0), (_, s1)) = (tokens.peek(at), tokens.peek_ahead(at));
    traceln!("parse_fn_call, token = {s0:?}");

    // Check for identifier followed by an opening paren
    let (Kind::Identifier, "(") = (t0.kind(), s1) else {
        return None;
    };
    let start = t0.location().start();
    let id = parse_identifier(tokens, at).map(|ast| *ast.tree);
    let Some(Expression::Identifier(function)) = id else {
        unreachable!("Token should have been be an identifier");
    };

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
    Some(Ok(ast! {
        (start..end).into() => Expression::FnCall(FnCall { function, arguments, })
    }))
}

fn parse_fun<'a>(tokens: &Tokens<'a>, at: &mut usize) -> Option<Result<Function<'a>, Error>> {
    let ((t0, s0), (t1, _)) = (tokens.peek(at), tokens.peek_ahead(at));
    traceln!("parse_fun, token = {s0:?}");

    // Check for "fun" followed by an identifier
    match (t0.kind(), s0, t1.kind()) {
        (Kind::Identifier, "fun", Kind::Identifier) => {}
        _ => return None,
    }

    // Consume "fun"
    tokens.consume(at);

    // Parse identifier
    let id = parse_identifier(tokens, at).map(|ast| *ast.tree);
    let Some(Expression::Identifier(identifier)) = id else {
        unreachable!("Token should have been be an identifier");
    };

    // Consume opening paren
    match tokens.consume_expect(at, (Kind::Punctuation, "(")) {
        Ok(_) => {}
        Err(e) => return Some(Err(e)),
    }

    // Parse parameters
    let mut parameters = Vec::new();
    while tokens.consume_expect(at, (Kind::Punctuation, ")")).is_err() {
        // Parse identifier
        let (token, _) = tokens.peek(at);
        let id = parse_identifier(tokens, at).map(|ast| *ast.tree);
        let Some(Expression::Identifier(identifier)) = id else {
            return Some(Err(Error::ExpectedIdentifier(token.kind())));
        };

        // Consume colon before parameter type
        match tokens.consume_expect(at, (Kind::Punctuation, ":")) {
            Ok(_) => {}
            Err(e) => return Some(Err(e)),
        }

        // Consume parameter type
        let ty = match tokens.consume(at) {
            (token, fragment) if token.kind() == Kind::Identifier => match Type::from_str(fragment)
            {
                Ok(ty) => ty,
                Err(s) => return Some(Err(Error::Type(s))),
            },
            (token, _) => return Some(Err(Error::ExpectedType(token.kind()))),
        };

        parameters.push(Parameter { identifier, ty });

        // Parameter ends in comma or closing paren
        match tokens.consume_expect(at, (Kind::Punctuation, ",")) {
            Ok(_) => {}
            Err(e) => match tokens.consume_expect(at, (Kind::Punctuation, ")")) {
                Ok(_) => break,
                Err(_) => return Some(Err(e)),
            },
        }
    }

    // Consume colon before return type
    match tokens.consume_expect(at, (Kind::Punctuation, ":")) {
        Ok(_) => {}
        Err(e) => return Some(Err(e)),
    }

    // Consume return type
    let returns = match tokens.consume(at) {
        (token, fragment) if token.kind() == Kind::Identifier => match Type::from_str(fragment) {
            Ok(ty) => ty,
            Err(s) => return Some(Err(Error::Type(s))),
        },
        (token, _) => return Some(Err(Error::ExpectedType(token.kind()))),
    };

    // Parse body
    let body = match parse_block(tokens, at) {
        Some(Ok(ast)) => ast,
        Some(Err(e)) => return Some(Err(e)),
        None => {
            return Some(Err(Error::FunDefBlock {
                fun: String::from(identifier.name),
            }));
        }
    };

    Some(Ok(Function {
        identifier,
        parameters,
        returns,
        body,
    }))
}

fn parse_block<'a>(tokens: &Tokens<'a>, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
    if tokens.consume_expect(at, (Kind::Punctuation, "{")).is_err() {
        return None;
    };

    Some(parse_block_contents(tokens, at, None))
}

fn parse_block_contents<'a>(
    tokens: &Tokens<'a>,
    at: &mut usize,
    mut module: Option<&mut Module<'a>>,
) -> Result<Ast<'a>, Error> {
    let (token, fragment) = tokens.peek(at);
    let start = token.location().start();
    traceln!("parse_block_contents, token = {:?}", fragment);

    let mut expressions = Vec::new();
    let mut result = None;

    let end = match module {
        Some(_) => (Kind::End, "EOF"),
        None => (Kind::Punctuation, "}"),
    };

    while tokens.consume_expect(at, end).is_err() {
        // Only modules can contain function definitions
        if let Some(module) = module.as_mut() {
            match parse_fun(tokens, at) {
                Some(Ok(fun)) => {
                    module.functions.push(fun);
                    continue;
                }
                Some(Err(e)) => return Err(e),
                None => {}
            }
        }

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
    let ast = ast! {
        (start..end).into() => Expression::Block(Block { expressions, result, })
    };
    Ok(ast)
}

fn parse_var<'a>(tokens: &Tokens<'a>, at: &mut usize) -> Option<Result<Ast<'a>, Error>> {
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
    let id = parse_identifier(tokens, at).map(|ast| *ast.tree);
    let Some(Expression::Identifier(id)) = id else {
        return Some(Err(Error::ExpectedIdentifier(tokens.peek(at).0.kind())));
    };

    // : <type>
    let typed = match tokens.consume_expect(at, (Kind::Punctuation, ":")) {
        Ok(_) => match tokens.consume(at) {
            (token, fragment) if token.kind() == Kind::Identifier => {
                match Type::from_str(fragment) {
                    Ok(ty) => Some(ty),
                    Err(s) => return Some(Err(Error::Type(s))),
                }
            }
            (token, _) => return Some(Err(Error::ExpectedType(token.kind()))),
        },
        Err(_) => None,
    };

    // Op::Assign
    match parse_op(tokens, at) {
        Ok(Operator::Assign) => {}
        Ok(op) => {
            return Some(Err(Error::ExpectedOneOf {
                of: vec!["=".to_owned()],
                token: op.to_string(),
            }));
        }
        Err(e) => return Some(Err(e)),
    };

    // <init>
    let init = match parse_expression(tokens, at) {
        Ok(ast) => ast,
        Err(e) => return Some(Err(e)),
    };

    let end = tokens.peek_behind(at).0.location().end();
    Some(Ok(ast! {
        (start..end).into() => Expression::Var(Var { id, typed, init })
    }))
}

fn parse_factor<'a>(tokens: &Tokens<'a>, at: &mut usize) -> Result<Ast<'a>, Error> {
    let (_, fragment) = tokens.peek(at);
    traceln!("parse_factor, token = {:?}", fragment);

    if fragment == "(" {
        return parse_parenthesized(tokens, at);
    } else {
        for parse_fn in [
            parse_block,
            parse_literal,
            parse_conditional,
            parse_while,
            parse_break,
            parse_continue,
            parse_return,
            parse_fn_call,
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

fn parse_parenthesized<'a>(tokens: &Tokens<'a>, at: &mut usize) -> Result<Ast<'a>, Error> {
    let (_, fragment) = tokens.peek(at);
    traceln!("parse_parenthesized, token = {:?}", fragment);

    tokens.consume_expect(at, (Kind::Punctuation, "("))?;
    let res = parse_expression(tokens, at);
    tokens.consume_expect(at, (Kind::Punctuation, ")"))?;
    res
}

fn parse_binary_expr_left<'a, 'b>(
    tokens: &'b Tokens<'a>,
    at: &mut usize,
    ops: &[&str],
    parse_term_fn: impl Fn(&'b Tokens<'a>, &mut usize) -> Result<Ast<'a>, Error>,
) -> Result<Ast<'a>, Error> {
    let (_, fragment) = tokens.peek(at);
    traceln!("parse_binary_expr_left for {ops:?}, token = {fragment:?} at {at}");

    // Accumulate tree into left
    let mut left = parse_term_fn(tokens, at)?;
    let start = left.location.start();

    loop {
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

        left = ast! {
            (start..right.location.end()).into() => Expression::BinaryOp(BinaryOp { left, op, right })
        };
    }
}

fn parse_assignment_expr_right<'a, 'b>(
    tokens: &'b Tokens<'a>,
    at: &mut usize,
    parse_term_fn: impl Fn(&'b Tokens<'a>, &mut usize) -> Result<Ast<'a>, Error>,
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
        right = ast! {
            (ast.location.start()..right.location.end()).into() => Expression::BinaryOp(BinaryOp { left: ast, op: Operator::Assign, right, })
        };
    }

    Ok(right)
}

fn parse_unary_expr<'a, 'b>(
    tokens: &'b Tokens<'a>,
    at: &mut usize,
    ops: &[&str],
    parse_term_fn: impl Fn(&'b Tokens<'a>, &mut usize) -> Result<Ast<'a>, Error>,
) -> Result<Ast<'a>, Error> {
    let (_, fragment) = tokens.peek(at);
    traceln!("parse_unary_expr for {ops:?}, token = {fragment:?} at {at}");

    let mut op = Vec::new();
    while ops.contains(&tokens.peek(at).1) {
        op.push((tokens.peek(at).0.location().start(), parse_op(tokens, at)?));
    }

    let mut ast = parse_term_fn(tokens, at)?;

    while let Some((sign_start, op)) = op.pop() {
        ast = ast! {
            (sign_start..ast.location.end()).into() =>Expression::UnaryOp(UnaryOp { op, right: ast })
        };
    }

    Ok(ast)
}

fn parse_expression<'a>(tokens: &Tokens<'a>, at: &mut usize) -> Result<Ast<'a>, Error> {
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

impl<'a, 'b> Tokens<'a> {
    fn peek_behind(&'b self, at: &usize) -> (Token, &'a str) {
        self.peek(&(*at - 1))
    }

    fn peek_ahead(&'b self, at: &usize) -> (Token, &'a str) {
        self.peek(&(*at + 1))
    }

    fn peek(&'b self, at: &usize) -> (Token, &'a str) {
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

    fn consume(&'b self, at: &mut usize) -> (Token, &'a str) {
        let (token, fragment) = self.peek(at);
        traceln!("Tokens::consume, at = {}, token = {:?}", *at, fragment);
        *at += 1;
        (token, fragment)
    }

    fn consume_expect(
        &'b self,
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

pub fn parse<'a>(tokens: &Tokens<'a>) -> Result<Module<'a>, Error> {
    start_trace!("Parser");
    let mut at = 0;

    let mut module = Module::default();
    module.main = Some(parse_block_contents(tokens, &mut at, Some(&mut module))?);
    end_trace!();

    if config::verbose() {
        dbg!(&module);
    }

    Ok(module)
}
