use super::Error;
use crate::{tokenizer::Tokens, trace::traceln};
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Leq,
    Lt,
    Geq,
    Gt,
    And,
    Or,
    Not,
    Assign,
}

impl Op {
    pub fn parse(tokens: &Tokens, at: &mut usize) -> Result<Self, Error> {
        let code = tokens.code();
        let token = tokens.peek(*at);
        traceln!("Op::parse, token = {token:?}");
        let op = match token.as_str(code) {
            "+" => Op::Add,
            "-" => Op::Sub,
            "*" => Op::Mul,
            "/" => Op::Div,
            "%" => Op::Rem,
            "==" => Op::Eq,
            "!=" => Op::Ne,
            "<=" => Op::Leq,
            "<" => Op::Lt,
            ">=" => Op::Geq,
            ">" => Op::Gt,
            "and" => Op::And,
            "or" => Op::Or,
            "not" => Op::Not,
            "=" => Op::Assign,
            str => {
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
                    token: str.into(),
                })
            }
        };
        traceln!("consuming...");

        tokens.consume(at);
        Ok(op)
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Add => "+",
                Op::Sub => "-",
                Op::Mul => "*",
                Op::Div => "/",
                Op::Rem => "%",
                Op::Eq => "==",
                Op::Ne => "!=",
                Op::Leq => "<=",
                Op::Lt => "<",
                Op::Geq => ">=",
                Op::Gt => ">",
                Op::And => "and",
                Op::Or => "or",
                Op::Not => "not",
                Op::Assign => "=",
            }
        )
    }
}
