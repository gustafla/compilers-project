use std::{fmt::Display, str::FromStr};

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

impl FromStr for Op {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let op = match s {
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
            _ => {
                return Err(());
            }
        };
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
