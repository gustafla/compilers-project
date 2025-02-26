use std::{fmt::Display, str::FromStr};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
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

impl FromStr for Operator {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let op = match s {
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div,
            "%" => Operator::Rem,
            "==" => Operator::Eq,
            "!=" => Operator::Ne,
            "<=" => Operator::Leq,
            "<" => Operator::Lt,
            ">=" => Operator::Geq,
            ">" => Operator::Gt,
            "and" => Operator::And,
            "or" => Operator::Or,
            "not" => Operator::Not,
            "=" => Operator::Assign,
            _ => {
                return Err(());
            }
        };
        Ok(op)
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl Operator {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::Rem => "%",
            Operator::Eq => "==",
            Operator::Ne => "!=",
            Operator::Leq => "<=",
            Operator::Lt => "<",
            Operator::Geq => ">=",
            Operator::Gt => ">",
            Operator::And => "and",
            Operator::Or => "or",
            Operator::Not => "not",
            Operator::Assign => "=",
        }
    }

    pub const fn function_name(&self, ary: Ary) -> &'static str {
        match ary {
            Ary::Unary => match self {
                Operator::Sub => "unary_sub",
                Operator::Not => "unary_not",
                _ => panic!("Requested invalid unary"),
            },
            Ary::Binary => match self {
                Operator::Add => "binary_add",
                Operator::Sub => "binary_sub",
                Operator::Mul => "binary_mul",
                Operator::Div => "binary_div",
                Operator::Rem => "binary_rem",
                Operator::Eq => "binary_eq",
                Operator::Ne => "binary_ne",
                Operator::Leq => "binary_leq",
                Operator::Lt => "binary_lt",
                Operator::Geq => "binary_geq",
                Operator::Gt => "binary_gt",
                Operator::And => "binary_and",
                Operator::Or => "binary_or",
                Operator::Assign => "binary_assign",
                Operator::Not => panic!("Requested invalid unary"),
            },
        }
    }
}

pub enum Ary {
    Binary,
    Unary,
}
