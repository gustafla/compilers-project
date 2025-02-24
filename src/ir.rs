use std::{borrow::Borrow, fmt::Display};

use crate::{Type, ast::Ast};

macro_rules! string_type {
    ($name: ident) => {
        #[derive(Debug, PartialEq, Eq, Hash)]
        pub struct $name(Box<str>);
        impl Borrow<str> for $name {
            fn borrow(&self) -> &str {
                self.0.as_ref()
            }
        }
        impl From<&str> for $name {
            fn from(value: &str) -> Self {
                Self(String::from(value).into_boxed_str())
            }
        }
        impl Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", Borrow::<str>::borrow(self))
            }
        }
    };
}

string_type!(Var);
string_type!(Label);

#[derive(Debug)]
pub enum Instruction {
    LoadBoolConst {
        value: bool,
        dest: Var,
    },
    LoadIntConst {
        value: bool,
        dest: Var,
    },
    Copy {
        source: Var,
        dest: Var,
    },
    Call {
        fun: Var,
        args: Vec<Var>,
        dest: Var,
    },
    Jump {
        label: Label,
    },
    CondJump {
        cond: Var,
        then_label: Label,
        else_label: Label,
    },
    Label(Label),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::LoadBoolConst { value, dest } => {
                write!(f, "LoadBoolConst({value}, {dest})")
            }
            Instruction::LoadIntConst { value, dest } => {
                write!(f, "LoadIntConst({value}, {dest})")
            }
            Instruction::Copy { source, dest } => {
                write!(f, "Copy({source}, {dest})")
            }
            Instruction::Call { fun, args, dest } => {
                write!(f, "Call({fun}, [")?;
                if let [a0, rest @ ..] = args.as_slice() {
                    write!(f, "{a0}")?;
                    for arg in rest {
                        write!(f, ", {arg}")?;
                    }
                }
                write!(f, "], {dest})")
            }
            Instruction::Jump { label } => {
                write!(f, "Jump({label})")
            }
            Instruction::CondJump {
                cond,
                then_label,
                else_label,
            } => {
                write!(f, "CondJump({cond}, {then_label}, {else_label})")
            }
            Instruction::Label(label) => {
                write!(f, "{label}")
            }
        }
    }
}

pub fn generate_ir<'a>(ast: &Ast<'a>, root_types: &[(&'a str, Type)]) -> Vec<Instruction> {
    todo!()
}
