use crate::{SymbolTable, Type, ast::Ast};
use std::{borrow::Cow, fmt::Display};

type Var<'a> = Cow<'a, str>;
type Label<'a> = Cow<'a, str>;

#[derive(Debug)]
pub enum Instruction<'a> {
    LoadBoolConst {
        value: bool,
        dest: Var<'a>,
    },
    LoadIntConst {
        value: bool,
        dest: Var<'a>,
    },
    Copy {
        source: Var<'a>,
        dest: Var<'a>,
    },
    Call {
        fun: Var<'a>,
        args: Vec<Var<'a>>,
        dest: Var<'a>,
    },
    Jump {
        label: Label<'a>,
    },
    CondJump {
        cond: Var<'a>,
        then_label: Label<'a>,
        else_label: Label<'a>,
    },
    Label(Label<'a>),
}

impl Display for Instruction<'_> {
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

pub fn generate_ir<'a>(ast: &Ast<'a>, root_types: &[(&'a str, Type)]) -> Vec<Instruction<'a>> {
    let root_vars: Vec<(&str, Var)> = root_types
        .iter()
        .map(|(k, _)| (*k, Var::from(*k)))
        .collect();
    let mut symtab = SymbolTable::from(root_vars);
    todo!()
}
