use crate::{
    Location, SymbolTable, Type,
    ast::{Ast, Expression, Int, Literal},
};
use std::{collections::HashMap, fmt::Display};

type Var = String;
type Label = String;

#[derive(Debug)]
pub enum Op {
    LoadBoolConst {
        value: bool,
        dest: Var,
    },
    LoadIntConst {
        value: Int,
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

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::LoadBoolConst { value, dest } => {
                write!(f, "LoadBoolConst({value}, {dest})")
            }
            Op::LoadIntConst { value, dest } => {
                write!(f, "LoadIntConst({value}, {dest})")
            }
            Op::Copy { source, dest } => {
                write!(f, "Copy({source}, {dest})")
            }
            Op::Call { fun, args, dest } => {
                write!(f, "Call({fun}, [")?;
                if let [a0, rest @ ..] = args.as_slice() {
                    write!(f, "{a0}")?;
                    for arg in rest {
                        write!(f, ", {arg}")?;
                    }
                }
                write!(f, "], {dest})")
            }
            Op::Jump { label } => {
                write!(f, "Jump({label})")
            }
            Op::CondJump {
                cond,
                then_label,
                else_label,
            } => {
                write!(f, "CondJump({cond}, {then_label}, {else_label})")
            }
            Op::Label(label) => {
                write!(f, "{label}")
            }
        }
    }
}

pub struct Instruction {
    location: Location,
    op: Op,
}

struct Generator<'a> {
    symtab: SymbolTable<'a, Var>,
    ins: Vec<Instruction>,
    var_types: HashMap<Var, Type>,
}

impl<'a> Generator<'a> {
    const UNIT: &'static str = "unit";

    pub fn new(root_types: &[(&'a str, Type)]) -> Self {
        let root_vars: Vec<(&str, Var)> = root_types
            .iter()
            .map(|(k, _)| (*k, Var::from(*k)))
            .collect();
        let symtab = SymbolTable::from(root_vars);

        Self {
            symtab,
            ins: Vec::new(),
            var_types: HashMap::from([(Self::UNIT.into(), Type::Unit)]),
        }
    }

    fn new_var(&mut self, ty: Type) -> Var {
        let var = format!("x{}", self.var_types.len());
        self.var_types.insert(var.clone(), ty);
        var
    }

    fn visit(&mut self, ast: &Ast<'a>) -> Var {
        let location = ast.location.clone();
        match ast.tree.as_ref() {
            Expression::Literal(literal) => match literal {
                Literal::Int(value) => {
                    let var = self.new_var(Type::Int);
                    self.ins.push(Instruction {
                        location,
                        op: Op::LoadIntConst {
                            value: *value,
                            dest: var.clone(),
                        },
                    });
                    var
                }
                Literal::Bool(_) => todo!(),
                Literal::Str(_) => todo!(),
            },
            Expression::Identifier(identifier) => todo!(),
            Expression::Conditional(conditional) => todo!(),
            Expression::FnCall(fn_call) => todo!(),
            Expression::Block(block) => todo!(),
            Expression::Var(var) => todo!(),
            Expression::BinaryOp(binary_op) => todo!(),
            Expression::UnaryOp(unary_op) => todo!(),
            Expression::While(_) => todo!(),
        }
    }
}

pub fn generate_ir<'a>(ast: &Ast<'a>, root_types: &[(&'a str, Type)]) -> Vec<Instruction> {
    todo!()
}
