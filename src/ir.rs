use crate::{
    Location, SymbolTable, Type,
    ast::{Ast, Expression, Int, Literal, op::Ary},
    symtab,
};
use std::{collections::HashMap, fmt::Display};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    UnresolvedIdentifier(#[from] symtab::Error),
}

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
    pub location: Location,
    pub op: Op,
}

struct Generator<'a> {
    // Maps AST/source identifiers to IR variables
    symtab: SymbolTable<'a, Var>,
    // Maps IR variables to types
    var_types: HashMap<Var, Type>,
    // Output IR instructions
    ins: Vec<Instruction>,
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

    pub fn visit(&mut self, ast: &Ast<'a>) -> Result<Var, Error> {
        let location = &ast.location;
        let ty = ast.ty.as_ref().expect("AST should have been type checked");
        match ast.tree.as_ref() {
            Expression::Literal(literal) => {
                let var = self.new_var(&Type::Int);
                match literal {
                    Literal::Int(value) => {
                        self.emit_load_int_const(location, *value, var.clone());
                    }
                    Literal::Bool(value) => {
                        self.emit_load_bool_const(location, *value, var.clone());
                    }
                    Literal::Str(_) => unimplemented!("String literals are not supported"),
                }
                Ok(var)
            }
            Expression::Identifier(identifier) => {
                Ok(self.symtab.resolve(identifier.name)?.get().clone())
            }
            Expression::Conditional(conditional) => todo!(),
            Expression::FnCall(fn_call) => todo!(),
            Expression::Block(block) => todo!(),
            Expression::Var(var) => todo!(),
            Expression::BinaryOp(binary_op) => {
                let var_op = self
                    .symtab
                    .resolve(binary_op.op.function_name(Ary::Binary))?
                    .get()
                    .clone();
                // TODO: Special cases for =, and and or
                let var_left = self.visit(&binary_op.left)?;
                let var_right = self.visit(&binary_op.right)?;
                let var_result = self.new_var(ty);
                self.emit_call(
                    location,
                    var_op,
                    vec![var_left, var_right],
                    var_result.clone(),
                );
                Ok(var_result)
            }
            Expression::UnaryOp(unary_op) => todo!(),
            Expression::While(_) => todo!(),
        }
    }

    pub fn type_of(&self, key: &str) -> &Type {
        match self.var_types.get(key) {
            Some(ty) => ty,
            None => panic!("Var {key} has no type info. This is a bug."),
        }
    }

    pub fn new_var(&mut self, ty: &Type) -> Var {
        let var = format!("x{}", self.var_types.len());
        self.var_types.insert(var.clone(), ty.clone());
        var
    }

    pub fn emit_load_int_const(&mut self, location: &Location, value: Int, dest: Var) {
        self.ins.push(Instruction {
            location: location.clone(),
            op: Op::LoadIntConst { value, dest },
        })
    }

    pub fn emit_load_bool_const(&mut self, location: &Location, value: bool, dest: Var) {
        self.ins.push(Instruction {
            location: location.clone(),
            op: Op::LoadBoolConst { value, dest },
        })
    }

    pub fn emit_copy(&mut self, location: &Location, source: Var, dest: Var) {
        self.ins.push(Instruction {
            location: location.clone(),
            op: Op::Copy { source, dest },
        })
    }

    pub fn emit_call(&mut self, location: &Location, fun: Var, args: Vec<Var>, dest: Var) {
        self.ins.push(Instruction {
            location: location.clone(),
            op: Op::Call { fun, args, dest },
        })
    }

    pub fn emit_jump(&mut self, location: &Location, label: Var) {
        self.ins.push(Instruction {
            location: location.clone(),
            op: Op::Jump { label },
        })
    }

    pub fn emit_cond_jump(
        &mut self,
        location: &Location,
        cond: Var,
        then_label: Var,
        else_label: Var,
    ) {
        self.ins.push(Instruction {
            location: location.clone(),
            op: Op::CondJump {
                cond,
                then_label,
                else_label,
            },
        })
    }
    pub fn emit_label(&mut self, location: &Location, label: Var) {
        self.ins.push(Instruction {
            location: location.clone(),
            op: Op::Label(label),
        })
    }
}

impl From<Generator<'_>> for Vec<Instruction> {
    fn from(val: Generator<'_>) -> Self {
        val.ins
    }
}

pub fn generate_ir<'a>(
    ast: &Ast<'a>,
    root_types: &[(&'a str, Type)],
) -> Result<Vec<Instruction>, Error> {
    let mut generator = Generator::new(root_types);
    let var_final_result = generator.visit(ast)?;
    match generator.type_of(&var_final_result) {
        Type::Int => generator.emit_call(
            &Location::default(),
            "print_int".into(),
            vec![var_final_result],
            Generator::UNIT.into(),
        ),
        Type::Bool => generator.emit_call(
            &Location::default(),
            "print_bool".into(),
            vec![var_final_result],
            Generator::UNIT.into(),
        ),
        Type::Unit => {}
        Type::Fun { .. } => unreachable!("Function values are not supported"),
    }
    Ok(generator.into())
}
