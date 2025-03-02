use crate::{
    Location, SymbolTable, Type,
    ast::{Ast, Expression, Int, Literal, Operator, op::Ary},
    symtab,
};
use std::{collections::HashMap, fmt::Display};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    UnresolvedIdentifier(#[from] symtab::Error),
}

pub type Var = String;
pub type Label = String;

#[derive(Debug)]
pub enum Op {
    Label(Label),
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
        fun: String,
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
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Indent, but not labels and nops
        match self {
            Op::Label(_) => {}
            _ => write!(f, "    ")?,
        };
        match self {
            Op::Label(label) => {
                write!(f, "{label}:")
            }
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
        }
    }
}

pub struct Instruction {
    pub location: Location,
    pub op: Op,
}

struct Generator<'a> {
    // Counters for vars and labels
    var: u32,
    label: u32,
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
            var: 0,
            label: 0,
            symtab,
            ins: vec![Instruction {
                location: Location::default(),
                op: Op::Label(Label::from("start")),
            }],
            var_types: HashMap::from([(Self::UNIT.into(), Type::Unit)]),
        }
    }

    pub fn visit(&mut self, ast: &Ast<'a>) -> Result<Var, Error> {
        let location = &ast.location;
        let ty = ast.ty.as_ref().expect("AST should have been type checked");
        match ast.tree.as_ref() {
            Expression::Literal(literal) => match literal {
                Literal::Int(value) => {
                    let var = self.new_var(&Type::Int);
                    self.emit_load_int_const(location, *value, &var);
                    Ok(var)
                }
                Literal::Bool(value) => {
                    let var = self.new_var(&Type::Bool);
                    self.emit_load_bool_const(location, *value, &var);
                    Ok(var)
                }
                Literal::Str(_) => unimplemented!("String literals are not supported"),
            },
            Expression::Identifier(identifier) => {
                Ok(self.symtab.resolve(identifier.name)?.get().clone())
            }
            Expression::Conditional(conditional) => {
                let then_label = self.new_label();
                let else_label = self.new_label();

                // Emit the main CondJump
                let var_cond = self.visit(&conditional.condition)?;
                self.emit_cond_jump(location, &var_cond, &then_label, &else_label);

                // Emit then-branch
                self.emit_label(location, &then_label);
                let var_result = self.visit(&conditional.then_expr)?;

                if let Some(else_expr) = &conditional.else_expr {
                    // Copy result to var in both branches of the IR
                    let var_output = self.new_var(ty);
                    self.emit_copy(location, &var_result, &var_output);

                    // Emit jump that ends the then-branch
                    let end_label = self.new_label();
                    self.emit_jump(location, &end_label);

                    // Emit label that starts the else branch
                    self.emit_label(location, &else_label);
                    let var_result = self.visit(else_expr)?;

                    // Copy result to var in both branches of the IR
                    self.emit_copy(location, &var_result, &var_output);

                    // Emit end label for the
                    self.emit_label(location, &end_label);

                    Ok(var_output)
                } else {
                    // Use else-label as an end label as nothing gets emitted after it
                    self.emit_label(location, &else_label);
                    Ok(Self::UNIT.into())
                }
            }
            Expression::FnCall(fn_call) => {
                let fun = self.symtab.resolve(fn_call.function.name)?.get().clone();
                let mut args = Vec::new();
                for arg in &fn_call.arguments {
                    args.push(self.visit(arg)?);
                }
                let dest = self.new_var(ty);
                self.emit_call(location, &fun, &args, &dest);
                Ok(dest)
            }
            Expression::Block(block) => {
                self.symtab.push();
                for expr in &block.expressions {
                    self.visit(expr)?;
                }
                let result = if let Some(result) = &block.result {
                    self.visit(result)?
                } else {
                    Self::UNIT.into()
                };
                self.symtab.pop();
                Ok(result)
            }
            Expression::Var(var) => {
                let init = self.visit(&var.init)?;
                let ir_var = self.new_var(var.init.ty.as_ref().unwrap());
                self.symtab.insert(var.id.name, ir_var.clone());
                self.emit_copy(location, &init, &ir_var);
                Ok(Self::UNIT.into())
            }
            Expression::BinaryOp(binary_op) => {
                if binary_op.op == Operator::Assign {
                    // Resolve identifier
                    let key = match binary_op.left.tree.as_ref() {
                        Expression::Identifier(identifier) => identifier.name,
                        _ => unreachable!("= requires identifier on the lhs"),
                    };
                    let var_left = self.symtab.resolve(key)?.get().clone();
                    let var_right = self.visit(&binary_op.right)?;
                    self.emit_copy(location, &var_right, &var_left);
                    return Ok(var_right);
                }

                let var_left = self.visit(&binary_op.left)?;
                let var_result = self.new_var(ty);

                match binary_op.op {
                    // Special cases for short-circuiting and and or
                    op @ (Operator::And | Operator::Or) => {
                        let yup = self.new_label();
                        let nope = self.new_label();
                        let end = self.new_label();

                        match op {
                            Operator::And => {
                                // Negate left
                                let not_fun = self
                                    .symtab
                                    .resolve(Operator::Not.function_name(Ary::Unary))?
                                    .get()
                                    .clone();
                                let not_left = self.new_var(&Type::Bool);
                                self.emit_call(location, not_fun, &[var_left.clone()], &not_left);

                                // Create conditional
                                self.emit_cond_jump(location, not_left, &yup, &nope);

                                // If left is false, short-circuit to false
                                self.emit_label(location, &yup);
                                self.emit_load_bool_const(location, false, &var_result);
                                self.emit_jump(location, &end);
                            }
                            Operator::Or => {
                                // Create conditional
                                self.emit_cond_jump(location, var_left, &yup, &nope);

                                // If left is true, short-circuit to true
                                self.emit_label(location, &yup);
                                self.emit_load_bool_const(location, true, &var_result);
                                self.emit_jump(location, &end);
                            }
                            _ => unreachable!(),
                        }

                        // Else check rhs
                        self.emit_label(location, &nope);
                        let var_right = self.visit(&binary_op.right)?;
                        self.emit_copy(location, &var_right, &var_result);

                        // End
                        self.emit_label(location, &end);
                    }
                    // Otherwise emit Call
                    op => {
                        let fun = op.function_name(Ary::Binary);
                        let var_right = self.visit(&binary_op.right)?;
                        self.emit_call(location, fun, &[var_left, var_right], &var_result);
                    }
                }
                Ok(var_result)
            }
            Expression::UnaryOp(unary_op) => {
                let fun = self
                    .symtab
                    .resolve(unary_op.op.function_name(Ary::Unary))?
                    .get()
                    .clone();
                let var_right = self.visit(&unary_op.right)?;
                let var_result = self.new_var(ty);
                self.emit_call(location, &fun, &[var_right], &var_result);
                Ok(var_result)
            }
            Expression::While(while_loop) => {
                let start = self.new_label();
                let body = self.new_label();
                let out = self.new_label();

                self.emit_label(location, &start);
                let cond = self.visit(&while_loop.condition)?;
                self.emit_cond_jump(location, &cond, &body, &out);
                self.emit_label(location, &body);
                self.visit(&while_loop.do_expr)?;
                self.emit_jump(location, &start);
                self.emit_label(location, &out);
                Ok(Self::UNIT.into())
            }
        }
    }

    pub fn type_of(&self, key: &str) -> &Type {
        match self.var_types.get(key) {
            Some(ty) => ty,
            None => panic!("Var {key} has no type info. This is a bug."),
        }
    }

    pub fn new_var(&mut self, ty: &Type) -> Var {
        let var = format!("x{}", self.var);
        self.var += 1;
        self.var_types.insert(var.clone(), ty.clone());
        var
    }

    pub fn new_label(&mut self) -> Label {
        let label = format!("L{}", self.label);
        self.label += 1;
        label
    }

    pub fn emit_label(&mut self, location: &Location, label: impl Into<Label>) {
        self.ins.push(Instruction {
            location: location.clone(),
            op: Op::Label(label.into()),
        });
    }

    pub fn emit_load_int_const(&mut self, location: &Location, value: Int, dest: impl Into<Var>) {
        self.ins.push(Instruction {
            location: location.clone(),
            op: Op::LoadIntConst {
                value,
                dest: dest.into(),
            },
        });
    }

    pub fn emit_load_bool_const(&mut self, location: &Location, value: bool, dest: impl Into<Var>) {
        self.ins.push(Instruction {
            location: location.clone(),
            op: Op::LoadBoolConst {
                value,
                dest: dest.into(),
            },
        });
    }

    pub fn emit_copy(&mut self, location: &Location, source: impl Into<Var>, dest: impl Into<Var>) {
        self.ins.push(Instruction {
            location: location.clone(),
            op: Op::Copy {
                source: source.into(),
                dest: dest.into(),
            },
        });
    }

    pub fn emit_call(
        &mut self,
        location: &Location,
        fun: impl Into<String>,
        args: impl IntoIterator<Item = impl Into<Var>>,
        dest: impl Into<Var>,
    ) {
        self.ins.push(Instruction {
            location: location.clone(),
            op: Op::Call {
                fun: fun.into(),
                args: args.into_iter().map(Into::into).collect(),
                dest: dest.into(),
            },
        });
    }

    pub fn emit_jump(&mut self, location: &Location, label: impl Into<Label>) {
        self.ins.push(Instruction {
            location: location.clone(),
            op: Op::Jump {
                label: label.into(),
            },
        });
    }

    pub fn emit_cond_jump(
        &mut self,
        location: &Location,
        cond: impl Into<Var>,
        then_label: impl Into<Label>,
        else_label: impl Into<Label>,
    ) {
        self.ins.push(Instruction {
            location: location.clone(),
            op: Op::CondJump {
                cond: cond.into(),
                then_label: then_label.into(),
                else_label: else_label.into(),
            },
        });
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
            "print_int",
            &[var_final_result],
            Generator::UNIT,
        ),
        Type::Bool => generator.emit_call(
            &Location::default(),
            "print_bool",
            &[var_final_result],
            Generator::UNIT,
        ),
        Type::Unit => {}
        Type::Fun { .. } => unreachable!("Function values are not supported"),
    }
    Ok(generator.into())
}
