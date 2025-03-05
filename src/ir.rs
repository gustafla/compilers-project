use crate::{
    Location, SymbolTable, Type,
    ast::{Ast, Expression, Int, Literal, Module, Operator, op::Ary},
    symtab,
};
use std::{collections::HashMap, fmt::Display, mem};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    UnresolvedIdentifier(#[from] symtab::Error),
    #[error("`break` used outside of loop")]
    BreakOutOfLoop,
    #[error("`continue` used outside of loop")]
    ContinueOutOfLoop,
}

pub type Var = u32;
pub type Label = u32;

#[derive(Clone)]
struct LoopLabels {
    pub start: Label,
    pub end: Label,
}

#[derive(Debug)]
pub enum Op<'a> {
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
        fun: &'a str,
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

impl Display for Op<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Indent, but not labels and nops
        match self {
            Op::Label(_) => {}
            _ => write!(f, "    ")?,
        };
        match self {
            Op::Label(label) => {
                write!(f, "L{label}:")
            }
            Op::LoadBoolConst { value, dest } => {
                write!(f, "LoadBoolConst({value}, x{dest})")
            }
            Op::LoadIntConst { value, dest } => {
                write!(f, "LoadIntConst({value}, x{dest})")
            }
            Op::Copy { source, dest } => {
                write!(f, "Copy(x{source}, x{dest})")
            }
            Op::Call { fun, args, dest } => {
                write!(f, "Call({fun}, [")?;
                if let [a0, rest @ ..] = args.as_slice() {
                    write!(f, "x{a0}")?;
                    for arg in rest {
                        write!(f, ", x{arg}")?;
                    }
                }
                write!(f, "], x{dest})")
            }
            Op::Jump { label } => {
                write!(f, "Jump(L{label})")
            }
            Op::CondJump {
                cond,
                then_label,
                else_label,
            } => {
                write!(f, "CondJump(x{cond}, L{then_label}, L{else_label})")
            }
        }
    }
}

pub struct Instruction<'a> {
    pub location: Location,
    pub op: Op<'a>,
}

struct Generator<'a> {
    // Counters for vars and labels
    var: u32,
    label: u32,
    // Maps AST identifiers to IR variables
    symtab: SymbolTable<'a, Var>,
    // Tracks AST function identifiers
    funtab: SymbolTable<'a, &'a str>,
    // Maps IR variables to types
    var_types: HashMap<Var, Type>,
    // Output IR instructions
    ins: Vec<Instruction<'a>>,
}

impl<'a> Generator<'a> {
    const UNIT: Var = 0;

    pub fn new(root_types: &[(&'a str, Type)]) -> Self {
        let root_funs: Vec<(&str, &str)> = root_types
            .iter()
            .filter(|(_, ty)| matches!(ty, Type::Fun { .. }))
            .map(|(k, _)| (*k, *k))
            .collect();
        let funtab = SymbolTable::from(root_funs);

        let mut var_types = HashMap::from([(Self::UNIT, Type::Unit)]);
        let mut symtab = SymbolTable::new();
        let mut var = 1; // Var(0) is UNIT
        for (id, ty) in root_types
            .iter()
            .filter(|(_, ty)| !matches!(ty, Type::Fun { .. }))
        {
            symtab.insert(id, var);
            var_types.insert(var, ty.clone());
            var += 1;
        }

        Self {
            var,
            label: 0,
            symtab,
            funtab,
            var_types,
            ins: Vec::new(),
        }
    }

    pub fn visit(&mut self, ast: &Ast<'a>, in_loop: Option<&LoopLabels>) -> Result<Var, Error> {
        let location = &ast.location;
        let ty = ast.ty.as_ref().expect("AST should have been type checked");
        match ast.tree.as_ref() {
            Expression::Literal(literal) => match literal {
                Literal::Int(value) => {
                    let var = self.new_var(&Type::Int);
                    self.emit_load_int_const(location, *value, var);
                    Ok(var)
                }
                Literal::Bool(value) => {
                    let var = self.new_var(&Type::Bool);
                    self.emit_load_bool_const(location, *value, var);
                    Ok(var)
                }
                Literal::Str(_) => unimplemented!("String literals are not supported"),
            },
            Expression::Identifier(identifier) => Ok(*self.symtab.resolve(identifier.name)?.get()),
            Expression::Conditional(conditional) => {
                let then_label = self.new_label();
                let else_label = self.new_label();

                // Emit the main CondJump
                let var_cond = self.visit(&conditional.condition, in_loop)?;
                self.emit_cond_jump(location, var_cond, then_label, else_label);

                // Emit then-branch
                self.emit_label(location, then_label);
                let var_result = self.visit(&conditional.then_expr, in_loop)?;

                if let Some(else_expr) = &conditional.else_expr {
                    // Copy result to var in both branches of the IR
                    let var_output = self.new_var(ty);
                    self.emit_copy(location, var_result, var_output);

                    // Emit jump that ends the then-branch
                    let end_label = self.new_label();
                    self.emit_jump(location, end_label);

                    // Emit label that starts the else branch
                    self.emit_label(location, else_label);
                    let var_result = self.visit(else_expr, in_loop)?;

                    // Copy result to var in both branches of the IR
                    self.emit_copy(location, var_result, var_output);

                    // Emit end label for the
                    self.emit_label(location, end_label);

                    Ok(var_output)
                } else {
                    // Use else-label as an end label as nothing gets emitted after it
                    self.emit_label(location, else_label);
                    Ok(Self::UNIT)
                }
            }
            Expression::FnCall(fn_call) => {
                let fun = *self.funtab.resolve(fn_call.function.name)?.get();
                let mut args = Vec::new();
                for arg in &fn_call.arguments {
                    args.push(self.visit(arg, in_loop)?);
                }
                let dest = self.new_var(ty);
                self.emit_call(location, fun, args, dest);
                Ok(dest)
            }
            Expression::Block(block) => {
                self.symtab.push();
                self.funtab.push();
                for expr in &block.expressions {
                    self.visit(expr, in_loop)?;
                }
                let result = if let Some(result) = &block.result {
                    self.visit(result, in_loop)?
                } else {
                    Self::UNIT
                };
                self.symtab.pop();
                self.funtab.pop();
                Ok(result)
            }
            Expression::Var(var) => {
                let init = self.visit(&var.init, in_loop)?;
                let ir_var = self.new_var(var.init.ty.as_ref().unwrap());
                self.symtab.insert(var.id.name, ir_var);
                self.emit_copy(location, init, ir_var);
                Ok(Self::UNIT)
            }
            Expression::BinaryOp(binary_op) => {
                if binary_op.op == Operator::Assign {
                    // Resolve identifier
                    let key = match binary_op.left.tree.as_ref() {
                        Expression::Identifier(identifier) => identifier.name,
                        _ => unreachable!("= requires identifier on the lhs"),
                    };
                    let var_left = *self.symtab.resolve(key)?.get();
                    let var_right = self.visit(&binary_op.right, in_loop)?;
                    self.emit_copy(location, var_right, var_left);
                    return Ok(var_right);
                }

                let var_left = self.visit(&binary_op.left, in_loop)?;
                let var_result = self.new_var(ty);

                match binary_op.op {
                    // Special cases for short-circuiting and and or
                    op @ (Operator::And | Operator::Or) => {
                        let short_circuit_label = self.new_label();
                        let check_rhs_label = self.new_label();
                        let end = self.new_label();

                        let (short_circuit_to, then_label, else_label) = match op {
                            Operator::And => (false, check_rhs_label, short_circuit_label),
                            Operator::Or => (true, short_circuit_label, check_rhs_label),
                            _ => unreachable!(),
                        };

                        // Create conditional
                        self.emit_cond_jump(location, var_left, then_label, else_label);

                        // Short-circuit branch
                        self.emit_label(location, short_circuit_label);
                        self.emit_load_bool_const(location, short_circuit_to, var_result);
                        self.emit_jump(location, end);

                        // Check rhs branch
                        self.emit_label(location, check_rhs_label);
                        let var_right = self.visit(&binary_op.right, in_loop)?;
                        self.emit_copy(location, var_right, var_result);

                        // End
                        self.emit_label(location, end);
                    }
                    // Otherwise emit Call
                    op => {
                        let fun = op.function_name(Ary::Binary);
                        let var_right = self.visit(&binary_op.right, in_loop)?;
                        self.emit_call(location, fun, [var_left, var_right], var_result);
                    }
                }
                Ok(var_result)
            }
            Expression::UnaryOp(unary_op) => {
                let fun = *self
                    .funtab
                    .resolve(unary_op.op.function_name(Ary::Unary))?
                    .get();
                let var_right = self.visit(&unary_op.right, in_loop)?;
                let var_result = self.new_var(ty);
                self.emit_call(location, fun, [var_right], var_result);
                Ok(var_result)
            }
            Expression::While(while_loop) => {
                let start = self.new_label();
                let body = self.new_label();
                let end = self.new_label();

                self.emit_label(location, start);
                let cond = self.visit(&while_loop.condition, in_loop)?;
                self.emit_cond_jump(location, cond, body, end);
                self.emit_label(location, body);
                let new_loop = LoopLabels { start, end };
                self.visit(&while_loop.do_expr, Some(&new_loop))?;
                self.emit_jump(location, start);
                self.emit_label(location, end);
                Ok(Self::UNIT)
            }
            Expression::Break => {
                match in_loop {
                    Some(LoopLabels { end, .. }) => self.emit_jump(location, *end),
                    None => return Err(Error::BreakOutOfLoop),
                }
                Ok(Self::UNIT)
            }
            Expression::Continue => {
                match in_loop {
                    Some(LoopLabels { start, .. }) => self.emit_jump(location, *start),
                    None => return Err(Error::ContinueOutOfLoop),
                }
                Ok(Self::UNIT)
            }
            Expression::Return(ast) => todo!(),
        }
    }

    pub fn type_of(&self, key: Var) -> &Type {
        match self.var_types.get(&key) {
            Some(ty) => ty,
            None => panic!("Var {key} has no type info. This is a bug."),
        }
    }

    pub fn new_var(&mut self, ty: &Type) -> Var {
        let var = self.var;
        self.var += 1;
        self.var_types.insert(var, ty.clone());
        var
    }

    pub fn new_label(&mut self) -> Label {
        let label = self.label;
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
        fun: &'a str,
        args: impl IntoIterator<Item = Var>,
        dest: impl Into<Var>,
    ) {
        self.ins.push(Instruction {
            location: location.clone(),
            op: Op::Call {
                fun,
                args: args.into_iter().collect(),
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

    pub fn take(&mut self) -> Vec<Instruction<'a>> {
        #[cfg(debug_assertions)]
        {
            assert_eq!(self.symtab.depth(), 1);
            assert_eq!(self.funtab.depth(), 1);
        }
        mem::take(&mut self.ins)
    }
}

pub fn generate_ir<'a>(
    module: &Module<'a>,
    root_types: &[(&'a str, Type)],
) -> Result<HashMap<&'a str, Vec<Instruction<'a>>>, Error> {
    let mut funs = HashMap::new();
    let mut generator = Generator::new(root_types);

    // Generate user-defined functions from module
    for fun in &module.functions {
        generator.visit(&fun.body, None)?;
        funs.insert(fun.identifier.name, generator.take());
        // TODO: return values
    }

    // Generate main function from module root
    // TODO: modules without main
    let var_final_result = generator.visit(module.main.as_ref().unwrap(), None)?;
    match generator.type_of(var_final_result) {
        Type::Int => generator.emit_call(
            &Location::default(),
            "print_int",
            [var_final_result],
            Generator::UNIT,
        ),
        Type::Bool => generator.emit_call(
            &Location::default(),
            "print_bool",
            [var_final_result],
            Generator::UNIT,
        ),
        Type::Unit => {}
        Type::Fun { .. } => unreachable!("Function values are not supported"),
    }
    funs.insert("main", generator.take());

    Ok(funs)
}
