use crate::{
    SymbolTable,
    ast::{Ast, Expression, Literal, Op},
    trace::{end_trace, start_trace, traceln},
};
use std::{fmt::Display, str::FromStr};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    UnresolvedIdentifier(#[from] crate::symtab::Error),
    #[error("Conditional expression branches have incompatible types `if .. then {0} else {1}`")]
    ConditionalBranches(Type, Type),
    #[error("Conditional expression condition has an incompatible type `if {0} then ..`")]
    ConditionalCondition(Type),
    #[error("Binary expression operands have incompatible types `{0} {1} {2}`")]
    BinaryExpr(Type, Op, Type),
    #[error("Cannot call {0:?}, not a function")]
    NotFn(String),
    #[error("In call to {0:?}: wrong number of arguments, has {1} but requires {2}")]
    FnArgumentCount(String, usize, usize),
    #[error("In call to {0:?}: argument {1} has an incompatible type {2}")]
    FnArgument(String, usize, Type),
    #[error("Cannot assign to {0}")]
    AssignWrongExpr(String),
    #[error("Cannot assign {2} to {0}, it has type {1}")]
    AssignWrongType(String, Type, Type),
    #[error("Cannot redefine variable {0}")]
    Redefinition(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Unit,
    Fun {
        parameters: Vec<Type>,
        result: Box<Type>,
    },
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "Int")?,
            Type::Bool => write!(f, "Bool")?,
            Type::Unit => write!(f, "Unit")?,
            Type::Fun { parameters, result } => match parameters.as_slice() {
                [] => write!(f, "()")?,
                [p0, rest @ ..] => {
                    write!(f, "({}", p0)?;
                    for p in rest {
                        write!(f, ", {}", p)?;
                    }
                    write!(f, ") => {}", result)?;
                }
            },
        }
        Ok(())
    }
}

impl FromStr for Type {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once("=>") {
            None => match s {
                "Int" => return Ok(Type::Int),
                "Bool" => return Ok(Type::Bool),
                "Unit" => return Ok(Type::Unit),
                _ => {}
            },
            Some((par, res)) => {
                let result = Self::from_str(res.trim_start())?;
                let mut parameters = Vec::new();
                if let Some(par) = par.strip_prefix("(").and_then(|par| par.strip_suffix(")")) {
                    for par in par.split(",") {
                        parameters.push(Self::from_str(par.trim())?);
                    }
                }
                return Ok(Self::Fun {
                    parameters,
                    result: Box::new(result),
                });
            }
        };
        Err(format!("Malformed type `{}`", s))
    }
}

fn check_fn<'a>(
    symtab: &mut SymbolTable<'a, Type>,
    key: &'a str,
    arguments: &[Type],
) -> Result<Type, Error> {
    let entry = symtab.resolve(key)?.get().clone();
    let Type::Fun { parameters, result } = entry else {
        return Err(Error::NotFn(String::from(key)));
    };
    if arguments.len() != parameters.len() {
        return Err(Error::FnArgumentCount(
            String::from(key),
            arguments.len(),
            parameters.len(),
        ));
    }
    for (i, (have, require)) in arguments.iter().zip(parameters.iter()).enumerate() {
        if have != require {
            return Err(Error::FnArgument(String::from(key), i, have.clone()));
        }
    }
    Ok(*result)
}

fn visit<'a>(ast: &mut Ast<'a>, symtab: &mut SymbolTable<'a, Type>) -> Result<Type, Error> {
    let expr = ast.tree.as_mut();
    traceln!("{expr}");
    let typ = match expr {
        Expression::Literal(literal) => match literal {
            Literal::Int(_) => Type::Int,
            Literal::Bool(_) => Type::Bool,
            Literal::Str(_) => unimplemented!("String literals are not supported"),
        },
        Expression::Identifier(identifier) => symtab.resolve(identifier.name)?.get().clone(),
        Expression::Conditional(conditional) => {
            let condition = visit(&mut conditional.condition, symtab)?;
            if condition != Type::Bool {
                return Err(Error::ConditionalCondition(condition));
            }
            let then_typ = visit(&mut conditional.then_expr, symtab)?;
            if let Some(else_expr) = &mut conditional.else_expr {
                let else_typ = visit(else_expr, symtab)?;
                if then_typ != else_typ {
                    return Err(Error::ConditionalBranches(then_typ, else_typ));
                }
                then_typ
            } else {
                Type::Unit
            }
        }
        Expression::FnCall(fn_call) => {
            let key = fn_call.function.name;
            let mut arguments = Vec::new();
            for arg in &mut fn_call.arguments {
                arguments.push(visit(arg, symtab)?);
            }
            check_fn(symtab, key, &arguments)?
        }
        Expression::Block(block) => {
            symtab.push();
            for expr in &mut block.expressions {
                visit(expr, symtab)?;
            }
            let result = if let Some(result) = &mut block.result {
                visit(result, symtab)?
            } else {
                Type::Unit
            };
            symtab.pop();
            result
        }
        Expression::Var(var) => {
            let key = var.id.name;
            let typ = visit(&mut var.init, symtab)?;
            if let Some(typed) = &var.typed {
                if typ != *typed {
                    return Err(Error::AssignWrongType(
                        format!("var {key}"),
                        typed.clone(),
                        typ,
                    ));
                }
            }
            if symtab.insert(key, typ).is_some() {
                return Err(Error::Redefinition(String::from(key)));
            }
            Type::Unit
        }
        Expression::BinaryOp(binary_op) => {
            // Special case: assignment
            if binary_op.op == Op::Assign {
                let key = match binary_op.left.tree.as_ref() {
                    Expression::Identifier(identifier) => identifier.name,
                    expr => return Err(Error::AssignWrongExpr(format!("{}", expr))),
                };
                let lhs = symtab.resolve(key)?.get().clone();
                let rhs = visit(&mut binary_op.right, symtab)?;
                if lhs != rhs {
                    return Err(Error::AssignWrongType(String::from(key), lhs, rhs));
                }
                Type::Unit
            } else {
                let lhs = visit(&mut binary_op.left, symtab)?;
                let rhs = visit(&mut binary_op.right, symtab)?;
                match (binary_op.op, &[lhs, rhs]) {
                    (op @ (Op::Eq | Op::Ne), [a, b]) => {
                        if a == b {
                            Type::Bool
                        } else {
                            return Err(Error::BinaryExpr(a.clone(), op, b.clone()));
                        }
                    }
                    (Op::Add, args) => check_fn(symtab, "binary_add", args)?,
                    (Op::Sub, args) => check_fn(symtab, "binary_sub", args)?,
                    (Op::Mul, args) => check_fn(symtab, "binary_mul", args)?,
                    (Op::Div, args) => check_fn(symtab, "binary_div", args)?,
                    (Op::Rem, args) => check_fn(symtab, "binary_rem", args)?,
                    (Op::Leq, args) => check_fn(symtab, "binary_leq", args)?,
                    (Op::Lt, args) => check_fn(symtab, "binary_lt", args)?,
                    (Op::Geq, args) => check_fn(symtab, "binary_geq", args)?,
                    (Op::Gt, args) => check_fn(symtab, "binary_gt", args)?,
                    (Op::And, args) => check_fn(symtab, "binary_and", args)?,
                    (Op::Or, args) => check_fn(symtab, "binary_or", args)?,
                    (op @ Op::Not, _) => unreachable!("Ast has {op} in BinaryOp"),
                    (op @ Op::Assign, _) => {
                        unreachable!("{op:#?} should have been handled in a special case")
                    }
                }
            }
        }
        Expression::UnaryOp(unary_op) => {
            let key = match unary_op.op {
                Op::Sub => "unary_sub",
                Op::Not => "unary_not",
                op => unreachable!("Ast has {op:#?} in UnaryOp"),
            };
            let arguments = &[visit(&mut unary_op.right, symtab)?];
            check_fn(symtab, key, arguments)?
        }
        Expression::While(_) => Type::Unit,
    };
    ast.ty = Some(typ.clone());
    Ok(typ)
}

pub fn typecheck<'a>(ast: &mut Ast<'a>, root_types: &[(&'a str, Type)]) -> Result<Type, Error> {
    start_trace!("Type checker");
    let mut symtab = SymbolTable::from(root_types.to_owned());
    let res = visit(ast, &mut symtab);
    end_trace!();
    res
}
