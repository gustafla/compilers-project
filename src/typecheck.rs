use crate::{
    ast::{Ast, Expression, Literal, Op},
    trace::{end_trace, start_trace, trace},
};
use std::{
    collections::hash_map::{Entry, HashMap, OccupiedEntry},
    fmt::Display,
};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Unresolved identifier {0:?}")]
    UnresolvedIdentifier(String),
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

macro_rules! fun {
    (($($par: expr),*$(,)?) => $res: expr) => {
        Type::Fun {
            parameters: vec![$($par),*],
            result: Box::new($res),
        }
    };
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

type SymbolTable<'a> = HashMap<&'a str, Type>;

fn resolve_sym<'a, 'b>(
    symtab: &'b mut [SymbolTable<'a>],
    key: &'a str,
) -> Result<OccupiedEntry<'b, &'a str, Type>, Error> {
    for table in symtab.iter_mut().rev() {
        if let Entry::Occupied(entry) = table.entry(key) {
            return Ok(entry);
        }
    }
    Err(Error::UnresolvedIdentifier(String::from(key)))
}

fn check_fn<'a>(
    symtab: &mut [SymbolTable<'a>],
    key: &'a str,
    arguments: &[Type],
) -> Result<Type, Error> {
    let entry = resolve_sym(symtab, key)?.get().clone();
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

fn check<'a>(ast: &Ast<'a>, symtab: &mut Vec<SymbolTable<'a>>) -> Result<Type, Error> {
    let expr = ast.tree.as_ref();
    trace!("{expr} ");
    let typ = match expr {
        Expression::Literal(literal) => match literal {
            Literal::Int(_) => Type::Int,
            Literal::Bool(_) => Type::Bool,
            Literal::Str(_) => unimplemented!("String literals are not supported"),
        },
        Expression::Identifier(identifier) => resolve_sym(symtab, identifier.name)?.get().clone(),
        Expression::Conditional(conditional) => {
            let condition = check(&conditional.condition, symtab)?;
            if condition != Type::Bool {
                return Err(Error::ConditionalCondition(condition));
            }
            let then_typ = check(&conditional.then_expr, symtab)?;
            if let Some(else_expr) = &conditional.else_expr {
                let else_typ = check(else_expr, symtab)?;
                if then_typ != else_typ {
                    return Err(Error::ConditionalBranches(then_typ, else_typ));
                }
                then_typ
            } else {
                Type::Unit
            }
        }
        Expression::FnCall(fn_call) => {
            let key = match fn_call.function.tree.as_ref() {
                Expression::Identifier(identifier) => identifier.name,
                _ => unreachable!("Ast function call identifier is not an identifier"),
            };
            let mut arguments = Vec::new();
            for arg in &fn_call.arguments {
                arguments.push(check(arg, symtab)?);
            }
            check_fn(symtab, key, &arguments)?
        }
        Expression::Block(block) => {
            symtab.push(HashMap::new());
            for expr in &block.expressions {
                check(expr, symtab)?;
            }
            let result = if let Some(result) = &block.result {
                check(result, symtab)?
            } else {
                Type::Unit
            };
            symtab.pop();
            result
        }
        Expression::Var(var) => {
            let key = match var.id.tree.as_ref() {
                Expression::Identifier(identifier) => identifier.name,
                _ => unreachable!("Ast variable declaration identifier is not an identifier"),
            };
            let typ = check(&var.init, symtab)?;
            if symtab.last_mut().unwrap().insert(key, typ).is_some() {
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
                let lhs = resolve_sym(symtab, key)?.get().clone();
                let rhs = check(&binary_op.right, symtab)?;
                if lhs != rhs {
                    return Err(Error::AssignWrongType(String::from(key), lhs, rhs));
                }
                Type::Unit
            } else {
                let lhs = check(&binary_op.left, symtab)?;
                let rhs = check(&binary_op.right, symtab)?;
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
            let arguments = &[check(&unary_op.right, symtab)?];
            check_fn(symtab, key, arguments)?
        }
        Expression::While(_) => Type::Unit,
    };
    Ok(typ)
}

pub fn typecheck(ast: &Ast<'_>) -> Result<Type, Error> {
    start_trace!("Type checker");
    let mut symtab = vec![HashMap::from([
        ("print_int", fun!((Type::Int) => Type::Unit)),
        ("print_bool", fun!((Type::Bool) => Type::Unit)),
        ("read_int", fun!(() => Type::Int)),
        ("binary_add", fun!((Type::Int, Type::Int) => Type::Int)),
        ("binary_sub", fun!((Type::Int, Type::Int) => Type::Int)),
        ("unary_sub", fun!((Type::Int) => Type::Int)),
        ("binary_mul", fun!((Type::Int, Type::Int) => Type::Int)),
        ("binary_div", fun!((Type::Int, Type::Int) => Type::Int)),
        ("binary_rem", fun!((Type::Int, Type::Int) => Type::Int)),
        ("binary_lt", fun!((Type::Int, Type::Int) => Type::Bool)),
        ("binary_leq", fun!((Type::Int, Type::Int) => Type::Bool)),
        ("binary_gt", fun!((Type::Int, Type::Int) => Type::Bool)),
        ("binary_geq", fun!((Type::Int, Type::Int) => Type::Bool)),
        ("binary_and", fun!((Type::Bool, Type::Bool) => Type::Bool)),
        ("binary_or", fun!((Type::Bool, Type::Bool) => Type::Bool)),
        ("unary_not", fun!((Type::Bool) => Type::Bool)),
    ])];
    let res = check(ast, &mut symtab);
    end_trace!();
    res
}
