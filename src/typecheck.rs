use crate::{
    SymbolTable,
    ast::{
        Ast, BinaryOp, Block, Conditional, Expression, FnCall, Literal, Module, Operator, UnaryOp,
        Var, While, op::Ary,
    },
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
    #[error("While loop condition has an incompatible type `while {0} do ..`")]
    WhileLoopCondition(Type),
    #[error("Binary expression operands have incompatible types `{0} {1} {2}`")]
    BinaryExpr(Type, Operator, Type),
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
    #[error("Cannot return from root level block")]
    ReturnOutOfFunction,
    #[error("Cannot return {0} from function `{1}`, which returns {2}")]
    ReturnType(Type, String, Type),
    #[error("Function `{0}` with a non-unit return type does not return")]
    Return(String),
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

fn contains_break_statement(ast: &Ast<'_>) -> bool {
    match ast.tree.as_ref() {
        Expression::Literal(_) => false,
        Expression::Identifier(_) => false,
        Expression::Conditional(Conditional {
            condition,
            then_expr,
            else_expr,
        }) => {
            if let Some(else_expr) = else_expr {
                contains_break_statement(condition)
                    || contains_break_statement(then_expr)
                    || contains_break_statement(else_expr)
            } else {
                contains_break_statement(condition) || contains_break_statement(then_expr)
            }
        }
        Expression::FnCall(FnCall { arguments, .. }) => {
            for arg in arguments {
                if contains_break_statement(arg) {
                    return true;
                }
            }
            false
        }
        Expression::Block(Block {
            expressions,
            result,
        }) => {
            for ex in expressions {
                if contains_break_statement(ex) {
                    return true;
                }
            }
            if let Some(res) = result {
                contains_break_statement(res)
            } else {
                false
            }
        }
        Expression::Var(Var { init, .. }) => contains_break_statement(init),
        Expression::BinaryOp(BinaryOp { left, right, .. }) => {
            contains_break_statement(left) || contains_break_statement(right)
        }
        Expression::UnaryOp(UnaryOp { right, .. }) => contains_break_statement(right),
        Expression::While(While { condition, do_expr }) => {
            contains_break_statement(condition) || contains_break_statement(do_expr)
        }
        Expression::Break => true,
        Expression::Continue => false,
        Expression::Return(Some(ast)) => contains_break_statement(ast),
        Expression::Return(None) => false,
    }
}

fn visit<'a>(
    ast: &mut Ast<'a>,
    symtab: &mut SymbolTable<'a, Type>,
    fun: Option<(&'a str, &Type)>,
) -> Result<(Type, bool), Error> {
    let expr = ast.tree.as_mut();
    traceln!("{expr}");
    let mut returns = false;
    let typ = match expr {
        Expression::Literal(literal) => match literal {
            Literal::Int(_) => Type::Int,
            Literal::Bool(_) => Type::Bool,
            Literal::Str(_) => unimplemented!("String literals are not supported"),
        },
        Expression::Identifier(identifier) => symtab.resolve(identifier.name)?.get().clone(),
        Expression::Conditional(conditional) => {
            let (condition, _) = visit(&mut conditional.condition, symtab, fun)?;
            if condition != Type::Bool {
                return Err(Error::ConditionalCondition(condition));
            }
            let (then_typ, then_returns) = visit(&mut conditional.then_expr, symtab, fun)?;
            // If condition is trivially true this node may be known to return
            if let Expression::Literal(Literal::Bool(true)) = conditional.condition.tree.as_ref() {
                returns |= then_returns;
            }
            if let Some(else_expr) = &mut conditional.else_expr {
                let (else_typ, else_returns) = visit(else_expr, symtab, fun)?;
                if then_typ != else_typ {
                    return Err(Error::ConditionalBranches(then_typ, else_typ));
                }
                returns |= then_returns && else_returns;
                then_typ
            } else {
                Type::Unit
            }
        }
        Expression::FnCall(fn_call) => {
            let key = fn_call.function.name;
            let mut arguments = Vec::new();
            for arg in &mut fn_call.arguments {
                let (arg_ty, arg_returns) = visit(arg, symtab, fun)?;
                arguments.push(arg_ty);
                returns |= arg_returns;
            }
            check_fn(symtab, key, &arguments)?
        }
        Expression::Block(block) => {
            symtab.push();
            for expr in &mut block.expressions {
                returns |= visit(expr, symtab, fun)?.1;
            }
            let result = if let Some(result) = &mut block.result {
                let (result_ty, result_returns) = visit(result, symtab, fun)?;
                returns |= result_returns;
                result_ty
            } else {
                Type::Unit
            };
            symtab.pop();
            result
        }
        Expression::Var(var) => {
            let key = var.id.name;
            let (typ, init_returns) = visit(&mut var.init, symtab, fun)?;
            returns |= init_returns;
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
            if binary_op.op == Operator::Assign {
                let key = match binary_op.left.tree.as_ref() {
                    Expression::Identifier(identifier) => identifier.name,
                    expr => return Err(Error::AssignWrongExpr(format!("{}", expr))),
                };
                let lhs = symtab.resolve(key)?.get().clone();
                let (rhs, assignment_returns) = visit(&mut binary_op.right, symtab, fun)?;
                returns |= assignment_returns;
                if lhs != rhs {
                    return Err(Error::AssignWrongType(String::from(key), lhs, rhs));
                }
                rhs
            } else {
                let (lhs, lhs_returns) = visit(&mut binary_op.left, symtab, fun)?;
                let (rhs, rhs_returns) = visit(&mut binary_op.right, symtab, fun)?;
                returns |= lhs_returns || rhs_returns;
                match (binary_op.op, &[lhs, rhs]) {
                    (op @ (Operator::Eq | Operator::Ne), [a, b]) => {
                        if a == b {
                            Type::Bool
                        } else {
                            return Err(Error::BinaryExpr(a.clone(), op, b.clone()));
                        }
                    }
                    (op, args) => check_fn(symtab, op.function_name(Ary::Binary), args)?,
                }
            }
        }
        Expression::UnaryOp(unary_op) => {
            let (argument, argument_returns) = visit(&mut unary_op.right, symtab, fun)?;
            returns |= argument_returns;
            check_fn(symtab, unary_op.op.function_name(Ary::Unary), &[argument])?
        }
        Expression::While(while_loop) => {
            let (condition, condition_returns) = visit(&mut while_loop.condition, symtab, fun)?;
            returns |= condition_returns;
            if condition != Type::Bool {
                return Err(Error::WhileLoopCondition(condition));
            }
            visit(&mut while_loop.do_expr, symtab, fun)?;
            // If loop is trivially infinite, this node may be known to return
            if let Expression::Literal(Literal::Bool(true)) = while_loop.condition.tree.as_ref() {
                returns |= !contains_break_statement(&while_loop.do_expr);
            }
            Type::Unit
        }
        Expression::Break => Type::Unit,
        Expression::Continue => Type::Unit,
        Expression::Return(ast) => {
            returns = true;
            let ty = match ast {
                Some(ast) => visit(ast, symtab, fun)?.0,
                None => Type::Unit,
            };
            let Some(fun) = fun else {
                return Err(Error::ReturnOutOfFunction);
            };
            let (identifier, returntype) = fun;
            if *returntype != ty {
                return Err(Error::ReturnType(
                    ty,
                    String::from(identifier),
                    returntype.clone(),
                ));
            }
            Type::Unit
        }
    };
    ast.ty = Some(typ.clone());
    Ok((typ, returns))
}

pub fn typecheck<'a>(module: &mut Module<'a>, root_types: &[(&'a str, Type)]) -> Result<(), Error> {
    start_trace!("Type checker");

    // Check functions
    for fun in &mut module.functions {
        let mut symtab = SymbolTable::from(root_types.to_owned());

        // Insert parameters
        symtab.push();
        for parm in &fun.parameters {
            symtab.insert(parm.identifier.name, parm.ty.clone());
        }

        // Check function body
        match visit(
            &mut fun.body,
            &mut symtab,
            Some((fun.identifier.name, &fun.returns)),
        ) {
            Ok((_, true)) => { /* Types are Ok and the function is guaranteed to return */ }
            Ok((ty, false)) => {
                // Types are Ok, but the function is not guaranteed to return, check whether it's okay
                if fun.returns != ty {
                    end_trace!();
                    return Err(Error::Return(String::from(fun.identifier.name)));
                }
            }
            Err(e) => {
                // Type error occurred
                end_trace!();
                return Err(e);
            }
        }
    }

    // Check root-level expression
    let res = if let Some(main) = &mut module.main {
        let mut symtab = SymbolTable::from(root_types.to_owned());
        visit(main, &mut symtab, None).map(|_| ())
    } else {
        Ok(())
    };

    end_trace!();
    res
}
