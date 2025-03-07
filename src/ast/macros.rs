#![allow(unused_macros, unused_imports, reason = "Macros defined for testing")]

macro_rules! ast {
    {$e: expr $(,)?} => {
        ast!(Default::default() => {$e})
    };
    {$loc: expr => $e: expr $(,)?} => {
        crate::ast::Ast {
            location: $loc,
            ty: None,
            tree: Box::new($e)
        }
    };
}

macro_rules! op {
    {$op: expr, $r: expr $(,)?} => {
        ast!{ crate::ast::Expression::UnaryOp(crate::ast::UnaryOp{
            op: $op,
            right: $r,
        })}
    };
    {$l: expr, $op: expr, $r: expr $(,)?} => {
        ast!{ crate::ast::Expression::BinaryOp(crate::ast::BinaryOp {
            left: $l,
            op: $op,
            right: $r,
        })}
    };
}

macro_rules! int {
    ($a: literal $(,)?) => {
        ast! {crate::ast::Expression::Literal(crate::ast::Literal::Int($a))}
    };
}

macro_rules! st {
    ($a: literal $(,)?) => {
        ast! {crate::ast::Expression::Literal(crate::ast::Literal::Str($a))}
    };
}

macro_rules! id {
    ($name: literal $(,)?) => {
        ast! {crate::ast::Expression::Identifier(crate::ast::Identifier { name: $name })}
    };
}

macro_rules! con {
    {$ie: expr, $te: expr $(,)?} => {
        ast! {crate::ast::Expression::Conditional(crate::ast::Conditional {
            condition: $ie,
            then_expr: $te,
            else_expr: None,
        })}
    };
    {$ie: expr, $te: expr, $ee: expr $(,)?} => {
        ast! {crate::ast::Expression::Conditional(crate::ast::Conditional {
            condition: $ie,
            then_expr: $te,
            else_expr: Some($ee),
        })}
    };
}

macro_rules! whi {
    {$we: expr, $de: expr $(,)?} => {
        ast! {crate::ast::Expression::While(crate::ast::While{
            condition: $we,
            do_expr: $de,
        })}
    };
}

macro_rules! tru {
    () => {
        ast! {crate::ast::Expression::Literal(crate::ast::Literal::Bool(true))}
    };
}

macro_rules! fal {
    () => {
        ast! {crate::ast::Expression::Literal(crate::ast::Literal::Bool(false))}
    };
}

macro_rules! cal {
    ($id: literal) => {
        cal!($id,)
    };
    ($id: literal, $($arg: expr),*) => {
        ast! {crate::ast::Expression::FnCall(crate::ast::FnCall {
            function: crate::ast::Identifier{name: $id},
            arguments: vec![$( $arg ),*],
        })}
    };
}

macro_rules! blk {
    {$($exprs: expr);*;} => {
        ast! {crate::ast::Expression::Block(crate::ast::Block {
            expressions: vec![$($exprs),*],
            result: None,
        })}
    };
    {$($exprs: expr);*, $res: expr} => {
        ast! {crate::ast::Expression::Block(crate::ast::Block {
            expressions: vec![$($exprs),*],
            result: Some($res),
        })}
    };
}

macro_rules! var {
    ($id: literal = $init: expr) => {
        ast! {crate::ast::Expression::Var(crate::ast::Var {
            id: crate::ast::Identifier{name: $id},
            typed: None,
            init: $init,
        })}
    };
    (($id: literal, $ty: expr) = $init: expr) => {
        ast! {crate::ast::Expression::Var(crate::ast::Var {
            id: crate::ast::Identifier{name: $id},
            typed: Some($ty),
            init: $init,
        })}
    };
}

macro_rules! brk {
    () => {
        ast! {crate::ast::Expression::Break}
    };
}

macro_rules! cnt {
    () => {
        ast! {crate::ast::Expression::Continue}
    };
}

macro_rules! mdl {
    {$e: expr $(,)?} => {
        crate::ast::Module {
            functions: Vec::new(),
            main: Some($e),
        }
    };
    {$($fun: expr),* => $e: expr} => {
        crate::ast::Module {
            functions: vec![$($fun),*],
            main: Some($e),
        }
    };
}

macro_rules! fun {
    ($id: literal ($($pid: literal = $pty: expr),*) -> $ty: expr, $body: expr) => {
        crate::ast::Function {
            identifier: crate::ast::Identifier { name: $id },
            parameters: vec![$(crate::ast::Parameter {
                identifier: crate::ast::Identifier {name: $pid},
                ty: $pty
            }),*],
            returns: $ty,
            body: $body,
        }
    };
}

macro_rules! ret {
    () => {
        ast! {crate::ast::Expression::Return(None)}
    };
    ($e: expr $(,)?) => {
        ast! {crate::ast::Expression::Return(Some($e))}
    };
}

pub(crate) use ast;
pub(crate) use blk;
pub(crate) use brk;
pub(crate) use cal;
pub(crate) use cnt;
pub(crate) use con;
pub(crate) use fal;
pub(crate) use fun;
pub(crate) use id;
pub(crate) use int;
pub(crate) use mdl;
pub(crate) use op;
pub(crate) use ret;
pub(crate) use st;
pub(crate) use tru;
pub(crate) use var;
pub(crate) use whi;
