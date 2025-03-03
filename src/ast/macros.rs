#![allow(unused_macros, unused_imports, reason = "Macros defined for testing")]

macro_rules! ast {
    {$e: expr $(,)?} => {
        ast!(Default::default() => {$e})
    };
    {$loc: expr => $e: expr $(,)?} => {
        Ast {
            location: $loc,
            ty: None,
            tree: Box::new($e)
        }
    };
}

macro_rules! op {
    {$op: expr, $r: expr $(,)?} => {
        ast!{ Expression::UnaryOp(UnaryOp{
            op: $op,
            right: $r,
        })}
    };
    {$l: expr, $op: expr, $r: expr $(,)?} => {
        ast!{ Expression::BinaryOp(BinaryOp {
            left: $l,
            op: $op,
            right: $r,
        })}
    };
}

macro_rules! int {
    ($a: literal $(,)?) => {
        ast! {Expression::Literal(Literal::Int($a))}
    };
}

macro_rules! st {
    ($a: literal $(,)?) => {
        ast! {Expression::Literal(Literal::Str($a))}
    };
}

macro_rules! id {
    ($name: literal $(,)?) => {
        ast! {Expression::Identifier(Identifier { name: $name })}
    };
}

macro_rules! con {
    {$ie: expr, $te: expr $(,)?} => {
        ast! {Expression::Conditional(Conditional {
            condition: $ie,
            then_expr: $te,
            else_expr: None,
        })}
    };
    {$ie: expr, $te: expr, $ee: expr $(,)?} => {
        ast! {Expression::Conditional(Conditional {
            condition: $ie,
            then_expr: $te,
            else_expr: Some($ee),
        })}
    };
}

macro_rules! whi {
    {$we: expr, $de: expr $(,)?} => {
        ast! {Expression::While(While{
            condition: $we,
            do_expr: $de,
        })}
    };
}

macro_rules! tru {
    () => {
        ast! {Expression::Literal(Literal::Bool(true))}
    };
}

macro_rules! fal {
    () => {
        ast! {Expression::Literal(Literal::Bool(false))}
    };
}

macro_rules! fun {
    ($id: literal) => {
        fun!($id,)
    };
    ($id: literal, $($arg: expr),*) => {
        ast! {Expression::FnCall(FnCall {
            function: Identifier{name: $id},
            arguments: vec![$( $arg ),*],
        })}
    };
}

macro_rules! blk {
    {$($exprs: expr);*;} => {
        ast! {Expression::Block(Block {
            expressions: vec![$($exprs),*],
            result: None,
        })}
    };
    {$($exprs: expr);*, $res: expr} => {
        ast! {Expression::Block(Block {
            expressions: vec![$($exprs),*],
            result: Some($res),
        })}
    };
}

macro_rules! var {
    ($id: literal = $init: expr) => {
        ast! {Expression::Var(Var {
            id: Identifier{name: $id},
            typed: None,
            init: $init,
        })}
    };
    (($id: literal, $ty: expr) = $init: expr) => {
        ast! {Expression::Var(Var {
            id: Identifier{name: $id},
            typed: Some($ty),
            init: $init,
        })}
    };
}

macro_rules! brk {
    () => {
        ast! {Expression::Break}
    };
}

macro_rules! cnt {
    () => {
        ast! {Expression::Continue}
    };
}

pub(crate) use ast;
pub(crate) use blk;
pub(crate) use brk;
pub(crate) use cnt;
pub(crate) use con;
pub(crate) use fal;
pub(crate) use fun;
pub(crate) use id;
pub(crate) use int;
pub(crate) use op;
pub(crate) use st;
pub(crate) use tru;
pub(crate) use var;
pub(crate) use whi;
