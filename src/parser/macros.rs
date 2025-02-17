#![allow(unused_macros, reason = "Does not matter, macros defined for testing")]

macro_rules! ast {
    {$e: expr $(,)?} => {Ast {location: Default::default(), tree: Box::new($e)}};
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
            function: id!($id),
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
            id: id!($id),
            init: $init,
        })}
    };
}
