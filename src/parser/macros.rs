#![allow(unused_macros, reason = "Does not matter, macros defined for testing")]

macro_rules! ast {
    {$e: expr $(,)?} => {Ast {tree: Box::new($e)}};
}

macro_rules! op {
    {$op: expr, $r: expr $(,)?} => {
        Expression::UnaryOp(UnaryOp{
            op: $op,
            right: Ast {
                tree: Box::new($r)
            }
        })
    };
    {$l: expr, $op: expr, $r: expr $(,)?} => {
        Expression::BinaryOp(BinaryOp {
            left: Ast {
                tree: Box::new($l)
            },
            op: $op,
            right: Ast {
                tree: Box::new($r)
            }
        })
    };
}

macro_rules! int {
    ($a: literal $(,)?) => {
        Expression::Literal(Literal::Int($a))
    };
}

macro_rules! st {
    ($a: literal $(,)?) => {
        Expression::Literal(Literal::Str($a))
    };
}

macro_rules! id {
    ($name: literal $(,)?) => {
        Expression::Identifier(Identifier { name: $name })
    };
}

macro_rules! con {
    {$ie: expr, $te: expr $(,)?} => {
        Expression::Conditional(Conditional {
            condition: Ast {
                tree: Box::new($ie),
            },
            then_expr: Ast {
                tree: Box::new($te),
            },
            else_expr: None,
        })
    };
    {$ie: expr, $te: expr, $ee: expr $(,)?} => {
        Expression::Conditional(Conditional {
            condition: Ast {
                tree: Box::new($ie),
            },
            then_expr: Ast {
                tree: Box::new($te),
            },
            else_expr: Some(Ast {
                tree: Box::new($ee),
            }),
        })
    };
}

macro_rules! tru {
    () => {
        Expression::Literal(Literal::Bool(true))
    };
}

macro_rules! fal {
    () => {
        Expression::Literal(Literal::Bool(false))
    };
}

macro_rules! fun {
    ($id: literal) => {
        fun!($id,)
    };
    ($id: literal, $($arg: expr),*) => {
        Expression::FnCall(FnCall {
            function: Identifier { name: $id },
            arguments: vec![$( $arg ),*],
        })
    };
}

macro_rules! blk {
    {$($exprs: expr);*;} => {
        Expression::Block(Block {
            expressions: vec![$($exprs),*].into_iter().map(|expr| Ast{
                tree: Box::new(expr)
            }).collect(),
            result: None,
        })
    };
    {$($exprs: expr);*, $res: expr} => {
        Expression::Block(Block {
            expressions: vec![$($exprs),*].into_iter().map(|expr| Ast{
                tree: Box::new(expr)
            }).collect(),
            result: Some(Ast{tree: Box::new($res)}),
        })
    };
}
