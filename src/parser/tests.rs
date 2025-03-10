#![cfg(test)]

use super::*;
use crate::{Type, ast::macros::*, tokenizer};

impl PartialEq for Expression<'_> {
    fn eq(&self, other: &Self) -> bool {
        use Expression as E;

        match (self, other) {
            (E::Literal(a), E::Literal(b)) => a == b,
            (E::Literal(..), _) => false,
            (E::Identifier(a), E::Identifier(b)) => a == b,
            (E::Identifier(..), _) => false,
            (E::BinaryOp(a), E::BinaryOp(b)) => {
                if a.op != b.op {
                    return false;
                }
                a.left == b.left && a.right == b.right
            }
            (E::BinaryOp(..), _) => false,
            (E::UnaryOp(a), E::UnaryOp(b)) => {
                if a.op != b.op {
                    return false;
                }
                a.right == b.right
            }
            (E::UnaryOp(..), _) => false,
            (E::Conditional(a), E::Conditional(b)) => {
                if a.condition != b.condition {
                    return false;
                }
                if a.then_expr != b.then_expr {
                    return false;
                }
                a.else_expr == b.else_expr
            }
            (E::Conditional(..), _) => false,
            (E::While(a), E::While(b)) => {
                if a.condition != b.condition {
                    return false;
                }
                a.do_expr == b.do_expr
            }
            (E::While(..), _) => false,
            (E::FnCall(a), E::FnCall(b)) => {
                if a.function != b.function {
                    return false;
                }
                a.arguments == b.arguments
            }
            (E::FnCall(..), _) => false,
            (E::Block(a), E::Block(b)) => {
                if a.expressions != b.expressions {
                    return false;
                }
                a.result == b.result
            }
            (E::Block(..), _) => false,
            (E::Var(a), E::Var(b)) => {
                if a.id != b.id {
                    return false;
                }
                a.init == b.init
            }
            (E::Var(..), _) => false,
            (E::Break, E::Break) => true,
            (E::Break, _) => false,
            (E::Continue, E::Continue) => true,
            (E::Continue, _) => false,
            (E::Return(a), E::Return(b)) => a == b,
            (E::Return(_), _) => false,
        }
    }
}

impl PartialEq for Ast<'_> {
    fn eq(&self, other: &Self) -> bool {
        // Ignore location
        self.tree == other.tree
    }
}

impl PartialEq for Module<'_> {
    fn eq(&self, other: &Self) -> bool {
        // Ignore functions for now (TODO: implement?)
        self.main == other.main
    }
}

#[test]
fn basic() {
    let code = "1 + 1";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {blk! {,op! {int!(1), Operator::Add, int!(1)}}}
    );
}

#[test]
fn error0() {
    let code = "a + b c";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens);
    assert!(matches!(expression, Err(..)));
}

#[test]
fn precedence_basic() {
    let code = "1+2+3";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
                blk! {,
                    op! {
                    op! {
                        int!(1),
                        Operator::Add,
                        int!(2),
                    },
                    Operator::Add,
                    int!(3),
                }
            }
        }
    );

    let code = "1+2*3";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                op! {
                    int!(1),
                    Operator::Add,
                    op! {
                        int!(2),
                        Operator::Mul,
                        int!(3),
                    },
                }
            }
        }
    );
}

#[test]
fn paren() {
    let code = "1+(2+3)";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    op! {
                    int!(1),
                    Operator::Add,
                    op! {
                        int!(2),
                        Operator::Add,
                        int!(3),
                    },
                }
            }
        }
    );
}

#[test]
fn identifiers() {
    let code = "a+b*c";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    op! {
                    id!("a"),
                    Operator::Add,
                    op! {
                        id!("b"),
                        Operator::Mul,
                        id!("c"),
                    },
                }
            }
        }
    );
}

#[test]
fn conditional() {
    let code = "if a then 1 + 1";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    con! {
                    id!("a"),
                    op! {
                        int!(1),
                        Operator::Add,
                        int!(1)
                    }
                }
            }
        }
    );
}

#[test]
fn binary_op_with_conditional() {
    let code = "1 + if true then 2 else 3";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    op! {
                    int!(1),
                    Operator::Add,
                    con! {
                        tru!(),
                        int!(2),
                        int!(3),
                    },
                }
            }
        }
    );
}

#[test]
fn nested_conditional() {
    let code = "if if condition then a else b then if if a then a else b then 32 else 42";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    con! {
                    con! {
                        id!("condition"),
                        id!("a"),
                        id!("b"),
                    },
                    con! {
                        con! {
                            id!("a"),
                            id!("a"),
                            id!("b"),
                        },
                        int!(32),
                        int!(42),
                    },
                }
            }
        }
    );
}

#[test]
fn fn_call() {
    let code = "5 - add(1, 1)";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    op! {
                    int!(5),
                    Operator::Sub,
                    cal!("add", int!(1), int!(1)),
                }
            }
        }
    );
}

#[test]
fn nested_fn_call() {
    let code = r#"printf("Error at packet %d: %s (context: %s)\n", packet, SDL_GetError(), describe_context(context))"#;
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    cal!(
                    "printf",
                    st!(r#"Error at packet %d: %s (context: %s)\n"#),
                    id!("packet"),
                    cal!("SDL_GetError"),
                    cal!("describe_context", id!("context"))
                )
            }
        }
    )
}

#[test]
fn assignment() {
    let code = "a = b = c";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    op! {
                    id!("a"),
                    Operator::Assign,
                    op! {
                        id!("b"),
                        Operator::Assign,
                        id!("c"),
                    },
                }
            }
        }
    );
}

#[test]
fn complex() {
    let code = "if true or false and foo then a = b = 1+2*sqrt(2)";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    con! {
                    op! {
                        tru!(),
                        Operator::Or,
                        op! {
                            fal!(),
                            Operator::And,
                            id!("foo"),
                        },
                    },
                    op! {
                        id!("a"),
                        Operator::Assign,
                        op! {
                            id!("b"),
                            Operator::Assign,
                            op! {
                                int!(1),
                                Operator::Add,
                                op! {
                                    int!(2),
                                    Operator::Mul,
                                    cal!("sqrt", int!(2)),
                                }
                            }
                        }
                    }
                }
            }
        }
    );
}

#[test]
fn unary() {
    let code = "if not foo then -1 else -2";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    con! {
                    op!{Operator::Not, id!("foo")},
                    op!{Operator::Sub, int!(1)},
                    op!{Operator::Sub, int!(2)}
                }
            }
        }
    )
}

#[test]
fn nested_unary() {
    let code = "if not not not not foo then --1 else -----2";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    con! {
                    op!{Operator::Not, op!{Operator::Not, op!{Operator::Not, op!{Operator::Not, id!("foo")}}}},
                    op!{Operator::Sub, op!{Operator::Sub, int!(1)}},
                    op!{Operator::Sub, op!{Operator::Sub, op!{Operator::Sub, op!{Operator::Sub, op!{Operator::Sub, int!(2)}}}}},
                }
            }
        }
    );
}

#[test]
fn precedence_complex() {
    let code = "- if true then a = 1 or 1 and 1 == 1 != 1 < 1 <= 1 > 1 >= 1 + 1 - 1 * 1 / 1 % -1 = x else xd";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    op! {Operator::Sub,
                    con! {
                        tru!{},
                        op! {
                            id!("a"),
                            Operator::Assign,
                            op! {
                                op! {
                                    int!(1),
                                    Operator::Or,
                                    op! {
                                        int!(1),
                                        Operator::And,
                                        op! {
                                            op! {
                                                int!(1),
                                                Operator::Eq,
                                                int!(1),
                                            },
                                            Operator::Ne,
                                            op ! {
                                                op! {
                                                    op! {
                                                        op! {
                                                            int!(1),
                                                            Operator::Lt,
                                                            int!(1),
                                                        },
                                                        Operator::Leq,
                                                        int!(1),
                                                    },
                                                    Operator::Gt,
                                                    int!(1),
                                                },
                                                Operator::Geq,
                                                op! {
                                                    op! {
                                                        int!(1),
                                                        Operator::Add,
                                                        int!(1),
                                                    },
                                                    Operator::Sub,
                                                    op! {
                                                        op! {
                                                            op! {
                                                                int!(1),
                                                                Operator::Mul,
                                                                int!(1),
                                                            },
                                                            Operator::Div,
                                                            int!(1),
                                                        },
                                                        Operator::Rem,
                                                        op! {
                                                            Operator::Sub,
                                                            int!(1),
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    },
                                },
                                Operator::Assign,
                                id!("x")
                            }
                        },
                        id!("xd")
                    },
                }
            }
        }
    );
}

#[test]
fn error1() {
    let code = "- if true then a = y = x not else xd";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens);
    assert!(expression.is_err());
}

#[test]
fn error2() {
    let code = "+ if true then a = y = x else xd";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens);
    assert!(expression.is_err());
}

#[test]
fn error3() {
    let code = "if true then a = y = x else xd -";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens);
    assert!(expression.is_err());
}

#[test]
fn error4() {
    let code = "if true then a++ else xd";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens);
    assert!(expression.is_err());
}

#[test]
fn block() {
    let code = "if ok then {fn1(); fn2(); exit(0);} else {exit(1)}";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    con! {
                    id!("ok"),
                    blk! {
                        cal!("fn1");
                        cal!("fn2");
                        cal!("exit", int!(0));
                    },
                    blk! {
                        , cal!("exit", int!(1))
                    }
                }
            }
        }
    )
}

#[test]
fn var() {
    let code = "if ok then {fn1(); fn2(); exit(0);} else {var retval = errno + 1; exit(retval)}";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    con! {
                    id!("ok"),
                    blk! {
                        cal!("fn1");
                        cal!("fn2");
                        cal!("exit", int!(0));
                    },
                    blk! {
                        var!("retval" = op!(id!("errno"), Operator::Add, int!(1)))
                        , cal!("exit", id!("retval"))
                    }
                }
            }
        }
    )
}

#[test]
fn var_error() {
    let code = "if var x = 1 then x";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens);
    assert!(matches!(expression, Err(..)))
}

#[test]
fn semicolons_omitted_in_nested_block() {
    let code = "if ok then {{fn1()} {fn2()} {exit(0)};} else {exit(1)}";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    con! {
                    id!("ok"),
                    blk! {
                        blk!{,cal!("fn1")};
                        blk!{,cal!("fn2")};
                        blk!{,cal!("exit", int!(0))};
                    },
                    blk! {
                        , cal!("exit", int!(1))
                    }
                }
            }
        }
    )
}

#[test]
fn omitted_semicolon_case0() {
    let code = "{{x}{y}}";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! { blk! {,blk! { blk!{,id!("x")}, blk!{,id!("y")} }}}
    );

    let code = "{{a}{x}{y}}";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! { blk! {,blk! { blk!{,id!("a")}; blk!{,id!("x")}, blk!{,id!("y")} }}}
    );
}

#[test]
fn omitted_semicolon_case1() {
    let code = "{a b }";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens);
    assert!(matches!(expression, Err(..)));
}

#[test]
fn omitted_semicolon_case2() {
    let code = "{ if true then { a } b }";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    blk! {
                    con!{
                        tru!(),
                        blk!{, id!("a")},
                    },
                    id!("b")
                }
            }
        }
    );
}

#[test]
fn omitted_semicolon_case3() {
    let code = "{ if true then { a }; b }";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    blk! {
                    con!{
                        tru!(),
                        blk!{, id!("a")},
                    },
                    id!("b")
                }
            }
        }
    );
}

#[test]
fn omitted_semicolon_case4() {
    let code = "{ if true then { a } b c }";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens);
    assert!(matches!(expression, Err(..)));
}

#[test]
fn omitted_semicolon_case5() {
    let code = "{ if true then { a } b; c }";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    blk! {
                    con!{
                        tru!(),
                        blk!{, id!("a")},
                    };
                    id!("b"),
                    id!("c")
                }
            }
        }
    );
}

#[test]
fn omitted_semicolon_case6() {
    let code = "{ if true then { a } else { b } c }";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    blk! {
                    con!{
                        tru!(),
                        blk!{, id!("a")},
                        blk!{, id!("b")},
                    },
                    id!("c")
                }
            }
        }
    );
}

#[test]
fn omitted_semicolon_case7() {
    let code = "x = { { f(a) } { b } }";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    op! {
                    id!("x"),
                    Operator::Assign,
                    blk!{
                        blk!{,cal!("f", id!("a"))},
                        blk!{,id!("b")}
                    }
                }
            }
        }
    );
}

#[test]
fn deeply_nested_blocks() {
    let code = "{{a}call();{{{{b}}}}if{{c}}then{happy()}else{edge_case()}}";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                    blk! {
                    blk!{,id!("a")};
                    cal!("call");
                    blk!{,blk!{,blk!{,blk!{,id!("b")}}}},
                    con!{
                        blk!{,blk!{,id!("c")}},
                        blk!{,cal!("happy")},
                        blk!{,cal!("edge_case")},
                    }
                }
            }
        }
    );
}

#[test]
fn root_level_block() {
    let code = "var x = 0; if true then { a } b; c(x);";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {
                var!("x" = int!(0));
                con!{
                    tru!(),
                    blk!{, id!("a")},
                };
                id!("b");
                cal!("c", id!("x"));
            }
        }
    );
}

#[test]
fn root_level_block2() {
    let code = "var x = {1+1}; if true then { a } b; c(x)";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {
                var!("x" = blk!{,op!{int!(1), Operator::Add, int!(1)}});
                con!{
                    tru!(),
                    blk!{, id!("a")},
                };
                id!("b"),
                cal!("c", id!("x"))
            }
        }
    );
}

#[test]
fn while_loop() {
    let code = "var x = 0; while true do {x = x + 1; print_int(x)}";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {
                var!("x" = int!(0)),
                whi! {
                    tru!(),
                    blk!{
                        op! {
                            id!("x"),
                            Operator::Assign,
                            op!{
                                id!("x"),
                                Operator::Add,
                                int!(1)
                            }
                        },
                        cal!("print_int", id!("x"))
                    }
                }
            }
        }
    );
}

#[test]
fn var_typed() {
    let code = "var x: Int = 1 + 1";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {,
                var!(("x", Type::Int) = op!{int!(1), Operator::Add, int!(1)})
            }
        }
    )
}

#[test]
fn while_loop_break_continue() {
    let code = "var x = 0; while true do {if x > 100 then break; x = x + 1; if x % 2 == 0 then continue; print_int(x);}";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            blk! {
                var!("x" = int!(0)),
                whi! {
                    tru!(),
                    blk!{
                        con! {
                            op!(id!("x"), Operator::Gt, int!(100)),
                            brk!(),
                        };
                        op! {
                            id!("x"),
                            Operator::Assign,
                            op!{
                                id!("x"),
                                Operator::Add,
                                int!(1)
                            }
                        };
                        con! {
                            op!(op!(id!("x"), Operator::Rem, int!(2)), Operator::Eq, int!(0)),
                            cnt!(),
                        };
                        cal!("print_int", id!("x"));
                    }
                }
            }
        }
    );
}

#[test]
fn function_definition() {
    let code = r#"
        fun square(x: Int): Int {
            return x * x;
        }

        print_int(square(8));
    "#;
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            fun!("square" ("x" = Type::Int) -> Type::Int, blk!{
                ret!(op!{id!("x"), Operator::Mul, id!("x")});
            })
            => blk!{cal!("print_int", cal!("square", int!(8)));}
        }
    );
}

#[test]
fn return_without_arg() {
    let code = r#"
        fun noop(): Unit {
            return;
        }

        noop();
    "#;
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            fun!("noop" () -> Type::Unit, blk!{
                ret!();
            })
            => blk!{cal!("noop");}
        }
    );
}

#[test]
fn back_to_back_fun() {
    let code = r#"
        fun noop(): Unit {
            return;
        }
        fun length(x: Int, y: Int, z: Int): Int {
            return sqrt(x*x + y*y + z*z);
        }

        noop();
    "#;
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        mdl! {
            fun!("noop" () -> Type::Unit, blk!{
                ret!();
            }),
            fun!("length" ("x" = Type::Int, "y" = Type::Int, "z" = Type::Int) -> Type::Int, blk!{
                ret!(cal!("sqrt", op!(
                    op!(
                        op!(
                            id!("x"),
                            Operator::Mul,
                            id!("x")
                        ),
                        Operator::Add,
                        op!(
                            id!("y"),
                            Operator::Mul,
                            id!("y")
                        )
                    ),
                    Operator::Add,
                    op!(
                        id!("z"),
                        Operator::Mul,
                        id!("z")
                    )
                )));
            })
            => blk!{cal!("noop");}
        }
    );
}
