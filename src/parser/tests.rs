#![cfg(test)]

use super::*;
use crate::tokenizer;

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
        }
    }
}

impl PartialEq for Ast<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.tree == other.tree
    }
}

#[test]
fn parse_expression_basic() {
    let code = "1 + 1";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(expression, ast! {op!{int!(1), Op::Add, int!(1)}});
}

#[test]
fn parse_expression_error0() {
    let code = "a + b c";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens);
    assert!(matches!(expression, Err(..)));
}

#[test]
fn parse_expression_precedence_basic() {
    let code = "1+2+3";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        ast! {
            op! {
                op! {
                    int!(1),
                    Op::Add,
                    int!(2),
                },
                Op::Add,
                int!(3),
            },
        },
    );

    let code = "1+2*3";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        ast! {
            op! {
                int!(1),
                Op::Add,
                op! {
                    int!(2),
                    Op::Mul,
                    int!(3),
                },
            },
        },
    );
}

#[test]
fn parse_expression_paren() {
    let code = "1+(2+3)";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        ast! {
            op! {
                int!(1),
                Op::Add,
                op! {
                    int!(2),
                    Op::Add,
                    int!(3),
                },
            },
        },
    );
}

#[test]
fn parse_expression_with_identifiers() {
    let code = "a+b*c";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        ast! {
            op! {
                id!("a"),
                Op::Add,
                op! {
                    id!("b"),
                    Op::Mul,
                    id!("c"),
                },
            },
        },
    );
}

#[test]
fn parse_expression_with_conditional() {
    let code = "if a then 1 + 1";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        ast! {
            con! {
                id!("a"),
                op! {
                    int!(1),
                    Op::Add,
                    int!(1)
                }
            }
        }
    );
}

#[test]
fn parse_binary_op_with_conditional() {
    let code = "1 + if true then 2 else 3";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        ast! {
            op! {
                int!(1),
                Op::Add,
                con! {
                    tru!(),
                    int!(2),
                    int!(3),
                },
            },
        },
    );
}

#[test]
fn parse_expression_with_nested_conditional() {
    let code = "if if condition then a else b then if if a then a else b then 32 else 42";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        ast! {
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
            },
        },
    );
}

#[test]
fn parse_expression_with_fn_call() {
    let code = "5 - add(1, 1)";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        ast! {
            op! {
                int!(5),
                Op::Sub,
                fun!("add", int!(1), int!(1)),
            }
        }
    );
}

#[test]
fn parse_expression_with_nested_fn_call() {
    let code = r#"printf("Error at packet %d: %s (context: %s)\n", packet, SDL_GetError(), describe_context(context))"#;
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        ast! {
            fun!(
                "printf",
                st!(r#"Error at packet %d: %s (context: %s)\n"#),
                id!("packet"),
                fun!("SDL_GetError"),
                fun!("describe_context", id!("context"))
            ),
        }
    )
}

#[test]
fn parse_expression_assignment() {
    let code = "a = b = c";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        ast! {
            op! {
                id!("a"),
                Op::Assign,
                op! {
                    id!("b"),
                    Op::Assign,
                    id!("c"),
                },
            },
        },
    );
}

#[test]
fn parse_expression_complex() {
    let code = "if true or false and foo then a = b = 1+2*sqrt(2)";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        ast! {
            con! {
                op! {
                    tru!(),
                    Op::Or,
                    op! {
                        fal!(),
                        Op::And,
                        id!("foo"),
                    },
                },
                op! {
                    id!("a"),
                    Op::Assign,
                    op! {
                        id!("b"),
                        Op::Assign,
                        op! {
                            int!(1),
                            Op::Add,
                            op! {
                                int!(2),
                                Op::Mul,
                                fun!("sqrt", int!(2)),
                            }
                        }
                    }
                }
            }
        }
    );
}

#[test]
fn parse_expression_unary() {
    let code = "if not foo then -1 else -2";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        ast! {
            con! {
                op!{Op::Not, id!("foo")},
                op!{Op::Sub, int!(1)},
                op!{Op::Sub, int!(2)}
            }
        }
    )
}

#[test]
fn parse_expression_nested_unary() {
    let code = "if not not not not foo then --1 else -----2";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        ast! {
            con! {
                op!{Op::Not, op!{Op::Not, op!{Op::Not, op!{Op::Not, id!("foo")}}}},
                op!{Op::Sub, op!{Op::Sub, int!(1)}},
                op!{Op::Sub, op!{Op::Sub, op!{Op::Sub, op!{Op::Sub, op!{Op::Sub, int!(2)}}}}},
            }
        }
    );
}

#[test]
fn parse_expression_precedence_complex() {
    let code =
        "- if true then a = 1 or 1 and 1 == 1 != 1 < 1 <= 1 > 1 >= 1 + 1 - 1 * 1 / 1 % -1 = x else xd";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        ast! {
            op!{Op::Sub,
                con! {
                    tru!{},
                    op! {
                        id!("a"),
                        Op::Assign,
                        op! {
                            op! {
                                int!(1),
                                Op::Or,
                                op! {
                                    int!(1),
                                    Op::And,
                                    op! {
                                        op! {
                                            int!(1),
                                            Op::Eq,
                                            int!(1),
                                        },
                                        Op::Ne,
                                        op ! {
                                            op! {
                                                op! {
                                                    op! {
                                                        int!(1),
                                                        Op::Lt,
                                                        int!(1),
                                                    },
                                                    Op::Leq,
                                                    int!(1),
                                                },
                                                Op::Gt,
                                                int!(1),
                                            },
                                            Op::Geq,
                                            op! {
                                                op! {
                                                    int!(1),
                                                    Op::Add,
                                                    int!(1),
                                                },
                                                Op::Sub,
                                                op! {
                                                    op! {
                                                        op! {
                                                            int!(1),
                                                            Op::Mul,
                                                            int!(1),
                                                        },
                                                        Op::Div,
                                                        int!(1),
                                                    },
                                                    Op::Rem,
                                                    op! {
                                                        Op::Sub,
                                                        int!(1),
                                                    }
                                                }
                                            }
                                        }
                                    }
                                },
                            },
                            Op::Assign,
                            id!("x")
                        }
                    },
                    id!("xd")
                },
            }
        }
    );
}

#[test]
fn parse_expression_error1() {
    let code = "- if true then a = y = x not else xd";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens);
    assert!(expression.is_err());
}

#[test]
fn parse_expression_error2() {
    let code = "+ if true then a = y = x else xd";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens);
    assert!(expression.is_err());
}

#[test]
fn parse_expression_error3() {
    let code = "if true then a = y = x else xd -";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens);
    assert!(expression.is_err());
}

#[test]
fn parse_expression_error4() {
    let code = "if true then a++ else xd";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens);
    assert!(expression.is_err());
}

#[test]
fn parse_expression_with_block() {
    let code = "if ok then {fn1(); fn2(); exit(0);} else {exit(1)}";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        ast! {
            con! {
                id!("ok"),
                blk! {
                    fun!("fn1");
                    fun!("fn2");
                    fun!("exit", int!(0));
                },
                blk! {
                    , fun!("exit", int!(1))
                }
            }
        }
    )
}
