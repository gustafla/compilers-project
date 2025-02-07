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
                a.left.eq(&b.left) && a.right.eq(&b.right)
            }
            (E::BinaryOp(..), _) => false,
            (E::UnaryOp(a), E::UnaryOp(b)) => {
                if a.op != b.op {
                    return false;
                }
                a.right.eq(&b.right)
            }
            (E::UnaryOp(..), _) => false,
            (E::Conditional(a), E::Conditional(b)) => {
                if a.condition != b.condition {
                    return false;
                }
                if a.then_expr != b.then_expr {
                    return false;
                }
                a.else_expr.eq(&b.else_expr)
            }
            (E::Conditional(..), _) => false,
            (E::FnCall(a), E::FnCall(b)) => {
                if a.function != b.function {
                    return false;
                }
                a.arguments.eq(&b.arguments)
            }
            (E::FnCall(..), _) => false,
        }
    }
}

impl PartialEq for Ast<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.tree.eq(&other.tree)
    }
}

#[test]
fn parse_expression_basic() {
    let code = "1 + 1";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        Ast {
            tree: Box::new(Expression::BinaryOp(BinaryOp {
                left: Ast {
                    tree: Box::new(Expression::Literal(Literal::Int(1)))
                },
                op: Op::Add,
                right: Ast {
                    tree: Box::new(Expression::Literal(Literal::Int(1)))
                },
            }))
        }
    );
}

#[test]
fn parse_expression_error() {
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
        Ast {
            tree: Box::new(Expression::BinaryOp(BinaryOp {
                left: Ast {
                    tree: Box::new(Expression::BinaryOp(BinaryOp {
                        left: Ast {
                            tree: Box::new(Expression::Literal(Literal::Int(1)))
                        },
                        op: Op::Add,
                        right: Ast {
                            tree: Box::new(Expression::Literal(Literal::Int(2)))
                        },
                    }))
                },
                op: Op::Add,
                right: Ast {
                    tree: Box::new(Expression::Literal(Literal::Int(3)))
                },
            }))
        }
    );

    let code = "1+2*3";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        Ast {
            tree: Box::new(Expression::BinaryOp(BinaryOp {
                left: Ast {
                    tree: Box::new(Expression::Literal(Literal::Int(1)))
                },
                op: Op::Add,
                right: Ast {
                    tree: Box::new(Expression::BinaryOp(BinaryOp {
                        left: Ast {
                            tree: Box::new(Expression::Literal(Literal::Int(2)))
                        },
                        op: Op::Mul,
                        right: Ast {
                            tree: Box::new(Expression::Literal(Literal::Int(3)))
                        },
                    }))
                },
            }))
        }
    );
}

#[test]
fn parse_expression_paren() {
    let code = "1+(2+3)";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        Ast {
            tree: Box::new(Expression::BinaryOp(BinaryOp {
                left: Ast {
                    tree: Box::new(Expression::Literal(Literal::Int(1)))
                },
                op: Op::Add,
                right: Ast {
                    tree: Box::new(Expression::BinaryOp(BinaryOp {
                        left: Ast {
                            tree: Box::new(Expression::Literal(Literal::Int(2)))
                        },
                        op: Op::Add,
                        right: Ast {
                            tree: Box::new(Expression::Literal(Literal::Int(3)))
                        },
                    }))
                },
            }))
        }
    );
}

#[test]
fn parse_expression_with_identifiers() {
    let code = "a+b*c";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        Ast {
            tree: Box::new(Expression::BinaryOp(BinaryOp {
                left: Ast {
                    tree: Box::new(Expression::Identifier(Identifier { name: "a" }))
                },
                op: Op::Add,
                right: Ast {
                    tree: Box::new(Expression::BinaryOp(BinaryOp {
                        left: Ast {
                            tree: Box::new(Expression::Identifier(Identifier { name: "b" }))
                        },
                        op: Op::Mul,
                        right: Ast {
                            tree: Box::new(Expression::Identifier(Identifier { name: "c" }))
                        },
                    }))
                },
            }))
        }
    );
}

#[test]
fn parse_expression_with_conditional() {
    let code = "if a then 1 + 1";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        Ast {
            tree: Box::new(Expression::Conditional(Conditional {
                condition: Ast {
                    tree: Box::new(Expression::Identifier(Identifier { name: "a" }))
                },
                then_expr: Ast {
                    tree: Box::new(Expression::BinaryOp(BinaryOp {
                        left: Ast {
                            tree: Box::new(Expression::Literal(Literal::Int(1)))
                        },
                        op: Op::Add,
                        right: Ast {
                            tree: Box::new(Expression::Literal(Literal::Int(1)))
                        }
                    }))
                },
                else_expr: None,
            }))
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
        Ast {
            tree: Box::new(Expression::BinaryOp(BinaryOp {
                left: Ast {
                    tree: Box::new(Expression::Literal(Literal::Int(1)))
                },
                op: Op::Add,
                right: Ast {
                    tree: Box::new(Expression::Conditional(Conditional {
                        condition: Ast {
                            tree: Box::new(Expression::Literal(Literal::Bool(true)))
                        },
                        then_expr: Ast {
                            tree: Box::new(Expression::Literal(Literal::Int(2)))
                        },
                        else_expr: Some(Ast {
                            tree: Box::new(Expression::Literal(Literal::Int(3)))
                        }),
                    }))
                }
            }))
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
        Ast {
            tree: Box::new(Expression::Conditional(Conditional {
                condition: Ast {
                    tree: Box::new(Expression::Conditional(Conditional {
                        condition: Ast {
                            tree: Box::new(Expression::Identifier(Identifier {
                                name: "condition"
                            })),
                        },
                        then_expr: Ast {
                            tree: Box::new(Expression::Identifier(Identifier { name: "a" })),
                        },
                        else_expr: Some(Ast {
                            tree: Box::new(Expression::Identifier(Identifier { name: "b" })),
                        })
                    }))
                },
                then_expr: Ast {
                    tree: Box::new(Expression::Conditional(Conditional {
                        condition: Ast {
                            tree: Box::new(Expression::Conditional(Conditional {
                                condition: Ast {
                                    tree: Box::new(Expression::Identifier(Identifier {
                                        name: "a"
                                    }))
                                },
                                then_expr: Ast {
                                    tree: Box::new(Expression::Identifier(Identifier {
                                        name: "a"
                                    }))
                                },
                                else_expr: Some(Ast {
                                    tree: Box::new(Expression::Identifier(Identifier {
                                        name: "b"
                                    }))
                                })
                            }))
                        },
                        then_expr: Ast {
                            tree: Box::new(Expression::Literal(Literal::Int(32)))
                        },
                        else_expr: Some(Ast {
                            tree: Box::new(Expression::Literal(Literal::Int(42)))
                        }),
                    }))
                },
                else_expr: None,
            }))
        }
    );
}

#[test]
fn parse_expression_with_fn_call() {
    let code = "5 - add(1, 1)";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        Ast {
            tree: Box::new(Expression::BinaryOp(BinaryOp {
                left: Ast {
                    tree: Box::new(Expression::Literal(Literal::Int(5)))
                },
                op: Op::Sub,
                right: Ast {
                    tree: Box::new(Expression::FnCall(FnCall {
                        function: Identifier { name: "add" },
                        arguments: vec![
                            Expression::Literal(Literal::Int(1)),
                            Expression::Literal(Literal::Int(1)),
                        ],
                    }))
                }
            }))
        }
    )
}

#[test]
fn parse_expression_with_nested_fn_call() {
    let code = r#"printf("Error at packet %d: %s (context: %s)\n", packet, SDL_GetError(), describe_context(context))"#;
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        Ast {
            tree: Box::new(Expression::FnCall(FnCall {
                function: Identifier { name: "printf" },
                arguments: vec![
                    Expression::Literal(Literal::Str(r#"Error at packet %d: %s (context: %s)\n"#)),
                    Expression::Identifier(Identifier { name: "packet" }),
                    Expression::FnCall(FnCall {
                        function: Identifier {
                            name: "SDL_GetError"
                        },
                        arguments: vec![]
                    }),
                    Expression::FnCall(FnCall {
                        function: Identifier {
                            name: "describe_context"
                        },
                        arguments: vec![Expression::Identifier(Identifier { name: "context" })],
                    })
                ],
            }))
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
        Ast {
            tree: Box::new(Expression::BinaryOp(BinaryOp {
                left: Ast {
                    tree: Box::new(Expression::Identifier(Identifier { name: "a" }))
                },
                op: Op::Assign,
                right: Ast {
                    tree: Box::new(Expression::BinaryOp(BinaryOp {
                        left: Ast {
                            tree: Box::new(Expression::Identifier(Identifier { name: "b" }))
                        },
                        op: Op::Assign,
                        right: Ast {
                            tree: Box::new(Expression::Identifier(Identifier { name: "c" }))
                        }
                    }))
                },
            }))
        }
    );
}

#[test]
fn parse_expression_complex() {
    let code = "if true or false and foo then a = b = 1+2*sqrt(2)";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse(&tokens).unwrap();
    assert_eq!(
        expression,
        Ast {
            tree: Box::new(Expression::Conditional(Conditional {
                condition: Ast {
                    tree: Box::new(Expression::BinaryOp(BinaryOp {
                        left: Ast {
                            tree: Box::new(Expression::Literal(Literal::Bool(true)))
                        },
                        op: Op::Or,
                        right: Ast {
                            tree: Box::new(Expression::BinaryOp(BinaryOp {
                                left: Ast {
                                    tree: Box::new(Expression::Literal(Literal::Bool(false)))
                                },
                                op: Op::And,
                                right: Ast {
                                    tree: Box::new(Expression::Identifier(Identifier {
                                        name: "foo"
                                    }))
                                }
                            }))
                        },
                    }))
                },
                then_expr: Ast {
                    tree: Box::new(Expression::BinaryOp(BinaryOp {
                        left: Ast {
                            tree: Box::new(Expression::Identifier(Identifier { name: "a" })),
                        },
                        op: Op::Assign,
                        right: Ast {
                            tree: Box::new(Expression::BinaryOp(BinaryOp {
                                left: Ast {
                                    tree: Box::new(Expression::Identifier(Identifier {
                                        name: "b"
                                    })),
                                },
                                op: Op::Assign,
                                right: Ast {
                                    tree: Box::new(Expression::BinaryOp(BinaryOp {
                                        left: Ast {
                                            tree: Box::new(Expression::Literal(Literal::Int(1)))
                                        },
                                        op: Op::Add,
                                        right: Ast {
                                            tree: Box::new(Expression::BinaryOp(BinaryOp {
                                                left: Ast {
                                                    tree: Box::new(Expression::Literal(
                                                        Literal::Int(2)
                                                    ))
                                                },
                                                op: Op::Mul,
                                                right: Ast {
                                                    tree: Box::new(Expression::FnCall(FnCall {
                                                        function: Identifier { name: "sqrt" },
                                                        arguments: vec![Expression::Literal(
                                                            Literal::Int(2)
                                                        )]
                                                    }))
                                                }
                                            }))
                                        }
                                    }))
                                },
                            })),
                        },
                    })),
                },
                else_expr: None,
            }))
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
        Ast {
            tree: Box::new(Expression::Conditional(Conditional {
                condition: Ast {
                    tree: Box::new(Expression::UnaryOp(UnaryOp {
                        op: Op::Not,
                        right: Ast {
                            tree: Box::new(Expression::Identifier(Identifier { name: "foo" }))
                        }
                    }))
                },
                then_expr: Ast {
                    tree: Box::new(Expression::UnaryOp(UnaryOp {
                        op: Op::Sub,
                        right: Ast {
                            tree: Box::new(Expression::Literal(Literal::Int(1)))
                        }
                    }))
                },
                else_expr: Some(Ast {
                    tree: Box::new(Expression::UnaryOp(UnaryOp {
                        op: Op::Sub,
                        right: Ast {
                            tree: Box::new(Expression::Literal(Literal::Int(2)))
                        }
                    }))
                }),
            }))
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
        Ast {
            tree: Box::new(Expression::Conditional(Conditional {
                condition: Ast {
                    tree: Box::new(Expression::UnaryOp(UnaryOp {
                        op: Op::Not,
                        right: Ast {
                            tree: Box::new(Expression::UnaryOp(UnaryOp {
                                op: Op::Not,
                                right: Ast {
                                    tree: Box::new(Expression::UnaryOp(UnaryOp {
                                        op: Op::Not,
                                        right: Ast {
                                            tree: Box::new(Expression::UnaryOp(UnaryOp {
                                                op: Op::Not,
                                                right: Ast {
                                                    tree: Box::new(Expression::Identifier(
                                                        Identifier { name: "foo" }
                                                    ))
                                                }
                                            }))
                                        }
                                    }))
                                }
                            }))
                        }
                    }))
                },
                then_expr: Ast {
                    tree: Box::new(Expression::UnaryOp(UnaryOp {
                        op: Op::Sub,
                        right: Ast {
                            tree: Box::new(Expression::UnaryOp(UnaryOp {
                                op: Op::Sub,
                                right: Ast {
                                    tree: Box::new(Expression::Literal(Literal::Int(1)))
                                }
                            }))
                        }
                    }))
                },
                else_expr: Some(Ast {
                    tree: Box::new(Expression::UnaryOp(UnaryOp {
                        op: Op::Sub,
                        right: Ast {
                            tree: Box::new(Expression::UnaryOp(UnaryOp {
                                op: Op::Sub,
                                right: Ast {
                                    tree: Box::new(Expression::UnaryOp(UnaryOp {
                                        op: Op::Sub,
                                        right: Ast {
                                            tree: Box::new(Expression::UnaryOp(UnaryOp {
                                                op: Op::Sub,
                                                right: Ast {
                                                    tree: Box::new(Expression::UnaryOp(UnaryOp {
                                                        op: Op::Sub,
                                                        right: Ast {
                                                            tree: Box::new(Expression::Literal(
                                                                Literal::Int(2)
                                                            ))
                                                        }
                                                    }))
                                                }
                                            }))
                                        }
                                    }))
                                }
                            }))
                        }
                    }))
                }),
            }))
        }
    )
}

#[test]
fn parse_expression_precedence_complex() {
    todo!()
}
