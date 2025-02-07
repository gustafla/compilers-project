#![cfg(test)]

use super::*;
use crate::tokenizer;

impl PartialEq for Expression<'_> {
    fn eq(&self, other: &Self) -> bool {
        use Expression as E;

        match (self, other) {
            (E::Literal(a), E::Literal(b)) => a == b,
            (E::Identifier(a), E::Identifier(b)) => a == b,
            (E::BinaryOp(a), E::BinaryOp(b)) => {
                if a.op != b.op {
                    return false;
                }

                a.left.eq(&b.left) && a.right.eq(&b.right)
            }
            (E::Conditional(a), E::Conditional(b)) => {
                if a.condition != b.condition {
                    return false;
                }
                if a.then_expr != b.then_expr {
                    return false;
                }
                a.else_expr.eq(&b.else_expr)
            }
            _ => false,
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
fn parse_expression_precedence() {
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
                    tree: Box::new(Expression::Identifier(Identifer { name: "a" }))
                },
                op: Op::Add,
                right: Ast {
                    tree: Box::new(Expression::BinaryOp(BinaryOp {
                        left: Ast {
                            tree: Box::new(Expression::Identifier(Identifer { name: "b" }))
                        },
                        op: Op::Mul,
                        right: Ast {
                            tree: Box::new(Expression::Identifier(Identifer { name: "c" }))
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
                    tree: Box::new(Expression::Identifier(Identifer { name: "a" }))
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
                            tree: Box::new(Expression::Identifier(Identifer { name: "condition" })),
                        },
                        then_expr: Ast {
                            tree: Box::new(Expression::Identifier(Identifer { name: "a" })),
                        },
                        else_expr: Some(Ast {
                            tree: Box::new(Expression::Identifier(Identifer { name: "b" })),
                        })
                    }))
                },
                then_expr: Ast {
                    tree: Box::new(Expression::Conditional(Conditional {
                        condition: Ast {
                            tree: Box::new(Expression::Conditional(Conditional {
                                condition: Ast {
                                    tree: Box::new(Expression::Identifier(Identifer { name: "a" }))
                                },
                                then_expr: Ast {
                                    tree: Box::new(Expression::Identifier(Identifer { name: "a" }))
                                },
                                else_expr: Some(Ast {
                                    tree: Box::new(Expression::Identifier(Identifer { name: "b" }))
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
