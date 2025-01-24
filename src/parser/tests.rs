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
            _ => false,
        }
    }
}

#[test]
fn parse_expression_basic() {
    let code = "1 + 1";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse_expression(&tokens).unwrap();
    assert!(matches!(
        expression,
        Expression::BinaryOp(BinaryOp {
            left: _,
            op: Op::Add,
            right: _
        })
    ));
}

#[test]
fn parse_expression_error() {
    let code = "a + b c";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse_expression(&tokens);
    assert!(matches!(expression, Err(..)));
}

#[test]
fn parse_expression_precedence() {
    let code = "1+2+3";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse_expression(&tokens).unwrap();
    assert_eq!(
        expression,
        Expression::BinaryOp(BinaryOp {
            left: Box::new(Expression::BinaryOp(BinaryOp {
                left: Box::new(Expression::Literal(Literal::Int(1))),
                op: Op::Add,
                right: Box::new(Expression::Literal(Literal::Int(2))),
            })),
            op: Op::Add,
            right: Box::new(Expression::Literal(Literal::Int(3))),
        })
    );

    let code = "1+2*3";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse_expression(&tokens).unwrap();
    assert_eq!(
        expression,
        Expression::BinaryOp(BinaryOp {
            left: Box::new(Expression::Literal(Literal::Int(1))),
            op: Op::Add,
            right: Box::new(Expression::BinaryOp(BinaryOp {
                left: Box::new(Expression::Literal(Literal::Int(2))),
                op: Op::Mul,
                right: Box::new(Expression::Literal(Literal::Int(3))),
            })),
        })
    );
}

#[test]
fn parse_expression_with_literals() {
    let code = "a+b*c";
    let tokens = tokenizer::tokenize(code).unwrap();
    let expression = parse_expression(&tokens).unwrap();
    assert_eq!(
        expression,
        Expression::BinaryOp(BinaryOp {
            left: Box::new(Expression::Identifier(Identifer { name: "a" })),
            op: Op::Add,
            right: Box::new(Expression::BinaryOp(BinaryOp {
                left: Box::new(Expression::Identifier(Identifer { name: "b" })),
                op: Op::Mul,
                right: Box::new(Expression::Identifier(Identifer { name: "c" })),
            })),
        })
    );
}
