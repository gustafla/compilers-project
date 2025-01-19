#![cfg(test)]

use super::*;
use crate::tokenizer;

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
