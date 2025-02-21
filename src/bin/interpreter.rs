use clap::Parser;
use compilers_project::{
    self,
    ast::{Expression, Literal, Op},
    parse, Ast, Config,
};
use std::{
    cmp::Ordering,
    error::Error,
    fs::{self},
    io::{self},
    ops::{Add, Div, Mul, Rem, Sub},
    path::{Path, PathBuf},
};

macro_rules! err {
    ($e: expr) => {{
        ::compilers_project::print_error($e);
        ::std::process::exit(1);
    }};
}

#[derive(Parser, Debug)]
#[command(version, about = "Compilers Course Project - Interpreter")]
struct Cli {
    #[command(flatten)]
    config: Config,
    #[clap(required = true)]
    files: Vec<PathBuf>,
}

#[derive(PartialEq, Eq)]
enum Value {
    Int(i64),
    Bool(bool),
    None,
}

impl From<&Literal<'_>> for Value {
    fn from(value: &Literal) -> Self {
        match value {
            Literal::Int(i) => Value::Int(*i),
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Str(_) => unimplemented!(),
        }
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            _ => panic!("+ requires integer operands"),
        }
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
            _ => panic!("- requires integer operands"),
        }
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
            _ => panic!("* requires integer operands"),
        }
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
            _ => panic!("/ requires integer operands"),
        }
    }
}

impl Rem for Value {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
            _ => panic!("% requires integer operands"),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a.partial_cmp(b),
            (Value::Bool(a), Value::Bool(b)) => a.partial_cmp(b),
            (Value::None, Value::None) => Some(Ordering::Equal),
            (Value::Int(_), Value::Bool(_)) => panic!("Can't compare int with bool"),
            (Value::Int(_), Value::None) => panic!("Can't compare int with none"),
            (Value::Bool(_), Value::Int(_)) => panic!("Can't compare bool with int"),
            (Value::Bool(_), Value::None) => panic!("Can't compare bool with none"),
            (Value::None, Value::Int(_)) => panic!("Can't compare none with int"),
            (Value::None, Value::Bool(_)) => panic!("Can't compare none with bool"),
        }
    }
}

fn interpret(ast: &Ast) -> Value {
    match ast.tree.as_ref() {
        Expression::Literal(literal) => literal.into(),
        Expression::Identifier(identifier) => todo!(),
        Expression::Conditional(conditional) => {
            let Value::Bool(condition) = interpret(&conditional.condition) else {
                panic!("if requires a boolean expression");
            };
            if condition {
                interpret(&conditional.then_expr)
            } else if let Some(else_expr) = &conditional.else_expr {
                interpret(else_expr)
            } else {
                Value::None
            }
        }
        Expression::FnCall(fn_call) => todo!(),
        Expression::Block(block) => {
            for expr in &block.expressions {
                interpret(expr);
            }
            if let Some(expr) = &block.result {
                interpret(expr)
            } else {
                Value::None
            }
        }
        Expression::Var(var) => todo!(),
        Expression::BinaryOp(binary_op) => {
            let a = interpret(&binary_op.left);
            let b = interpret(&binary_op.right);
            match binary_op.op {
                Op::Add => a + b,
                Op::Sub => a - b,
                Op::Mul => a * b,
                Op::Div => a / b,
                Op::Rem => a % b,
                Op::Eq => Value::Bool(a == b),
                Op::Ne => Value::Bool(a != b),
                Op::Leq => Value::Bool(a <= b),
                Op::Lt => Value::Bool(a < b),
                Op::Geq => Value::Bool(a >= b),
                Op::Gt => Value::Bool(a > b),
                Op::And => match (a, b) {
                    (Value::Bool(a), Value::Bool(b)) => Value::Bool(a && b),
                    _ => panic!("&& requires boolean operands"),
                },
                Op::Or => match (a, b) {
                    (Value::Bool(a), Value::Bool(b)) => Value::Bool(a || b),
                    _ => panic!("|| requires boolean operands"),
                },
                Op::Not => unreachable!(),
                Op::Assign => todo!(),
            }
        }
        Expression::UnaryOp(unary_op) => {
            let value = interpret(&unary_op.right);
            match (&unary_op.op, value) {
                (Op::Not, Value::Bool(a)) => Value::Bool(!a),
                (Op::Not, _) => panic!("not requires a boolean operand"),
                (Op::Sub, Value::Int(a)) => Value::Int(-a),
                (Op::Sub, _) => panic!("- requires an integer operand"),
                _ => unreachable!(),
            }
        }
        Expression::While(node) => {
            while match interpret(&node.condition) {
                Value::Bool(condition) => condition,
                _ => panic!("while requires a boolean condition"),
            } {
                interpret(&node.do_expr);
            }
            Value::None
        }
    }
}

fn main() {
    let mut cli = Cli::parse();

    let input = {
        if cli.files.len() > 1 {
            let error: Box<dyn Error> = "Only one source file is currently supported".into();
            err!(error.as_ref());
        }
        #[allow(clippy::unwrap_used, reason = "Required argument can't be empty")]
        cli.files.pop().unwrap()
    };

    let code = match if input == Path::new("-") {
        io::read_to_string(io::stdin().lock())
    } else {
        fs::read_to_string(&input)
    } {
        Ok(code) => code,
        Err(ref e) => err!(e),
    };

    let ast = match parse(&code, &cli.config) {
        Ok(ast) => ast,
        Err(ref e) => err!(e),
    };

    let code = match interpret(&ast) {
        Value::Int(i) => i.clamp(0, 255) as i32,
        Value::Bool(b) => {
            if b {
                0
            } else {
                1
            }
        }
        Value::None => 0,
    };

    std::process::exit(code);
}
