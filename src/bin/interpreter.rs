use clap::Parser;
use compilers_project::{
    self,
    ast::{Expression, Literal, Op},
    parse, Ast, Config,
};
use std::{
    collections::{
        hash_map::{Entry, OccupiedEntry},
        HashMap,
    },
    error::Error,
    fmt::Display,
    fs::{self},
    io::{self},
    path::{Path, PathBuf},
    rc::Rc,
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

type Fun = Rc<dyn Fn(&[Value]) -> Value>;

#[derive(Clone)]
enum Value {
    Int(i64),
    Bool(bool),
    Unit,
    Fun(Fun),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(a) => write!(f, "{}", a),
            Value::Bool(a) => write!(f, "{}", a),
            Value::Unit => write!(f, "<unit>"),
            Value::Fun(_) => write!(f, "<function>"),
        }
    }
}

impl From<&Literal<'_>> for Value {
    fn from(value: &Literal) -> Self {
        match value {
            Literal::Int(i) => Value::Int(*i),
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Str(_) => unimplemented!("String literals are not supported"),
        }
    }
}

type SymbolTable<'a> = HashMap<&'a str, Value>;

fn resolve_sym<'a, 'b>(
    symtab: &'b mut [SymbolTable<'a>],
    key: &'a str,
) -> OccupiedEntry<'b, &'a str, Value> {
    for table in symtab.iter_mut().rev() {
        if let Entry::Occupied(entry) = table.entry(key) {
            return entry;
        }
    }
    panic!("unresolved identifier: {}", key);
}

fn interpret<'a>(ast: &Ast<'a>, symtab: &mut Vec<SymbolTable<'a>>) -> Value {
    match ast.tree.as_ref() {
        Expression::Literal(literal) => literal.into(),
        Expression::Identifier(identifier) => resolve_sym(symtab, identifier.name).get().clone(),
        Expression::Conditional(conditional) => {
            let Value::Bool(condition) = interpret(&conditional.condition, symtab) else {
                panic!("if requires a boolean expression");
            };
            if condition {
                interpret(&conditional.then_expr, symtab)
            } else if let Some(else_expr) = &conditional.else_expr {
                interpret(else_expr, symtab)
            } else {
                Value::Unit
            }
        }
        Expression::FnCall(fn_call) => {
            let key = match fn_call.function.tree.as_ref() {
                Expression::Identifier(identifier) => identifier.name,
                _ => unreachable!(),
            };
            let mut args: Vec<Value> = Vec::new();
            for arg in &fn_call.arguments {
                args.push(interpret(arg, symtab));
            }
            match resolve_sym(symtab, key).get() {
                Value::Fun(f) => f(&args),
                _ => panic!(""),
            }
        }
        Expression::Block(block) => {
            symtab.push(HashMap::new());
            for expr in &block.expressions {
                interpret(expr, symtab);
            }
            let result = if let Some(expr) = &block.result {
                interpret(expr, symtab)
            } else {
                Value::Unit
            };
            symtab.pop();
            result
        }
        Expression::Var(var) => {
            let key = match var.id.tree.as_ref() {
                Expression::Identifier(identifier) => identifier.name,
                _ => unreachable!(),
            };
            let value = interpret(&var.init, symtab);
            symtab.last_mut().unwrap().insert(key, value);
            Value::Unit
        }
        Expression::BinaryOp(binary_op) => {
            if binary_op.op == Op::Assign {
                let key = match binary_op.left.tree.as_ref() {
                    Expression::Identifier(identifier) => identifier.name,
                    _ => panic!("= requires identifier on the lhs"),
                };
                let value = interpret(&binary_op.right, symtab);
                resolve_sym(symtab, key).insert(value);
                return Value::Unit;
            }

            let a = interpret(&binary_op.left, symtab);
            let b = interpret(&binary_op.right, symtab);
            match (a, &binary_op.op, b) {
                (Value::Int(a), Op::Add, Value::Int(b)) => Value::Int(a + b),
                (Value::Int(a), Op::Sub, Value::Int(b)) => Value::Int(a - b),
                (Value::Int(a), Op::Mul, Value::Int(b)) => Value::Int(a * b),
                (Value::Int(a), Op::Div, Value::Int(b)) => Value::Int(a / b),
                (Value::Int(a), Op::Rem, Value::Int(b)) => Value::Int(a % b),
                (Value::Int(a), Op::Eq, Value::Int(b)) => Value::Bool(a == b),
                (Value::Bool(a), Op::Eq, Value::Bool(b)) => Value::Bool(a == b),
                (Value::Int(a), Op::Ne, Value::Int(b)) => Value::Bool(a != b),
                (Value::Bool(a), Op::Ne, Value::Bool(b)) => Value::Bool(a != b),
                (Value::Int(a), Op::Leq, Value::Int(b)) => Value::Bool(a <= b),
                (Value::Bool(a), Op::Leq, Value::Bool(b)) => Value::Bool(a <= b),
                (Value::Int(a), Op::Lt, Value::Int(b)) => Value::Bool(a < b),
                (Value::Bool(a), Op::Lt, Value::Bool(b)) => Value::Bool(!a & b),
                (Value::Int(a), Op::Geq, Value::Int(b)) => Value::Bool(a >= b),
                (Value::Bool(a), Op::Geq, Value::Bool(b)) => Value::Bool(a >= b),
                (Value::Int(a), Op::Gt, Value::Int(b)) => Value::Bool(a > b),
                (Value::Bool(a), Op::Gt, Value::Bool(b)) => Value::Bool(a & !b),
                (Value::Bool(a), Op::And, Value::Bool(b)) => Value::Bool(a && b),
                (Value::Bool(a), Op::Or, Value::Bool(b)) => Value::Bool(a || b),
                (_, Op::Assign, _) => unreachable!(),
                (_, Op::Not, _) => unreachable!(),
                (a, op, b) => panic!("Unsupported operation: {a} {op} {b}"),
            }
        }
        Expression::UnaryOp(unary_op) => {
            let value = interpret(&unary_op.right, symtab);
            match (&unary_op.op, value) {
                (Op::Not, Value::Bool(a)) => Value::Bool(!a),
                (Op::Not, _) => panic!("not requires a boolean operand"),
                (Op::Sub, Value::Int(a)) => Value::Int(-a),
                (Op::Sub, _) => panic!("- requires an integer operand"),
                _ => unreachable!(),
            }
        }
        Expression::While(node) => {
            while match interpret(&node.condition, symtab) {
                Value::Bool(condition) => condition,
                _ => panic!("while requires a boolean condition"),
            } {
                interpret(&node.do_expr, symtab);
            }
            Value::Unit
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

    let mut symtab = vec![HashMap::from([
        (
            "print_int",
            Value::Fun(Rc::new(|args: &[Value]| {
                match args {
                    [Value::Int(a)] => {
                        println!("{}", a)
                    }
                    _ => panic!("Incorrect arguments to print_int"),
                }
                Value::Unit
            })),
        ),
        (
            "print_bool",
            Value::Fun(Rc::new(|args: &[Value]| {
                match args {
                    [Value::Bool(a)] => {
                        println!("{}", a)
                    }
                    _ => panic!("Incorrect arguments to print_bool"),
                }
                Value::Unit
            })),
        ),
        (
            "read_int",
            Value::Fun(Rc::new(|args: &[Value]| {
                match args {
                    [] => {}
                    _ => panic!("Incorrect arguments to read_int"),
                }
                let mut buf = String::new();
                io::stdin().read_line(&mut buf).unwrap();
                Value::Int(buf.trim().parse().unwrap())
            })),
        ),
    ])];

    let code = match interpret(&ast, &mut symtab) {
        Value::Int(i) => i.clamp(0, 255) as i32,
        Value::Bool(b) => {
            if b {
                0
            } else {
                1
            }
        }
        Value::Unit => 0,
        Value::Fun(_) => unreachable!(),
    };

    std::process::exit(code);
}
