use clap::Parser;
use compilers_project::{
    self, Config,
    ast::{Ast, Expression, Literal, Op},
    parse,
};
use std::{
    collections::{
        HashMap,
        hash_map::{Entry, OccupiedEntry},
    },
    error::Error,
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

macro_rules! fun {
    ($id: literal, $valid_args: pat, $body: expr) => {
        (
            $id,
            Value::Fun(Rc::new(|args: &[Value]| {
                let $valid_args = args else {
                    panic!(
                        "Invalid arguments to {}, expected: {}, have: {args:?}",
                        $id,
                        stringify!($valid_args)
                    );
                };
                $body
            })),
        )
    };
}

macro_rules! call {
    ($symtab: expr, $id: literal, $args: expr) => {
        match resolve_sym($symtab, $id).get() {
            Value::Fun(f) => f($args),
            _ => unreachable!("Invalid builtin call"),
        }
    };
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

impl std::fmt::Debug for Value {
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
                _ => unreachable!("Ast function call identifier is not an identifier"),
            };
            let mut args: Vec<Value> = Vec::new();
            for arg in &fn_call.arguments {
                args.push(interpret(arg, symtab));
            }
            match resolve_sym(symtab, key).get() {
                Value::Fun(f) => f(&args),
                _ => panic!("{} is not a function", key),
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
                _ => unreachable!("Ast variable declaration identifier is not an identifier"),
            };
            let value = interpret(&var.init, symtab);
            symtab.last_mut().unwrap().insert(key, value);
            Value::Unit
        }
        Expression::BinaryOp(binary_op) => {
            // Special case: Assignment
            if binary_op.op == Op::Assign {
                let key = match binary_op.left.tree.as_ref() {
                    Expression::Identifier(identifier) => identifier.name,
                    _ => panic!("= requires identifier on the lhs"),
                };
                let value = interpret(&binary_op.right, symtab);
                resolve_sym(symtab, key).insert(value);
                return Value::Unit;
            }

            // Interpret lhs
            let a = interpret(&binary_op.left, symtab);

            // Short-circuit and
            if binary_op.op == Op::And {
                if let Value::Bool(a) = a {
                    if !a {
                        return Value::Bool(false);
                    }
                }
            }

            // Short-circuit or
            if binary_op.op == Op::Or {
                if let Value::Bool(a) = a {
                    if a {
                        return Value::Bool(true);
                    }
                }
            }

            // Interpret rhs
            let b = interpret(&binary_op.right, symtab);

            // Special cases: Eq and Ne
            match (a, binary_op.op, b) {
                (Value::Int(a), Op::Eq, Value::Int(b)) => Value::Bool(a == b),
                (Value::Bool(a), Op::Eq, Value::Bool(b)) => Value::Bool(a == b),
                (Value::Unit, Op::Eq, Value::Unit) => Value::Bool(true),
                (Value::Int(a), Op::Ne, Value::Int(b)) => Value::Bool(a != b),
                (Value::Bool(a), Op::Ne, Value::Bool(b)) => Value::Bool(a != b),
                (Value::Unit, Op::Ne, Value::Unit) => Value::Bool(false),
                (_, op @ (Op::Eq | Op::Ne), _) => {
                    panic!(
                        "Invalid arguments to {op}, expected to have same type between operands",
                    );
                }
                // Delegate rest to builtins
                (a, op, b) => match op {
                    Op::Add => call!(symtab, "binary_add", &[a, b]),
                    Op::Sub => call!(symtab, "binary_sub", &[a, b]),
                    Op::Mul => call!(symtab, "binary_mul", &[a, b]),
                    Op::Div => call!(symtab, "binary_div", &[a, b]),
                    Op::Rem => call!(symtab, "binary_rem", &[a, b]),
                    Op::Leq => call!(symtab, "binary_leq", &[a, b]),
                    Op::Lt => call!(symtab, "binary_lt", &[a, b]),
                    Op::Geq => call!(symtab, "binary_geq", &[a, b]),
                    Op::Gt => call!(symtab, "binary_gt", &[a, b]),
                    Op::And => call!(symtab, "binary_and", &[a, b]),
                    Op::Or => call!(symtab, "binary_or", &[a, b]),
                    op @ Op::Not => unreachable!("Ast has {op} in a binary operation"),
                    op @ (Op::Assign | Op::Eq | Op::Ne) => {
                        unreachable!("{op:#?} should have been handled in a special case")
                    }
                },
            }
        }
        Expression::UnaryOp(unary_op) => {
            let value = interpret(&unary_op.right, symtab);
            match &unary_op.op {
                Op::Sub => call!(symtab, "unary_sub", &[value]),
                Op::Not => call!(symtab, "unary_not", &[value]),
                op => unreachable!("Ast has {op:?} in an unary operation"),
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
        fun!("print_int", [Value::Int(a)], {
            println!("{}", a);
            Value::Unit
        }),
        fun!("print_bool", [Value::Bool(a)], {
            println!("{}", a);
            Value::Unit
        }),
        fun!("read_int", [], {
            let mut buf = String::new();
            io::stdin().read_line(&mut buf).unwrap();
            Value::Int(buf.trim().parse().unwrap())
        }),
        fun!("binary_add", [Value::Int(a), Value::Int(b)], {
            Value::Int(a + b)
        }),
        fun!("binary_sub", [Value::Int(a), Value::Int(b)], {
            Value::Int(a - b)
        }),
        fun!("unary_sub", [Value::Int(a)], Value::Int(-a)),
        fun!("binary_mul", [Value::Int(a), Value::Int(b)], {
            Value::Int(a * b)
        }),
        fun!("binary_div", [Value::Int(a), Value::Int(b)], {
            Value::Int(a / b)
        }),
        fun!("binary_rem", [Value::Int(a), Value::Int(b)], {
            Value::Int(a % b)
        }),
        fun!("binary_lt", [Value::Int(a), Value::Int(b)], {
            Value::Bool(a < b)
        }),
        fun!("binary_leq", [Value::Int(a), Value::Int(b)], {
            Value::Bool(a <= b)
        }),
        fun!("binary_gt", [Value::Int(a), Value::Int(b)], {
            Value::Bool(a > b)
        }),
        fun!("binary_geq", [Value::Int(a), Value::Int(b)], {
            Value::Bool(a >= b)
        }),
        fun!("binary_and", [Value::Bool(a), Value::Bool(b)], {
            Value::Bool(*a && *b)
        }),
        fun!("binary_or", [Value::Bool(a), Value::Bool(b)], {
            Value::Bool(*a || *b)
        }),
        fun!("unary_not", [Value::Bool(a)], Value::Bool(!a)),
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
        _ => 0,
    };

    std::process::exit(code);
}
