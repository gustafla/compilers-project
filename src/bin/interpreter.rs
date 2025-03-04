use clap::Parser;
use compilers_project::{
    self, Config, SymbolTable,
    ast::{Ast, Expression, Literal, Operator, op::Ary},
    parse,
};
use std::{
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
    ($id: expr, $valid_args: pat, $body: expr) => {
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
    ($symtab: expr, $id: expr, $args: expr) => {
        match resolve!($symtab, $id).get() {
            Value::Fun(f) => f($args),
            _ => unreachable!("Invalid builtin call"),
        }
    };
}

macro_rules! resolve {
    ($symtab: expr, $key: expr) => {
        match $symtab.resolve($key) {
            Ok(value) => value,
            Err(e) => panic!("{e:?}"),
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
            Literal::Int(i) => Value::Int((*i).try_into().expect("Integer does not fit into i64")),
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Str(_) => unimplemented!("String literals are not supported"),
        }
    }
}

fn interpret<'a>(ast: &Ast<'a>, symtab: &mut SymbolTable<'a, Value>) -> Value {
    match ast.tree.as_ref() {
        Expression::Literal(literal) => literal.into(),
        Expression::Identifier(identifier) => resolve!(symtab, identifier.name).get().clone(),
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
            let key = fn_call.function.name;
            let mut args: Vec<Value> = Vec::new();
            for arg in &fn_call.arguments {
                args.push(interpret(arg, symtab));
            }
            match resolve!(symtab, key).get() {
                Value::Fun(f) => f(&args),
                _ => panic!("{} is not a function", key),
            }
        }
        Expression::Block(block) => {
            symtab.push();
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
            let key = var.id.name;
            let value = interpret(&var.init, symtab);
            if symtab.insert(key, value).is_some() {
                panic!("Identifier {key} is already defined (can't shadow in the same scope)");
            };
            Value::Unit
        }
        Expression::BinaryOp(binary_op) => {
            // Special case: Assignment
            if binary_op.op == Operator::Assign {
                let key = match binary_op.left.tree.as_ref() {
                    Expression::Identifier(identifier) => identifier.name,
                    _ => panic!("= requires identifier on the lhs"),
                };
                let value = interpret(&binary_op.right, symtab);
                resolve!(symtab, key).insert(value.clone());
                return value;
            }

            // Interpret lhs
            let a = interpret(&binary_op.left, symtab);

            // Short-circuit and
            if binary_op.op == Operator::And {
                if let Value::Bool(a) = a {
                    if !a {
                        return Value::Bool(false);
                    }
                }
            }

            // Short-circuit or
            if binary_op.op == Operator::Or {
                if let Value::Bool(a) = a {
                    if a {
                        return Value::Bool(true);
                    }
                }
            }

            // Interpret rhs
            let b = interpret(&binary_op.right, symtab);

            // Special cases: Eq and Ne
            match (&a, binary_op.op, &b) {
                (Value::Int(a), Operator::Eq, Value::Int(b)) => return Value::Bool(a == b),
                (Value::Bool(a), Operator::Eq, Value::Bool(b)) => return Value::Bool(a == b),
                (Value::Unit, Operator::Eq, Value::Unit) => return Value::Bool(true),
                (Value::Int(a), Operator::Ne, Value::Int(b)) => return Value::Bool(a != b),
                (Value::Bool(a), Operator::Ne, Value::Bool(b)) => return Value::Bool(a != b),
                (Value::Unit, Operator::Ne, Value::Unit) => return Value::Bool(false),
                (_, op @ (Operator::Eq | Operator::Ne), _) => {
                    panic!(
                        "Invalid arguments to {op}, expected to have same type between operands",
                    );
                }
                _ => {}
            };

            // Delegate rest to builtins
            call!(symtab, binary_op.op.function_name(Ary::Binary), &[a, b])
        }
        Expression::UnaryOp(unary_op) => {
            let value = interpret(&unary_op.right, symtab);
            call!(symtab, unary_op.op.function_name(Ary::Unary), &[value])
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
        Expression::Break => todo!(),
        Expression::Continue => todo!(),
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

    let module = match parse(&code, &cli.config) {
        Ok(ast) => ast,
        Err(ref e) => err!(e),
    };

    let mut symtab = SymbolTable::from(vec![
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
        fun!(
            Operator::Add.function_name(Ary::Binary),
            [Value::Int(a), Value::Int(b)],
            Value::Int(a + b)
        ),
        fun!(
            Operator::Sub.function_name(Ary::Binary),
            [Value::Int(a), Value::Int(b)],
            Value::Int(a - b)
        ),
        fun!(
            Operator::Sub.function_name(Ary::Unary),
            [Value::Int(a)],
            Value::Int(-a)
        ),
        fun!(
            Operator::Mul.function_name(Ary::Binary),
            [Value::Int(a), Value::Int(b)],
            Value::Int(a * b)
        ),
        fun!(
            Operator::Div.function_name(Ary::Binary),
            [Value::Int(a), Value::Int(b)],
            Value::Int(a / b)
        ),
        fun!(
            Operator::Rem.function_name(Ary::Binary),
            [Value::Int(a), Value::Int(b)],
            Value::Int(a % b)
        ),
        fun!(
            Operator::Lt.function_name(Ary::Binary),
            [Value::Int(a), Value::Int(b)],
            Value::Bool(a < b)
        ),
        fun!(
            Operator::Leq.function_name(Ary::Binary),
            [Value::Int(a), Value::Int(b)],
            Value::Bool(a <= b)
        ),
        fun!(
            Operator::Gt.function_name(Ary::Binary),
            [Value::Int(a), Value::Int(b)],
            Value::Bool(a > b)
        ),
        fun!(
            Operator::Geq.function_name(Ary::Binary),
            [Value::Int(a), Value::Int(b)],
            Value::Bool(a >= b)
        ),
        fun!(
            Operator::And.function_name(Ary::Binary),
            [Value::Bool(a), Value::Bool(b)],
            Value::Bool(*a && *b)
        ),
        fun!(
            Operator::Or.function_name(Ary::Binary),
            [Value::Bool(a), Value::Bool(b)],
            Value::Bool(*a || *b)
        ),
        fun!(
            Operator::Not.function_name(Ary::Unary),
            [Value::Bool(a)],
            Value::Bool(!a)
        ),
    ]);

    let code = match interpret(&module.root, &mut symtab) {
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
