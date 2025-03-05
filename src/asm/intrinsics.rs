use super::emit_ind;
use crate::ast::{Operator, op::Ary};
use std::{collections::HashMap, fmt::Write};

pub struct Args<'a, 'o> {
    pub arg_refs: &'a [&'a str],
    pub result_register: &'static str,
    pub out: &'o mut String,
}

pub type Emitter = Box<dyn Fn(Args)>;
pub type Intrinsics = HashMap<&'static str, Emitter>;

pub fn intrinsics() -> Intrinsics {
    let binary_add: Emitter = Box::new(|a: Args| {
        if a.result_register != a.arg_refs[0] {
            emit_ind!(a.out, "movq {}, {}", a.arg_refs[0], a.result_register);
        }
        emit_ind!(a.out, "addq {}, {}", a.arg_refs[1], a.result_register);
    });
    let binary_sub: Emitter = Box::new(|a: Args| {
        if a.result_register != a.arg_refs[0] {
            emit_ind!(a.out, "movq {}, {}", a.arg_refs[0], a.result_register);
        }
        emit_ind!(a.out, "subq {}, {}", a.arg_refs[1], a.result_register);
    });
    let unary_sub: Emitter = Box::new(|a: Args| {
        emit_ind!(a.out, "movq {}, {}", a.arg_refs[0], a.result_register);
        emit_ind!(a.out, "negq {}", a.result_register);
    });
    let binary_mul: Emitter = Box::new(|a: Args| {
        if a.result_register != a.arg_refs[0] {
            emit_ind!(a.out, "movq {}, {}", a.arg_refs[0], a.result_register);
        }
        emit_ind!(a.out, "imulq {}, {}", a.arg_refs[1], a.result_register);
    });
    let binary_div: Emitter = Box::new(|a: Args| {
        emit_ind!(a.out, "movq {}, %rax", a.arg_refs[0]);
        emit_ind!(a.out, "cqto");
        emit_ind!(a.out, "idivq {}", a.arg_refs[1]);
        if a.result_register != "%rax" {
            emit_ind!(a.out, "movq %rax, {}", a.result_register);
        }
    });
    let binary_rem: Emitter = Box::new(|a: Args| {
        emit_ind!(a.out, "movq {}, %rax", a.arg_refs[0]);
        emit_ind!(a.out, "cqto");
        emit_ind!(a.out, "idivq {}", a.arg_refs[1]);
        if a.result_register != "%rdx" {
            emit_ind!(a.out, "movq %rdx, {}", a.result_register);
        }
    });
    let binary_eq: Emitter = Box::new(|a: Args| int_comparison(a, "sete"));
    let binary_ne: Emitter = Box::new(|a: Args| int_comparison(a, "setne"));
    let binary_le: Emitter = Box::new(|a: Args| int_comparison(a, "setle"));
    let binary_lt: Emitter = Box::new(|a: Args| int_comparison(a, "setl"));
    let binary_ge: Emitter = Box::new(|a: Args| int_comparison(a, "setge"));
    let binary_gt: Emitter = Box::new(|a: Args| int_comparison(a, "setg"));
    let unary_not: Emitter = Box::new(|a: Args| {
        emit_ind!(a.out, "movq {}, {}", a.arg_refs[0], a.result_register);
        emit_ind!(a.out, "xorq $1, {}", a.result_register);
    });

    HashMap::from([
        (Operator::Add.function_name(Ary::Binary), binary_add),
        (Operator::Sub.function_name(Ary::Binary), binary_sub),
        (Operator::Sub.function_name(Ary::Unary), unary_sub),
        (Operator::Mul.function_name(Ary::Binary), binary_mul),
        (Operator::Div.function_name(Ary::Binary), binary_div),
        (Operator::Rem.function_name(Ary::Binary), binary_rem),
        (Operator::Eq.function_name(Ary::Binary), binary_eq),
        (Operator::Ne.function_name(Ary::Binary), binary_ne),
        (Operator::Leq.function_name(Ary::Binary), binary_le),
        (Operator::Lt.function_name(Ary::Binary), binary_lt),
        (Operator::Geq.function_name(Ary::Binary), binary_ge),
        (Operator::Gt.function_name(Ary::Binary), binary_gt),
        (Operator::Not.function_name(Ary::Unary), unary_not),
    ])
}

fn int_comparison(a: Args, setcc_insn: &str) {
    emit_ind!(a.out, "xor %rax, %rax");
    emit_ind!(a.out, "movq {}, %rdx", a.arg_refs[0]);
    emit_ind!(a.out, "cmpq {}, %rdx", a.arg_refs[1]);
    emit_ind!(a.out, "{} %al", setcc_insn);
    if a.result_register != "%rax" {
        emit_ind!(a.out, "movq %rax, {}", a.result_register);
    }
}
