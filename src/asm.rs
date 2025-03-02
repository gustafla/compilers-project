mod assembler;
mod intrinsics;

use crate::ir;
pub use assembler::{Error, assemble};
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
};

struct Locals {
    stack_used: usize,
    var_location: HashMap<ir::Var, String>,
}

impl Locals {
    fn new(ins: &[ir::Instruction]) -> Self {
        let vars = Self::get_all_ir_variables(ins);
        let stack_used = vars.len() * 8;
        let mut var_location = HashMap::new();
        for (i, var) in vars.into_iter().enumerate() {
            var_location.insert(var, format!("-{}(%rbp)", 8 * (i + 1)));
        }
        Self {
            stack_used,
            var_location,
        }
    }

    fn get_all_ir_variables(ins: &[ir::Instruction]) -> HashSet<ir::Var> {
        let mut set = HashSet::new();

        for insn in ins {
            match &insn.op {
                ir::Op::Label(_) => {}
                ir::Op::LoadBoolConst { dest, .. } => {
                    set.insert(dest.clone());
                }
                ir::Op::LoadIntConst { dest, .. } => {
                    set.insert(dest.clone());
                }
                ir::Op::Copy { source, dest } => {
                    set.insert(source.clone());
                    set.insert(dest.clone());
                }
                ir::Op::Call {
                    /*fun, */ args,
                    dest,
                    ..
                } => {
                    for arg in args {
                        set.insert(arg.clone());
                    }
                    set.insert(dest.clone());
                }
                ir::Op::Jump { .. } => {}
                ir::Op::CondJump { cond, .. } => {
                    set.insert(cond.clone());
                }
            }
        }

        set
    }

    fn get_ref(&self, var: &ir::Var) -> &str {
        match self.var_location.get(var) {
            Some(s) => s,
            None => panic!("Invalid reference to {}", var),
        }
    }

    fn stack_used(&self) -> usize {
        self.stack_used
    }
}

macro_rules! emit {
    ($out: expr, $($arg: tt)*) => {{
        writeln!(&mut $out, $($arg)*).ok();
    }};
}
pub(crate) use emit;

macro_rules! emit_ind {
    ($out: expr, $($arg: tt)*) => {{
        write!(&mut $out, "    ").ok();
        crate::asm::emit!($out, $($arg)*);
    }};
}
pub(crate) use emit_ind;

pub fn generate_assembly(ins: &[ir::Instruction]) -> String {
    let mut out = String::new();

    let locals = Locals::new(ins);
    let intrinsics = intrinsics::intrinsics();

    emit_ind!(out, ".extern print_int");
    emit_ind!(out, ".extern print_bool");
    emit_ind!(out, ".extern read_int");
    emit_ind!(out, ".global main");
    emit_ind!(out, ".type main, @function");
    emit!(out, "");
    emit_ind!(out, ".section .text");
    emit!(out, "");
    emit!(out, "main:");
    emit_ind!(out, "pushq %rbp");
    emit_ind!(out, "movq %rsp, %rbp");
    emit_ind!(out, "subq ${}, %rsp", locals.stack_used());

    for insn in ins {
        if let ir::Op::Label(..) = insn.op {
            emit!(out, "");
        } else {
            emit_ind!(out, "# {}", insn.op);
        }

        match &insn.op {
            ir::Op::Label(label) => {
                emit!(out, ".L{}:", label);
            }
            ir::Op::LoadBoolConst { value, dest } => {
                emit_ind!(out, "movq ${}, {}", *value as i64, locals.get_ref(dest));
            }
            ir::Op::LoadIntConst { value, dest } => {
                if *value < 2u64.pow(31) || *value >= 0xFFFFFFFF80000000 {
                    emit_ind!(out, "movq ${}, {}", value, locals.get_ref(dest));
                } else {
                    emit_ind!(out, "movabsq ${}, %rax", value);
                    emit_ind!(out, "movq %rax, {}", locals.get_ref(dest));
                }
            }
            ir::Op::Copy { source, dest } => {
                emit_ind!(out, "movq {}, %rax", locals.get_ref(source));
                emit_ind!(out, "movq %rax, {}", locals.get_ref(dest));
            }
            ir::Op::Call { fun, args, dest } => {
                if let Some(intrinsic) = intrinsics.get(fun.as_str()) {
                    let arg_refs: Vec<&str> = args.iter().map(|a| locals.get_ref(a)).collect();
                    intrinsic(intrinsics::Args {
                        arg_refs: &arg_refs,
                        result_register: "%rax",
                        out: &mut out,
                    });
                    emit_ind!(out, "movq %rax, {}", locals.get_ref(dest));
                    continue;
                }

                for (reg, arg) in ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"]
                    .iter()
                    .zip(args.iter().take(6))
                {
                    emit_ind!(out, "movq {}, {}", locals.get_ref(arg), reg);
                }
                let mut pushed = 0;
                for arg in args.iter().skip(6).rev() {
                    emit_ind!(out, "pushq {}", locals.get_ref(arg));
                    pushed += 8;
                }
                emit_ind!(out, "callq {}", fun);
                emit_ind!(out, "addq ${}, %rsp", pushed);
                emit_ind!(out, "movq %rax, {}", locals.get_ref(dest));
            }
            ir::Op::Jump { label } => {
                emit_ind!(out, "jmp .L{}", label);
            }
            ir::Op::CondJump {
                cond,
                then_label,
                else_label,
            } => {
                emit_ind!(out, "cmpq $0, {}", locals.get_ref(cond));
                emit_ind!(out, "jne .L{}", then_label);
                emit_ind!(out, "jmp .L{}", else_label);
            }
        }
    }

    emit_ind!(out, "movq %rbp, %rsp");
    emit_ind!(out, "popq %rbp");
    emit_ind!(out, "ret");

    out
}
