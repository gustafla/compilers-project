mod assembler;
mod intrinsics;

use crate::ir;
pub use assembler::{Error, assemble};
use intrinsics::Intrinsics;
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
};

const REGISTER_ARGUMENTS: &[&str] = &["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

struct Locals {
    stack_used: usize,
    var_location: HashMap<ir::Var, String>,
}

impl Locals {
    pub fn new(parm: &[ir::Var], ins: &[ir::Instruction]) -> Self {
        let vars = Self::get_all_ir_variables(parm, ins);
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

    pub fn get_ref(&self, var: &ir::Var) -> &str {
        match self.var_location.get(var) {
            Some(s) => s,
            None => panic!("Invalid reference to {}", var),
        }
    }

    pub fn stack_used(&self) -> usize {
        self.stack_used
    }

    fn get_all_ir_variables(parm: &[ir::Var], ins: &[ir::Instruction]) -> HashSet<ir::Var> {
        let mut set: HashSet<ir::Var> = parm.iter().copied().collect();

        for insn in ins {
            match &insn.op {
                ir::Op::Label(_) => {}
                ir::Op::LoadBoolConst { dest, .. } => {
                    set.insert(*dest);
                }
                ir::Op::LoadIntConst { dest, .. } => {
                    set.insert(*dest);
                }
                ir::Op::Copy { source, dest } => {
                    set.insert(*source);
                    set.insert(*dest);
                }
                ir::Op::Call {
                    /*fun, */ args,
                    dest,
                    ..
                } => {
                    for arg in args {
                        set.insert(*arg);
                    }
                    set.insert(*dest);
                }
                ir::Op::Jump { .. } => {}
                ir::Op::CondJump { cond, .. } => {
                    set.insert(*cond);
                }
                ir::Op::Return { arg: Some(arg) } => {
                    set.insert(*arg);
                }
                ir::Op::Return { arg: None } => {}
            }
        }

        set
    }
}

macro_rules! emit {
    ($out: expr, $($arg: tt)*) => {{
        writeln!($out, $($arg)*).ok();
    }};
}
pub(crate) use emit;

macro_rules! emit_ind {
    ($out: expr, $($arg: tt)*) => {{
        write!($out, "    ").ok();
        crate::asm::emit!($out, $($arg)*);
    }};
}
pub(crate) use emit_ind;

fn emit_function(
    out: &mut String,
    intrinsics: &Intrinsics,
    fun: &ir::Function,
    ins: &[ir::Instruction<'_>],
) {
    let locals = Locals::new(&fun.parameters, ins);
    emit!(out, "");
    emit!(out, ".global {}", fun.identifier);
    emit!(out, ".type {}, @function", fun.identifier);
    emit!(out, "{}:", fun.identifier);
    emit_ind!(out, "pushq %rbp");
    emit_ind!(out, "movq %rsp, %rbp");
    emit_ind!(out, "subq ${}, %rsp", locals.stack_used());

    // Copy register arguments to locals
    for (reg, arg) in REGISTER_ARGUMENTS.iter().zip(&fun.parameters) {
        emit_ind!(out, "movq {}, {}", reg, locals.get_ref(arg));
    }
    // Copy stack arguments to locals
    for (i, arg) in fun.parameters.iter().skip(6).enumerate() {
        emit_ind!(out, "movq {}(%rbp), {}", 8 * (i + 2), locals.get_ref(arg));
    }

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
                // Emit intrinsic if defined
                if let Some(intrinsic) = intrinsics.get(fun) {
                    let arg_refs: Vec<&str> = args.iter().map(|a| locals.get_ref(a)).collect();
                    intrinsic(intrinsics::Args {
                        arg_refs: &arg_refs,
                        result_register: "%rax",
                        out,
                    });
                    emit_ind!(out, "movq %rax, {}", locals.get_ref(dest));
                    continue;
                }

                // Set register arguments
                for (reg, arg) in REGISTER_ARGUMENTS.iter().zip(args.iter()) {
                    emit_ind!(out, "movq {}, {}", locals.get_ref(arg), reg);
                }

                // Push stack arguments
                let stack_args = args.iter().skip(6).count();
                // If stack wouldn't be 16-byte aligned, insert alignment
                let pushed = if stack_args % 2 != 0 {
                    emit_ind!(out, "subq $8, %rsp");
                    stack_args * 8 + 8
                } else {
                    stack_args * 8
                };
                for arg in args.iter().skip(6).rev() {
                    emit_ind!(out, "pushq {}", locals.get_ref(arg));
                }

                // Call function
                emit_ind!(out, "callq {}", fun);

                // Pop stack arguments
                if pushed != 0 {
                    emit_ind!(out, "addq ${}, %rsp", pushed);
                }

                // Set return value to destination
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
            ir::Op::Return { arg } => {
                emit_ind!(
                    out,
                    "movq {}, %rax",
                    match arg {
                        Some(var) => locals.get_ref(var),
                        None => "$0",
                    }
                );
                emit_ind!(out, "movq %rbp, %rsp");
                emit_ind!(out, "popq %rbp");
                emit_ind!(out, "ret");
            }
        }
    }
}

pub fn generate_assembly<'a>(funs: &[(ir::Function<'a>, Vec<ir::Instruction<'a>>)]) -> String {
    let mut out = String::new();

    let intrinsics = intrinsics::intrinsics();

    emit_ind!(out, ".extern print_int");
    emit_ind!(out, ".extern print_bool");
    emit_ind!(out, ".extern read_int");

    emit!(out, "");
    emit_ind!(out, ".section .text");

    for (fun, ins) in funs {
        emit_function(&mut out, &intrinsics, fun, ins);
    }

    out
}
