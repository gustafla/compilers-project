use std::collections::{HashMap, HashSet};

use crate::ir;

struct Locals {
    var_location: HashMap<ir::Var, String>,
    stack_used: usize,
}

impl Locals {
    fn new(ins: &[ir::Instruction]) -> Self {
        todo!()
    }

    fn get_all_ir_variables(ins: &[ir::Instruction]) -> Vec<ir::Var> {
        let mut vars = Vec::new();
        let mut set = HashSet::new();

        let mut add = |var: &ir::Var| {
            if !set.contains(var) {
                vars.push(var.clone());
                set.insert(var.clone());
            }
        };

        for i in ins {
            match &i.op {
                ir::Op::Label(_) => {}
                ir::Op::Nop => {}
                ir::Op::LoadBoolConst { dest, .. } => add(dest),
                ir::Op::LoadIntConst { dest, .. } => add(dest),
                ir::Op::Copy { source, dest } => {
                    add(source);
                    add(dest);
                }
                ir::Op::Call {
                    /*fun, */ args,
                    dest,
                    ..
                } => {
                    for arg in args {
                        add(arg);
                    }
                    add(dest);
                }
                ir::Op::Jump { .. } => {}
                ir::Op::CondJump { cond, .. } => add(cond),
            }
        }

        vars
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

fn generate_assembly(ins: &[ir::Instruction]) -> String {
    todo!()
}
