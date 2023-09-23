// 1. jitdata .. holds the complete information about for the compilation process
// 3. codeinfo .. compiled function info, so machine code information (funcinfo, codegendata)
// 2. funcinfo .. higher level information of a function
// 4. codegendata .. function specific machine code data (such as mcodebase, mcodeptr, etc.)

use crate::ast::{FuncDec, Program};
use crate::jit::compiler::compile_func;
use crate::jit::funcinfo::FuncInfo;
use crate::jit::jitdata::JitData;
use log::{info, log};

mod codegendata;
mod codeinfo;
mod compiler;
mod funcinfo;
mod jitdata;
mod reg_alloc;
mod scope;
mod lir;

pub struct JIT<'a> {
    jit_data: JitData<'a>,
}

impl<'a> JIT<'a> {
    pub fn new(ast: &'a Program) -> Self {
        let funcs = ast.functions.iter().collect();
        let jit_data = JitData::new(funcs);
        JIT { jit_data }
    }

    pub fn run(&'a mut self) {
        info!("JIT start...");

        // reg alloc for main

        // compile entry function
        // compile_func(<func_info_with_regs>, &mut self.jit_data);
    }
}

fn func_dec_to_info<'a>(dec: &'a FuncDec) -> FuncInfo<'a> {
    FuncInfo {
        name: dec.name.name,
        ast: dec,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checker::check_lil;
    use crate::parser::parse_lil_program;

    fn create_prog(str: &str) -> Program {
        let prog = parse_lil_program(str).expect("Couldn't parse program");
        let err = check_lil(&prog);
        err.expect("Some checks failed!");
        prog
    }

    #[test]
    fn test_minimal_prog() {
        let prog = create_prog("fn main() {}");
        let mut jit = JIT::new(&prog);
        jit.run();
    }
}
