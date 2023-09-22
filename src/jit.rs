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

pub struct JIT<'a> {
    jit_data: JitData<'a>,
}

impl<'a> JIT<'a> {
    pub fn new(ast: &'a Program) -> Self {
        // TODO: get all program functions and store them in jit_data
        let funcs: Vec<FuncInfo> = ast.functions.iter().map(func_dec_to_info).collect();
        let jit_data = JitData::new(funcs);
        JIT { jit_data }
    }

    pub fn run(&'a mut self) {
        info!("JIT start...");
        // compile entry function
        compile_func("main", &mut self.jit_data);
    }
}

// impl<'a> JIT<'a> {
//     pub fn compile(&mut self, )
// }

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
