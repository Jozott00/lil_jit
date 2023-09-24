// 1. jitdata .. holds the complete information about for the compilation process
// 3. codeinfo .. compiled function info, so machine code information (funcinfo, codegendata)
// 2. funcinfo .. higher level information of a function
// 4. codegendata .. function specific machine code data (such as mcodebase, mcodeptr, etc.)

use crate::ast::Program;

use crate::jit::arch_def::arm64::Arm64;
use crate::jit::compiler::compile_func;
use crate::jit::funcinfo::FuncInfo;
use crate::jit::jitdata::JitData;
use crate::jit::lir::compile_to_lir;
use crate::jit::reg_alloc::alloc_reg;

pub mod arch_def;
mod codegendata;
mod codeinfo;
mod compiler;
mod funcinfo;
mod jitdata;
mod lir;
mod reg_alloc;
mod scope;

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
        self.compile("main");
    }

    pub fn compile(&'a mut self, funcname: &'a str) {
        log::info!(target: "verbose", "COMPILING {} ...", funcname);

        let uncompiled_func = self
            .jit_data
            .uncompiled_funcs
            .remove(funcname)
            .expect("Tried to compile unknown function");

        let lir = compile_to_lir(uncompiled_func);
        log::info!(target: "dump-ir", "------\nLIR DUMP FOR {}:\n{}\n------\n", funcname, lir);

        let reg_mapping = alloc_reg::<Arm64>(&lir);
        log::info!(target: "dump-rec-alloc", "------\nREGISTER ALLOCATION DUMP FOR {}:\n{:?}\n------\n", funcname, reg_mapping);

        let func_info = FuncInfo::new(funcname, lir, reg_mapping);
        let mut code_info = compile_func::<Arm64>(func_info, &mut self.jit_data);
        log::info!(target: "dump-disasm", "-----\nDISASSEMBLY FOR {}:\n{}\n-------\n", funcname, code_info.codegen_data);

        // TODO: REMOVE -- just a demo
        code_info.codegen_data.make_executable();
        let func = code_info.codegen_data.nullary_fn_ptr();
        let result = unsafe { func() };
        println!("Received result: {}", result);

        // add compiled function
        self.jit_data.compiled_funcs.insert(funcname, code_info);
    }
}

#[cfg(test)]
mod tests {
    use crate::checker::check_lil;
    use crate::parser::parse_lil_program;

    use super::*;

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
