// 1. jitdata .. holds the complete information about for the compilation process
// 3. codeinfo .. compiled function info, so machine code information (funcinfo, codegendata)
// 2. funcinfo .. higher level information of a function
// 4. codegendata .. function specific machine code data (such as mcodebase, mcodeptr, etc.)

use crate::ast::Program;
use armoured_rust::instruction_encoding::branch_exception_system::unconditional_branch_immediate::UnconditionalBranchImmediateWithAddress;
use armoured_rust::types::InstructionPointer;
use log::{debug, info};
use std::arch::global_asm;

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
mod stub;
mod stub_ref_store;

/// Address reference to JIT object. Accessed by stub. (Highly Unsafe)
///
/// Only valid as long as JIT is running
static mut JIT_REF: Option<usize> = None;

/// Starts the JIT compilation and program execution of
/// the given Program.
///
/// Uses unsafe operations to make the created JIT object statically available.
pub fn run_jit(ast: &Program) {
    let mut jit = JIT::new(&ast);
    unsafe {
        // store reference to jit object statically
        JIT_REF = Some((&jit) as *const _ as usize);
    }
    jit.run();
}

pub struct JIT<'a> {
    jit_data: JitData<'a>,
}

impl<'a> JIT<'a> {
    fn new(ast: &'a Program) -> Self {
        let funcs = ast.functions.iter().collect();
        let jit_data = JitData::new(funcs);
        JIT { jit_data }
    }

    fn run(&mut self) {
        self.compile("main");

        let main_info = self
            .jit_data
            .compiled_funcs
            .get("main")
            .expect("main function not compiled!");

        let main = main_info.codegen_data.nullary_fn_ptr();
        let exit_code = unsafe { main() };

        println!("EXIT CODE: {exit_code}")
    }

    fn compile(&mut self, funcname: &'a str) {
        log::info!(target: "verbose", "COMPILING {} ...", funcname);

        if self.jit_data.compiled_funcs.contains_key(funcname) {
            debug!("Function {funcname} was already compiled...");
            return;
        }

        let uncompiled_func = self
            .jit_data
            .uncompiled_funcs
            .remove(funcname)
            .expect("Tried to compile unknown function");

        let lir = compile_to_lir(uncompiled_func);
        log::info!(target: "dump-ir", "------\nLIR DUMP FOR {}:\n{}\n------\n", funcname, lir);

        let reg_mapping = alloc_reg::<Arm64>(&lir);
        log::info!(target: "dump-reg-alloc", "------\nREGISTER ALLOCATION DUMP FOR {}:\n{:?}\n------\n", funcname, reg_mapping);

        let func_info = FuncInfo::new(funcname, lir, reg_mapping);
        let mut code_info = compile_func::<Arm64>(func_info, &mut self.jit_data);
        log::info!(target: "dump-disasm", "-----\nDISASSEMBLY FOR {}:\n{}-------\n", funcname, code_info.codegen_data);

        code_info.codegen_data.make_executable();

        // add compiled function
        self.jit_data.compiled_funcs.insert(funcname, code_info);
    }

    /// TODO: Refactor into own module
    fn compile_by_caller_addr(&'a mut self, caller: InstructionPointer) {
        let func_name = *self
            .jit_data
            .stub_ref_store
            .get_function_name(caller)
            .expect("Function Name not found for given reference!");

        let refs = self
            .jit_data
            .stub_ref_store
            .resolve_function(&func_name)
            .expect("Couldn't find any references to unresolved function name");

        self.compile(&func_name);

        let code_info = self
            .jit_data
            .compiled_funcs
            .get(func_name)
            .expect("Must exist as we just compiled it");
        let func_ptr = code_info.codegen_data.base_ptr();

        // patch all references
        for (caller_func, code_ref) in refs {
            let caller_info =
                self.jit_data.compiled_funcs.get_mut(caller_func).expect(
                    "Caller function must be compiled already, otherwise it could call stub!",
                );

            caller_info.codegen_data.patch_at(code_ref, |cd| {
                cd.bl_to_addr(func_ptr as usize);
            });
        }
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
