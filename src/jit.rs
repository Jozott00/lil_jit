// 1. jitdata .. holds the complete information about for the compilation process
// 3. codeinfo .. compiled function info, so machine code information (funcinfo, codegendata)
// 2. funcinfo .. higher level information of a function
// 4. codegendata .. function specific machine code data (such as mcodebase, mcodeptr, etc.)

use armoured_rust::types::InstructionPointer;

use crate::ast::Program;
use crate::jit::arch_def::arm64::Arm64;
use crate::jit::arch_def::RegDefinition;
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
pub fn run_jit(ast: &Program) -> i32 {
    let mut jit = JIT::<Arm64>::new(&ast);
    unsafe {
        // store reference to jit object statically
        JIT_REF = Some((&jit) as *const _ as usize);
    }
    jit.run()
}

pub struct JIT<'a, D: RegDefinition> {
    jit_data: JitData<'a, D>,
}

impl<'a, D: RegDefinition> JIT<'a, D> {
    fn new(ast: &'a Program) -> Self {
        let funcs = ast.functions.iter().collect();
        let jit_data = JitData::new(funcs);
        JIT { jit_data }
    }

    fn run(&mut self) -> i32 {
        self.compile("main");

        let main_info = self
            .jit_data
            .compiled_funcs
            .get("main")
            .expect("main function not compiled!");

        let main = main_info.codegen_data.nullary_fn_ptr();
        let exit_code = unsafe { main() };

        exit_code
    }

    fn compile(&mut self, funcname: &'a str) {
        log::info!(target: "verbose", "COMPILING {} ...", funcname);

        if self.jit_data.compiled_funcs.contains_key(funcname) {
            log::info!(target: "verbose", "WARN: Function {funcname} was already compiled...");
            return;
        }

        let uncompiled_func = self
            .jit_data
            .uncompiled_funcs
            .remove(funcname)
            .expect("Tried to compile unknown function");

        let lir = compile_to_lir(uncompiled_func);
        log::info!(target: "dump-ir", "------\nLIR DUMP FOR {}:\n{}\n------\n", funcname, lir);

        let reg_mapping = alloc_reg::<D>(&lir);
        log::info!(target: "dump-reg-alloc", "------\nREGISTER ALLOCATION DUMP FOR {}:\n{}\n------\n", funcname, reg_mapping);

        let func_info = FuncInfo::new(funcname, lir, reg_mapping);
        let mut code_info = compile_func::<D>(func_info, &mut self.jit_data);
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
            .expect(
                format!(
                    "Function Name not found for given reference {:#x}!",
                    caller as usize
                )
                .as_str(),
            );

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
        for (caller_func, code_refs) in refs {
            let caller_info =
                self.jit_data.compiled_funcs.get_mut(caller_func).expect(
                    "Caller function must be compiled already, otherwise it could call stub!",
                );

            for code_ref in code_refs {
                log::info!(target: "verbose", "PATCH CALL TO {func_name} ({:#x}) BY {caller_func} AT REFERENCE {:#x}", func_ptr as usize, (code_ref as usize));

                caller_info.codegen_data.patch_at(code_ref, |cd| {
                    cd.func_call(func_ptr as usize, D::temp3());
                });
            }

            log::info!(target: "dump-disasm-patch", "-----\nDISASSEMBLY AFTER PATCH FOR {}:\n{}-------\n", caller_info.func_info.name(), caller_info.codegen_data);
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
        let mut jit = JIT::<Arm64>::new(&prog);
        jit.run();
    }
}
