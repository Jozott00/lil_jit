use crate::jit::funcinfo::FuncInfo;
use crate::jit::jitdata::JitData;

pub fn compile_func<'a>(func_name: &str, jit_data: &'a mut JitData<'a>) {
    // let mut compiler = Compiler::new(func_name, jit_data);
    // compiler.compile();
}

/// Compiler that compiles provided function `func` in context of `jit_data`.
///
/// A compiler can be invoked by `compile()` which consumes itself, and therefore isn't usable anymore.
struct Compiler<'a> {
    func: &'a FuncInfo<'a>,
    jit_data: &'a mut JitData<'a>,
}

impl<'a> Compiler<'a> {
    // fn new(func_name: &str, jit_data: &'a mut JitData<'a>) -> Self {
    //     let func = jit_data.uncompiled_funcs.get(func_name).unwrap();

    // Compiler { func, jit_data }
    // }
    fn compile(mut self) {}
}
