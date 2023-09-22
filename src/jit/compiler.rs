use crate::jit::codeinfo::CodeInfo;
use crate::jit::funcinfo::FuncInfo;
use crate::jit::jitdata::JitData;
use crate::visitor::{walk_funcdec, NodeVisitor};

pub fn compile_func<'a>(func_info: FuncInfo<'a>, jit_data: &'a mut JitData<'a>) {
    let mut compiler = Compiler::new(func_info, jit_data);
    compiler.compile();
}

/// Compiler that compiles provided function `func` in context of `jit_data`.
///
/// A compiler can be invoked by `compile()` which consumes itself, and therefore isn't usable anymore.
struct Compiler<'a> {
    jit_data: &'a mut JitData<'a>,
    code_info: CodeInfo<'a>,
}

impl<'a> Compiler<'a> {
    fn new(func_info: FuncInfo<'a>, jit_data: &'a mut JitData<'a>) -> Self {
        let code_info = CodeInfo::new(func_info);

        Compiler {
            jit_data,
            code_info,
        }
    }
    fn compile(mut self) {
        let fn_ast = self.code_info.func_info.ast;
        walk_funcdec(&mut self, fn_ast);
    }
}

impl<'a> NodeVisitor<'a> for Compiler<'a> {}
