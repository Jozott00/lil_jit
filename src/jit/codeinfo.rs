use crate::jit::codegendata::CodegenData;
use crate::jit::funcinfo::FuncInfo;

/// Holds the information of a compiled function (or one that is currently in compilation process)
///
/// - `FuncInfo` holds high-level information about the function that is compiled.
/// - `CodegenData` holds the function memory, function memory pointer, etc.
pub struct CodeInfo<'a> {
    func_info: FuncInfo<'a>,
    codegen_data: CodegenData,
}

impl<'a> CodeInfo<'a> {
    pub fn new(func_info: FuncInfo<'a>) -> Self {
        let codegen_data = CodegenData::new().expect("Failed to create codegen data");

        Self {
            func_info,
            codegen_data,
        }
    }
}
