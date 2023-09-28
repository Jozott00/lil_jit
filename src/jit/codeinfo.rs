use crate::jit::arch_def::RegDefinition;
use crate::jit::codegendata::CodegenData;
use crate::jit::funcinfo::FuncInfo;
use std::collections::HashMap;

/// Holds the information of a compiled function (or one that is currently in compilation process)
///
/// - `FuncInfo` holds high-level information about the function that is compiled.
/// - `CodegenData` holds the function memory, function memory pointer, etc.
#[derive(Debug)]
pub struct CodeInfo<'a, D: RegDefinition> {
    pub func_info: FuncInfo<'a, D>,
    pub codegen_data: CodegenData,
}

impl<'a, D: RegDefinition> CodeInfo<'a, D> {
    pub fn new(func_info: FuncInfo<'a, D>) -> Self {
        let codegen_data = CodegenData::new().expect("Failed to create codegen data");

        Self {
            func_info,
            codegen_data,
        }
    }
}
