use crate::jit::codegendata::CodegenData;
use crate::jit::codeinfo::CodeInfo;
use crate::jit::funcinfo::FuncInfo;

pub struct JitData {
    func_info: FuncInfo,
    code_info: CodeInfo,
    codegen_data: CodegenData
}

impl JitData {
    pub fn new(func_info: FuncInfo) -> JitData {
        let code_info = CodeInfo::new();
        let codegen_data = CodegenData::new().expect("Failed to create codegen data");

        JitData {
            func_info,
            code_info,
            codegen_data
        }
    }
}