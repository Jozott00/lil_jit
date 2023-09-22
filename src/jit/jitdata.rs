use crate::jit::codeinfo::CodeInfo;
use crate::jit::funcinfo::FuncInfo;
use std::collections::HashMap;

pub struct JitData<'a> {
    pub compiled_funcs: HashMap<&'a str, CodeInfo<'a>>,
    pub uncompiled_funcs: HashMap<&'a str, FuncInfo<'a>>,
}

impl<'a> JitData<'a> {
    pub fn new(funcs: Vec<FuncInfo<'a>>) -> Self {
        let uncompiled_funcs = funcs.into_iter().map(|e| (e.name, e)).collect();

        Self {
            uncompiled_funcs,
            compiled_funcs: HashMap::default(),
        }
    }
}

impl<'a> Default for JitData<'a> {
    fn default() -> Self {
        Self {
            compiled_funcs: HashMap::new(),
            uncompiled_funcs: HashMap::new(),
        }
    }
}
