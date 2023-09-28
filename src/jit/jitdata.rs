use std::collections::{HashMap, HashSet};

use crate::ast::FuncDec;
use crate::jit::arch_def::RegDefinition;
use crate::jit::codeinfo::CodeInfo;
use crate::jit::stub_ref_store::StubRefStore;

#[derive(Debug)]
pub struct JitData<'a, D: RegDefinition> {
    pub compiled_funcs: HashMap<&'a str, CodeInfo<'a, D>>,
    pub uncompiled_funcs: HashMap<&'a str, &'a FuncDec<'a>>,
    pub texts: HashSet<String>,
    pub stub_ref_store: StubRefStore<'a>,
}

impl<'a, D: RegDefinition> JitData<'a, D> {
    pub fn new(funcs: Vec<&'a FuncDec<'a>>) -> Self {
        let uncompiled_funcs = funcs.into_iter().map(|e| (e.name.name, e)).collect();

        Self {
            uncompiled_funcs,
            compiled_funcs: HashMap::default(),
            texts: HashSet::default(),
            stub_ref_store: StubRefStore::default(),
        }
    }
}

impl<'a, D: RegDefinition> Default for JitData<'a, D> {
    fn default() -> Self {
        Self {
            compiled_funcs: HashMap::new(),
            uncompiled_funcs: HashMap::new(),
            texts: HashSet::default(),
            stub_ref_store: Default::default(),
        }
    }
}
