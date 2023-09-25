use std::collections::{HashMap, HashSet};

use crate::ast::FuncDec;
use crate::jit::codeinfo::CodeInfo;

#[derive(Debug)]
pub struct JitData<'a> {
    pub compiled_funcs: HashMap<&'a str, CodeInfo<'a>>,
    pub uncompiled_funcs: HashMap<&'a str, &'a FuncDec<'a>>,
    pub texts: HashSet<String>,
}

impl<'a> JitData<'a> {
    pub fn new(funcs: Vec<&'a FuncDec<'a>>) -> Self {
        let uncompiled_funcs = funcs.into_iter().map(|e| (e.name.name, e)).collect();

        Self {
            uncompiled_funcs,
            compiled_funcs: HashMap::default(),
            texts: HashSet::default(),
        }
    }
}

impl<'a> Default for JitData<'a> {
    fn default() -> Self {
        Self {
            compiled_funcs: HashMap::new(),
            uncompiled_funcs: HashMap::new(),
            texts: HashSet::default(),
        }
    }
}
