use crate::ast::FuncDec;
use crate::jit::lir::{LirFunction, LirReg};
use crate::jit::reg_alloc::reg_off::RegOff;
use std::collections::HashMap;

#[derive(Debug)]
pub struct FuncInfo<'a> {
    name: &'a str,
    lir: LirFunction,
    reg_alloc: HashMap<LirReg, RegOff>,
}

impl<'a> FuncInfo<'a> {
    pub fn new(name: &'a str, lir: LirFunction, reg_alloc: HashMap<LirReg, RegOff>) -> Self {
        Self {
            name,
            lir,
            reg_alloc,
        }
    }

    pub fn name(&self) -> &'a str {
        self.name
    }

    pub fn lir(&self) -> &LirFunction {
        &self.lir
    }

    pub fn reg_alloc(&self) -> &HashMap<LirReg, RegOff> {
        &self.reg_alloc
    }
}
