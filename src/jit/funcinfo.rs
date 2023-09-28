use crate::ast::FuncDec;
use crate::jit::arch_def::RegDefinition;
use crate::jit::lir::{LirFunction, LirReg};
use crate::jit::reg_alloc::reg_off::RegOff;
use std::collections::HashMap;

#[derive(Debug)]
pub struct FuncInfo<'a, D: RegDefinition> {
    name: &'a str,
    lir: LirFunction,
    reg_alloc: HashMap<LirReg, RegOff>,
}

impl<'a, D: RegDefinition> FuncInfo<'a, D> {
    pub fn new(name: &'a str, lir: LirFunction, reg_alloc: RegMapping<D>) -> Self {
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

    pub fn reg_alloc(&self) -> &RegMapping<D> {
        &self.reg_alloc
    }
}
