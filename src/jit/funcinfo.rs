use crate::jit::arch_def::RegDefinition;
use crate::jit::lir::LirFunction;
use crate::jit::reg_alloc::reg_mapping::RegMapping;

#[derive(Debug)]
pub struct FuncInfo<'a, D: RegDefinition> {
    name: &'a str,
    lir: LirFunction,
    reg_alloc: RegMapping<D>,
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
