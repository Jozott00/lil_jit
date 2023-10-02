use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;

use crate::jit::arch_def::{RegDefinition, Register};
use crate::jit::lir::LirReg;
use crate::jit::reg_alloc::reg_off::RegOff;

#[derive(Debug)]
pub struct RegMapping<D: RegDefinition> {
    reg_def: PhantomData<D>,
    reg_map: HashMap<LirReg, RegOff>,
    used_callee_saved: Vec<Register>,
}

impl<'a, D: RegDefinition> RegMapping<D> {
    pub fn new() -> Self {
        Self {
            reg_def: Default::default(),
            reg_map: Default::default(),
            used_callee_saved: Default::default(),
        }
    }

    pub fn regoff_for(&self, r: &LirReg) -> Option<&RegOff> {
        self.reg_map.get(r)
    }

    pub fn insert(&mut self, k: LirReg, v: RegOff) {
        self.reg_map.insert(k, v);

        let RegOff::Reg(r)  = v else {
            return;
        };

        if !self.used_callee_saved.contains(&r) && D::callee_saved().contains(&r) {
            self.used_callee_saved.push(r);
        }
    }

    pub fn callee_saved(&self) -> Vec<Register> {
        return self.used_callee_saved.clone();
    }

    pub fn number_of_spills(&self) -> usize {
        self.reg_map
            .iter()
            .filter(|(_, v)| matches!(v, RegOff::Off(_)))
            .count()
    }
}

impl<D: RegDefinition> Display for RegMapping<D> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Register Mappings:\n")?;

        let mut vec: Vec<(&LirReg, &RegOff)> = self.reg_map.iter().collect();
        vec.sort_by(|a, b| a.0.cmp(&b.0));

        for (lir_reg, reg_off) in vec {
            match reg_off {
                RegOff::Reg(reg) => {
                    write!(f, "\t{lir_reg} assigned to {}\n", D::reg_as_str(*reg))?;
                }
                RegOff::Off(off) => {
                    write!(f, "\t{lir_reg} spilled to offset {off}\n")?;
                }
            }
        }

        write!(
            f,
            "Used Callee-Saved Registers: {}",
            self.callee_saved()
                .iter()
                .map(|r| D::reg_as_str(*r))
                .collect::<Vec<String>>()
                .join(", ")
        )?;

        Ok(())
    }
}
