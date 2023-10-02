use std::marker::PhantomData;
use std::ptr;

use crate::jit::arch_def::{RegDefinition, Register};
use crate::jit::lir::LirFunction;
use crate::jit::reg_alloc::live_interval::{compute_live_intervals, LiveInterval, LiveIntervals};
use crate::jit::reg_alloc::reg_mapping::RegMapping;
use crate::jit::reg_alloc::reg_off::RegOff;

mod live_interval;
pub mod reg_mapping;
pub mod reg_off;

pub fn alloc_reg<D: RegDefinition>(func: &LirFunction) -> RegMapping<D> {
    let live_intervals = compute_live_intervals(func);
    let reg_alloc = RegAllocator::<D>::new();
    reg_alloc.linear_scan(live_intervals)
}

/// - 0: The pool where the register is from
/// - 1: The register popped from the pool
type InternalRegister = (RegPoolOption, Register);

enum RegPoolOption {
    CalleeSaved,
    SaveCallerSaved,
}

struct RegAllocator<D: RegDefinition> {
    reg_def: PhantomData<D>, // to be able to use the generic static type RegDefinition
    active: Vec<(InternalRegister, LiveInterval)>,
    finalized: RegMapping<D>,
    pool_callee_saved: Vec<Register>,
    pool_save_caller_saved: Vec<Register>, // caller saved registers that are not argument registers (so no conflicts for args and params)
    spill_offset: usize,
}

impl<D: RegDefinition> RegAllocator<D> {
    fn new() -> Self {
        let pool_callee_saved = D::callee_saved().to_vec(); // TODO: Later use also caller saved registers
        let pool_save_caller_saved = D::caller_saved()
            .iter()
            .filter(|e| !D::arg_regs().contains(e))
            .copied()
            .collect();

        Self {
            reg_def: Default::default(),
            active: Vec::new(),
            finalized: RegMapping::new(),
            pool_callee_saved,
            pool_save_caller_saved,
            spill_offset: 0,
        }
    }

    fn linear_scan(mut self, mut live_intervals: LiveIntervals) -> RegMapping<D> {
        // sort intervals by start point
        live_intervals.sort_by(|a, b| a.start.cmp(&b.start));

        for li in live_intervals {
            self.expire_old_intervals(&li);

            if let Some(reg) = self.pop_reg_from_pool(&li) {
                self.active_new_interval(li, reg);
            } else {
                self.spill_at_interval(li)
            }
        }

        for ((_, reg), li) in self.active {
            self.finalized.insert(li.var, RegOff::Reg(reg));
        }

        self.finalized
    }

    fn expire_old_intervals(&mut self, current_interval: &LiveInterval) {
        let mut i = 0;

        while i < self.active.len() {
            let (_, li) = &self.active[i];

            if li.end <= current_interval.start {
                // Remove the element from active and add it to finalized
                let (internal @ (_, reg), fli) = self.active.remove(i);
                self.finalized.insert(fli.var, RegOff::Reg(reg));

                // Push the register back to the pool
                self.push_reg_to_pool(internal);
            } else {
                // TODO: Maybe just return
                i += 1;
            }
        }
    }

    fn active_new_interval(&mut self, interval: LiveInterval, reg: InternalRegister) {
        self.active.push((reg, interval));
    }

    fn spill_at_interval(&mut self, interval: LiveInterval) {
        // unwrap as if we spill we have some active intervals
        let spill_offset = self.next_spill_offset();

        // sort actives and get last (with last interval end)
        self.active.sort_by(|(_, a), (_, b)| a.end.cmp(&b.end));
        let (_, spill) = self.active.last().unwrap();

        if spill.end > interval.end {
            // change last active interval with current interval
            let (reg, il) = self.active.pop().unwrap();
            self.active.push((reg, interval));

            // spill last active interval
            self.finalized.insert(il.var, RegOff::Off(spill_offset));
        } else {
            // spill current interval
            self.finalized
                .insert(interval.var, RegOff::Off(spill_offset));
        }
    }

    /// Pops register from register pool.
    ///
    /// If live interval doesnt contain register call,
    /// save caller saved register were preferred.
    /// Otherwise only callee saved register may be used.
    fn pop_reg_from_pool(&mut self, live_interval: &LiveInterval) -> Option<InternalRegister> {
        if live_interval.contains_func_call {
            self.pool_callee_saved
                .pop()
                .map(|r| (RegPoolOption::CalleeSaved, r))
        } else {
            self.pool_save_caller_saved
                .pop()
                .map(|r| (RegPoolOption::SaveCallerSaved, r))
                .or_else(|| {
                    self.pool_callee_saved
                        .pop()
                        .map(|r| (RegPoolOption::CalleeSaved, r))
                })
        }
    }

    fn push_reg_to_pool(&mut self, (pool, reg): InternalRegister) {
        match pool {
            RegPoolOption::CalleeSaved => {
                self.pool_callee_saved.push(reg);
            }
            RegPoolOption::SaveCallerSaved => {
                self.pool_save_caller_saved.push(reg);
            }
        }
    }

    fn next_spill_offset(&mut self) -> usize {
        let off = self.spill_offset;
        self.spill_offset += 4;
        off
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use crate::checker::check_lil;
    use crate::jit::lir::compile_to_lir;
    use crate::parser::parse_lil_program;

    use super::*;

    #[allow(dead_code)]
    fn create_prog(str: &str) -> LirFunction {
        let prog = parse_lil_program(str).expect("Couldn't parse program");
        let err = check_lil(&prog);
        err.expect("Some checks failed!");
        let first_func = prog.functions.first().unwrap();
        compile_to_lir(first_func)
    }

    #[derive(Debug)]
    pub struct TestArch;

    impl RegDefinition for TestArch {
        fn callee_saved() -> &'static [Register] {
            const CALLEE_REGS: [Register; 3] = [3, 2, 1];
            return &CALLEE_REGS;
        }

        fn caller_saved() -> &'static [Register] {
            const CALLER_REGS: [Register; 12] = [3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15];
            return &CALLER_REGS;
        }

        fn arg_regs() -> &'static [Register] {
            const ARG_REGS: [Register; 12] = [3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15];
            return &ARG_REGS;
        }

        fn ret_reg() -> Register {
            0
        }

        fn temp1() -> Register {
            0
        }

        fn temp2() -> Register {
            1
        }

        fn temp3() -> Register {
            2
        }

        fn reg_as_str(reg: Register) -> String {
            reg.to_string()
        }
    }
}
