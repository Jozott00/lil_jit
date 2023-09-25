mod live_interval;
pub mod reg_off;
mod reg_repo;

use crate::jit::arch_def::{RegDefinition, Register};
use crate::jit::lir::LirFunction;
use crate::jit::lir::LirReg;
use crate::jit::reg_alloc::live_interval::{compute_live_intervals, LiveInterval, LiveIntervals};
use crate::jit::reg_alloc::reg_off::RegOff;
use std::collections::HashMap;
use std::marker::PhantomData;

pub fn alloc_reg<D: RegDefinition>(func: &LirFunction) -> HashMap<LirReg, RegOff> {
    let live_intervals = compute_live_intervals(func);
    let reg_alloc = RegAllocator::<D>::new();
    reg_alloc.linear_scan(live_intervals)
}

struct RegAllocator<D: RegDefinition> {
    reg_def: PhantomData<D>, // to be able to use the generic static type RegDefinition
    active: Vec<(Register, LiveInterval)>,
    finalized: HashMap<LirReg, RegOff>,
    pool: Vec<Register>,
    spill_offset: usize,
}

impl<D: RegDefinition> RegAllocator<D> {
    fn new() -> Self {
        let pool = D::callee_saved().to_vec(); // TODO: Later use also caller saved registers

        Self {
            reg_def: Default::default(),
            active: Vec::new(),
            finalized: HashMap::new(),
            pool,
            spill_offset: 0,
        }
    }

    fn linear_scan(mut self, mut live_intervals: LiveIntervals) -> HashMap<LirReg, RegOff> {
        // sort intervals by start point
        live_intervals.sort_by(|a, b| a.start.cmp(&b.start));

        for li in live_intervals {
            self.expire_old_intervals(&li);

            if let Some(reg) = self.pool.pop() {
                self.active_new_interval(li, reg);
            } else {
                self.spill_at_interval(li)
            }
        }

        for (reg, li) in self.active {
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
                let (reg, fli) = self.active.remove(i);
                self.finalized.insert(fli.var, RegOff::Reg(reg));

                // Push the register back to the pool
                self.pool.push(reg);
            } else {
                // TODO: Maybe just return
                i += 1;
            }
        }
    }

    fn active_new_interval(&mut self, interval: LiveInterval, reg: Register) {
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

    fn next_spill_offset(&mut self) -> usize {
        let off = self.spill_offset;
        self.spill_offset += 4;
        off
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checker::check_lil;
    use crate::jit::lir::{compile_to_lir, LirReg};
    use crate::parser::parse_lil_program;

    fn create_prog(str: &str) -> LirFunction {
        let prog = parse_lil_program(str).expect("Couldn't parse program");
        let err = check_lil(&prog);
        err.expect("Some checks failed!");
        let first_func = prog.functions.first().unwrap();
        compile_to_lir(first_func)
    }

    #[test]
    fn test_minimal_prog() {
        let lir = create_prog(
            "
            fn main() {
                let c = 1
                c = c + 1
                let a = 2 + 3 * c
                let b = 2 * (a + 2)
                a = b
            }
            ",
        );

        println!("LIR FUNCTION DUMP:");
        println!("{lir}");
        println!("------------------\n");

        let allocs = alloc_reg::<TestArch>(&lir);

        println!("REGISTER ALLOC DUMP:");
        for (var, reg_off) in allocs {
            println!("{}: {:?}", var, reg_off);
        }
        println!("------------------\n");
    }

    #[test]
    fn test_live_range() {
        let live_intervals = vec![
            LiveInterval {
                var: LirReg::Var("a".to_string()),
                start: 1,
                end: 10,
            },
            LiveInterval {
                var: LirReg::Var("b".to_string()),
                start: 1,
                end: 4,
            },
            LiveInterval {
                var: LirReg::Var("c".to_string()),
                start: 1,
                end: 3,
            },
            LiveInterval {
                var: LirReg::Var("d".to_string()),
                start: 2,
                end: 8,
            },
            LiveInterval {
                var: LirReg::Var("e".to_string()),
                start: 3,
                end: 6,
            },
            LiveInterval {
                var: LirReg::Var("f".to_string()),
                start: 3,
                end: 10,
            },
            LiveInterval {
                var: LirReg::Var("g".to_string()),
                start: 4,
                end: 8,
            },
        ];

        let reg_alloc = RegAllocator::<TestArch>::new();
        let allocs = reg_alloc.linear_scan(live_intervals);
        println!("REGISTER ALLOC DUMP:");
        for (var, reg_off) in allocs {
            println!("{}: {:?}", var, reg_off);
        }
        println!("------------------\n");
    }

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
    }
}
