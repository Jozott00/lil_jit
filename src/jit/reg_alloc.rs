mod live_interval;
mod reg_off;
mod reg_repo;

use crate::ast::FuncDec;
use crate::jit::funcinfo::FuncInfo;
use crate::jit::lir::LirFunction;
use crate::jit::reg_alloc::live_interval::LiveIntervals;
use crate::visitor::NodeVisitor;

pub fn alloc_reg<'a>(func_dec: &'a FuncDec<'a>) -> FuncInfo<'a> {
    todo!()
    // let reg_alloc = RegAllocator::new(func_dec);
    // reg_alloc.alloc_regs()
}

struct RegAllocator {
    live_intervals: LiveIntervals,
}

impl RegAllocator {
    // fn new(func: &LirFunction) -> Self {
    //     // let live_intervals =
    //     // RegAllocator { func_dec }
    // }

    fn alloc_regs(mut self) {
        todo!()
    }
}

/* / 3 regs, 1 tmp default
let a = (3 + (2 - 1))

         0      t
    0       0


let a = 3 + func(2, 3, 4)
    0


let b = a
    1   1

b = b + 1
    1   2

showln(a)
*/
