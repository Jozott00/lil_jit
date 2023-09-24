mod reg_off;
mod reg_repo;

use crate::ast::FuncDec;
use crate::jit::funcinfo::FuncInfo;
use crate::visitor::NodeVisitor;

pub fn alloc_reg<'a>(func_dec: &'a FuncDec<'a>) -> FuncInfo<'a> {
    let reg_alloc = RegAllocator::new(func_dec);
    reg_alloc.alloc_regs()
}

struct RegAllocator<'a> {
    // var_regs: HashMap<&str, >
    func_dec: &'a FuncDec<'a>,
}

impl<'a> RegAllocator<'a> {
    fn new(func_dec: &'a FuncDec<'a>) -> Self {
        RegAllocator { func_dec }
    }

    fn alloc_regs(mut self) -> FuncInfo<'a> {
        todo!()
    }
}

impl<'a> NodeVisitor<'a> for RegAllocator<'a> {}

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
