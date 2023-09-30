use std::collections::HashMap;

use crate::jit::reg_alloc::reg_off::RegOff;
use crate::jit::scope::Scope;

// TODO: Write somewhere else
const MAX_CALLEE_REG: i32 = 18;

struct RegRepo<'a> {
    vars: HashMap<&'a str, RegOff>,
    curr_reg: i32,
}

impl<'a> Scope<RegRepo<'a>> {
    pub fn acquire(var: &'a str) -> RegOff {
        todo!()
    }
}

impl<'a> Default for RegRepo<'a> {
    fn default() -> Self {
        todo!()
    }
}
