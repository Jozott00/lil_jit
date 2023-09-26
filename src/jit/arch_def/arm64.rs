use crate::jit::arch_def::{RegDefinition, Register};

pub struct Arm64;

impl RegDefinition for Arm64 {
    fn callee_saved() -> &'static [Register] {
        const CALLEE_REGS: [Register; 10] = [19, 20, 21, 22, 23, 24, 25, 26, 27, 28];
        return &CALLEE_REGS;
    }

    fn caller_saved() -> &'static [Register] {
        const CALLER_REGS: [Register; 12] = [3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15];
        return &CALLER_REGS;
    }

    fn arg_regs() -> &'static [Register] {
        const ARG_REGS: [Register; 8] = [0, 1, 2, 3, 4, 5, 6, 7];
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
}
