use crate::jit::arch_def::{RegDefinition, Register};

#[derive(Debug)]
pub struct Arm64;

impl RegDefinition for Arm64 {
    fn callee_saved() -> &'static [Register] {
        // const CALLEE_REGS: [Register; 10] = [19, 20, 21, 22, 23, 24, 25, 26, 27, 28];
        const CALLEE_REGS: [Register; 1] = [19];
        return &CALLEE_REGS;
    }

    fn caller_saved() -> &'static [Register] {
        // do not contain temporary registers
        const CALLER_REGS: [Register; 12] = [2, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15];
        return &CALLER_REGS;
    }

    fn arg_regs() -> &'static [Register] {
        // const ARG_REGS: [Register; 8] = [0, 1, 2, 3, 4, 5, 6, 7];
        const ARG_REGS: [Register; 1] = [0];
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
        9 // caller saved but non-argument-register
    }

    fn reg_as_str(reg: Register) -> String {
        match reg {
            0..=28 => format!("R{reg}"),
            29 => "FP".to_string(),
            30 => "LR".to_string(),
            31 => "SP".to_string(),
            _ => format!("Reg {reg} is no valid register!"),
        }
    }
}
