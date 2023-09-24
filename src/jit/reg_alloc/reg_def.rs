pub type Register = armoured_rust::types::Register;

pub trait RegDefinition {
    fn callee_saved() -> &'static [Register];
    fn caller_saved() -> &'static [Register];
}

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
}
