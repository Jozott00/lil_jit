use std::fmt::Debug;

pub mod arm64;

pub type Register = armoured_rust::types::Register;

pub trait RegDefinition: Debug {
    fn callee_saved() -> &'static [Register];
    fn caller_saved() -> &'static [Register];

    fn arg_regs() -> &'static [Register];

    fn ret_reg() -> Register;

    fn temp1() -> Register;
    fn temp2() -> Register;
    fn temp3() -> Register;

    fn reg_as_str(reg: Register) -> String;
}
