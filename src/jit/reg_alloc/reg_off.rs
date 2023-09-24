use armoured_rust::types::Register;

/// Represents the value of a register or offset
#[derive(Debug, Copy, Clone)]
pub enum RegOff {
    Reg(Register),
    Off(usize),
}
