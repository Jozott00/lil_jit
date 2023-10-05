use super::*;

pub fn calc_constant(op: &BinaryOp, lhs: i32, rhs: i32) -> i32 {
    match op {
        BinaryOp::Add => lhs.wrapping_add(rhs),
        BinaryOp::Minus => lhs.wrapping_sub(rhs),
        BinaryOp::Multi => lhs.wrapping_mul(rhs),
        BinaryOp::Divide => {
            if rhs == 0 {
                0
            } else {
                lhs.wrapping_div(rhs)
            }
        }
        BinaryOp::Equals => (lhs == rhs) as i32,
        BinaryOp::NotEqual => (lhs != rhs) as i32,
        BinaryOp::Greater => (lhs > rhs) as i32,
        BinaryOp::GreaterEqual => (lhs >= rhs) as i32,
        BinaryOp::Less => (lhs < rhs) as i32,
        BinaryOp::LessEqual => (lhs <= rhs) as i32,
        BinaryOp::LogicalAnd => (lhs != 0 && rhs != 0) as i32,
        BinaryOp::LogicalOr => (lhs != 0 || rhs != 0) as i32,
        BinaryOp::Modulo => {
            if rhs == 0 {
                lhs
            } else {
                lhs % rhs
            }
        }
    }
}
