use super::*;
use crate::jit::lir::helper::calc_constant;
use std::collections::{HashMap, HashSet};

impl LirFunction {
    /// Optimizes assigns of constants that are never reassigned
    pub fn optimize_constant_single_assigns(self) -> LirFunction {
        let mut variable_usages: HashMap<&LirReg, usize> = HashMap::new();

        for instr in &self.0 {
            if let Assign(lir, _) = instr {
                if let Some(val) = variable_usages.get(lir) {
                    variable_usages.insert(lir, *val + 1);
                } else {
                    variable_usages.insert(lir, 1);
                }
            }
        }

        let mut optimized_vars: HashMap<&LirReg, i32> = HashMap::new();
        let mut new_func: Vec<LIR> = vec![];

        for instr in &self.0 {
            match instr {
                Assign(reg, op) => {
                    let new_op = new_operand_of(op, &optimized_vars);

                    let new_instr = Assign(reg.clone(), new_op.clone());

                    let Some(use_count) = variable_usages.get(&reg) else {
                        new_func.push(new_instr);
                        continue;
                    };

                    // cannot optimize as variable changes unpredictable
                    if *use_count > 1 {
                        new_func.push(new_instr);
                        continue;
                    }

                    // if value isn't constant, we cannot optimize it
                    let LirOperand::Constant(constant) = new_op else {
                        new_func.push(new_instr);
                        continue;
                    };

                    if !encodable_as_imm12_shift(constant) {
                        new_func.push(new_instr);
                        continue;
                    }

                    optimized_vars.insert(reg, constant);
                }
                BinaryExpr(dest, op, lhs, rhs) => {
                    let new_lhs = new_operand_of(lhs, &optimized_vars);
                    let new_rhs = new_operand_of(rhs, &optimized_vars);

                    // if both sides are constants now, we calculate them and add the variable as optimized
                    if let (LirOperand::Constant(lhs), LirOperand::Constant(rhs)) =
                        (new_lhs.clone(), new_rhs.clone())
                    {
                        let this_constant = calc_constant(op, lhs, rhs);
                        optimized_vars.insert(dest, this_constant);
                        continue;
                    } else {
                        //else add it to function list as normal
                        new_func.push(BinaryExpr(dest.clone(), op.clone(), new_lhs, new_rhs))
                    }
                }
                Return(src) => {
                    let new_src = new_operand_of(src, &optimized_vars);
                    new_func.push(Return(new_src))
                }
                Call(dest, name, args) => {
                    let args = args
                        .iter()
                        .map(|a| new_operand_of(a, &optimized_vars))
                        .collect();

                    new_func.push(Call(dest.clone(), name.clone(), args))
                }
                JumpIfFalse(src, label) => {
                    let src = new_operand_of(src, &optimized_vars);
                    new_func.push(JumpIfFalse(src, label.clone()))
                }
                _ => new_func.push(instr.clone()),
            }
        }

        LirFunction(new_func)
    }
}

fn new_operand_of(op: &LirOperand, optimized_vars: &HashMap<&LirReg, i32>) -> LirOperand {
    if let LirOperand::Reg(op) = op {
        if let Some(constant) = optimized_vars.get(op) {
            LirOperand::Constant(*constant)
        } else {
            LirOperand::Reg(op.clone())
        }
    } else {
        op.clone()
    }
}

fn encodable_as_imm12_shift(value: i32) -> bool {
    if value >= 0 && value <= 0xFFF {
        return true;
    }

    // Check for 12-bit shift
    if (value & 0xFFF) == 0 && (value >> 12) <= 0xFFF {
        return true;
    }

    return false;
}
