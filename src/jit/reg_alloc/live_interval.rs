use std::collections::HashMap;

use crate::jit::lir::{LIR, LirFunction, LirReg};

/// Vector of `LiveInterval`, representing all live intervals in functions.
pub type LiveIntervals = Vec<LiveInterval>;

/// # LiveInterval
/// Struct representing a live interval within the code.
/// The `LiveInterval` structure contains a variable (LirReg),
/// its start and end positions. These define the range of instructions
/// in which the variable `var` is live.
///
/// # Fields
/// * `var` - Variable of type LirReg for which the live interval is computed.
/// * `start` - The start position of the live interval.
/// * `end` - The end position of the live interval.
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct LiveInterval {
    pub var: LirReg,
    pub start: usize,
    pub end: usize,
    // TODO: Maybe check if contains function call?
}

/// # compute_live_intervals
/// This function computes the live intervals for the variables in the given `LirMethod`.
///
/// # Arguments
/// * `instrs` - A reference to a LirMethod.
///
/// # Outcome
/// The function returns a unsorted list of LiveIntervals that holds all variable/register
/// intervals of a function.
pub fn compute_live_intervals(func: &LirFunction) -> LiveIntervals {
    let mut intervals = HashMap::new();

    for (i, instr) in func.instrs().iter().enumerate() {
        match instr {
            LIR::BinaryExpr(dest, _, lhs, rhs) => {
                update_var(dest, i, &mut intervals);
                update_var(lhs, i, &mut intervals);
                update_var(rhs, i, &mut intervals);
            }
            LIR::InputArgLoad(dest, _) => {
                update_var(dest, i, &mut intervals);
            }
            LIR::Assign(dest, src) => {
                update_var(dest, i, &mut intervals);
                update_var(src, i, &mut intervals);
            }
            LIR::LoadConst(dest, _) => {
                update_var(dest, i, &mut intervals);
            }
            LIR::JumpIfFalse(src, _) => {
                update_var(src, i, &mut intervals);
            }
            LIR::Call(dest, _, args) => {
                update_var(dest, i, &mut intervals);
                for a in args {
                    update_var(a, i, &mut intervals);
                }
            }
            LIR::CallText(dest, _, _) => {
                update_var(dest, i, &mut intervals);
            }
            LIR::Return(src) => update_var(src, i, &mut intervals),

            // no lir regs to update
            LIR::Label(_) => {}
            LIR::Jump(_) => {}
        }
    }

    return intervals.into_iter().map(|(_, i)| i).collect();
}

/// # update_var
/// This function updates the live interval of a LirReg variable.
///
/// # Arguments
/// * `var` - A reference to the variable of type LirReg.
/// * `pos` - The current position in the instructions list.
/// * `intervals` - A mutable reference to a hashmap with variable as key and LiveInterval as value.
///
/// # Outcome
/// If the variable is already in 'intervals', it updates the end of the interval with the current position.
/// If the variable is not in 'intervals', it inserts a new LiveInterval beign at the current position and ends at the same.
fn update_var(var: &LirReg, pos: usize, intervals: &mut HashMap<LirReg, LiveInterval>) {
    let Some(entry) = intervals.get_mut(var) else {
        // if var not yet in intervals, add it
        intervals.insert(
            var.clone(),
            LiveInterval {
                var: var.clone(),
                start: pos,
                end: pos,
            },
        );
        return;
    };

    // update last occurrence
    entry.end = pos;
}
