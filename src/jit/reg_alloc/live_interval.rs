use std::collections::HashMap;
use std::hash::Hash;

use crate::jit::lir::{Label, LirFunction, LirOperand, LirReg, LIR};

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
    pub contains_func_call: bool,
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
    let mut label_pos: HashMap<&Label, usize> = HashMap::new();
    let instr_count = func.instrs().len();
    let mut call_positions = vec![false; instr_count];

    for (i, instr) in func.instrs().iter().enumerate() {
        match instr {
            LIR::BinaryExpr(dest, _, lhs, rhs) => {
                update_var(dest, i, &call_positions, &mut intervals);

                if let LirOperand::Reg(lhs) = lhs {
                    update_var(lhs, i, &call_positions, &mut intervals);
                }

                if let LirOperand::Reg(rhs) = rhs {
                    update_var(rhs, i, &call_positions, &mut intervals);
                }
            }
            LIR::InputArgLoad(dest, _) => {
                update_var(dest, i, &call_positions, &mut intervals);
            }
            LIR::Assign(dest, src) => {
                update_var(dest, i, &call_positions, &mut intervals);

                if let LirOperand::Reg(src) = src {
                    update_var(src, i, &call_positions, &mut intervals);
                }
            }
            LIR::LoadConst(dest, _) => {
                update_var(dest, i, &call_positions, &mut intervals);
            }
            LIR::JumpIfFalse(src, _) => {
                if let LirOperand::Reg(src) = src {
                    update_var(src, i, &call_positions, &mut intervals);
                }
            }
            LIR::Call(dest, _, args) => {
                update_var(dest, i, &call_positions, &mut intervals);
                for a in args {
                    if let LirOperand::Reg(a) = a {
                        update_var(a, i, &call_positions, &mut intervals);
                    }
                }
                call_positions[i] = true;
            }
            LIR::CallText(dest, _, _) => {
                update_var(dest, i, &call_positions, &mut intervals);
                call_positions[i] = true;
            }
            LIR::Return(src) => {
                if let LirOperand::Reg(src) = src {
                    update_var(src, i, &call_positions, &mut intervals)
                }
            }

            // no lir regs to update
            LIR::Label(label) => {
                label_pos.insert(label, i);
            }
            LIR::Jump(dst) => {
                let Some(label_index) = label_pos.get(dst) else {
                    continue;
                };

                let extendees: Vec<LirReg> = intervals
                    .iter()
                    .filter(|(_k, v)| v.start <= *label_index && *label_index <= v.end)
                    .map(|(k, _v)| k.clone())
                    .collect();

                for extendee in extendees {
                    update_var(&extendee, i, &call_positions, &mut intervals);
                }
            }
            LIR::Breakpoint(_) => {}
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
/// If the variable is not in 'intervals', it inserts a new LiveInterval beginning and ending at the current position.
fn update_var(
    var: &LirReg,
    pos: usize,
    func_calls: &Vec<bool>,
    intervals: &mut HashMap<LirReg, LiveInterval>,
) {
    let Some(entry) = intervals.get_mut(var) else {
        // if var not yet in intervals, add it
        intervals.insert(
            var.clone(),
            LiveInterval {
                var: var.clone(),
                start: pos,
                end: pos,
                contains_func_call: false
            },
        );
        return;
    };

    // update last occurrence
    entry.end = pos;

    // update contains_func_call if needed
    if func_calls[entry.start..entry.end].iter().any(|e| *e) {
        entry.contains_func_call = true;
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::checker::check_lil;
    use crate::jit::lir::compile_to_lir;
    use crate::jit::lir::LirReg::Var;
    use crate::parser::parse_lil_program;

    use super::*;

    fn create_prog(str: &str) -> LirFunction {
        let prog = parse_lil_program(str).expect("Couldn't parse program");
        let err = check_lil(&prog);
        err.expect("Some checks failed!");
        let first_func = prog.functions.first().unwrap();
        compile_to_lir(first_func)
    }

    #[test]
    fn loop_contains_func_call() {
        let lir = create_prog(
            r"
            fn main() {
                for let i = 0; i < 2; i = i+1 {
                    show(0)
                }
            }
        ",
        );

        let intervals = compute_live_intervals(&lir);

        let around_function_calls: Vec<LirReg> = intervals
            .iter()
            .filter(|e| e.contains_func_call)
            .map(|e| e.var.clone())
            .collect();

        assert_eq!(around_function_calls, [Var("i".to_string())])
    }

    #[test]
    fn multi_vars() {
        let lir = create_prog(
            r"
            fn main() {
                let a = 1
                let b = 2
                let c = 3
                show(a)
                show(b)
                let d = 2
                show(b)
                c + d
            }
        ",
        );

        let intervals = compute_live_intervals(&lir);

        let around_function_calls: HashSet<LirReg> = intervals
            .iter()
            .filter(|e| e.contains_func_call)
            .map(|e| e.var.clone())
            .collect();

        assert_eq!(
            around_function_calls,
            [
                Var("b".to_string()),
                Var("c".to_string()),
                Var("d".to_string())
            ]
            .into_iter()
            .collect()
        )
    }
}
