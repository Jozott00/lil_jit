mod compiler;
mod helper;
pub mod optimize;

use std::fmt;
use std::fmt::{Display, Formatter};

use crate::ast;
use crate::ast::ExprKind::StringLiteral;
use crate::ast::{BinaryOp, Expr, ExprKind, FuncDec, Stmt, StmtKind};
use crate::jit::lir::compiler::LirCompiler;
use crate::jit::lir::LirReg::{Tmp, Var};
use crate::jit::lir::LIR::{
    Assign, BinaryExpr, Breakpoint, Call, CallText, InputArgLoad, Jump, JumpIfFalse, Return,
};

pub fn compile_to_lir<'a>(func: &'a FuncDec<'a>) -> LirFunction {
    let compiler = LirCompiler::new(func);
    compiler.compile()
}

#[derive(Debug)]
pub struct LirFunction(Vec<LIR>);

impl LirFunction {
    pub fn instrs(&self) -> &Vec<LIR> {
        &self.0
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum LIR {
    // TODO: Split up into op specific instructions?
    BinaryExpr(LirReg, ast::BinaryOp, LirOperand, LirOperand),

    // mem movement
    InputArgLoad(LirReg, usize), // (destination, argument index)
    Assign(LirReg, LirOperand),

    // control flow
    Label(Label),
    Jump(Label),
    JumpIfFalse(LirOperand, Label),

    // func
    Call(LirReg, String, Vec<LirOperand>),
    // dest, func_name, args
    CallText(LirReg, bool, String),
    Return(LirOperand),

    // debug
    Breakpoint(usize),
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub enum LirReg {
    Var(String),
    Tmp(u32),
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd)]
pub enum LirOperand {
    Reg(LirReg),
    Constant(i32),
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub struct Label(u32);

// DISPLAY IMPLEMENTATIONS

impl fmt::Display for LirReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LirReg::Var(var) => write!(f, "Var({})", var),
            LirReg::Tmp(tmp) => write!(f, "Tmp({})", tmp),
        }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Label({})", self.0)
    }
}

impl<'a> fmt::Display for LIR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LIR::BinaryExpr(reg_a, op, reg_b, reg_c) => {
                write!(f, "BinaryExpr({}, {}, {}, {})", reg_a, op, reg_b, reg_c)
            }
            InputArgLoad(dst, i) => write!(f, "InputArgLoad({}, {})", dst, i),
            LIR::Assign(reg_a, reg_b) => write!(f, "Assign({}, {})", reg_a, reg_b),
            LIR::Label(label) => write!(f, "Label({})", label),
            LIR::Jump(label) => write!(f, "Jump({})", label),
            LIR::JumpIfFalse(reg, label) => write!(f, "JumpIfFalse({}, {})", reg, label),
            LIR::Call(reg, func_name, args) => {
                write!(f, "Call({}, {}, ", reg, func_name)?;
                let args_strings: Vec<String> = args.iter().map(ToString::to_string).collect();
                write!(f, "[{}])", args_strings.join(", "))
            }
            LIR::CallText(_, has_newline, text) => {
                write!(f, "CallText(\"{}\", newline: {})", text, has_newline)
            }
            LIR::Return(reg) => write!(f, "Return({})", reg),
            Breakpoint(line) => write!(f, "Breakpoint({})", line),
        }
    }
}

impl fmt::Display for LirFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let joined_str = self
            .0
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<String>>()
            .join("\n");
        write!(f, "{}", joined_str)
    }
}

impl Display for LirOperand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LirOperand::Reg(reg) => {
                write!(f, "{reg}")
            }
            LirOperand::Constant(c) => {
                write!(f, "Constant({c})")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{BinaryOp, Program};
    use crate::checker::check_lil;
    use crate::parser::parse_lil_program;

    use super::*;

    fn create_prog(str: &str) -> Program {
        let prog = parse_lil_program(str).expect("Couldn't parse program");
        let err = check_lil(&prog);
        err.expect("Some checks failed!");
        prog
    }

    #[test]
    fn test_minimal_prog() {
        let prog = create_prog("fn main() {}");
        let first_func = prog.functions.first().unwrap();
        let lir = compile_to_lir(first_func);

        assert_eq!(lir.instrs().len(), 1);
        let expected_lir = vec![LIR::Return(LirOperand::Constant(0))];
        assert_eq!(lir.0, expected_lir);
    }

    #[test]
    fn test_small_add() {
        let prog = create_prog(
            "
        fn main() {
            let a = 2 + 3            
        }
        ",
        );
        let first_func = prog.functions.first().unwrap();
        let lir = compile_to_lir(first_func);

        let expected_lir = vec![
            LIR::Assign(LirReg::Var("a".to_string()), LirOperand::Constant(5)),
            LIR::Return(LirOperand::Constant(0)),
        ];

        assert_eq!(lir.0, expected_lir);
    }

    #[test]
    fn test_two_vars() {
        let prog = create_prog(
            "
        fn main() {
            let a = 2 + 3
            let b = 2 * (a + 2)
            a = b   
            return b         
        }
        ",
        );
        let first_func = prog.functions.first().unwrap();
        let lir = compile_to_lir(first_func);

        let expected_lir = vec![
            Assign(Var("a".to_string()), LirOperand::Constant(5)),
            BinaryExpr(
                Tmp(0),
                BinaryOp::Add,
                LirOperand::Reg(Var("a".to_string())),
                LirOperand::Constant(2),
            ),
            BinaryExpr(
                Tmp(1),
                BinaryOp::Multi,
                LirOperand::Constant(2),
                LirOperand::Reg(Tmp(0)),
            ),
            Assign(Var("b".to_string()), LirOperand::Reg(Tmp(1))),
            Assign(Var("a".to_string()), LirOperand::Reg(Var("b".to_string()))),
            Return(LirOperand::Reg(Var("b".to_string()))),
        ];

        let str = lir
            .instrs()
            .iter()
            .map(|lir| format!("{:?}", lir))
            .collect::<Vec<String>>()
            .join("\n");
        print!("{str}");

        assert_eq!(*lir.instrs(), expected_lir);
    }
}
