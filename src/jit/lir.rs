use std::fmt;
use std::fmt::{write, Display, Formatter};

use crate::ast;
use crate::ast::ExprKind::StringLiteral;
use crate::ast::{BinaryOp, Expr, ExprKind, FuncDec, Stmt, StmtKind};
use crate::jit::lir::LirReg::{Tmp, Var};
use crate::jit::lir::LIR::{
    Assign, BinaryExpr, Breakpoint, Call, CallText, InputArgLoad, Jump, JumpIfFalse, LoadConst,
    Return,
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
    LoadConst(LirReg, i32),

    // control flow
    Label(Label),
    Jump(Label),
    JumpIfFalse(LirReg, Label),

    // func
    Call(LirReg, String, Vec<LirReg>),
    // dest, func_name, args
    CallText(LirReg, bool, String),
    Return(LirReg),

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

struct LirCompiler<'a> {
    ast: &'a FuncDec<'a>,
    instrs: Vec<LIR>,
    tmp_count: u32,
    label_count: u32,
}

impl<'a> LirCompiler<'a> {
    fn new(ast: &'a FuncDec<'a>) -> LirCompiler {
        LirCompiler {
            ast,
            instrs: Vec::new(),
            tmp_count: 0,
            label_count: 0,
        }
    }

    fn compile(mut self) -> LirFunction {
        self.flat_funcdec(&self.ast);
        LirFunction(self.instrs)
    }
}

impl<'a> LirCompiler<'a> {
    fn flat_funcdec(&mut self, func_dec: &'a FuncDec<'a>) {
        for (i, arg) in func_dec.params.iter().enumerate() {
            self.instrs.push(InputArgLoad(Var(arg.name.to_string()), i))
        }

        self.flat_stmt(&func_dec.body);

        // if last is stmt kind, return
        if let Some(last) = self.instrs.last() {
            if matches!(last, Return(_)) {
                return;
            }
        }

        // else add return 0
        let tmp = self.new_tmp();
        self.instrs.push(LoadConst(tmp.clone(), 0));
        self.instrs.push(Return(tmp));
    }
    fn flat_stmt(&mut self, node: &'a Stmt) {
        match &node.kind {
            StmtKind::Assignment(_, var, expr) => {
                let src = self.flat_expr(expr);
                let instr = Assign(Var(var.name.to_string()), src);
                self.instrs.push(instr);
            }
            StmtKind::Block(stmts) => {
                for s in stmts {
                    self.flat_stmt(s)
                }
            }
            StmtKind::If(cond, then_block, else_block) => {
                let then_label = self.new_label();
                let else_label = self.new_label();
                let end_label = self.new_label();

                // flat condition expression
                let cond_dest = self.flat_expr(cond);
                let cond_dest = self.load_flat_result(cond_dest);

                // add jump if false
                self.instrs.push(JumpIfFalse(cond_dest, else_label));

                // flat then block
                self.instrs.push(LIR::Label(then_label));
                self.flat_stmt(then_block);
                self.instrs.push(Jump(end_label));

                // flat else block
                self.instrs.push(LIR::Label(else_label));
                if let Some(else_block) = else_block {
                    self.flat_stmt(else_block)
                }

                // push end label
                self.instrs.push(LIR::Label(end_label));
            }
            StmtKind::For(init, cond, update, body) => {
                let start_label = self.new_label();
                let end_label = self.new_label();

                // flat init statement
                if let Some(init) = init {
                    self.flat_stmt(init);
                }

                // push start label of loop
                self.instrs.push(LIR::Label(start_label));

                // flat condition
                let cond_dest = self.flat_expr(cond);
                let cond_dest = self.load_flat_result(cond_dest);

                // create JumpIfFalse, to jump to end if condition not met
                self.instrs.push(JumpIfFalse(cond_dest, end_label));

                // flat body
                self.flat_stmt(body);

                // flat update if existing
                if let Some(update) = update {
                    self.flat_stmt(update)
                }

                // jump to start label for condition checking
                self.instrs.push(Jump(start_label));

                // push end label
                self.instrs.push(LIR::Label(end_label));
            }
            StmtKind::ExprStmt(expr) => {
                self.flat_expr(expr);
            }
            StmtKind::Return(expr) => {
                let src = self.flat_expr(expr);
                let src = self.load_flat_result(src);
                let instr = Return(src);
                self.instrs.push(instr)
            }
            StmtKind::Breakpoint(line) => self.instrs.push(Breakpoint(*line)),
        }
    }

    fn flat_expr(&mut self, node: &'a Expr) -> LirOperand {
        match &node.kind {
            ExprKind::IntegerLiteral(num) => {
                // let dest = self.new_tmp();
                // self.instrs.push(LoadConst(dest.clone(), *num));
                // dest
                LirOperand::Constant(*num)
            }
            ExprKind::StringLiteral(_) => LirOperand::Reg(self.new_tmp()),
            ExprKind::FunctionCall(func_data) => {
                if func_data.function_name.name == "showtext" {
                    let dest = self.new_tmp();
                    let StringLiteral(text) = func_data.arguments.get(0).unwrap().kind else {
                        panic!("This should not happen");
                    };
                    self.instrs
                        .push(CallText(dest.clone(), false, text.to_string()));
                    return LirOperand::Reg(dest);
                }
                if func_data.function_name.name == "showtextln" {
                    let dest = self.new_tmp();
                    let StringLiteral(text) = func_data.arguments.get(0).unwrap().kind else {
                        panic!("This should not happen");
                    };
                    self.instrs
                        .push(CallText(dest.clone(), true, text.to_string()));
                    return LirOperand::Reg(dest);
                }

                let mut arg_dests = Vec::new();
                for e in &func_data.arguments {
                    let e_res = self.flat_expr(e);
                    let e_dest = self.load_flat_result(e_res);
                    arg_dests.push(e_dest);
                }
                let dest = self.new_tmp();
                let instr = Call(
                    dest.clone(),
                    func_data.function_name.name.to_string(),
                    arg_dests,
                );
                self.instrs.push(instr);
                LirOperand::Reg(dest)
            }
            ExprKind::BinaryExpr(lhs, op, rhs) => {
                let lhs_dest = self.flat_expr(lhs);
                let rhs_dest = self.flat_expr(rhs);

                match (&lhs_dest, &rhs_dest) {
                    (LirOperand::Constant(lhs_c), LirOperand::Constant(rhs_c)) => {
                        return LirOperand::Constant(calc_constant(op, *lhs_c, *rhs_c))
                    }
                    _ => {}
                }

                let dest = self.new_tmp();
                self.instrs
                    .push(BinaryExpr(dest.clone(), (*op).clone(), lhs_dest, rhs_dest));
                LirOperand::Reg(dest)
            }
            ExprKind::Identifier(name) => LirOperand::Reg(Var(name.name.to_string())),
            ExprKind::Grouped(expr) => self.flat_expr(expr),
        }
    }

    /// Produces a LoadConst in a new temp register if
    /// the flat_result is a Constant. Otherwise it returns
    /// the LirReg of the flat_result.
    fn load_flat_result(&mut self, flat_result: LirOperand) -> LirReg {
        match flat_result {
            LirOperand::Reg(reg) => reg,
            LirOperand::Constant(c) => {
                let dest = self.new_tmp();
                self.instrs.push(LoadConst(dest.clone(), c));
                dest
            }
        }
    }

    fn new_tmp(&mut self) -> LirReg {
        let tmp = Tmp(self.tmp_count);
        self.tmp_count += 1;
        tmp
    }

    fn new_label(&mut self) -> Label {
        let l = self.label_count;
        self.label_count += 1;
        Label(l)
    }
}

fn calc_constant(op: &BinaryOp, lhs: i32, rhs: i32) -> i32 {
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
            LIR::LoadConst(reg, const_val) => write!(f, "LoadConst({}, {})", reg, const_val),
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

        assert_eq!(lir.instrs().len(), 2);
        let expected_lir = vec![
            LIR::LoadConst(LirReg::Tmp(0), 0),
            LIR::Return(LirReg::Tmp(0)),
        ];
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
            LIR::LoadConst(LirReg::Tmp(0), 0),
            LIR::Return(LirReg::Tmp(0)),
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
            Return(Var("b".to_string())),
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
