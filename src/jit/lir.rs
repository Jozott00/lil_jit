use crate::ast;
use crate::ast::{Expr, ExprKind, FuncDec, Program, Stmt, StmtKind};
use crate::jit::lir::LirReg::{Tmp, Var};
use crate::jit::lir::LIR::{Assign, BinaryExpr, Call, Jump, JumpIfFalse, LoadConst, Return};
use crate::visitor::NodeVisitor;
use std::fmt;
use std::fmt::Formatter;

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
    BinaryExpr(LirReg, ast::BinaryOp, LirReg, LirReg),

    // mem movement
    Assign(LirReg, LirReg),
    LoadConst(LirReg, i32),

    // control flow
    Label(Label),
    Jump(Label),
    JumpIfFalse(LirReg, Label),

    // func
    Call(LirReg, String, Vec<LirReg>), // dest, func_name, args
    Return(LirReg),
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum LirReg {
    Var(String),
    Tmp(u32),
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
                let instr = Return(src);
                self.instrs.push(instr)
            }
        }
    }

    fn flat_expr(&mut self, node: &'a Expr) -> LirReg {
        match &node.kind {
            ExprKind::IntegerLiteral(num) => {
                let dest = self.new_tmp();
                self.instrs.push(LoadConst(dest.clone(), *num));
                dest
            }
            ExprKind::StringLiteral(_) => {
                todo!("Not yet implemented")
            }
            ExprKind::FunctionCall(func_data) => {
                let mut arg_dests = Vec::new();
                for e in &func_data.arguments {
                    let e_dest = self.flat_expr(e);
                    arg_dests.push(e_dest);
                }
                let dest = self.new_tmp();
                let instr = Call(
                    dest.clone(),
                    func_data.function_name.name.to_string(),
                    arg_dests,
                );
                self.instrs.push(instr);
                dest
            }
            ExprKind::BinaryExpr(lhs, op, rhs) => {
                let lhs_dest = self.flat_expr(lhs);
                let rhs_dest = self.flat_expr(rhs);
                let dest = self.new_tmp();
                self.instrs
                    .push(BinaryExpr(dest.clone(), (*op).clone(), lhs_dest, rhs_dest));
                dest
            }
            ExprKind::Identifier(name) => Var(name.name.to_string()),
            ExprKind::Grouped(expr) => self.flat_expr(expr),
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

impl fmt::Display for LIR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LIR::BinaryExpr(reg_a, op, reg_b, reg_c) => {
                write!(f, "BinaryExpr({}, {}, {}, {})", reg_a, op, reg_b, reg_c)
            }
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
            LIR::Return(reg) => write!(f, "Return({})", reg),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::BinaryOp;
    use crate::checker::check_lil;
    use crate::parser::parse_lil_program;

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
            LIR::LoadConst(LirReg::Tmp(0), 2),
            LIR::LoadConst(LirReg::Tmp(1), 3),
            LIR::BinaryExpr(
                LirReg::Tmp(2),
                ast::BinaryOp::Add,
                LirReg::Tmp(0),
                LirReg::Tmp(1),
            ),
            LIR::Assign(LirReg::Var("a".to_string()), LirReg::Tmp(2)),
            LIR::LoadConst(LirReg::Tmp(3), 0),
            LIR::Return(LirReg::Tmp(3)),
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
            LoadConst(Tmp(0), 2),
            LoadConst(Tmp(1), 3),
            BinaryExpr(Tmp(2), BinaryOp::Add, Tmp(0), Tmp(1)),
            Assign(Var("a".to_string()), Tmp(2)),
            LoadConst(Tmp(3), 2),
            LoadConst(Tmp(4), 2),
            BinaryExpr(Tmp(5), BinaryOp::Add, Var("a".to_string()), Tmp(4)),
            BinaryExpr(Tmp(6), BinaryOp::Multi, Tmp(3), Tmp(5)),
            Assign(Var("b".to_string()), Tmp(6)),
            Assign(Var("a".to_string()), Var("b".to_string())),
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
