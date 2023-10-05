use super::*;
use crate::jit::lir::helper::calc_constant;

pub struct LirCompiler<'a> {
    ast: &'a FuncDec<'a>,
    instrs: Vec<LIR>,
    tmp_count: u32,
    label_count: u32,
}

impl<'a> LirCompiler<'a> {
    pub fn new(ast: &'a FuncDec<'a>) -> LirCompiler {
        LirCompiler {
            ast,
            instrs: Vec::new(),
            tmp_count: 0,
            label_count: 0,
        }
    }

    pub fn compile(mut self) -> LirFunction {
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
        self.instrs.push(Return(LirOperand::Constant(0)));
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
                    let e_arg = self.flat_expr(e);
                    arg_dests.push(e_arg);
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
