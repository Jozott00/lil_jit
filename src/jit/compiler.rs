use crate::ast::BinaryOp;
use crate::jit::arch_def::{RegDefinition, Register};
use crate::jit::codegendata::CodegenData;
use crate::jit::codeinfo::CodeInfo;
use crate::jit::funcinfo::FuncInfo;
use crate::jit::jitdata::JitData;
use crate::jit::lir::{LirReg, LIR};
use crate::jit::reg_alloc::reg_off::RegOff;
use log::warn;
use std::marker::PhantomData;

pub fn compile_func<'a, D: RegDefinition>(func_info: FuncInfo<'a>, jit_data: &'a mut JitData<'a>) {
    let mut compiler = Compiler::<D>::new(func_info, jit_data);
    compiler.compile();
}

/// Compiler that compiles provided function `func` in context of `jit_data`.
///
/// A compiler can be invoked by `compile()` which consumes itself, and therefore isn't usable anymore.
struct Compiler<'a, D: RegDefinition> {
    reg_def: PhantomData<D>, // to be able to use the generic static type RegDefinition
    jit_data: &'a mut JitData<'a>,
    code_info: CodeInfo<'a>,
}

impl<'a, D: RegDefinition> Compiler<'a, D> {
    fn new(func_info: FuncInfo<'a>, jit_data: &'a mut JitData<'a>) -> Self {
        let code_info = CodeInfo::new(func_info);

        Compiler {
            reg_def: Default::default(),
            jit_data,
            code_info,
        }
    }
    fn compile(mut self) {
        let func_info = self.code_info.func_info;
        let cd = self.cd();

        for instr in func_info.lir().instrs() {
            match instr {
                LIR::BinaryExpr(dest, op, lhs, rhs) => {
                    let lhs = self.load_reg(lhs, D::temp1());
                    let rhs = self.load_reg(rhs, D::temp2());
                    let dreg = self.get_dst(dest, D::temp1());

                    todo!("Implement binary operation code generation");
                    match op {
                        BinaryOp::Add => {}
                        BinaryOp::Minus => {}
                        BinaryOp::Multi => {}
                        BinaryOp::Divide => {}
                        BinaryOp::Equals => {}
                        BinaryOp::NotEqual => {}
                        BinaryOp::Greater => {}
                        BinaryOp::GreaterEqual => {}
                        BinaryOp::Less => {}
                        BinaryOp::LessEqual => {}
                    }

                    // if dest is offset, store dreg on stack at offset
                    self.store_dst(dest, dreg);
                }
                LIR::Assign(_, _) => {}
                LIR::LoadConst(_, _) => {}
                LIR::Label(_) => {}
                LIR::Jump(_) => {}
                LIR::JumpIfFalse(_, _) => {}
                LIR::Call(_, _, _) => {}
                LIR::Return(_) => {}
            }
        }
    }

    fn load_reg(&mut self, reg: &LirReg, alt: Register) -> Register {
        let cd = self.cd();
        match self.find_reg_off(reg) {
            RegOff::Reg(reg) => reg,
            RegOff::Off(offset) => {
                todo!("Load value from offset in alt register");

                return alt;
            }
        }
    }

    fn get_dst(&self, reg: &LirReg, alt: Register) -> Register {
        match self.find_reg_off(reg) {
            RegOff::Reg(reg) => reg,
            RegOff::Off(_) => alt,
        }
    }

    fn store_dst(&mut self, dst: &LirReg, reg: Register) {
        let cd = self.cd();
        match self.find_reg_off(dst) {
            RegOff::Reg(dst) => {
                if dst == reg {
                    return;
                }

                // move value from reg to dst
                warn!(target: "compiler", "moving register when storing. Should not happen...");
                cd.mov_64_reg(dst, reg);
            }
            RegOff::Off(offset) => {
                todo!("Store reg at offset")
            }
        }
    }

    fn find_reg_off(&self, reg: &LirReg) -> RegOff {
        *self
            .code_info
            .func_info
            .reg_alloc()
            .get(reg)
            .expect(&*format!(
                "MAJOR PROBLEM! No register or offset for lir {}",
                reg
            ))
    }

    fn cd(&mut self) -> &mut CodegenData {
        return &mut self.code_info.codegen_data;
    }
}
