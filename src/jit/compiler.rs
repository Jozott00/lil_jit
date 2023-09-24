use crate::ast::BinaryOp;
use crate::jit::arch_def::{RegDefinition, Register};
use crate::jit::codegendata::CodegenData;
use crate::jit::codeinfo::CodeInfo;
use crate::jit::funcinfo::FuncInfo;
use crate::jit::jitdata::JitData;
use crate::jit::lir::{LirReg, LIR};
use crate::jit::reg_alloc::reg_off::RegOff;
use armoured_rust::instruction_encoding::branch_exception_system::unconditional_branch_register::UnconditionalBranchRegister;
use armoured_rust::instruction_encoding::common_aliases::CommonAliases;
use armoured_rust::instruction_encoding::data_proc_imm::mov_wide_imm::MovWideImmediate;
use armoured_rust::types::HW;
use log::warn;
use std::marker::PhantomData;

pub fn compile_func<'a, 'b, D: RegDefinition>(
    func_info: FuncInfo<'a>,
    jit_data: &'b JitData<'a>,
) -> CodeInfo<'a> {
    let compiler: Compiler<D> = Compiler::<D>::new(func_info, jit_data);
    compiler.compile()
}

/// Compiler that compiles provided function `func` in context of `jit_data`.
///
/// A compiler can be invoked by `compile()` which consumes itself, and therefore isn't usable anymore.
struct Compiler<'a, 'b, D: RegDefinition> {
    reg_def: PhantomData<D>, // to be able to use the generic static type RegDefinition
    jit_data: &'b JitData<'a>,
    code_info: CodeInfo<'a>,
}

impl<'a, 'b, D: RegDefinition> Compiler<'a, 'b, D> {
    fn new(func_info: FuncInfo<'a>, jit_data: &'b JitData<'a>) -> Self {
        let code_info = CodeInfo::new(func_info);

        Compiler {
            reg_def: Default::default(),
            jit_data,
            code_info,
        }
    }
    fn compile(mut self) -> CodeInfo<'a> {
        let instrs = &self.code_info.func_info.lir().instrs().to_vec();

        for instr in instrs {
            self.compile_instr(instr);
        }

        self.code_info
    }
    fn compile_instr(&mut self, instr: &LIR) {
        // TODO: some way without coping the whole function ir?
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
            LIR::Assign(dest, src) => {
                let src = self.load_reg(src, D::temp1());
                let dreg = self.get_dst(dest, D::temp1());

                let cd = self.cd();
                cd.mov_64_reg(src, dreg);

                self.store_dst(dest, dreg);
            }
            LIR::LoadConst(dest, num) => {
                let dreg = self.get_dst(dest, D::temp1());
                let cd = self.cd();

                let lower_16 = (*num & 0xFFFF) as u16;
                let upper_16 = ((*num >> 16) & 0xFFFF) as u16;

                cd.movz_32_imm(dreg, lower_16);
                if upper_16 != 0 {
                    cd.movk_32_imm_lsl(dreg, upper_16, HW::LSL16);
                }

                self.store_dst(dest, dreg);
            }
            LIR::Label(_) => {
                todo!()
            }
            LIR::Jump(_) => {
                todo!()
            }
            LIR::JumpIfFalse(_, _) => {
                todo!()
            }
            LIR::Call(_, _, _) => {
                todo!()
            }
            LIR::Return(src) => {
                let src = self.load_reg(src, D::temp1());
                let cd = self.cd();

                cd.mov_64_reg(0, src);
                cd.ret()
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
        match self.find_reg_off(dst) {
            RegOff::Reg(dst) => {
                if dst == reg {
                    return;
                }

                // move value from reg to dst
                warn!(target: "compiler", "moving register when storing. Should not happen...");
                self.cd().mov_64_reg(dst, reg);
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
