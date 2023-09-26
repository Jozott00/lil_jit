use crate::ast::BinaryOp;
use crate::jit::arch_def::{RegDefinition, Register};
use crate::jit::codegendata::CodegenData;
use crate::jit::codeinfo::CodeInfo;
use crate::jit::funcinfo::FuncInfo;
use crate::jit::jitdata::JitData;
use crate::jit::lir::{Label, LirReg, LIR};
use crate::jit::reg_alloc::reg_off::RegOff;
use armoured_rust::instruction_encoding::branch_exception_system::unconditional_branch_immediate::{UnconditionalBranchImmediate, UnconditionalBranchImmediateWithAddress};
use armoured_rust::instruction_encoding::branch_exception_system::unconditional_branch_register::UnconditionalBranchRegister;
use armoured_rust::instruction_encoding::common_aliases::CommonAliases;
use armoured_rust::instruction_encoding::data_proc_imm::mov_wide_imm::MovWideImmediate;

use crate::built_in::BUILTIN_FUNCS;
use crate::jit::stub::compile_stub;
use armoured_rust::instruction_encoding::branch_exception_system::compare_and_branch_imm::CompareAndBranchImm;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_reg_pre_post_indexed::LoadStoreRegisterPrePostIndexed;
use armoured_rust::types::{InstructionPointer, HW};
use log::warn;
use std::collections::HashMap;
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
    label_indices: HashMap<Label, usize>,
    patch_requests: HashMap<Label, Vec<(LIR, InstructionPointer)>>,
}

impl<'a, 'b, D: RegDefinition> Compiler<'a, 'b, D> {
    fn new(func_info: FuncInfo<'a>, jit_data: &'b JitData<'a>) -> Self {
        let code_info = CodeInfo::new(func_info);

        Compiler {
            reg_def: Default::default(),
            jit_data,
            code_info,
            label_indices: Default::default(),
            patch_requests: Default::default(),
        }
    }
    fn compile(mut self) -> CodeInfo<'a> {
        // TODO: move arguments to allocated registers
        // Be careful: if allocated registers are caller saved, this might result in collisions -> argument variables must not get caller saved registers

        let instrs = &self.code_info.func_info.lir().instrs().to_vec();

        for instr in instrs {
            self.compile_instr(instr);
        }

        if !self.patch_requests.is_empty() {
            panic!("Some branches could not be patched!")
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

                let cd = self.cd();

                match op {
                    BinaryOp::Add => {
                        cd.add_32_reg(dreg, lhs, rhs);
                    }
                    BinaryOp::Minus => {
                        cd.sub_32_reg(dreg, lhs, rhs);
                    }
                    BinaryOp::Multi => {
                        todo!()
                    }
                    BinaryOp::Divide => {
                        todo!()
                    }
                    BinaryOp::Equals => {
                        todo!()
                    }
                    BinaryOp::NotEqual => {
                        todo!()
                    }
                    BinaryOp::Greater => {
                        todo!()
                    }
                    BinaryOp::GreaterEqual => {
                        todo!()
                    }
                    BinaryOp::Less => {
                        todo!()
                    }
                    BinaryOp::LessEqual => {
                        todo!()
                    }
                }

                // if dest is offset, store dreg on stack at offset
                self.store_dst(dest, dreg);
            }
            LIR::Assign(dest, src) => {
                let src = self.load_reg(src, D::temp1());
                let dreg = self.get_dst(dest, D::temp1());

                let cd = self.cd();
                if src != dreg {
                    cd.mov_64_reg(dreg, src);
                }

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
            LIR::Label(label) => {
                let index = self.code_info.codegen_data.ins_count();
                self.label_indices.insert(*label, index);

                let Some(requests) = self.patch_requests.remove(label) else {
                    return
                };

                // Since we now have the label, let's see if there are some patch requests and if so let's patch them
                for (ins, ins_ptr) in &requests {
                    let offset = self.code_info.codegen_data.code_ptr() as i32 - *ins_ptr as i32;

                    match ins {
                        LIR::Jump(_) => {
                            self.cd().patch_at(*ins_ptr, |cd| {
                                cd.b_from_byte_offset(offset);
                            });
                        }
                        LIR::JumpIfFalse(lir_reg, _) => {
                            let creg = self.load_reg(lir_reg, D::temp1());

                            self.code_info.codegen_data.patch_at(*ins_ptr, |cd| {
                                cd.cbz_32_from_byte_offset(creg, offset);
                            })
                        }
                        _ => unimplemented!(),
                    }
                }
            }
            LIR::Jump(label) => {
                let Some(label_index) = self.label_indices.get(label) else {

                    let code_ptr = self.code_info.codegen_data.code_ptr();
                    match self.patch_requests.get_mut(label) {
                        Some(requests) =>  requests.push((instr.clone(), code_ptr)),
                        None => {self.patch_requests.insert(*label, vec!((instr.clone(), code_ptr)));},
                    }

                    self.cd().nop();
                    return;
                };

                let label_index = *label_index;
                let cd = self.cd();
                let offset = label_index as i32 - cd.ins_count() as i32;
                cd.b_from_byte_offset(offset);
            }
            LIR::JumpIfFalse(lir_reg, label) => {
                let Some(label_index) = self.label_indices.get(label) else {

                    let code_ptr = self.code_info.codegen_data.code_ptr();
                    match self.patch_requests.get_mut(label) {
                        Some(requests) =>  requests.push((instr.clone(), code_ptr)),
                        None => {self.patch_requests.insert(*label, vec!((instr.clone(), code_ptr)));},
                    }

                    self.cd().nop();
                    return;
                };

                let label_index = *label_index;
                let creg = self.load_reg(lir_reg, D::temp1());
                let cd = self.cd();
                let offset = label_index as i32 - cd.ins_count() as i32;
                cd.cbz_32_from_byte_offset(creg, offset);
            }
            LIR::Call(dest, func_name, args) => {
                let dreg = self.get_dst(dest, D::temp1());

                // store x30 (link) on stack
                self.cd().str_64_imm_pre_index(30, 31, -16);

                // check if func is builtin or custom
                let func_ptr = if let Some(built_in) = BUILTIN_FUNCS.get(func_name.as_str()) {
                    built_in.mem_ptr as InstructionPointer
                } else {
                    match self.jit_data.compiled_funcs.get(func_name.as_str()) {
                        Some(code_info) => code_info.codegen_data.base_ptr(),
                        None => compile_stub as InstructionPointer,
                    }
                };

                // move arg registers to argument passing registers
                for (i, arg_reg) in args.iter().enumerate() {
                    let r = self.load_reg(arg_reg, i as Register);
                    self.mov_reg(i as Register, r)
                    // TODO: handle spilled arguments
                }

                self.cd().bl_to_addr(func_ptr as usize);
                self.mov_reg(dreg, D::ret_reg());

                self.store_dst(dest, dreg);

                self.cd().ldr_64_imm_post_index(30, 31, 16);
            }
            LIR::Return(src) => {
                let src = self.load_reg(src, D::temp1());
                let cd = self.cd();

                cd.mov_64_reg(0, src);
                cd.ret()
            }
        }
    }

    fn mov_reg(&mut self, dest: Register, src: Register) {
        if dest != src {
            self.cd().mov_64_reg(dest, src)
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
