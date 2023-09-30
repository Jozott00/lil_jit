use std::collections::HashMap;
use std::marker::PhantomData;

use armoured_rust::instruction_encoding::branch_exception_system::compare_and_branch_imm::CompareAndBranchImm;
use armoured_rust::instruction_encoding::branch_exception_system::exception_generation::ExceptionGeneration;
use armoured_rust::instruction_encoding::branch_exception_system::unconditional_branch_immediate::{UnconditionalBranchImmediate, UnconditionalBranchImmediateWithAddress};
use armoured_rust::instruction_encoding::branch_exception_system::unconditional_branch_register::UnconditionalBranchRegister;
use armoured_rust::instruction_encoding::common_aliases::CommonAliases;
use armoured_rust::instruction_encoding::data_proc_imm::add_substract_imm::AddSubtractImmediate;
use armoured_rust::instruction_encoding::data_proc_imm::mov_wide_imm::MovWideImmediate;
use armoured_rust::instruction_encoding::data_proc_reg::conditional_select::ConditionalSelect;
use armoured_rust::instruction_encoding::data_proc_reg::data_proc_two_src::DataProcessingTwoSource;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_reg_pre_post_indexed::LoadStoreRegisterPrePostIndexed;
use armoured_rust::types::{HW, InstructionPointer, UImm16};
use armoured_rust::types::condition::Condition::{EQ, GE, GT, LE, LT, NE};
use armoured_rust::types::register::WZR;
use log::{info, warn};

use crate::ast::BinaryOp;
use crate::built_in::BUILTIN_FUNCS;
use crate::jit::arch_def::{RegDefinition, Register};
use crate::jit::codegendata::{CodegenData, InstrCount};
use crate::jit::codeinfo::CodeInfo;
use crate::jit::funcinfo::FuncInfo;
use crate::jit::jitdata::JitData;
use crate::jit::lir::{Label, LirReg, LIR};
use crate::jit::reg_alloc::reg_off::RegOff;
use crate::jit::stub::compile_stub;

pub fn compile_func<'a, 'b, D: RegDefinition>(
    func_info: FuncInfo<'a, D>,
    jit_data: &'b mut JitData<'a, D>,
) -> CodeInfo<'a, D> {
    let compiler: Compiler<D> = Compiler::<D>::new(func_info, jit_data);
    compiler.compile()
}

/// Compiler that compiles provided function `func` in context of `jit_data`.
///
/// A compiler can be invoked by `compile()` which consumes itself, and therefore isn't usable anymore.
struct Compiler<'a, 'b, D: RegDefinition> {
    reg_def: PhantomData<D>,
    // to be able to use the generic static type RegDefinition
    jit_data: &'b mut JitData<'a, D>,
    code_info: CodeInfo<'a, D>,
    label_indices: HashMap<Label, usize>,
    patch_requests: HashMap<Label, Vec<(LIR, InstructionPointer)>>,
    stub_refs: HashMap<&'a str, Vec<InstrCount>>,
}

impl<'a, 'b, D: RegDefinition> Compiler<'a, 'b, D> {
    fn new(func_info: FuncInfo<'a, D>, jit_data: &'b mut JitData<'a, D>) -> Self {
        let code_info = CodeInfo::new(func_info);

        Compiler {
            reg_def: Default::default(),
            jit_data,
            code_info,
            label_indices: Default::default(),
            patch_requests: Default::default(),
            stub_refs: Default::default(),
        }
    }
    fn compile(mut self) -> CodeInfo<'a, D> {
        self.compile_prolog();

        let instrs = &self.code_info.func_info.lir().instrs().to_vec();

        for instr in instrs {
            self.compile_instr(instr);
        }

        if !self.patch_requests.is_empty() {
            panic!("Some branches could not be patched!")
        }

        // store stub references
        for (func, instr_cnts) in &self.stub_refs {
            let call_ptrs = instr_cnts
                .iter()
                .map(|cnt| self.code_info.codegen_data.code_ptr_from_instr_count(*cnt))
                .collect();
            self.jit_data.stub_ref_store.add_unresolved(
                func,
                self.code_info.func_info.name(),
                call_ptrs,
            )
        }

        self.code_info
    }

    fn compile_prolog(&mut self) {
        // self.cd().brk(0);

        // save x30 (link) on stack
        self.cd().str_64_imm_pre_index(30, 31, -16);

        // save callee saved registers
        // TODO: Be careful: if allocated registers are caller saved, this might result in collisions -> argument variables must not get caller saved registers
        for r in &self.code_info.func_info.reg_alloc().callee_saved() {
            // FIXME: The 16 byte offset is for allignment but packing them together
            // and fixing the allignment later would be leaner.
            self.cd().str_64_imm_pre_index(*r, 31, -16);
        }

        // save framepointer on the stack
        self.cd().str_64_imm_pre_index(29, 31, -16);

        // Set the framepointer on to the current stack pointer
        self.cd().add_64_imm(29, 31, 0);
    }

    // x30 // link
    // callee saved registers
    // old_fp <- fp
    // stack variables

    fn compile_epilog(&mut self) {
        // sp <- fp
        self.cd().add_64_imm(31, 29, 0);

        // fp <- pop old_fp
        self.cd().ldr_64_imm_post_index(29, 31, 16);

        // restore callee saved registers
        for r in self
            .code_info
            .func_info
            .reg_alloc()
            .callee_saved()
            .iter()
            .rev()
        {
            self.cd().ldr_64_imm_post_index(*r, 31, 16);
        }

        // restore x30 (link) from stack
        self.cd().ldr_64_imm_post_index(30, 31, 16);
        self.cd().ret()
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
                        cd.mul_32_reg(dreg, lhs, rhs);
                    }
                    BinaryOp::Divide => {
                        cd.sdiv_32(dreg, lhs, rhs);
                    }
                    BinaryOp::Equals => {
                        // FIXME: This code could be more readable with cmp and cset.
                        // This code is functionally equal but less readable, also because csinc
                        // needs to invert the condition.
                        cd.subs_32_reg(WZR, lhs, rhs);
                        cd.csinc_32(dreg, WZR, WZR, NE);
                    }
                    BinaryOp::NotEqual => {
                        // FIXME: This code could be more readable with cmp and cset.
                        // This code is functionally equal but less readable, also because csinc
                        // needs to invert the condition.
                        cd.subs_32_reg(WZR, lhs, rhs);
                        cd.csinc_32(dreg, WZR, WZR, EQ);
                    }
                    BinaryOp::Greater => {
                        // FIXME: This code could be more readable with cmp and cset.
                        // This code is functionally equal but less readable, also because csinc
                        // needs to invert the condition.
                        cd.subs_32_reg(WZR, lhs, rhs);
                        cd.csinc_32(dreg, WZR, WZR, LE);
                    }
                    BinaryOp::GreaterEqual => {
                        // FIXME: This code could be more readable with cmp and cset.
                        // This code is functionally equal but less readable, also because csinc
                        // needs to invert the condition.
                        cd.subs_32_reg(WZR, lhs, rhs);
                        cd.csinc_32(dreg, WZR, WZR, LT);
                    }
                    BinaryOp::Less => {
                        // FIXME: This code could be more readable with cmp and cset.
                        // This code is functionally equal but less readable, also because csinc
                        // needs to invert the condition.
                        cd.subs_32_reg(WZR, lhs, rhs);
                        cd.csinc_32(dreg, WZR, WZR, GE);
                    }
                    BinaryOp::LessEqual => {
                        // FIXME: This code could be more readable with cmp and cset.
                        // This code is functionally equal but less readable, also because csinc
                        // needs to invert the condition.
                        cd.subs_32_reg(WZR, lhs, rhs);
                        cd.csinc_32(dreg, WZR, WZR, GT);
                    }
                }

                // if dest is offset, store dreg on stack at offset
                self.store_dst(dest, dreg);
            }
            LIR::InputArgLoad(dest, i) => {
                let dreg = self.get_dst(dest, D::temp1());
                // TODO: Check if register to load is spilled and handle it!
                assert!(
                    D::arg_regs().len() > *i,
                    "argument would be spilled! spilling isnt implemented yet!"
                );
                self.cd().mov_64_reg(dreg, *i as Register);
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
                    return;
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
                        Some(requests) => requests.push((instr.clone(), code_ptr)),
                        None => {
                            self.patch_requests
                                .insert(*label, vec![(instr.clone(), code_ptr)]);
                        }
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
                        Some(requests) => requests.push((instr.clone(), code_ptr)),
                        None => {
                            self.patch_requests
                                .insert(*label, vec![(instr.clone(), code_ptr)]);
                        }
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

                let mut is_stub_call = false;

                // check if func is builtin or recursive or custom or not yet compiled
                let func_ptr = if let Some(built_in) = BUILTIN_FUNCS.get(func_name.as_str()) {
                    built_in.mem_ptr as InstructionPointer
                } else {
                    if func_name == self.code_info.func_info.name() {
                        self.cd().base_ptr()
                    } else {
                        match self.jit_data.compiled_funcs.get(func_name.as_str()) {
                            Some(code_info) => code_info.codegen_data.base_ptr(),
                            None => {
                                is_stub_call = true;
                                compile_stub as InstructionPointer
                            }
                        }
                    }
                };

                // move arg registers to argument passing registers
                for (i, arg_reg) in args.iter().enumerate() {
                    let r = self.load_reg(arg_reg, i as Register);
                    self.mov_reg(i as Register, r)
                    // TODO: handle spilled arguments
                }

                info!(target: "verbose", "EMIT CALL TO: {:#x}", func_ptr as usize);

                // save the instruction count 4 instruction before actual call
                let call_instr_count = self.cd().instr_count();
                self.cd().func_call(func_ptr as usize, D::temp3());

                self.mov_reg(dreg, D::ret_reg());
                self.store_dst(dest, dreg);

                // if we call a stub, add add function name together with instructionCounter to
                // local stub_refs map. After the function compilation this is stored in the
                // jit_data.stub_ref_store. The late storage is required, as the method may extend
                // while compilation!
                if is_stub_call {
                    let func_name = self
                        .jit_data
                        .uncompiled_funcs
                        .get(func_name.as_str())
                        .expect("As function not yet compiled it must be uncompiled_function map!")
                        .name
                        .name;
                    self.stub_refs
                        .entry(func_name)
                        .or_insert_with(Vec::new)
                        .push(call_instr_count);
                }
            }
            LIR::CallText(dest, has_newline, text) => {
                let dreg = self.get_dst(dest, D::temp1());

                let func_ptr = BUILTIN_FUNCS
                    .get(if *has_newline {
                        "showtextln"
                    } else {
                        "showtext"
                    })
                    .unwrap()
                    .mem_ptr;

                self.jit_data.texts.insert(text.to_string());
                let text_ptr = self.jit_data.texts.get(text).unwrap().as_ptr() as usize;

                let p1_16 = (text_ptr & 0xFFFF) as u16;
                let p2_16 = ((text_ptr >> 16) & 0xFFFF) as u16;
                let p3_16 = ((text_ptr >> 32) & 0xFFFF) as u16;
                let p4_16 = ((text_ptr >> 48) & 0xFFFF) as u16;

                self.cd().movz_64_imm(0, p1_16);
                self.cd().movk_64_imm_lsl(0, p2_16, HW::LSL16);
                self.cd().movk_64_imm_lsl(0, p3_16, HW::LSL32);
                self.cd().movk_64_imm_lsl(0, p4_16, HW::LSL48);

                self.cd().func_call(func_ptr, D::temp3());
                self.mov_reg(dreg, D::ret_reg());

                self.store_dst(dest, dreg);
            }
            LIR::Return(src) => {
                let src = self.load_reg(src, D::temp1());
                let cd = self.cd();

                cd.mov_64_reg(0, src);
                self.compile_epilog()
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
            .regoff_for(reg)
            .expect(&*format!(
                "MAJOR PROBLEM! No register or offset for lir {}",
                reg
            ))
    }

    fn cd(&mut self) -> &mut CodegenData {
        return &mut self.code_info.codegen_data;
    }
}
