use std::collections::HashMap;
use std::marker::PhantomData;

use armoured_rust::instruction_encoding::branch_exception_system::compare_and_branch_imm::CompareAndBranchImm;
use armoured_rust::instruction_encoding::branch_exception_system::exception_generation::ExceptionGeneration;
use armoured_rust::instruction_encoding::branch_exception_system::unconditional_branch_immediate::UnconditionalBranchImmediate;
use armoured_rust::instruction_encoding::branch_exception_system::unconditional_branch_register::UnconditionalBranchRegister;
use armoured_rust::instruction_encoding::common_aliases::CommonAliases;
use armoured_rust::instruction_encoding::data_proc_imm::add_substract_imm::AddSubtractImmediate;
use armoured_rust::instruction_encoding::data_proc_imm::logical_imm::LogicalImmediate;
use armoured_rust::instruction_encoding::data_proc_imm::mov_wide_imm::MovWideImmediate;
use armoured_rust::instruction_encoding::data_proc_reg::add_sub_shift_reg::AddSubtractShiftedRegister;
use armoured_rust::instruction_encoding::data_proc_reg::cond_compare_imm::ConditionalCompareImmediate;
use armoured_rust::instruction_encoding::data_proc_reg::conditional_select::ConditionalSelect;
use armoured_rust::instruction_encoding::data_proc_reg::data_proc_three_src::DataProcessingThreeSource;
use armoured_rust::instruction_encoding::data_proc_reg::data_proc_two_src::DataProcessingTwoSource;
use armoured_rust::instruction_encoding::data_proc_reg::logical_shift_reg::LogicalShiftRegister;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_reg_pair_post_indexed::LoadStoreRegisterPairPostIndexed;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_reg_pair_pre_indexed::LoadStoreRegisterPairPreIndexed;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_reg_pre_post_indexed::LoadStoreRegisterPrePostIndexed;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_reg_unscaled_imm::LoadStoreRegisterUnscaledImmediate;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_register_unsigned_imm::LoadStoreRegisterUnsignedImmediate;

use armoured_rust::types::condition::Condition;
use armoured_rust::types::condition::Condition::{EQ, GE, GT, LE, LT, NE};
use armoured_rust::types::register::WZR;
use armoured_rust::types::shifts::Shift1;
use armoured_rust::types::{Imm12, Imm9, InstructionPointer, UImm12, UImm14, UImm16, UImm32, HW};
use log::{info, warn};

use crate::ast::BinaryOp;
use crate::built_in::BUILTIN_FUNCS;
use crate::jit::arch_def::{RegDefinition, Register};
use crate::jit::codegendata::{CodegenData, InstrCount};
use crate::jit::codeinfo::CodeInfo;
use crate::jit::funcinfo::FuncInfo;
use crate::jit::jitdata::JitData;
use crate::jit::lir::{Label, LirOperand, LirReg, LIR};
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
    spilled_arg_offset: usize, // offset from fp to first arg
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
            spilled_arg_offset: 0,
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
        // save callee saved registers
        // TODO: Be careful: if allocated registers are caller saved, this might result in collisions -> argument variables must not get caller saved registers
        let callee_saved = &self.code_info.func_info.reg_alloc().callee_saved();
        for r in callee_saved {
            // FIXME: The 16 byte offset is for allignment but packing them together
            // and fixing the allignment later would be leaner.
            self.cd().str_64_imm_pre_index(*r, 31, -16);
        }

        // save save framepointer (lower addr) and x30 (link, higher addr) on the stack
        self.cd().stp_64_pre_index(29, 30, 31, -16);

        // set spilled argument offset
        // TODO: Probably changing with better handling of callee saved registers
        // fp + link + (callee_saved * 16)
        self.spilled_arg_offset = 8 + 8 + (callee_saved.len() * 16);

        // Set the framepointer on to the current stack pointer
        self.cd().add_64_imm(29, 31, 0);

        // allocate local variables spills on stack
        let number_spills = self.code_info.func_info.reg_alloc().number_of_spills();
        let spill_space = 4 * number_spills;
        // round to nearest multiply of 16
        let stack_space_needed = (spill_space + 15) & !15;
        if stack_space_needed > 0 {
            self.cd().sub_64_imm(31, 31, stack_space_needed as Imm12);
        }
    }

    // callee saved registers
    // x30 // link
    // old_fp <- fp
    // stack variables

    fn compile_epilog(&mut self) {
        // sp <- fp
        self.cd().add_64_imm(31, 29, 0);

        // (fp, link) <- pop (old_fp, link)
        // restore old fp and x30 (link) from stack
        self.cd().ldp_64_post_index(29, 30, 31, 16);

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

        self.cd().ret()
    }

    fn compile_instr(&mut self, instr: &LIR) {
        // TODO: some way without coping the whole function ir?
        match instr {
            LIR::BinaryExpr(dest, op, lhs, rhs) => {
                let dreg = self.get_dst(dest, D::temp1());
                match (lhs, rhs) {
                    (LirOperand::Reg(lhs), LirOperand::Reg(rhs)) => {
                        let lhs = self.load_reg(lhs, D::temp1());
                        let rhs = self.load_reg(rhs, D::temp2());
                        self.bin_op_no_const(dreg, op, lhs, rhs)
                    }

                    (LirOperand::Constant(lhs), LirOperand::Reg(rhs)) => {
                        let rhs = self.load_reg(rhs, D::temp2());
                        self.bin_op_lhs_const(dreg, op, *lhs, rhs)
                    }
                    (LirOperand::Reg(lhs), LirOperand::Constant(rhs)) => {
                        let lhs = self.load_reg(lhs, D::temp1());
                        self.bin_op_rhs_const(dreg, op, lhs, *rhs)
                    }
                    (LirOperand::Constant(_), LirOperand::Constant(_)) => {
                        panic!("Not possible, constant folding should have fold that!")
                    }
                }
                self.store_dst(dest, dreg);
            }
            LIR::InputArgLoad(dest, i) => {
                let dreg = self.get_dst(dest, D::temp1());

                let arg_regs = D::arg_regs().len();

                if *i < arg_regs {
                    self.cd().mov_64_reg(dreg, *i as Register);
                } else {
                    // offset from frame_pointer
                    let arg_offset = (i - arg_regs) * 4;
                    let fp_offset = self.spilled_arg_offset + arg_offset;
                    self.cd()
                        .ldr_32_imm_unsigned_offset(dreg, 29, fp_offset as UImm12);
                }
                self.store_dst(dest, dreg);
            }
            LIR::Assign(dest, src) => {
                let dreg = self.get_dst(dest, D::temp1());

                match src {
                    LirOperand::Reg(src) => {
                        let src = self.load_reg(src, D::temp1());
                        if src != dreg {
                            self.cd().mov_64_reg(dreg, src);
                        }
                    }
                    LirOperand::Constant(c) => self.cd().mov_arbitrary_imm(dreg, *c as u64, false),
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
                        LIR::JumpIfFalse(lir_operand, _) => match lir_operand {
                            LirOperand::Reg(reg) => {
                                let creg = self.load_reg(reg, D::temp1());

                                self.code_info.codegen_data.patch_at(*ins_ptr, |cd| {
                                    cd.cbz_32_from_byte_offset(creg, offset);
                                })
                            }
                            LirOperand::Constant(constant) => {
                                self.code_info.codegen_data.patch_at(*ins_ptr, |cd| {
                                    // if the constant is false (0), jump to offset
                                    if *constant == 0 {
                                        cd.b_from_byte_offset(offset)
                                    } else {
                                        cd.nop()
                                    }
                                })
                            }
                        },
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
                let offset = (label_index as i32 - cd.ins_count() as i32) * 4;
                cd.b_from_byte_offset(offset);
            }
            LIR::JumpIfFalse(lir_operand, label) => {
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
                let offset = label_index as i32 - self.cd().ins_count() as i32;

                match lir_operand {
                    LirOperand::Reg(reg) => {
                        let creg = self.load_reg(reg, D::temp1());
                        self.cd().cbz_32_from_byte_offset(creg, offset);
                    }
                    LirOperand::Constant(constant) => {
                        if *constant == 0 {
                            self.cd().b_from_byte_offset(offset)
                        }
                    }
                }
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

                // split args in args stored in registers and args spilled on stack
                let (args_reg, args_spilled) = args.split_at(args.len().min(D::arg_regs().len()));

                // move arg registers to argument passing registers
                for (i, arg_operand) in args_reg.iter().enumerate() {
                    match arg_operand {
                        LirOperand::Reg(arg_operand) => {
                            let r = self.load_reg(arg_operand, i as Register);
                            self.mov_reg(i as Register, r)
                        }
                        LirOperand::Constant(c) => {
                            self.cd().mov_arbitrary_imm(i as Register, *c as u64, false)
                        }
                    };
                }

                // calculate required stack space (with alignment)
                let space_for_args = args_spilled.len() * 4;
                let aligned_space_needed = (space_for_args + 15) & !15;
                if aligned_space_needed > 0 {
                    self.cd().sub_64_imm(31, 31, aligned_space_needed as Imm12);
                }

                // store args that are passed via stack
                for (i, arg_operand) in args_spilled.iter().enumerate() {
                    match arg_operand {
                        LirOperand::Reg(arg_operand) => {
                            let r = self.load_reg(arg_operand, D::temp3());
                            self.cd()
                                .str_32_imm_unsigned_offset(r, 31, (i * 4) as UImm14);
                        }
                        LirOperand::Constant(c) => {
                            self.cd().mov_arbitrary_imm(D::temp3(), *c as u64, false);
                            self.cd()
                                .str_32_imm_unsigned_offset(D::temp3(), 31, (i * 4) as UImm14);
                        }
                    };
                }

                if is_stub_call {
                    info!(target: "verbose", "EMIT CALL TO STUB({:#x}) FOR {func_name} AT REF {:p}", func_ptr as usize, self.cd().code_ptr());
                } else {
                    info!(target: "verbose", "EMIT CALL TO {func_name} ({:#x})", func_ptr as usize);
                }

                // save the instruction count 4 instruction before actual call
                let call_instr_count = self.cd().instr_count();
                self.cd().func_call(func_ptr as usize, D::temp3());
                self.mov_reg(dreg, D::ret_reg());

                // pop passed arguments from stack
                if aligned_space_needed > 0 {
                    self.cd().add_64_imm(31, 31, aligned_space_needed as Imm12);
                }

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
                let string_ref = self.jit_data.texts.get(text).unwrap();
                let text_ptr = string_ref as *const String as usize;

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
                match src {
                    LirOperand::Reg(src) => {
                        let src = self.load_reg(src, D::temp1());
                        self.cd().mov_64_reg(D::ret_reg(), src);
                    }
                    LirOperand::Constant(c) => {
                        self.cd().mov_arbitrary_imm(D::ret_reg(), *c as u64, false)
                    }
                }

                self.compile_epilog()
            }
            LIR::Breakpoint(line) => self.cd().brk(*line as UImm16),
        }
    }

    fn bin_op_no_const(&mut self, dest: Register, op: &BinaryOp, lhs: Register, rhs: Register) {
        let cd = self.cd();

        match op {
            BinaryOp::Add => {
                cd.add_32_reg(dest, lhs, rhs);
            }
            BinaryOp::Minus => {
                cd.sub_32_reg(dest, lhs, rhs);
            }
            BinaryOp::Multi => {
                cd.mul_32_reg(dest, lhs, rhs);
            }
            BinaryOp::Divide => {
                cd.sdiv_32(dest, lhs, rhs);
            }
            BinaryOp::Modulo => {
                cd.sdiv_32(D::temp3(), lhs, rhs);
                cd.msub_32(dest, D::temp3(), rhs, lhs)
            }
            BinaryOp::Equals => {
                // FIXME: This code could be more readable with cmp and cset.
                // This code is functionally equal but less readable, also because csinc
                // needs to invert the condition.
                cd.subs_32_reg(WZR, lhs, rhs);
                cd.csinc_32(dest, WZR, WZR, NE);
            }
            BinaryOp::NotEqual => {
                // FIXME: This code could be more readable with cmp and cset.
                // This code is functionally equal but less readable, also because csinc
                // needs to invert the condition.
                cd.subs_32_reg(WZR, lhs, rhs);
                cd.csinc_32(dest, WZR, WZR, EQ);
            }
            BinaryOp::Greater => {
                // FIXME: This code could be more readable with cmp and cset.
                // This code is functionally equal but less readable, also because csinc
                // needs to invert the condition.
                cd.subs_32_reg(WZR, lhs, rhs);
                cd.csinc_32(dest, WZR, WZR, LE);
            }
            BinaryOp::GreaterEqual => {
                // FIXME: This code could be more readable with cmp and cset.
                // This code is functionally equal but less readable, also because csinc
                // needs to invert the condition.
                cd.subs_32_reg(WZR, lhs, rhs);
                cd.csinc_32(dest, WZR, WZR, LT);
            }
            BinaryOp::Less => {
                // FIXME: This code could be more readable with cmp and cset.
                // This code is functionally equal but less readable, also because csinc
                // needs to invert the condition.
                cd.subs_32_reg(WZR, lhs, rhs);
                cd.csinc_32(dest, WZR, WZR, GE);
            }
            BinaryOp::LessEqual => {
                // FIXME: This code could be more readable with cmp and cset.
                // This code is functionally equal but less readable, also because csinc
                // needs to invert the condition.
                cd.subs_32_reg(WZR, lhs, rhs);
                cd.csinc_32(dest, WZR, WZR, GT);
            }
            BinaryOp::LogicalOr => {
                // FIXME: This code could be more readable with cmp and cset.
                // This code is functionally equal but less readable, also because csinc
                // needs to invert the condition.
                cd.orr_32(dest, lhs, rhs, None);
                cd.subs_32_imm(WZR, dest, 0);
                cd.csinc_32(dest, WZR, WZR, EQ);
            }
            BinaryOp::LogicalAnd => {
                // FIXME: This code could be more readable with cmp and cset.
                // This code is functionally equal but less readable, also because csinc
                // needs to invert the condition.
                cd.subs_32_imm(WZR, lhs, 0); // is lhs != 0?
                cd.ccmp_32_imm(rhs, 0, 0b0100, NE); // upper conditions if rhs != 0, 0b0100 (zero flag) otherwise
                cd.csinc_32(dest, WZR, WZR, EQ); // same as cmp(dest, NE)
            }
        }
    }

    fn bin_op_lhs_const(&mut self, dest: Register, op: &BinaryOp, lhs: i32, rhs: Register) {
        let default = |c: &mut Compiler<D>| {
            c.cd().mov_arbitrary_imm(D::temp1(), lhs as u64, false);
            c.bin_op_no_const(dest, op, D::temp1(), rhs)
        };

        match op {
            BinaryOp::Add => self.test_shiftable_const(lhs, default, |comp, c, s| {
                comp.cd().add_32_imm_lsl(dest, rhs, c, s)
            }),
            BinaryOp::Minus => self.test_shiftable_const(lhs, default, |comp, lhs, s| {
                // 3 + a == -a + 3
                comp.cd().neg_32_reg(rhs, rhs, None);
                comp.cd().add_32_imm_lsl(dest, rhs, lhs, s);
            }),
            // sadly no straight forward optimization available
            BinaryOp::Multi => default(self),
            // sadly no straight forward optimization available
            BinaryOp::Divide => default(self),
            // sadly no straight forward optimization available
            BinaryOp::Modulo => default(self),
            BinaryOp::Equals => self.test_shiftable_const(lhs, default, |comp, lhs, s| {
                // 3 == a === a == 3
                let cd = comp.cd();
                cd.subs_32_imm_lsl(WZR, rhs, lhs, s);
                cd.csinc_32(dest, WZR, WZR, Condition::NE)
            }),
            BinaryOp::NotEqual => self.test_shiftable_const(lhs, default, |comp, lhs, s| {
                // 3 != a === a != 3
                let cd = comp.cd();
                cd.subs_32_imm_lsl(WZR, rhs, lhs, s);
                cd.csinc_32(dest, WZR, WZR, Condition::EQ)
            }),
            BinaryOp::Greater => self.test_shiftable_const(lhs, default, |comp, lhs, s| {
                let cd = comp.cd();
                cd.subs_32_imm_lsl(WZR, rhs, lhs, s);
                cd.csinc_32(dest, WZR, WZR, Condition::GE)
            }),
            BinaryOp::GreaterEqual => self.test_shiftable_const(lhs, default, |comp, lhs, s| {
                let cd = comp.cd();
                cd.subs_32_imm_lsl(WZR, rhs, lhs, s);
                cd.csinc_32(dest, WZR, WZR, Condition::GT)
            }),
            BinaryOp::Less => self.test_shiftable_const(lhs, default, |comp, lhs, s| {
                let cd = comp.cd();
                cd.subs_32_imm_lsl(WZR, rhs, lhs, s);
                cd.csinc_32(dest, WZR, WZR, Condition::LE)
            }),
            BinaryOp::LessEqual => self.test_shiftable_const(lhs, default, |comp, lhs, s| {
                let cd = comp.cd();
                cd.subs_32_imm_lsl(WZR, rhs, lhs, s);
                cd.csinc_32(dest, WZR, WZR, Condition::LT)
            }),
            // and with immediate will fail for some constant values
            BinaryOp::LogicalAnd => {
                let cd = self.cd();
                if lhs != 0 {
                    // result is true iff rhs is true
                    cd.subs_32_imm(WZR, rhs, 0);
                    cd.csinc_32(dest, WZR, WZR, Condition::EQ)
                } else {
                    // result is false as constant is false
                    cd.movz_32_imm(dest, 0)
                };
            }
            BinaryOp::LogicalOr => {
                let cd = self.cd();
                if lhs != 0 {
                    // result is true as constant is true
                    cd.movz_32_imm(dest, 1);
                } else {
                    // result is true iff rhs is true
                    cd.subs_32_imm(WZR, rhs, 0);
                    cd.csinc_32(dest, WZR, WZR, Condition::EQ)
                }
            }
        }
    }

    fn bin_op_rhs_const(&mut self, dest: Register, op: &BinaryOp, lhs: Register, rhs: i32) {
        let default = |c: &mut Compiler<D>| {
            c.cd().mov_arbitrary_imm(D::temp2(), rhs as u64, false);
            c.bin_op_no_const(dest, op, lhs, D::temp2())
        };

        match op {
            // use lhs const as a + b == b + a
            BinaryOp::Add => self.bin_op_lhs_const(dest, op, rhs, lhs),
            BinaryOp::Minus => self.test_shiftable_const(rhs, default, |comp, rhs, s| {
                let cd = comp.cd();
                cd.sub_32_imm_lsl(dest, lhs, rhs, s)
            }),
            BinaryOp::Multi => self.bin_op_lhs_const(dest, op, rhs, lhs),
            // sadly no straight forward optimization available
            BinaryOp::Divide => default(self),
            BinaryOp::Modulo => default(self),
            BinaryOp::Equals => self.bin_op_lhs_const(dest, op, rhs, lhs),
            BinaryOp::NotEqual => self.bin_op_lhs_const(dest, op, rhs, lhs),
            BinaryOp::Greater => self.test_shiftable_const(rhs, default, |comp, rhs, s| {
                let cd = comp.cd();
                cd.subs_32_imm_lsl(WZR, lhs, rhs, s);
                cd.csinc_32(dest, WZR, WZR, Condition::LE)
            }),
            BinaryOp::GreaterEqual => self.test_shiftable_const(rhs, default, |comp, rhs, s| {
                let cd = comp.cd();
                cd.subs_32_imm_lsl(WZR, lhs, rhs, s);
                cd.csinc_32(dest, WZR, WZR, Condition::LT)
            }),
            BinaryOp::Less => self.test_shiftable_const(rhs, default, |comp, rhs, s| {
                let cd = comp.cd();
                cd.subs_32_imm_lsl(WZR, lhs, rhs, s);
                cd.csinc_32(dest, WZR, WZR, Condition::GE)
            }),
            BinaryOp::LessEqual => self.test_shiftable_const(rhs, default, |comp, rhs, s| {
                let cd = comp.cd();
                cd.subs_32_imm_lsl(WZR, lhs, rhs, s);
                cd.csinc_32(dest, WZR, WZR, Condition::GT)
            }),
            BinaryOp::LogicalAnd => self.bin_op_lhs_const(dest, op, rhs, lhs),
            BinaryOp::LogicalOr => self.bin_op_lhs_const(dest, op, rhs, lhs),
        }
    }

    fn test_shiftable_const<DF: FnOnce(&mut Self), F: FnOnce(&mut Self, Imm12, Shift1)>(
        &mut self,
        constant: i32,
        default: DF,
        f: F,
    ) {
        match find_shift(constant) {
            None => default(self),
            Some((c, s)) => f(self, c, s),
        }
    }

    fn lhs_const_with_shift_or_default<F: FnOnce(Imm12, Shift1)>(
        &mut self,
        dest: Register,
        op: &BinaryOp,
        lhs: i32,
        rhs: Register,
        f: F,
    ) {
        match find_shift(lhs) {
            None => {
                self.cd().mov_arbitrary_imm(D::temp1(), lhs as u64, false);
                self.bin_op_no_const(dest, op, D::temp1(), rhs)
            }
            Some((c, s)) => f(c, s),
        }
    }

    fn mov_reg(&mut self, dest: Register, src: Register) {
        if dest != src {
            self.cd().mov_64_reg(dest, src)
        }
    }

    fn load_reg(&mut self, reg: &LirReg, alt: Register) -> Register {
        match self.find_reg_off(reg) {
            RegOff::Reg(reg) => reg,
            RegOff::Off(offset) => {
                let fp_offset = -(offset as Imm9) - 0x8; // offset + 8byte frame_pointer
                self.cd().ldur_32(alt, 29, fp_offset);
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
                let fp_offset = -(offset as Imm9) - 0x8; // offset + 8byte frame_pointer
                self.cd().stur_32(reg, 29, fp_offset);
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

fn find_shift(value: i32) -> Option<(Imm12, Shift1)> {
    // Check if the value fits in 12 bits without shifting
    if value >= 0 && value <= 0xFFF {
        return Some((value as Imm12, Shift1::LSL0));
    }

    // Check for 12-bit shift
    if (value & 0xFFF) == 0 && (value >> 12) <= 0xFFF {
        return Some(((value >> 12) as Imm12, Shift1::LSL12));
    }

    // Value cannot be represented as a shifted 12-bit immediate
    None
}
