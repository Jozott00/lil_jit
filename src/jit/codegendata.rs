use std::ffi::c_void;
use std::fmt::{Display, Formatter};
use std::mem::size_of;
use std::ptr::null_mut;
use std::{mem, ptr, slice};

use armoured_rust::instruction_encoding::{AddressableInstructionProcessor, InstructionProcessor, InstructionSet, InstructionSetWithAddress};
use armoured_rust::instruction_encoding::branch_exception_system::{BranchExceptionSystem, BranchExceptionSystemWithAddress};
use armoured_rust::instruction_encoding::branch_exception_system::barriers::Barriers;
use armoured_rust::instruction_encoding::branch_exception_system::compare_and_branch_imm::{CompareAndBranchImm, CompareAndBranchImmWithAddress};
use armoured_rust::instruction_encoding::branch_exception_system::conditional_branch_imm::{ConditionalBranchImmediate, ConditionalBranchImmediateWithAddress};
use armoured_rust::instruction_encoding::branch_exception_system::exception_generation::ExceptionGeneration;
use armoured_rust::instruction_encoding::branch_exception_system::pstate::PStateInstructions;
use armoured_rust::instruction_encoding::branch_exception_system::system_instr_w_register_arg::SystemInstructionsWithRegArg;
use armoured_rust::instruction_encoding::branch_exception_system::system_instructions::SystemInstructions;
use armoured_rust::instruction_encoding::branch_exception_system::system_register_move::SystemRegisterMove;
use armoured_rust::instruction_encoding::branch_exception_system::test_and_branch_imm::{TestAndBranchImmediate, TestAndBranchImmediateWithAddress};
use armoured_rust::instruction_encoding::branch_exception_system::unconditional_branch_immediate::{UnconditionalBranchImmediate, UnconditionalBranchImmediateWithAddress};
use armoured_rust::instruction_encoding::branch_exception_system::unconditional_branch_register::UnconditionalBranchRegister;
use armoured_rust::instruction_encoding::common_aliases::CommonAliases;
use armoured_rust::instruction_encoding::data_proc_imm::{DataProcessingImmediate, DataProcessingImmediateWithAddress};
use armoured_rust::instruction_encoding::data_proc_imm::add_substract_imm::AddSubtractImmediate;
use armoured_rust::instruction_encoding::data_proc_imm::bitfield::BitfieldInstructions;
use armoured_rust::instruction_encoding::data_proc_imm::extract::ExtractInstructions;
use armoured_rust::instruction_encoding::data_proc_imm::logical_imm::LogicalImmediate;
use armoured_rust::instruction_encoding::data_proc_imm::mov_wide_imm::MovWideImmediate;
use armoured_rust::instruction_encoding::data_proc_imm::pc_rel_addr::{PcRelAddressing, PcRelAddressingWithAddress};
use armoured_rust::instruction_encoding::data_proc_reg::add_sub_carry::AddSubtractWithCarry;
use armoured_rust::instruction_encoding::data_proc_reg::add_sub_ext_reg::AddSubtractExtendedRegister;
use armoured_rust::instruction_encoding::data_proc_reg::add_sub_shift_reg::AddSubtractShiftedRegister;
use armoured_rust::instruction_encoding::data_proc_reg::cond_compare_imm::ConditionalCompareImmediate;
use armoured_rust::instruction_encoding::data_proc_reg::cond_compare_reg::ConditionalCompareRegister;
use armoured_rust::instruction_encoding::data_proc_reg::conditional_select::ConditionalSelect;
use armoured_rust::instruction_encoding::data_proc_reg::data_proc_one_src::DataProcessingOneSource;
use armoured_rust::instruction_encoding::data_proc_reg::data_proc_three_src::DataProcessingThreeSource;
use armoured_rust::instruction_encoding::data_proc_reg::data_proc_two_src::DataProcessingTwoSource;
use armoured_rust::instruction_encoding::data_proc_reg::DataProcessingRegister;
use armoured_rust::instruction_encoding::data_proc_reg::evaluate_into_flags::EvaluateIntoFlags;
use armoured_rust::instruction_encoding::data_proc_reg::logical_shift_reg::LogicalShiftRegister;
use armoured_rust::instruction_encoding::data_proc_reg::rotate_right_into_flags::RotateRightIntoFlags;
use armoured_rust::instruction_encoding::loads_and_stores::{LoadsAndStores, LoadsAndStoresWithAddress};
use armoured_rust::instruction_encoding::loads_and_stores::advanced_simd_ldr_str_multi_structures::AdvancedSIMDLoadStoreMultipleStructures;
use armoured_rust::instruction_encoding::loads_and_stores::advanced_simd_ldr_str_single_structures::AdvancedSIMDLoadStoreSingleStructures;
use armoured_rust::instruction_encoding::loads_and_stores::atomic_memory_operations::AtomicMemoryOperatinos;
use armoured_rust::instruction_encoding::loads_and_stores::compare_and_swap::CompareAndSwap;
use armoured_rust::instruction_encoding::loads_and_stores::compare_and_swap_pair::CompareAndSwapPair;
use armoured_rust::instruction_encoding::loads_and_stores::ldapr_stlr_unscale_imm::LdaprStlrUnscaleImmediate;
use armoured_rust::instruction_encoding::loads_and_stores::load_register_literal::{LoadRegisterLiteral, LoadRegisterLiteralWithAddress};
use armoured_rust::instruction_encoding::loads_and_stores::load_store_exclusive_pair::LoadStoreExclusivePair;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_exclusive_register::LoadStoreExclusiveRegister;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_memory_tags::LoadStoreMemoryTags;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_no_allocate_pair_offset::LoadStoreNoAllocatePairOffset;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_ordered::LoadStoreOrdered;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_reg_pair_offset::LoadStoreRegisterPairOffset;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_reg_pair_post_indexed::LoadStoreRegisterPairPostIndexed;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_reg_pair_pre_indexed::LoadStoreRegisterPairPreIndexed;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_reg_pre_post_indexed::LoadStoreRegisterPrePostIndexed;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_reg_unprivileged::LoadStoreRegisterUnprivileged;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_reg_unscaled_imm::LoadStoreRegisterUnscaledImmediate;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_register_pac::LoadStoreRegisterPac;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_register_regoffset::LoadStoreRegisterRegisterOffset;
use armoured_rust::instruction_encoding::loads_and_stores::load_store_register_unsigned_imm::LoadStoreRegisterUnsignedImmediate;
use armoured_rust::instruction_encoding::loads_and_stores::memory_copy_and_memory_set::MemoryCopyAndMemorySet;
use armoured_rust::types::{HW, Instruction, InstructionPointer, Offset32, Register, UImm16};
use bad64::disasm;
use libc::{MAP_ANON, MAP_PRIVATE, PROT_READ, PROT_WRITE};
use log::{info, warn};

/// Memory extension multiplier, used in CodegenData::extend_memory
#[allow(dead_code)]
const MEMORY_ALLOCATION_MULTIPLIER: usize = 2;
/// Maximum offset of instruction pointer relative jumps
const MAX_OFFSET: usize = i32::MAX as usize;

pub type InstrCount = usize;

/// The `CodegenData` struct holds the necessary data for code generation, including pointers to the machine code base and current position,
/// as well as the length of the allocated memory block.
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct CodegenData {
    mcbase: InstructionPointer,
    // The base address of the machine code memory block.
    len: usize,
    // The length of the allocated memory block.
    mcodeptr: InstructionPointer, // The current position in the machine code memory block.

    // if memory is currently executable
    executable: bool,
}

/// Implementation of the `Drop` trait for `CodegenData`.
/// This ensures that the allocated memory is unmapped when the `CodegenData` instance is dropped.
impl Drop for CodegenData {
    fn drop(&mut self) {
        // Unmap the allocated memory block.
        unsafe {
            libc::munmap(self.mcbase as *mut c_void, self.len);
        }
    }
}

impl CodegenData {
    /// Creates a new `CodegenData` instance with an allocated memory block of system page size.
    pub fn new() -> Result<Self, std::io::Error> {
        let page_size = unsafe { libc::sysconf(libc::_SC_PAGESIZE) } as usize;
        alloc_mem(page_size).map(|addr| CodegenData {
            mcbase: addr as InstructionPointer,
            len: page_size,
            mcodeptr: addr as InstructionPointer,
            executable: false,
        })
    }

    pub fn ins_count(&self) -> usize {
        // The number of instructions currently stored.
        // TODO: This should not be architecture dependent
        // Devide by 4 because all instructions on ARM64 are 32bit long
        return (self.mcodeptr as usize - self.mcbase as usize) / 4;
    }

    pub fn code_ptr(&self) -> InstructionPointer {
        return self.mcodeptr;
    }

    pub fn base_ptr(&self) -> InstructionPointer {
        return self.mcbase;
    }

    pub fn instr_count(&self) -> InstrCount {
        ((self.mcodeptr as usize) - self.mcbase as usize) / 4
    }

    pub fn code_ptr_from_instr_count(&self, instr_count: InstrCount) -> InstructionPointer {
        (self.mcbase as usize + instr_count * 4) as InstructionPointer
    }

    pub fn patch_at<F>(&mut self, dst: InstructionPointer, function: F)
    where
        F: FnOnce(&mut Self) -> (),
    {
        // FIXME: REquire that instruction is in the correct range

        let was_executable = self.executable;
        if self.executable {
            self.make_writable()
        }

        let old_mcodeptr = self.mcodeptr;
        self.mcodeptr = dst;

        function(self);

        self.mcodeptr = old_mcodeptr;

        if was_executable {
            self.make_executable()
        }
    }

    pub fn mov_arbitrary_imm(&mut self, dest: Register, imm: u64, fill_with_nops: bool) {
        for i in 0..4 {
            let chunk = ((imm >> (16 * i)) & 0xFFFF) as UImm16;

            if chunk == 0 && i != 0 {
                if fill_with_nops {
                    self.nop()
                }
                continue;
            }

            if i == 0 {
                self.movz_64_imm_lsl(dest, chunk, HW::from(i));
            } else {
                self.movk_64_imm_lsl(dest, chunk, HW::from(i));
            }
        }
    }
}

// private methods

impl CodegenData {
    /// Calculates the upper bound of the allocated memory block.
    fn bound_ptr(&self) -> usize {
        self.mcbase as usize + self.len
    }

    /// Extends the allocated memory block to twice its current size, preserving the existing machine code.
    #[allow(unused)]
    fn extend_memory(&mut self) -> Result<(), std::io::Error> {
        let new_size = self.len * MEMORY_ALLOCATION_MULTIPLIER;
        let new_mem = alloc_mem(new_size)?;

        info!(target: "verbose", "Extend memory... old_pos: {:#x}, new_pos: {:#x}", self.mcbase as usize, new_mem);

        let mcode_off = self.mcodeptr as usize - self.mcbase as usize;

        unsafe {
            _ = libc::memcpy(
                new_mem as *mut c_void,
                self.mcbase as *mut c_void,
                mcode_off,
            );
            libc::munmap(self.mcbase as *mut c_void, self.len);
        }

        self.len = new_size;
        self.mcbase = new_mem as InstructionPointer;
        self.mcodeptr = (new_mem + mcode_off) as InstructionPointer;

        Ok(())
    }

    /// Emits a new instruction to the machine code memory block, advancing the code pointer.
    /// Extends memory if the operation would write beyond the bounds of the allocated memory block.
    fn emit(&mut self, instr: Instruction) {
        if !self.in_bound() {
            warn!("Need to extend memory as no mc memory space left!");
            panic!("No more memory to store instruction. Extending memory is currently disabled!");
            // self.extend_memory().expect("Wasn't able to extend memory!")
        }

        unsafe {
            // Write the instruction to the current position in the machine code memory block.
            ptr::write(self.mcodeptr, instr);
            // Advance the code pointer.
            self.mcodeptr = self.mcodeptr.add(1);
        }
    }

    pub fn make_executable(&mut self) {
        unsafe {
            if libc::mprotect(
                self.mcbase as *mut c_void,
                self.len,
                libc::PROT_READ | libc::PROT_EXEC,
            ) != 0
            {
                panic!("Failed to set memory as executable");
            }
        }

        self.executable = true;
    }

    fn make_writable(&mut self) {
        unsafe {
            if libc::mprotect(
                self.mcbase as *mut c_void,
                self.len,
                libc::PROT_READ | libc::PROT_WRITE,
            ) != 0
            {
                panic!("Failed to set memory as writable");
            }
        }

        self.executable = false;
    }

    #[inline(always)]
    pub fn nullary_fn_ptr(&self) -> unsafe extern "C" fn() -> i32 {
        unsafe { mem::transmute(self.mcbase as usize) }
    }

    /// Checks whether the current code pointer is within the bounds of the allocated memory block.
    fn in_bound(&self) -> bool {
        self.mcodeptr as usize <= self.bound_ptr() - size_of::<Instruction>()
    }
}

/// Additional instruction emitting shortcuts
impl CodegenData {
    /// Emits instruction to call a function at the given `addr`.
    ///
    /// There are two different emit approaches:
    /// - If the relative offset to the addr is within 32 bit, it uses
    /// a pc relative `bl` instruction.
    /// - Otherwise it loads the absolute instruction in multiple immediate moves in
    /// given `temp` and emits a `blr` with `temp`.
    ///
    /// In both cases a total of 5 instructions are emitted. In case
    /// of pc relative, the starting 4 instructions consist of `nop` instructions.
    pub fn func_call(&mut self, addr: usize, temp: Register) {
        let offset_abs = (self.mcodeptr as usize)
            .checked_sub(addr)
            .unwrap_or_else(|| addr.checked_sub(self.mcodeptr as usize).unwrap());

        if offset_abs <= MAX_OFFSET {
            // fill four instruction with nop
            self.nop();
            self.nop();
            self.nop();
            self.nop();
            // pc relative branch
            self.bl_to_addr(addr);
        } else {
            // write addr in temp register. Fill with nops, so it will always emit 4 instructions
            self.mov_arbitrary_imm(temp, addr as u64, true);
            self.blr(temp);
        }
    }
}

impl CodegenData {
    fn written_memory(&self) -> &[u8] {
        let len = (self.mcodeptr as usize) - (self.mcbase as usize);
        let ptr = self.mcbase as *const u8;

        assert_eq!(len % 4, 0, "Len is not a multiple of 4");
        assert_eq!(
            ptr as usize % mem::align_of::<u32>(),
            0,
            "Memory not u32 aligned"
        );
        assert!(self.len >= len, "Requested length exceeds memory map!");

        unsafe { slice::from_raw_parts(ptr, len) }
    }
}

impl Display for CodegenData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let decoded_iter = disasm(self.written_memory(), self.mcbase as u64);
        for instr in decoded_iter {
            match instr {
                Ok(instr) => {
                    let encoding = instr.opcode().to_le_bytes();
                    let enc_str = encoding.map(|e| format!("{e:02x}")).join(" ");
                    _ = write!(f, "{:#x}: {enc_str}      {instr}\n", instr.address());
                }
                Err(err) => {
                    _ = write!(f, "{err}\n");
                }
            }
        }

        Ok(())
    }
}

/// Allocates a block of memory of the specified size using `mmap`.
/// Returns an error if mmap failed.
fn alloc_mem(size: usize) -> Result<usize, std::io::Error> {
    let addr = unsafe {
        libc::mmap(
            null_mut(),
            size,
            PROT_WRITE | PROT_READ,
            MAP_ANON | MAP_PRIVATE,
            -1,
            0,
        )
    };

    if addr == libc::MAP_FAILED {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(addr as usize)
    }
}

/// Implementation of the `InstructionProcessor` trait for `CodegenData`.
/// Allows processing of individual instructions by emitting them to the machine code memory block.
impl InstructionProcessor<()> for CodegenData {
    fn process(&mut self, instr: Instruction) -> () {
        self.emit(instr)
    }
}

/// Implementation of the `AddressableInstructionProcessor` trait for `CodegenData`.
/// Provides a method to calculate the offset between the current code pointer and a specified address.
impl AddressableInstructionProcessor<()> for CodegenData {
    fn intr_ptr_offset_to(&self, addr: usize) -> Offset32 {
        let pc = self.mcodeptr as usize;
        let offset_abs = pc
            .checked_sub(addr)
            .unwrap_or_else(|| addr.checked_sub(pc).unwrap());

        debug_assert!(
            offset_abs <= MAX_OFFSET,
            "Offset to address is too large: {:#x} (exceeds maximum of {:#x})\nHelp: code_ptr: {:#x} addr: {:#x}\n",
            offset_abs,
            MAX_OFFSET,
            pc,
            addr,
        );

        if addr >= pc {
            offset_abs as i32
        } else {
            -(offset_abs as i32)
        }
    }
}

impl InstructionSetWithAddress<()> for CodegenData {}

impl CommonAliases<()> for CodegenData {}

impl InstructionSet<()> for CodegenData {}

impl DataProcessingImmediate<()> for CodegenData {}

impl PcRelAddressing<()> for CodegenData {}

impl AddSubtractImmediate<()> for CodegenData {}

impl LogicalImmediate<()> for CodegenData {}

impl MovWideImmediate<()> for CodegenData {}

impl BitfieldInstructions<()> for CodegenData {}

impl ExtractInstructions<()> for CodegenData {}

impl BranchExceptionSystem<()> for CodegenData {}

impl ConditionalBranchImmediate<()> for CodegenData {}

impl ExceptionGeneration<()> for CodegenData {}

impl SystemInstructionsWithRegArg<()> for CodegenData {}

impl Barriers<()> for CodegenData {}

impl PStateInstructions<()> for CodegenData {}

impl SystemInstructions<()> for CodegenData {}

impl SystemRegisterMove<()> for CodegenData {}

impl UnconditionalBranchRegister<()> for CodegenData {}

impl UnconditionalBranchImmediate<()> for CodegenData {}

impl LoadsAndStores<()> for CodegenData {}

impl CompareAndSwapPair<()> for CodegenData {}

impl LoadRegisterLiteral<()> for CodegenData {}

impl LoadStoreMemoryTags<()> for CodegenData {}

impl LoadStoreExclusivePair<()> for CodegenData {}

impl LoadStoreExclusiveRegister<()> for CodegenData {}

impl LoadStoreOrdered<()> for CodegenData {}

impl CompareAndSwap<()> for CodegenData {}

impl LdaprStlrUnscaleImmediate<()> for CodegenData {}

impl MemoryCopyAndMemorySet<()> for CodegenData {}

impl LoadStoreNoAllocatePairOffset<()> for CodegenData {}

impl LoadStoreRegisterPairPostIndexed<()> for CodegenData {}

impl LoadStoreRegisterPairOffset<()> for CodegenData {}

impl LoadStoreRegisterPairPreIndexed<()> for CodegenData {}

impl LoadStoreRegisterUnscaledImmediate<()> for CodegenData {}

impl LoadStoreRegisterPrePostIndexed<()> for CodegenData {}

impl LoadStoreRegisterUnprivileged<()> for CodegenData {}

impl LoadStoreRegisterUnsignedImmediate<()> for CodegenData {}

impl LoadStoreRegisterRegisterOffset<()> for CodegenData {}

impl LoadStoreRegisterPac<()> for CodegenData {}

impl AdvancedSIMDLoadStoreMultipleStructures<()> for CodegenData {}

impl AdvancedSIMDLoadStoreSingleStructures<()> for CodegenData {}

impl AtomicMemoryOperatinos<()> for CodegenData {}

impl DataProcessingRegister<()> for CodegenData {}

impl DataProcessingTwoSource<()> for CodegenData {}

impl DataProcessingOneSource<()> for CodegenData {}

impl LogicalShiftRegister<()> for CodegenData {}

impl AddSubtractShiftedRegister<()> for CodegenData {}

impl AddSubtractExtendedRegister<()> for CodegenData {}

impl AddSubtractWithCarry<()> for CodegenData {}

impl RotateRightIntoFlags<()> for CodegenData {}

impl EvaluateIntoFlags<()> for CodegenData {}

impl ConditionalCompareRegister<()> for CodegenData {}

impl ConditionalCompareImmediate<()> for CodegenData {}

impl ConditionalSelect<()> for CodegenData {}

impl DataProcessingThreeSource<()> for CodegenData {}

impl DataProcessingImmediateWithAddress<()> for CodegenData {}

impl PcRelAddressingWithAddress<()> for CodegenData {}

impl CompareAndBranchImm<()> for CodegenData {}

impl TestAndBranchImmediate<()> for CodegenData {}

impl CompareAndBranchImmWithAddress<()> for CodegenData {}

impl TestAndBranchImmediateWithAddress<()> for CodegenData {}

impl BranchExceptionSystemWithAddress<()> for CodegenData {}

impl ConditionalBranchImmediateWithAddress<()> for CodegenData {}

impl UnconditionalBranchImmediateWithAddress<()> for CodegenData {}

impl LoadsAndStoresWithAddress<()> for CodegenData {}

impl LoadRegisterLiteralWithAddress<()> for CodegenData {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extend_mem() {
        let mut cd = CodegenData::new().unwrap();
        cd.emit(123);
        let ptr_between = cd.mcodeptr;
        cd.emit(234);

        let val: u8 = unsafe { ptr::read(cd.mcbase as *const u8) };
        assert_eq!(val, 123);
        let val: u8 = unsafe { ptr::read(ptr_between as *const u8) };
        assert_eq!(val, 234);

        cd.extend_memory();

        let val: u8 = unsafe { ptr::read(cd.mcbase as *const u8) };
        assert_eq!(val, 123);
        let val: u8 = unsafe { ptr::read((cd.mcodeptr.sub(1)) as *const u8) };
        assert_eq!(val, 234);
    }
}
