use std::ffi::c_void;
use std::mem::size_of;
use std::ptr;
use std::ptr::null_mut;
use armoured_rust::instruction_encoding::{AddressableInstructionProcessor, InstructionProcessor, InstructionSet, InstructionSetWithAddress};
use armoured_rust::instruction_encoding::branch_exception_system::barriers::Barriers;
use armoured_rust::instruction_encoding::branch_exception_system::{BranchExceptionSystem, BranchExceptionSystemWithAddress};
use armoured_rust::instruction_encoding::branch_exception_system::conditional_branch_imm::{ConditionalBranchImmediate, ConditionalBranchImmediateWithAddress};
use armoured_rust::instruction_encoding::branch_exception_system::exception_generation::ExceptionGeneration;
use armoured_rust::instruction_encoding::branch_exception_system::pstate::PStateInstructions;
use armoured_rust::instruction_encoding::branch_exception_system::system_instr_w_register_arg::SystemInstructionsWithRegArg;
use armoured_rust::instruction_encoding::branch_exception_system::system_instructions::SystemInstructions;
use armoured_rust::instruction_encoding::branch_exception_system::system_register_move::SystemRegisterMove;
use armoured_rust::instruction_encoding::branch_exception_system::unconditional_branch_immediate::{UnconditionalBranchImmediate, UnconditionalBranchImmediateWithAddress};
use armoured_rust::instruction_encoding::branch_exception_system::unconditional_branch_register::UnconditionalBranchRegister;
use armoured_rust::instruction_encoding::data_proc_imm::add_substract_imm::AddSubtractImmediate;
use armoured_rust::instruction_encoding::data_proc_imm::bitfield::BitfieldInstructions;
use armoured_rust::instruction_encoding::data_proc_imm::{DataProcessingImmediate, DataProcessingImmediateWithAddress};
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
use armoured_rust::instruction_encoding::loads_and_stores::{LoadsAndStores, LoadsAndStoresWithAddress};
use armoured_rust::instruction_encoding::loads_and_stores::memory_copy_and_memory_set::MemoryCopyAndMemorySet;
use armoured_rust::types::{Instruction, InstructionPointer, Offset32};
use libc::{MAP_ANON, MAP_PRIVATE, PROT_READ, PROT_WRITE};
use log::warn;

/// Memory extension multiplier, used in CodegenData::extend_memory
const MEMORY_ALLOCATION_MULTIPLIER: usize = 2;
/// Maximum offset of instruction pointer relative jumps
const MAX_OFFSET: usize = i32::MAX as usize;

/// The `CodegenData` struct holds the necessary data for code generation, including pointers to the machine code base and current position,
/// as well as the length of the allocated memory block.
pub struct CodegenData {
    mcbase: InstructionPointer, // The base address of the machine code memory block.
    len: usize,                 // The length of the allocated memory block.
    mcodeptr: InstructionPointer, // The current position in the machine code memory block.
}

/// Implementation of the `Drop` trait for `CodegenData`.
/// This ensures that the allocated memory is unmapped when the `CodegenData` instance is dropped.
impl Drop for CodegenData {
    fn drop(&mut self) {
        // Unmap the allocated memory block.
        unsafe { libc::munmap(self.mcbase as *mut c_void, self.len); }
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
        })
    }
}

// private methods

impl CodegenData {
    /// Calculates the upper bound of the allocated memory block.
    fn bound_ptr(&self) -> usize {
        self.mcbase as usize + self.len
    }

    /// Extends the allocated memory block to twice its current size, preserving the existing machine code.
    fn extend_memory(&mut self) -> Result<(), std::io::Error> {
        let new_size = self.len * MEMORY_ALLOCATION_MULTIPLIER;
        let new_mem = alloc_mem(new_size)?;

        let mcode_off = self.mcodeptr as usize - self.mcbase as usize;

        unsafe {
            _ = libc::memcpy(new_mem as *mut c_void, self.mcbase as *mut c_void, mcode_off);
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
            self.extend_memory().expect("Wasn't able to extend memory!")
        }

        unsafe {
            // Write the instruction to the current position in the machine code memory block.
            ptr::write(self.mcodeptr, instr);
            // Advance the code pointer.
            self.mcodeptr = self.mcodeptr.add(1);
        }
    }

    /// Checks whether the current code pointer is within the bounds of the allocated memory block.
    fn in_bound(&self) -> bool {
        self.mcodeptr as usize <= self.bound_ptr() - size_of::<Instruction>()
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
        let offset_abs = pc.checked_sub(addr)
            .unwrap_or_else(|| addr.checked_sub(pc).unwrap());

        debug_assert!(offset_abs <= MAX_OFFSET, "Offset to address is too large (exceeds maximum of {:x})", MAX_OFFSET);

        if addr >= pc { offset_abs as i32 } else { -(offset_abs as i32) }
    }
}


impl InstructionSetWithAddress<()> for CodegenData {}
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

        let val: u8 = unsafe { ptr::read(cd.mcbase as *const u8)};
        assert_eq!(val, 123);
        let val: u8 = unsafe { ptr::read(ptr_between as *const u8)};
        assert_eq!(val, 234);

        cd.extend_memory();

        let val: u8 = unsafe { ptr::read(cd.mcbase as *const u8)};
        assert_eq!(val, 123);
        let val: u8 = unsafe { ptr::read((cd.mcodeptr.sub(1)) as *const u8)};
        assert_eq!(val, 234);

    }
}