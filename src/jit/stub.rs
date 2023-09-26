use crate::jit::{JIT, JIT_REF};
use armoured_rust::types::InstructionPointer;
use std::arch::global_asm;

// This global assembler block defines a stub for ARM64 Arch-based systems.
// This function uses the stack to save context, triggers compilation, then restore the context.
//
// Note: This stub is, in practice, interacting with the `JIT` object using the
// `compile_by_caller_addr` function. This stub features a sequence of instructions:
// - The very first instruction subtracts 4 from the x30 reg, so it points to the original call instruction
// - The next couple of instructions save the order's state for later restoration.
// - It loads the link (x30) into the first argument register, so it passed to the `stub_call` function
// - `bl _stub_call` is where the actual stub call is made, which eventually calls `_stub_call` in the `jit`
//    via the JIT compiler.
// - The final sequence of instructions restores the state so that it appears as though the
//    function call did nothing, disregarding side effects.
global_asm!(
    "
    .global _compile_stub
    _compile_stub:
        sub x30, x30, #4        // Get actual call address (instruction before x30) 
        str x30, [sp, #-16]!    // Save link on stack

        // Save all possible argument registers
        stp x0, x1, [sp, #-16]!
        stp x2, x3, [sp, #-16]!
        stp x4, x5, [sp, #-16]!
        stp x6, x7, [sp, #-16]!

        mov x0, x30             // Pass modified link as argument to stub call

        // trigger jit compiler
        bl _stub_call           
        
        // Load all possible argument registers
        ldp x6, x7, [sp], #16
        ldp x4, x5, [sp], #16
        ldp x2, x3, [sp], #16
        ldp x0, x1, [sp], #16

        ldr x30, [sp], #16      // restore modified link
        ret
"
);

extern "C" {
    /// `compile_stub` is declared but defined by the assembly block.
    /// This function is written in Assembly and made visible to the Rust code through the `extern` keyword.
    ///
    /// # Safety
    /// This is a foreign function interplay, and it is always `unsafe` for Rust.
    pub fn compile_stub() -> u64;
}

/// `stub_call` acts as the bridge between the assembly code and the rest of the Rust program.
/// It interacts with the JIT compiler to apply compilation. It is only called by assembly.
/// The function takes one parameter: `caller`, which is the address of stub's call instruction.
///
/// # Safety
/// This function is `unsafe` for two reasons: It is primarily a FFI operation with assembly code,
/// and it employs raw pointers to interact with the `JIT` object.
///
/// # Panics
/// This function will panic if `JIT_REF` is `None`,
/// meaning no JIT reference is available to compile the uncompiled method.
#[no_mangle]
unsafe extern "C" fn stub_call(caller: u64) {
    let Some(jit_ref) = JIT_REF else {
        panic!("NO JIT_REF SET! Cannot compile uncompiled method as no JIT reference is available")
    };

    let jit = &mut *(jit_ref as *mut JIT);
    jit.compile_by_caller_addr(caller as InstructionPointer);
}
