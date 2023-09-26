use crate::jit::{JIT, JIT_REF};
use armoured_rust::types::InstructionPointer;
use std::arch::global_asm;

// TODO: save and restore caller saved registers
global_asm!(
    "
    .global _compile_stub
    _compile_stub:

        sub x30, x30, #4        // Get actual call address (instruction before x30) 
        mov x0, x30             // Pass modified link as argument
        str x30, [sp, #-16]!    // Save link on stack

        bl _stub_call           // trigger jit compiler

        ldr x30, [sp], #16      // restore modified link
        ret
"
);

extern "C" {
    pub fn compile_stub() -> u64;
}

/// SUPER UNSAFE FUNCTION! Only called by assembly
#[no_mangle]
unsafe extern "C" fn stub_call(caller: u64) {
    let Some(jit_ref) = JIT_REF else {
        panic!("NO JIT_REF SET! Cannot compile uncompiled method as no JIT reference is available")
    };

    let jit = &mut *(jit_ref as *mut JIT);
    jit.compile_by_caller_addr(caller as InstructionPointer);
}
