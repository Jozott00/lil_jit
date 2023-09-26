use armoured_rust::types::InstructionPointer;
use std::collections::{HashMap, HashSet};

// TODO: Maybe store (function_name, vec![instruction_ptr]) as a function could call a unresolved function multiple times
#[derive(Debug, Default)]
pub struct StubRefStore<'a> {
    // holds for a instruction pointer of a call the function name that was tried to be called
    ref_to_func: HashMap<InstructionPointer, &'a str>,
    // holds for a function name all references that have to get patched
    func_to_refs: HashMap<&'a str, HashSet<(&'a str, InstructionPointer)>>,
}

impl<'a> StubRefStore<'a> {
    /// Add an unresolved function and its caller (instruction pointer and function name)
    ///
    /// ## Arguments
    /// - `func_name`: The function that was not yet resolved
    /// - `caller_func`: The function that calls the `func_name` function
    /// - `at_instr_ptr`: The instruction pointer of `caller_func` were the `func_name` was called
    pub fn add_unresolved(
        &mut self,
        func_name: &'a str,
        caller_func: &'a str,
        at_instr_ptr: InstructionPointer,
    ) {
        self.ref_to_func.insert(at_instr_ptr, func_name);

        self.func_to_refs
            .entry(func_name)
            .or_insert_with(HashSet::new)
            .insert((caller_func, at_instr_ptr));
    }

    /// Resolve a function, returning all its callers for patching
    ///
    /// - Removes all associations with the given `func_name`
    pub fn resolve_function(
        &mut self,
        func_name: &'a str,
    ) -> Option<HashSet<(&'a str, InstructionPointer)>> {
        if let Some(callers) = self.func_to_refs.remove(func_name) {
            for (_, caller_addr) in &callers {
                self.ref_to_func.remove(&caller_addr);
            }
            Some(callers)
        } else {
            None
        }
    }

    /// Retrieve the function name for a given instruction pointer
    pub fn get_function_name(&self, instr_ptr: InstructionPointer) -> Option<&&'a str> {
        self.ref_to_func.get(&instr_ptr)
    }
}
