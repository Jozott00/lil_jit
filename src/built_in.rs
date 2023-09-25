use lazy_static::lazy_static;
use std::arch::asm;
use std::collections::HashMap;

lazy_static! {
    pub static ref BUILTIN_FUNCS: HashMap<&'static str, BuiltIn> = {
        let mut map = HashMap::new();
        map.insert("cool", BuiltIn::new(0, cool_builtin as usize));
        map
    };
}

pub struct BuiltIn {
    pub arity: u8,
    pub mem_ptr: usize,
}

impl BuiltIn {
    fn new(arity: u8, mem_ptr: usize) -> Self {
        BuiltIn { arity, mem_ptr }
    }
}

extern "C" fn cool_builtin() -> i32 {
    println!("Cool");
    return 13;
}
