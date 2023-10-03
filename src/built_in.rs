use std::collections::HashMap;
use std::io::Write;

use lazy_static::lazy_static;

use crate::{Writable, STDOUT};

lazy_static! {
    pub static ref BUILTIN_FUNCS: HashMap<&'static str, BuiltIn> = {
        let mut map = HashMap::new();
        map.insert("pow", BuiltIn::new(2, pow_builtin as usize));
        map.insert("cool", BuiltIn::new(0, cool_builtin as usize));
        map.insert("show", BuiltIn::new(1, show_builtin as usize));
        map.insert("showln", BuiltIn::new(1, showln_builtin as usize));
        map.insert("showascii", BuiltIn::new(1, showascii_builtin as usize));
        map.insert("showtext", BuiltIn::new(1, showtext_builtin as usize));
        map.insert("showtextln", BuiltIn::new(1, showtextln_builtin as usize));
        map.insert("showall", BuiltIn::new(10, showall10_builtin as usize));
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

static DEFAULT_RETURN: i32 = 0;

extern "C" fn pow_builtin(a: i32, b: i32) -> i32 {
    a.pow(b as u32)
}

extern "C" fn cool_builtin() -> i32 {
    print(format!("Cool\n"));
    DEFAULT_RETURN
}

extern "C" fn show_builtin(n: i32) -> i32 {
    print(format!("{}", n));
    DEFAULT_RETURN
}

extern "C" fn showln_builtin(n: i32) -> i32 {
    print(format!("{}\n", n));

    DEFAULT_RETURN
}

extern "C" fn showascii_builtin(n: i32) -> i32 {
    print(format!("{}", n as u8 as char));

    DEFAULT_RETURN
}

extern "C" fn showtext_builtin(text_ptr: usize) -> i32 {
    let str_ref: &String = unsafe { &*(text_ptr as *const String) };
    print!("{}", str_ref);
    DEFAULT_RETURN
}

extern "C" fn showtextln_builtin(text_ptr: usize) -> i32 {
    let str_ref: &String = unsafe { &*(text_ptr as *const String) };
    print!("{}\n", str_ref);
    DEFAULT_RETURN
}

extern "C" fn showall10_builtin(
    i1: i32,
    i2: i32,
    i3: i32,
    i4: i32,
    i5: i32,
    i6: i32,
    i7: i32,
    i8: i32,
    i9: i32,
    i10: i32,
) -> i32 {
    print(format!(
        "{i1}, {i2}, {i3}, {i4}, {i5}, {i6}, {i7}, {i8}, {i9}, {i10}\n"
    ));
    DEFAULT_RETURN
}

fn print(str: String) {
    let mut handle = STDOUT.lock().unwrap();
    match &mut *handle {
        Writable::Stdout(stdout) => {
            write!(stdout, "{}", str).unwrap();
            stdout.flush().unwrap();
        }
        Writable::Buffer(buf) => {
            write!(buf, "{}", str).unwrap();
        }
    }
}
