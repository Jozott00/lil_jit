use std::collections::HashMap;
use std::ffi::CStr;
use std::io::Write;

use lazy_static::lazy_static;
use libc::c_char;

use crate::{STDOUT, Writable};

lazy_static! {
    pub static ref BUILTIN_FUNCS: HashMap<&'static str, BuiltIn> = {
        let mut map = HashMap::new();
        map.insert("cool", BuiltIn::new(0, cool_builtin as usize));
        map.insert("show", BuiltIn::new(1, show_builtin as usize));
        map.insert("showln", BuiltIn::new(1, showln_builtin as usize));
        map.insert("showascii", BuiltIn::new(1, showascii_builtin as usize));
        map.insert("showtext", BuiltIn::new(1, showtext_builtin as usize));
        map.insert("showtextln", BuiltIn::new(1, showtextln_builtin as usize));
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

extern "C" fn showtext_builtin(cptr: *const c_char) -> i32 {
    let c_str: &CStr = unsafe { CStr::from_ptr(cptr) };
    print(format!("{}", c_str.to_str().unwrap()));

    DEFAULT_RETURN
}

extern "C" fn showtextln_builtin(cptr: *const c_char) -> i32 {
    let c_str: &CStr = unsafe { CStr::from_ptr(cptr) };
    print(format!("{}\n", c_str.to_str().unwrap()));
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
