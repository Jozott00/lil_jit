use lil_jit::run;
use std::io;
use std::io::Write;

mod pass {
    include!("pass/minimal.rs");
    include!("pass/binary_ops.rs");
}
pub fn run_passing(code: &str) -> (i32, String) {
    capture_stdout(|| run(code).expect("Test shouldn't fail"))
}

pub fn capture_stdout<T, F: FnOnce() -> T>(f: F) -> (T, String) {
    let mut buffer = Vec::new();
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    let mut writer = io::Cursor::new(&mut buffer);

    let result = f();

    handle.flush().unwrap();
    writer.flush().unwrap();

    (result, String::from_utf8(buffer).unwrap())
}
