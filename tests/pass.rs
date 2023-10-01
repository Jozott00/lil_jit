include!("pass/minimal.rs");
include!("pass/binary_ops.rs");
include!("pass/assignment.rs");
include!("pass/for_loop.rs");
include!("pass/builtin_calls.rs");
include!("pass/function.rs");

use std::io::{Cursor, Read};
use std::sync::Mutex;

use lazy_static::lazy_static;
use lil_jit::{run, Writable, STDOUT};

lazy_static! {
    static ref TEST_LOCK: Mutex<()> = Mutex::new(());
}

fn setup<T, F: FnOnce() -> T>(f: F) -> T {
    // Acquire the test lock, handling a poisoned lock if necessary
    let _capture_guard = match TEST_LOCK.lock() {
        Ok(guard) => guard,
        Err(poisoned) => poisoned.into_inner(),
    };

    // let _ = simple_logger::init();

    f()
}

pub fn run_passing(code: &str) -> i32 {
    setup(|| run(code).expect("Test shouldn't fail"))
}

pub fn run_passing_captured(code: &str) -> (i32, String) {
    setup(|| capture_stdout(|| run(code).expect("Test shouldn't fail")))
}

fn run_body(code: &str) -> i32 {
    run_passing(
        format!(
            "
        fn main() {{
            {code}
        }}
        "
        )
        .as_str(),
    )
}

fn run_body_captured(code: &str) -> (i32, String) {
    run_passing_captured(
        format!(
            "
        fn main() {{
            {code}
        }}
        "
        )
        .as_str(),
    )
}

/// Replaces the static `STDOUT` in `lib.rs` by a vector buffer, to
/// capture all program output. As multiple parallel tests lead to
/// concurrency issues, those are synchronized via the `CAPTURE_LOCK`.
pub fn capture_stdout<T, F: FnOnce() -> T>(f: F) -> (T, String) {
    // Create a new in-memory buffer
    let buffer = Cursor::new(Vec::new());

    // Replace the global STDOUT with the new buffer
    let original_stdout = {
        let mut stdout = STDOUT.lock().unwrap();
        std::mem::replace(&mut *stdout, Writable::Buffer(buffer))
    };

    // Execute the closure
    let result = f();

    // Retrieve the buffer and restore the original STDOUT
    let mut buffer = {
        let mut stdout = STDOUT.lock().unwrap();
        if let Writable::Buffer(buff) = std::mem::replace(&mut *stdout, original_stdout) {
            buff
        } else {
            panic!("Wasn't buffer but stdout... should be impossible")
        }
    };

    // Convert the buffer into a String
    let captured_output = {
        let mut output = String::new();
        let position = buffer.position();
        buffer.set_position(0);
        buffer.read_to_string(&mut output).unwrap();
        buffer.set_position(position); // Restore original position
        output
    };

    (result, captured_output)
}
