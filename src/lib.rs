use std::io;
use std::io::Cursor;
use std::sync::Mutex;

use lazy_static::lazy_static;

use crate::checker::check_lil;
use crate::jit::run_jit;
use crate::parser::parse_lil_program;

mod ast;
mod built_in;
mod checker;
mod error;
mod jit;
mod location;
pub mod logger;
mod parser;
pub mod settings;
mod visitor;

pub enum Writable {
    Stdout(io::Stdout),
    Buffer(Cursor<Vec<u8>>),
}

lazy_static! {
    pub static ref STDOUT: Mutex<Writable> = Mutex::new(Writable::Stdout(io::stdout()));
}

pub fn run(code: &str) -> Result<i32, i32> {
    let ast = parse_lil_program(code).map_err(|error| {
        error.print(&code);
        return 1;
    })?;

    log::info!(target: "dump-ast", "AST DUMP:\n{:#?}", ast);

    check_lil(&ast).map_err(|errors| {
        for error in errors {
            error.print(code);
        }
        return 1;
    })?;

    Ok(run_jit(&ast))
}
