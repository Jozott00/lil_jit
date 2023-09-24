use lazy_static::lazy_static;
use log::{Level, LevelFilter, Metadata, Record, SetLoggerError};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Mutex;

pub static LILLOGGER: LilLogger = LilLogger {
    verbose: AtomicBool::new(false),
    dump_ast: AtomicBool::new(false),
    dump_ir: AtomicBool::new(false),
    dump_reg_alloc: AtomicBool::new(false),
    dump_disasm: AtomicBool::new(false),
};

pub struct LilLogger {
    pub verbose: AtomicBool,
    pub dump_ast: AtomicBool,
    pub dump_ir: AtomicBool,
    pub dump_reg_alloc: AtomicBool,
    pub dump_disasm: AtomicBool,
}

impl log::Log for LilLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        match metadata.target() {
            "verbose" => self.verbose.fetch_and(true, Ordering::Relaxed),
            "dump-ast" => self.dump_ast.fetch_and(true, Ordering::Relaxed),
            "dump-ir" => self.dump_ir.fetch_and(true, Ordering::Relaxed),
            "dump-reg-alloc" => self.dump_reg_alloc.fetch_and(true, Ordering::Relaxed),
            "dump-disasm" => self.dump_disasm.fetch_and(true, Ordering::Relaxed),
            _ => true,
        }
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            println!("{}", record.args());
        }
    }

    fn flush(&self) {}
}
