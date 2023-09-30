use std::fs;
use std::process::exit;
use std::sync::atomic::Ordering;

use clap::Parser;
use log::LevelFilter;

use lil_jit;
use lil_jit::checker::check_lil;
use lil_jit::jit::run_jit;
use lil_jit::logger::LILLOGGER;
use lil_jit::parser::parse_lil_program;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    filename: String,

    #[arg(long)]
    dump_ast: bool,

    #[arg(long)]
    dump_ir: bool,

    #[arg(long)]
    dump_reg_alloc: bool,

    #[arg(long)]
    dump_disasm: bool,

    #[arg(long)]
    dump_disasm_patch: bool,

    #[arg(long, short)]
    verbose: bool,
}

fn main() {
    let cli = Cli::parse();
    LILLOGGER.verbose.store(cli.verbose, Ordering::Relaxed);
    LILLOGGER.dump_ast.store(cli.dump_ast, Ordering::Relaxed);
    LILLOGGER.dump_ir.store(cli.dump_ir, Ordering::Relaxed);
    LILLOGGER
        .dump_reg_alloc
        .store(cli.dump_reg_alloc, Ordering::Relaxed);
    LILLOGGER
        .dump_disasm
        .store(cli.dump_disasm, Ordering::Relaxed);
    LILLOGGER
        .dump_disasm_patch
        .store(cli.dump_disasm_patch, Ordering::Relaxed);
    log::set_logger(&LILLOGGER).unwrap();
    log::set_max_level(LevelFilter::Info);

    let code = fs::read_to_string(&cli.filename)
        .expect(&*format!("Unable to read file: {}", &cli.filename));

    let ast = parse_lil_program(code.as_str()).unwrap_or_else(|error| {
        error.print(&code);
        exit(1);
    });

    log::info!(target: "dump-ast", "AST DUMP:\n{:#?}", ast);

    check_lil(&ast).unwrap_or_else(|errors| {
        for error in errors {
            error.print(code.as_str());
        }
        exit(1)
    });

    run_jit(&ast);
}
