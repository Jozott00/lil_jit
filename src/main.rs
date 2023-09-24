use std::fs;
use std::process::exit;

use clap::Parser;

use lil_jit;
use lil_jit::checker::check_lil;
use lil_jit::jit::JIT;
use lil_jit::parser::parse_lil_program;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    filename: String,

    #[arg(long)]
    dump_ast: bool,

    #[arg(long)]
    dump_ir: bool,

    #[arg(long, short)]
    verbose: bool,
}

fn main() {
    let cli = Cli::parse();

    let code = fs::read_to_string(&cli.filename)
        .expect(&*format!("Unable to read file: {}", &cli.filename));

    let ast = parse_lil_program(code.as_str()).unwrap_or_else(|error| {
        error.print(&code);
        exit(1);
    });

    if cli.dump_ast {
        println!("AST DUMP:");
        println!("{:#?}", ast);
        exit(0)
    }

    check_lil(&ast).unwrap_or_else(|errors| {
        for error in errors {
            error.print(code.as_str());
        }
        exit(1)
    });

    let mut jit = JIT::new(&ast, cli.verbose, cli.dump_ir);
    jit.run();
}
