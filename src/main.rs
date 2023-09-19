use crate::parser::{parse_lil_program};

mod parser;
mod ast;
mod location;

fn main() {
    let code = r#"
    fn flo ( x , y ) {
        x
    }

    fn paul(){}
    "#;
    match parse_lil_program(code) {
        Ok(v) => println!("{v:#?}"),
        Err(e) => println!("Error: {e:#?}"),
    }
}
