use crate::parser::{parse_lil_program};

mod parser;
mod ast;
mod location;
mod error;
mod chekcer;

fn main() {
    // let code = r#"
    // fn flo ( x , y ) {
    //     y
    //     x
    // }
    //
    // fn paul(){}
    // "#;
    let code = "xxxxxxx";
    match parse_lil_program(code) {
        Ok(v) => println!("{v:#?}"),
        Err(e) => println!("Error: {e:#?}"),
    }
}
