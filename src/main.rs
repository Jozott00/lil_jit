use lil_jit;
use lil_jit::parser::parse_lil_program;

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
