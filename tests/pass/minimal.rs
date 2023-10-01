#[test]
fn minimal() {
    let exit = run_passing(
        r"
        fn main() {}
        ",
    );

    assert_eq!(exit, 0);
}

#[test]
fn return_stmt() {
    let exit = run_passing(
        r"
        fn main() {return 1}
        ",
    );

    assert_eq!(exit, 1);
}
