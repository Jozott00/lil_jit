use crate::run_passing;

#[test]
fn minimal() {
    let (exit, stdout) = run_passing(
        r"
        fn main() {}
        ",
    );

    assert_eq!(exit, 0);
    assert_eq!(stdout, "");
}

#[test]
fn return_stmt() {
    let (exit, stdout) = run_passing(
        r"
        fn main() {return 1}
        ",
    );

    assert_eq!(exit, 1);
    assert_eq!(stdout, "");
}
