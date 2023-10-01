#[test]
fn loop_minimal() {
    let exit = run_body(
        r"
                let a = 99
                for let i = 0; 0; i=i+1: a = i
                return a",
    );
    assert_eq!(exit, 99);
}

#[test]
fn loop_sum() {
    let exit = run_body(
        r"
                let a = 0
                for let i = 0; i < 100; i=i+1: a = a + 1
                return a",
    );
    assert_eq!(exit, 100);
}

#[test]
fn loop_return() {
    let exit = run_body(
        r"
                let a = 5
                for let i = 1; i < 100; i=i+1: return i
                return a",
    );
    assert_eq!(exit, 1);
}
