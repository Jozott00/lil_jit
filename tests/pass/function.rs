#[test]
fn function_minimal() {
    let exit = run_passing(
        r"
        fn test() { return 3 }
        fn main() {
            return test()
        }
        ",
    );

    assert_eq!(exit, 3);
}

#[test]
fn function_double_call() {
    let exit = run_passing(
        r"
        fn test() { return 3 }
        fn main() {
            let a = test()
            let b = test()
            return a + b
        }
        ",
    );

    assert_eq!(exit, 6);
}

#[test]
fn function_single_param() {
    let exit = run_passing(
        r"
        fn test(a) { return a + 1 }
        fn main() {
            let a = test(2)
            let b = test(3)
            return a + b
        }
        ",
    );

    assert_eq!(exit, 7);
}

#[test]
fn function_multi_param() {
    let (exit, stdout) = run_passing_captured(
        r"
        fn test(a, b, c, d, e, f) {
            show(a)
            show(b)
            show(c)
            show(d)
            show(e)
            show(f)
            return a + b + c + d + e + f
        }
        fn main() {
            return test(1,2,3,4,5,6)
        }
        ",
    );

    assert_eq!(exit, 21);
    assert_eq!(stdout, "123456")
}

#[test]
fn function_multi_funcs() {
    let (exit, stdout) = run_passing_captured(
        r#"
            fn testa() {
                showtextln("testa")
                testb()
            }
            fn testc() {
                showtextln("testc")
            }
            fn testb() {
                showtextln("testb")
                testc()
            }
            fn main() {
                testa()
                testb()
                testc()
            }
            "#,
    );

    assert_eq!(exit, 0);
    assert_eq!(stdout, "testa\ntestb\ntestc\ntestb\ntestc\ntestc\n")
}
