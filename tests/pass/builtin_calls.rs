#[test]
fn builtin_cool() {
    let (exit, stdout) = run_body_captured(
        r"
            let a = 3
            cool()
            a = 4
            return a
        ",
    );
    assert_eq!(exit, 4);
    assert_eq!(stdout, "Cool\n");
}

#[test]
fn builtin_show() {
    let (exit, stdout) = run_body_captured(
        r"
            let a = 3
            show(a + 1)
        ",
    );

    assert_eq!(exit, 0);
    assert_eq!(stdout, "4");
}

#[test]
fn builtin_showln() {
    let (exit, stdout) = run_body_captured(
        r"
            let a = 3
            showln(a + 1)
        ",
    );

    assert_eq!(exit, 0);
    assert_eq!(stdout, "4\n");
}

#[test]
fn builtin_showtext() {
    let (exit, stdout) = run_body_captured(
        r#"
            showtext("Hello World")
        "#,
    );

    assert_eq!(exit, 0);
    assert_eq!(stdout, "Hello World");
}

#[test]
fn builtin_showtextln() {
    let (exit, stdout) = run_body_captured(
        r#"
            showtextln("Hello World")
        "#,
    );

    assert_eq!(exit, 0);
    assert_eq!(stdout, "Hello World\n");
}

#[test]
fn builtin_show_mix() {
    let (exit, stdout) = run_body_captured(
        r#"
            let a = 3
            let b = a + 1
            showtext("Value a: ")
            showln(a)
            show(b)
            showtextln(" ... is value b")
        "#,
    );

    assert_eq!(exit, 0);
    assert_eq!(stdout, "Value a: 3\n4 ... is value b\n");
}
