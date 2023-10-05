#[test]
fn if_min_not() {
    let (exit, out) = run_body_captured(
        r"
                if 0: return 1
                return 2
                ",
    );
    assert_eq!(exit, 2);
}

#[test]
fn if_min_neg() {
    let (exit, out) = run_body_captured(
        r"
                if (0 - 1): return 1
                return 2
                ",
    );
    assert_eq!(exit, 1);
}

#[test]
fn if_var() {
    let (exit, out) = run_body_captured(
        r#"
            let condition = 1 + 1
            if condition {
                showtextln("if")
            } else {
                showtextln("else")
            }
            
            showtextln("end")
        "#,
    );
    assert_eq!(out, "if\nend\n")
}

#[test]
fn else_var() {
    let (exit, out) = run_body_captured(
        r#"
            let condition = 1 + (0 - 1)
            if condition {
                showtextln("if")
            } else {
                showtextln("else")
            }
            
            showtextln("end")
        "#,
    );
    assert_eq!(out, "else\nend\n")
}

#[test]
fn if_func() {
    let (exit, out) = run_passing_captured(
        r#"
        fn test(condition) {
            if condition {
                showtextln("if")
            } else {
                showtextln("else")
            }
            showtextln("end")
        }
        
        fn main() {
            test(0-1)
        }   
        "#,
    );
    assert_eq!(out, "if\nend\n")
}

#[test]
fn else_func() {
    let (exit, out) = run_passing_captured(
        r#"
        fn test(condition) {
            if condition {
                showtextln("if")
            } else {
                showtextln("else")
            }
            showtextln("end")
        }
        
        fn main() {
            test(0)
        }   
        "#,
    );
    assert_eq!(out, "else\nend\n")
}
