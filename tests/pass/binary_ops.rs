fn run_bin_op(code: &str) -> i32 {
    run_passing(
        format!(
            "
        fn main() {{
            return {code}
        }}
        "
        )
        .as_str(),
    )
}

/// Returns result in order of: (all_const, rhs_const, lhs_const, no_const)
fn run_bin_op_adv(code: &str) -> (i32, i32, i32, i32) {
    let parts: Vec<&str> = code.split_whitespace().collect();
    let (lhs, op, rhs) = (parts[0], parts[1], parts[2]);

    let code_input = format!(
        r"
                fn rhs_const(lhs) {{
                    showln(lhs {op} {rhs})
                }}
                
                fn lhs_const(rhs) {{
                    showln({lhs} {op} rhs)
                }}
                
                fn no_const(lhs, rhs) {{
                    showln(lhs {op} rhs)
                }}
                
                fn main() {{
                    showln({lhs} {op} {rhs})
                    rhs_const({lhs})
                    lhs_const({rhs})
                    no_const({lhs}, {rhs})
                }}
                
            "
    );

    let (_, out) = run_passing_captured(&code_input);

    println!("{out}");

    let results = out
        .splitn(4, '\n')
        .map(|e| e.trim().parse::<i32>().unwrap())
        .collect::<Vec<i32>>();
    return (results[0], results[1], results[2], results[3]);
}

fn all(n: i32) -> (i32, i32, i32, i32) {
    (n, n, n, n)
}

#[test]
fn add() {
    let exit = run_bin_op_adv("2 + 3");
    assert_eq!(exit, (5, 5, 5, 5));
}

#[test]
fn add_overflow() {
    use std::i32;
    let max_value = i32::MAX;
    let overflow_value = max_value.wrapping_add(1); // This will wrap around on overflow

    let test_str = format!("{} + 1", max_value); // This should overflow
    let exit = run_bin_op(&test_str);

    assert_eq!(exit, overflow_value);
    // Replace with the actual stdout message for overflow
}

#[test]
fn sub() {
    let exit = run_bin_op_adv("2 - 3");
    assert_eq!(exit, (-1, -1, -1, -1));
}

#[test]
fn sub_underflow() {
    use std::i32;
    let max_value = i32::MAX; // Smallest representable value + 1

    let test_str = format!("(0 - ({} + 1)) - 1", max_value); // This should underflow
    let exit = run_bin_op(&test_str);

    assert_eq!(exit, i32::MAX); // Replace with the actual underflow value
                                // Replace with the actual stdout message for underflow
}

#[test]
fn multi() {
    let res = run_bin_op_adv("2 * 3");
    assert_eq!(res, (6, 6, 6, 6))
}

#[test]
fn multi_overflow() {
    use std::i32;
    let max_value = i32::MAX;
    let overflow_value = max_value.wrapping_add(1); // This will overflow if computed in a 32-bit integer

    let test_str = format!("{} * 2", max_value / 2 + 1); // This should overflow
    let exit = run_bin_op_adv(&test_str);

    assert_eq!(
        exit,
        (
            overflow_value,
            overflow_value,
            overflow_value,
            overflow_value
        )
    );
    // Replace with the actual stdout message for overflow
}

#[test]
fn divide() {
    let exit = run_bin_op_adv("6 / 3");
    assert_eq!(exit, (2, 2, 2, 2));

    let exit = run_bin_op_adv("5 / 2");
    assert_eq!(exit, (2, 2, 2, 2)); // Round to zero

    let exit = run_bin_op_adv("5 / 0");
    assert_eq!(exit, (0, 0, 0, 0)); // / 0 equals 0
}

#[test]
fn modulo() {
    let exit = run_bin_op_adv("6 % 3");
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv("5 % 2");
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv("5 % 0");
    assert_eq!(exit, (5, 5, 5, 5)); // to zero

    let exit = run_bin_op_adv("2 % 5");
    assert_eq!(exit, all(2)); // Round to zero
}

#[test]
fn equals() {
    let exit = run_bin_op_adv("3 == 3");
    assert_eq!(exit, (1, 1, 1, 1)); // true

    let exit = run_bin_op_adv("3 == 4");
    assert_eq!(exit, (0, 0, 0, 0)); // false
}

#[test]
fn not_equals() {
    let exit = run_bin_op_adv("3 != 4");
    assert_eq!(exit, (1, 1, 1, 1)); // true

    let exit = run_bin_op_adv("3 != 3");
    assert_eq!(exit, (0, 0, 0, 0)); // false
}

#[test]
fn less() {
    let exit = run_bin_op_adv("2 < 3");
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv("3 < 3");
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv("3 < 2");
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv(format!("{} < {}", i32::MAX, i32::MAX).as_str());
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv(format!("{} < {}", i32::MAX - 1, i32::MAX).as_str());
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv(format!("{} < {}", i32::MAX, i32::MAX - 1).as_str());
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv(format!("(0-{}) < (0-{})", i32::MAX, i32::MAX).as_str());
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv(format!("(0-{}) < (0-{})", i32::MAX, i32::MAX - 1).as_str());
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv(format!("(0-{}) < (0-{})", i32::MAX - 1, i32::MAX).as_str());
    assert_eq!(exit, (0, 0, 0, 0));
}

#[test]
fn greater() {
    let exit = run_bin_op_adv("2 > 3");
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv("3 > 3");
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv("3 > 2");
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv(format!("{} > {}", i32::MAX, i32::MAX).as_str());
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv(format!("{} > {}", i32::MAX - 1, i32::MAX).as_str());
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv(format!("{} > {}", i32::MAX, i32::MAX - 1).as_str());
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv(format!("(0-{}) > (0-{})", i32::MAX, i32::MAX).as_str());
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv(format!("(0-{}) > (0-{})", i32::MAX, i32::MAX - 1).as_str());
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv(format!("(0-{}) > (0-{})", i32::MAX - 1, i32::MAX).as_str());
    assert_eq!(exit, (1, 1, 1, 1));
}

#[test]
fn less_eq() {
    let exit = run_bin_op_adv("2 <= 3");
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv("3 <= 3");
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv("3 <= 2");
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv(format!("{} <= {}", i32::MAX, i32::MAX).as_str());
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv(format!("{} <= {}", i32::MAX - 1, i32::MAX).as_str());
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv(format!("{} <= {}", i32::MAX, i32::MAX - 1).as_str());
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv(format!("(0-{}) <= (0-{})", i32::MAX, i32::MAX).as_str());
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv(format!("(0-{}) <= (0-{})", i32::MAX, i32::MAX - 1).as_str());
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv(format!("(0-{}) <= (0-{})", i32::MAX - 1, i32::MAX).as_str());
    assert_eq!(exit, (0, 0, 0, 0));
}

#[test]
fn greater_eq() {
    let exit = run_bin_op_adv("2 >= 3");
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv("3 >= 3");
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv("3 >= 2");
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv(format!("{} >= {}", i32::MAX, i32::MAX).as_str());
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv(format!("{} >= {}", i32::MAX - 1, i32::MAX).as_str());
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv(format!("{} >= {}", i32::MAX, i32::MAX - 1).as_str());
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv(format!("(0-{}) >= (0-{})", i32::MAX, i32::MAX).as_str());
    assert_eq!(exit, (1, 1, 1, 1));

    let exit = run_bin_op_adv(format!("(0-{}) >= (0-{})", i32::MAX, i32::MAX - 1).as_str());
    assert_eq!(exit, (0, 0, 0, 0));

    let exit = run_bin_op_adv(format!("(0-{}) >= (0-{})", i32::MAX - 1, i32::MAX).as_str());
    assert_eq!(exit, (1, 1, 1, 1));
}

#[test]
fn logical_and() {
    let exit = run_bin_op_adv("1 and 2");
    assert_eq!(exit, (1, 1, 1, 1,));

    let exit = run_bin_op_adv("2 and 1");
    assert_eq!(exit, (1, 1, 1, 1,));

    let exit = run_bin_op_adv("0 and 1");
    assert_eq!(exit, (0, 0, 0, 0));
}

#[test]
fn logical_or() {
    let exit = run_bin_op_adv("1 or 2");
    assert_eq!(exit, (1, 1, 1, 1,));

    let exit = run_bin_op_adv("0 or 1");
    assert_eq!(exit, (1, 1, 1, 1,));

    let exit = run_bin_op_adv("0 or 0");
    assert_eq!(exit, (0, 0, 0, 0));
}
