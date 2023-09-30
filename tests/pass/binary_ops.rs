fn run_bin_op(code: &str) -> (i32, String) {
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

#[test]
fn add() {
    let (exit, stdout) = run_bin_op("2 + 3");
    assert_eq!(exit, 5);
    assert_eq!(stdout, "");
}

#[test]
fn add_overflow() {
    use std::i32;
    let max_value = i32::MAX;
    let overflow_value = max_value.wrapping_add(1); // This will wrap around on overflow

    let test_str = format!("{} + 1", max_value); // This should overflow
    let (exit, stdout) = run_bin_op(&test_str);

    assert_eq!(exit, overflow_value);
    assert_eq!(stdout, ""); // Replace with the actual stdout message for overflow
}

#[test]
fn sub() {
    let (exit, stdout) = run_bin_op("2 - 3");
    assert_eq!(exit, -1);
    assert_eq!(stdout, "");
}

#[test]
fn sub_underflow() {
    use std::i32;
    let max_value = i32::MAX; // Smallest representable value + 1

    let test_str = format!("(0 - ({} + 1)) - 1", max_value); // This should underflow
    let (exit, stdout) = run_bin_op(&test_str);

    assert_eq!(exit, i32::MAX); // Replace with the actual underflow value
    assert_eq!(stdout, ""); // Replace with the actual stdout message for underflow
}

#[test]
fn multi() {
    let (exit, stdout) = run_bin_op("2 * 3");
    assert_eq!(exit, 6);
    assert_eq!(stdout, "");
}

#[test]
fn multi_overflow() {
    use std::i32;
    let max_value = i32::MAX;
    let overflow_value = max_value.wrapping_add(1); // This will overflow if computed in a 32-bit integer

    let test_str = format!("{} * 2", max_value / 2 + 1); // This should overflow
    let (exit, stdout) = run_bin_op(&test_str);

    assert_eq!(exit, overflow_value);
    assert_eq!(stdout, ""); // Replace with the actual stdout message for overflow
}

#[test]
fn divide() {
    let (exit, stdout) = run_bin_op("6 / 3");
    assert_eq!(exit, 2);
    assert_eq!(stdout, "");

    let (exit, stdout) = run_bin_op("5 / 2");
    assert_eq!(exit, 2); // Round to zero
    assert_eq!(stdout, "");

    let (exit, stdout) = run_bin_op("5 / 0");
    assert_eq!(exit, 0); // / 0 equals 0
    assert_eq!(stdout, "");
}

#[test]
fn equals() {
    let (exit, stdout) = run_bin_op("3 == 3");
    assert_eq!(exit, 1); // true
    assert_eq!(stdout, "");

    let (exit, stdout) = run_bin_op("3 == 4");
    assert_eq!(exit, 0); // false
    assert_eq!(stdout, "");
}

#[test]
fn not_equals() {
    let (exit, stdout) = run_bin_op("3 != 4");
    assert_eq!(exit, 1); // true
    assert_eq!(stdout, "");

    let (exit, stdout) = run_bin_op("3 != 3");
    assert_eq!(exit, 0); // false
    assert_eq!(stdout, "");
}

#[test]
fn less() {
    let (exit, stdout) = run_bin_op("2 < 3");
    assert_eq!(exit, 1);
    assert_eq!(stdout, "");
    let (exit, stdout) = run_bin_op("3 < 3");
    assert_eq!(exit, 0);
    assert_eq!(stdout, "");
    let (exit, stdout) = run_bin_op("3 < 2");
    assert_eq!(exit, 0);
    assert_eq!(stdout, "");
}

#[test]
fn greater() {
    let (exit, stdout) = run_bin_op("2 > 3");
    assert_eq!(exit, 0);
    assert_eq!(stdout, "");
    let (exit, stdout) = run_bin_op("3 > 3");
    assert_eq!(exit, 0);
    assert_eq!(stdout, "");
    let (exit, stdout) = run_bin_op("3 > 2");
    assert_eq!(exit, 1);
    assert_eq!(stdout, "");
}

#[test]
fn less_eq() {
    let (exit, stdout) = run_bin_op("2 <= 3");
    assert_eq!(exit, 1);
    assert_eq!(stdout, "");
    let (exit, stdout) = run_bin_op("3 <= 3");
    assert_eq!(exit, 1);
    assert_eq!(stdout, "");
    let (exit, stdout) = run_bin_op("3 <= 2");
    assert_eq!(exit, 0);
    assert_eq!(stdout, "");
}

#[test]
fn greater_eq() {
    let (exit, stdout) = run_bin_op("2 >= 3");
    assert_eq!(exit, 0);
    assert_eq!(stdout, "");
    let (exit, stdout) = run_bin_op("3 >= 3");
    assert_eq!(exit, 1);
    assert_eq!(stdout, "");
    let (exit, stdout) = run_bin_op("3 >= 2");
    assert_eq!(exit, 1);
    assert_eq!(stdout, "");
}
