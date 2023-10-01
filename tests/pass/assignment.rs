use crate::run_body;

#[test]
fn assignment_single() {
    let exit = run_body(
        r"let a = 3
                return a",
    );
    assert_eq!(exit, 3);
}

#[test]
fn assignment_reassignment() {
    let exit = run_body(
        r"let a = 3
                a = 10
                return a",
    );
    assert_eq!(exit, 10);
}

#[test]
fn assignment_two_vars() {
    let exit = run_body(
        r"let a = 3
                let b = a
                return b",
    );
    assert_eq!(exit, 3);
}

#[test]
fn assignment_two_vars_2() {
    let exit = run_body(
        r"let a = 3
                let b = 10
                b = a
                return b",
    );
    assert_eq!(exit, 3);
}

#[test]
fn assignment_35_vars() {
    let exit = run_body(
        r"    
    let one = 1
let two = 1
let three = 1
let four = 1
let five = 1
let six = 1
let seven = 1
let eight = 1
let nine = 1
let ten = 1
let eleven = 1
let twelve = 1
let thirteen = 1
let fourteen = 1
let fifteen = 1
let sixteen = 1
let seventeen = 1
let eighteen = 1
let nineteen = 1
let twenty = 1
let twentyone = 1
let twentytwo = 1
let twentythree = 1
let twentyfour = 1
let twentyfive = 1
let twentysix = 1
let twentyseven = 1
let twentyeight = 1
let twentynine = 1
let thirty = 1
let thirtyone = 1
let thirtytwo = 1
let thirtythree = 1
let thirtyfour = 1
let thirtyfive = 1

return one + two + three + four + five + six + seven + eight + nine + ten + eleven + twelve + thirteen + fourteen + fifteen + sixteen + seventeen + eighteen + nineteen + twenty + twentyone + twentytwo + twentythree + twentyfour + twentyfive + twentysix + twentyseven + twentyeight + twentynine + thirty + thirtyone + thirtytwo + thirtythree + thirtyfour + thirtyfive
",
    );
    assert_eq!(exit, 35);
}
