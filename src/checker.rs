use crate::ast::AstNode;
use crate::ast::FuncDec;
use crate::ast::Program;
use crate::error::LilError;
use crate::visitor;
use crate::visitor::NodeVisitor;
use std::collections::HashMap;
use std::collections::HashSet;

pub fn check_lil(program: &Program) -> Result<(), Vec<LilError>> {
    let checker = Checker::new(program);
    let errors = checker.check();
    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(())
}

struct Scope<'a> {
    enclosing: Option<Box<Scope<'a>>>,
    functions: HashMap<&'a str, usize>,
    variables: HashSet<&'a str>,
}

struct Checker<'a> {
    prog: &'a Program<'a>,
    scope: Scope<'a>,
    errors: Vec<LilError>,
}

impl<'a> Checker<'a> {
    fn new(prog: &'a Program<'a>) -> Checker<'a> {
        let mut scope = Scope {
            enclosing: None,
            functions: Default::default(),
            variables: Default::default(),
        };
        // FIXME: Insert builtin functions into scope

        Checker {
            prog,
            scope,
            errors: Vec::new(),
        }
    }

    fn check(mut self) -> Vec<LilError> {
        self.visit_prog(self.prog);
        self.errors
    }
}

impl<'a> NodeVisitor<'a> for Checker<'a> {
    fn visit_prog(&mut self, node: &'a Program) {
        // Visit all functions
        visitor::walk_prog(self, node);

        if !self.scope.functions.contains_key("main") {
            self.errors.push(LilError {
                header: "No Main Function".to_string(),
                location: None,
                message: "All lil programs need an entry point".to_string(),
            })
        }
    }

    fn visit_funcdec(&mut self, node: &'a FuncDec) {
        if self.scope.functions.contains_key(node.name.name) {
            self.errors.push(LilError {
                header: "Function Name Clash".to_string(),
                location: Some(node.name.location),
                message: "A function with that name was already defined".to_string(),
            });
            return;
        }

        self.scope
            .functions
            .insert(node.name.name, node.params.len());

        if node.name.name == "main" && node.params.len() != 0 {
            self.errors.push(LilError {
                header: "Too Many Arguments".to_string(),
                location: Some(node.location),
                message: "The main function should not have any arguments.".to_string(),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_lil_program;

    #[test]
    fn test_no_main() {
        let src = "
        fn test() {}
        fn test_zwei() {}
        ";
        let prog = parse_lil_program(src).unwrap();
        let res = check_lil(&prog);

        assert!(res.is_err());
        let Err(errs) = res else {
            assert!(false, "Shouldnt be the case");
            return;
        };

        assert_eq!(errs.len(), 1);
    }

    #[test]
    fn test_dup_fn_name() {
        let src = "
        fn test() {}
        fn test() {}
        fn main() {}
        ";
        let prog = parse_lil_program(src).unwrap();
        let res = check_lil(&prog);

        assert!(res.is_err());
        let Err(errs) = res else {
            assert!(false, "Shouldnt be the case");
            return;
        };

        assert_eq!(errs.len(), 1);
    }

    #[test]
    fn test_invalid_arg_num() {
        let src = "
        fn test(a, b, c) {}
        fn main() {
            test(1, 2)
        }
        ";
        let prog = parse_lil_program(src).unwrap();
        let res = check_lil(&prog);

        assert!(res.is_err());
        let Err(errs) = res else {
            assert!(false, "Shouldnt be the case");
            return;
        };

        assert_eq!(errs.len(), 1);
    }
}
