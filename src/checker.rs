use std::collections::HashMap;
use std::collections::HashSet;

use crate::ast::AstNode;
use crate::ast::FuncDec;
use crate::ast::Program;
use crate::error::LilError;
use crate::visitor;
use crate::visitor::{walk_funcdec, NodeVisitor};

pub fn check_lil(program: &Program) -> Result<(), Vec<LilError>> {
    let mut checker = Checker::new(program);
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

impl<'a> Default for Scope<'a> {
    fn default() -> Self {
        Scope {
            enclosing: None,
            functions: Default::default(),
            variables: Default::default(),
        }
    }
}

impl<'a> Scope<'a> {
    fn new() -> Self {
        Self::default()
    }

    fn push(&mut self) -> Self {
        let mut new_scope = Scope::new();
        new_scope.enclosing = Some(Box::new(std::mem::take(self)));
        new_scope
    }

    fn pop(&mut self) -> Option<Box<Self>> {
        std::mem::take(&mut self.enclosing)
    }

    fn has_function(&self, name: &'a str) -> bool {
        if self.functions.contains_key(name) {
            return true;
        }

        match &self.enclosing {
            Some(s) => s.has_function(name),
            None => false,
        }
    }

    fn get_function(&self, name: &str) -> Option<usize> {
        if self.functions.contains_key(name) {
            return Some(*self.functions.get(name).unwrap());
        }

        match &self.enclosing {
            Some(s) => s.get_function(name),
            None => None,
        }
    }

    fn add_function(&mut self, name: &'a str, arity: usize) {
        self.functions.insert(name, arity);
    }
}

struct Checker<'a> {
    prog: &'a Program<'a>,
    scope: Box<Scope<'a>>,
    errors: Vec<LilError>,
}

impl<'a> Checker<'a> {
    fn new(prog: &'a Program<'a>) -> Self {
        let mut scope = Scope::new();
        // FIXME: Insert builtin functions into scope

        Checker {
            prog,
            scope: Box::new(scope),
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

        if !self.scope.has_function("main") {
            self.errors.push(LilError {
                header: "No Main Function".to_string(),
                location: None,
                message: "All lil programs need an entry point".to_string(),
            })
        }
    }

    fn visit_funcdec(&mut self, node: &'a FuncDec) {
        if self.scope.has_function(node.name.name) {
            self.errors.push(LilError {
                header: "Function Name Clash".to_string(),
                location: Some(node.name.location),
                message: "A function with that name was already defined".to_string(),
            });

            // Even though this is an error we want to check the body of the function.
            walk_funcdec(self, node);
            return;
        }

        let mut args = HashSet::new();
        for arg in &node.params {
            if args.contains(arg.name) {
                self.errors.push(LilError {
                    header: "Argument Name Clash".to_string(),
                    location: Some(arg.location),
                    message: "Arguments to a function must be unique".to_string(),
                });
            }
            args.insert(arg.name);
        }

        self.scope.add_function(node.name.name, node.params.len());

        if node.name.name == "main" && node.params.len() != 0 {
            self.errors.push(LilError {
                header: "Too Many Arguments".to_string(),
                location: Some(node.location),
                message: "The main function should not have any arguments.".to_string(),
            })
        }

        self.scope = Box::new(self.scope.push());
        walk_funcdec(self, node);
        self.scope = self.scope.pop().unwrap();
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse_lil_program;

    use super::*;

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
    fn test_dup_arg_name() {
        let src = "
        fn test(a, b, a) {}
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
