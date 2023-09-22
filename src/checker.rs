use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::format;

use crate::ast::Program;
use crate::ast::{AstNode, Stmt, StmtKind};
use crate::ast::{Expr, ExprKind, FuncDec};
use crate::error::LilError;
use crate::visitor;
use crate::visitor::{walk_expr, walk_funcdec, walk_stmt, walk_stmt_list, NodeVisitor};

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

    fn has_var(&self, name: &'a str) -> bool {
        if self.variables.contains(name) {
            return true;
        }

        match &self.enclosing {
            Some(s) => s.has_function(name),
            None => false,
        }
    }

    fn add_var(&mut self, name: &'a str) {
        self.variables.insert(name);
    }
}

struct Checker<'a> {
    prog: &'a Program<'a>,
    scope: Box<Scope<'a>>,
    errors: Vec<LilError>,
}

impl<'a> Checker<'a> {
    fn new(prog: &'a Program<'a>) -> Self {
        let scope = Scope::new();
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
            self.scope.add_var(arg.name);
        }

        self.scope.add_function(node.name.name, node.params.len());

        if node.name.name == "main" && node.params.len() != 0 {
            self.errors.push(LilError {
                header: "Too Many Arguments".to_string(),
                location: Some(node.location),
                message: "The main function should not have any arguments.".to_string(),
            })
        }

        walk_funcdec(self, node);
    }

    fn visit_expr(&mut self, node: &'a Expr) {
        match &node.kind {
            ExprKind::IntegerLiteral(_) => {}
            ExprKind::StringLiteral(_) => {}
            ExprKind::FunctionCall(func_data) => {
                let Some(arity) = self.scope.get_function(func_data.function_name.name) else {
                    self.errors.push(LilError {
                        header: "Unknown Function".to_string(),
                        location: Some(func_data.function_name.location()),
                        message: "No function with that name exists".to_string(),
                    });
                    // Even though this is an error we want to check the arguments
                    walk_expr(self, node);
                    return;
                };

                if arity != func_data.arguments.len() {
                    self.errors.push(LilError {
                        header: "Wrong Number of Arguments".to_string(),
                        location: Some(node.location()),
                        message: format!(
                            "The function expected {} arguments but {} were provided",
                            arity,
                            func_data.arguments.len()
                        ),
                    });
                }
            }
            ExprKind::BinaryExpr(_, _, _) => {}
            ExprKind::Identifier(ident) => {
                if !self.scope.has_var(ident.name) {
                    self.errors.push(LilError {
                        header: "Unknown Variable".to_string(),
                        location: Some(node.location()),
                        message: "No variable with that name exists".to_string(),
                    })
                }
            }
            ExprKind::Grouped(_) => {}
        }

        walk_expr(self, node);
    }

    fn visit_stmt(&mut self, node: &'a Stmt) {
        match &node.kind {
            StmtKind::Assignment(is_decl, ident, _) => {
                if *is_decl && self.scope.has_var(ident.name) {
                    self.errors.push(LilError {
                        header: "Variable Name Clash".to_string(),
                        location: Some(ident.location()),
                        message: "A variable with that name already exists".to_string(),
                    })
                } else if !*is_decl && !self.scope.has_var(ident.name) {
                    self.errors.push(LilError {
                        header: "Unknown Variable".to_string(),
                        location: Some(ident.location()),
                        message: "No variable with that name exists.\nTip: Did you forget to declare it with `let`?".to_string(),
                    })
                }

                if *is_decl {
                    self.scope.add_var(ident.name)
                }
            }
            StmtKind::Block(stmts) => {
                self.scope = Box::new(self.scope.push());
                walk_stmt_list(self, stmts);
                self.scope = self.scope.pop().unwrap();
                return;
            }
            StmtKind::If(_, _, _) => {}
            StmtKind::For(_, _, _, _) => {
                // The init part of the loop (eg: `let i = 0`) should be in the scope of the loop and not of the
                // sourounding scope.
                self.scope = Box::new(self.scope.push());
                walk_stmt(self, node);
                self.scope = self.scope.pop().unwrap();
                return;
            }
            StmtKind::ExprStmt(_) => {}
            StmtKind::Return(_) => {}
        }

        walk_stmt(self, node);
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse_lil_program;

    use super::*;

    #[test]
    fn test_minimal_working() {
        let src = "fn main() {}";
        let prog = parse_lil_program(src).unwrap();
        let res = check_lil(&prog);

        assert!(res.is_ok());
    }

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

    #[test]
    fn test_undefined_function() {
        let src = "
        fn hello() {}
        fn main() {
            hallo()
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

    #[test]
    fn test_single_variable() {
        let src = "
        fn main() {
            let a = 3
        }
        ";
        let prog = parse_lil_program(src).unwrap();
        let res = check_lil(&prog);

        assert!(res.is_ok());
    }

    #[test]
    fn test_redeclare_variable() {
        let src = "
        fn main() {
            let a = 3
            let a = 5
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

    #[test]
    fn test_redeclare_argument() {
        let src = "
        fn test(n) {
            let n = 666
            return n
        }

        fn main() {
            test(3)
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

    #[test]
    fn test_recursion() {
        let src = "
        fn test(n) {
            if n < 1: return 0
            return test(n-1) + 1
        }

        fn main() {
            test(3)
        }
        ";
        let prog = parse_lil_program(src).unwrap();
        let res = check_lil(&prog);

        assert!(res.is_ok());
    }
}
