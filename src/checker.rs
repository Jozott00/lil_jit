use std::collections::HashMap;
use std::collections::HashSet;
use crate::ast::Program;
use crate::error::LilError;

struct Scope<'a> {
    enclosing: Option<Box<Scope<'a>>>,
    functions: HashMap<&'a str, i64>,
    variables: HashSet<&'a str>,
}

pub fn check_lil(program: &Program) -> Result<(), Vec<LilError>> {
    let mut scope = Scope{
        enclosing: None,
        functions: Default::default(),
        variables: Default::default(),
    };

    // FIXME: Insert builtin functions into scope

    let mut errors: Vec<LilError> = Vec::new();

    // First insert all functions into the global scope
    for func in &program.functions {
        if scope.functions.contains_key(func.name.name) {
            errors.push(LilError{
                header: "Function Name Clash".to_string(),
                location: Some(func.name.location),
                message: "A function with that name was already defined".to_string(),
            });
            continue;
        }
    }

    if !scope.functions.contains_key("main") {
        errors.push(LilError {
            header: "No Main Function".to_string(),
            location: None,
            message: "All lil programs need an entry point".to_string(),
        })
    } else if *scope.functions.get("main").unwrap() != 0 {
        // FIXME: We should add the location here but how it is programmed at the moment this is not possible
        errors.push(LilError {
            header: "Too Many Arguments".to_string(),
            location: None,
            message: "The main function should not have any arguments.".to_string(),
        })
    }

    // Check the functions themself
    for func in &program.functions {
        //check_function(func);
    }

    if !errors.is_empty() {
        return Err(errors)
    }

    Ok(())
}




// 1) Variables, functions exist
// 2) No double arguments in function declarations
// 3) A main function exists