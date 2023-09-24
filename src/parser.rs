use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case, take_until, take_while1};
use nom::character::complete::{char, digit1, multispace0, multispace1};
use nom::combinator::{complete, eof, opt, value};
use nom::error::Error;
use nom::multi::{many0, many1, separated_list0};
use nom::sequence::tuple;
use nom::IResult;
use nom_locate::LocatedSpan;

use crate::ast::StmtKind::{Assignment, ExprStmt, For, If, Return};
use crate::ast::{
    AstNode, BinaryOp, Expr, ExprKind, FuncDec, FunctionCallData, Identifier, Program, Stmt,
    StmtKind,
};
use crate::error::LilError;
use crate::location::Location;

type Span<'a> = LocatedSpan<&'a str>;

pub fn parse_lil_program(source: &str) -> Result<Program, LilError> {
    let input = Span::new(source);
    match parse_program(input) {
        Ok((_, program)) => Ok(program),
        Err(e) => match &e {
            nom::Err::Error(s) => Err(LilError {
                header: "Parsing Error".to_string(),
                location: Some(Location::from_span(&s.input)),
                message: e.to_string(),
            }),
            _ => Err(LilError {
                header: "Parsing Error".to_string(),
                location: None,
                message: e.to_string(),
            }),
        },
    }
}

fn parse_program(input: Span) -> IResult<Span, Program, Error<Span>> {
    let (input, _) = multispace0(input)?;
    let (input, funcs) = complete(many1(parse_function_declaration))(input)?;
    eof(input)?;

    let location = funcs.iter().fold(Location::new(0, 0, 1), |acc, func| {
        acc.merge(&func.location())
    });

    Ok((
        input,
        Program {
            functions: funcs,
            location,
        },
    ))
}

fn parse_function_declaration(input: Span) -> IResult<Span, FuncDec> {
    let (input, (s, _, ident, _, _, _)) = tuple((
        tag("fn"),
        multispace0,
        parse_identifier,
        multispace0,
        tag("("),
        multispace0,
    ))(input)?;
    let mut location = Location::from_span(&s);

    let (input, args) = separated_list0(
        tuple((multispace0, tag(","), multispace0)),
        parse_identifier,
    )(input)?;

    let (input, (_, _, _, block)) =
        tuple((multispace0, tag(")"), multispace0, parse_block))(input)?;

    location = location.merge(&block.location());

    Ok((
        input,
        FuncDec {
            name: ident,
            params: args,
            body: block,
            location,
        },
    ))
}

// So this function should not eat the trailing newline, because for likes to not need a
fn parse_stmt(input: Span) -> IResult<Span, Stmt> {
    // newline.
    let (input, stmt) = (alt((
        parse_assignment,
        parse_if,
        parse_for,
        parse_return,
        parse_expr_stmt,
    )))(input)?;

    Ok((input, stmt))
}

fn parse_assignment(input: Span) -> IResult<Span, Stmt> {
    let (input, decl) = opt(tuple((tag("let"), multispace1)))(input)?;
    let (input, (name, _, _, _, expr)) = tuple((
        parse_identifier,
        multispace0,
        tag("="),
        multispace0,
        parse_expr,
    ))(input)?;

    let start = match decl {
        Some((kw, _)) => Location::from_span(&kw),
        None => name.location(),
    };

    let is_declaration = decl.is_some();
    let location = start.merge(&expr.location());

    let stmt = Stmt {
        kind: Assignment(is_declaration, name, expr),
        location,
    };

    Ok((input, stmt))
}

fn parse_if(input: Span) -> IResult<Span, Stmt> {
    let (input, (start, _, cond, _, block)) =
        tuple((tag("if"), multispace0, parse_expr, multispace0, parse_block))(input)?;

    let (input, other) = opt(tuple((multispace0, tag("else"), multispace0, parse_block)))(input)?;

    let other = other.map(|(_, _, _, else_block)| Box::new(else_block));

    let mut location = Location::from_span(&start).merge(&block.location());
    if let Some(other) = &other {
        location.merge(&other.location());
    }

    let stmt = Stmt {
        kind: If(cond, Box::new(block), other),
        location,
    };

    Ok((input, stmt))
}

fn parse_for(input: Span) -> IResult<Span, Stmt> {
    let (input, (start, _, pre, _, _, _, cond, _, _, _, post, _, block)) = tuple((
        tag("for"),
        multispace0,
        opt(parse_stmt),
        multispace0,
        tag(";"),
        multispace0,
        parse_expr,
        multispace0,
        tag(";"),
        multispace0,
        opt(parse_stmt),
        multispace0,
        parse_block,
    ))(input)?;

    let pre = pre.map(Box::new);
    let post = post.map(Box::new);
    let location = Location::from_span(&start).merge(&block.location());

    let stmt = Stmt {
        kind: For(pre, cond, post, Box::new(block)),
        location,
    };

    Ok((input, stmt))
}

fn parse_return(input: Span) -> IResult<Span, Stmt> {
    let (input, (first, _, expr)) = tuple((tag("return"), multispace0, parse_expr))(input)?;
    let location = Location::from_span(&first).merge(&expr.location());

    Ok((
        input,
        Stmt {
            kind: Return(expr),
            location,
        },
    ))
}

fn parse_expr_stmt(input: Span) -> IResult<Span, Stmt> {
    let (input, expr) = parse_expr(input)?;
    let location = expr.location();
    Ok((
        input,
        Stmt {
            kind: ExprStmt(expr),
            location,
        },
    ))
}

fn parse_expr(input: Span) -> IResult<Span, Expr> {
    let (input, _) = multispace0(input)?;
    let (input, mut expr) = (alt((
        parse_grouped_expr,
        parse_showtext_call,
        parse_func_call,
        parse_identifier_expr,
        parse_int_lit,
    )))(input)?;

    let (input, _) = multispace0(input)?;
    let (input, opt_bin_expr) = opt(parse_binary_expr(expr.clone()))(input)?;

    if let Some(bin_expr) = opt_bin_expr {
        expr = bin_expr
    }

    let (input, _) = multispace0(input)?;
    Ok((input, expr))
}

fn parse_block(input: Span) -> IResult<Span, Stmt> {
    // Single statement blocks
    // Example: if false: return 666
    if input.starts_with(":") {
        let (input, (_, _, stmt)) = tuple((tag(":"), multispace0, parse_stmt))(input)?;
        return Ok((input, stmt));
    }

    // Multi statement blocks
    // Example: if false { return 666}
    let (input, (first, _)) = tuple((tag("{"), multispace0))(input)?;
    let (input, stmts) = many0(parse_stmt)(input)?;
    let (input, (last, _)) = tuple((tag("}"), multispace0))(input)?;
    let location = Location::from_span(&first).merge(&Location::from_span(&last));

    Ok((
        input,
        Stmt {
            kind: StmtKind::Block(stmts),
            location: location,
        },
    ))
}

fn parse_showtext_call(input: Span) -> IResult<Span, Expr> {
    let (input, ident) = parse_identifier(input)?;
    let (input, (_, _, message, _, last)) = tuple((
        tag("("),
        multispace0,
        parse_string_lit,
        multispace0,
        tag(")"),
    ))(input)?;

    let location = ident.location().merge(&last.into());

    let expr = Expr {
        kind: ExprKind::FunctionCall(FunctionCallData {
            function_name: ident,
            arguments: vec![message],
            location,
        }),
        location,
    };

    Ok((input, expr))
}

fn parse_func_call(input: Span) -> IResult<Span, Expr> {
    let (input, ident) = parse_identifier(input)?;
    let (input, (_, args, end)) = tuple((tag("("), parse_args, tag(")")))(input)?;

    let location = ident.location().merge(&Location::from_span(&end));

    let expr = Expr {
        kind: ExprKind::FunctionCall(FunctionCallData {
            function_name: ident,
            arguments: args,
            location,
        }),
        location,
    };

    Ok((input, expr))
}

fn parse_binary_expr<'a>(first: Expr<'a>) -> impl Fn(Span<'a>) -> IResult<Span<'a>, Expr<'a>> {
    move |input: Span<'a>| {
        let (input, (op, e2)) = tuple((parse_binary_op, parse_expr))(input)?;
        let location = first.location().merge(&e2.location());

        let expr = Expr {
            kind: ExprKind::BinaryExpr(Box::new(first.clone()), op, Box::new(e2)),
            location,
        };

        Ok((input, expr))
    }
}

fn parse_binary_op(input: Span) -> IResult<Span, BinaryOp> {
    let (input, binary_op) = alt((
        value(BinaryOp::Add, tag_no_case("+")),
        value(BinaryOp::Minus, tag_no_case("-")),
        value(BinaryOp::Multi, tag_no_case("*")),
        value(BinaryOp::Divide, tag_no_case("/")),
        value(BinaryOp::Equals, tag_no_case("==")),
        value(BinaryOp::NotEqual, tag_no_case("!=")),
        value(BinaryOp::Greater, tag_no_case(">")),
        value(BinaryOp::GreaterEqual, tag_no_case(">=")),
        value(BinaryOp::Less, tag_no_case("<")),
        value(BinaryOp::LessEqual, tag_no_case("<=")),
    ))(input)?;

    Ok((input, binary_op))
}

fn parse_args(input: Span) -> IResult<Span, Vec<Expr>> {
    separated_list0(char(','), parse_expr)(input)
}

fn parse_grouped_expr(input: Span) -> IResult<Span, Expr> {
    let (input, (start, expr, end)) = tuple((tag("("), parse_expr, tag(")")))(input)?;
    let location = Location::from_span(&start).merge(&end.into());

    let expr = Expr {
        kind: ExprKind::Grouped(Box::new(expr)),
        location,
    };

    Ok((input, expr))
}

fn parse_int_lit(input: Span) -> IResult<Span, Expr> {
    let (input, num_span) = digit1(input)?;
    let num = num_span.fragment().parse::<i32>().unwrap();

    let expr = Expr {
        kind: ExprKind::IntegerLiteral(num),
        location: Location::from_span(&num_span),
    };
    Ok((input, expr))
}

fn parse_string_lit(input: Span) -> IResult<Span, Expr> {
    let (input, (start, str, end)) = tuple((tag("\""), take_until("\""), tag("\"")))(input)?;

    let expr = Expr {
        kind: ExprKind::StringLiteral(str.fragment()),
        location: Location::from_span(&start).merge(&Location::from_span(&end)),
    };
    Ok((input, expr))
}

fn parse_identifier_expr(input: Span) -> IResult<Span, Expr> {
    let (input, ident) = parse_identifier(input)?;
    let location = ident.location();

    let expr = Expr {
        kind: ExprKind::Identifier(ident),
        location,
    };
    Ok((input, expr))
}

fn parse_identifier(input: Span) -> IResult<Span, Identifier> {
    let (input, name) = take_while1(|c: char| c.is_alphabetic() || c == '_')(input)?;
    Ok((
        input,
        Identifier {
            name: name.fragment(),
            location: Location::from_span(&name),
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    // Identifier tests

    #[test]
    fn identifier_normal_test() {
        let prog = Span::new("asdf");
        let (input, result) = parse_identifier(prog).unwrap();

        assert_eq!(input.fragment(), &"");
        assert_eq!(result.name, "asdf");
    }

    #[test]
    fn identifier_nums_test() {
        let prog = Span::new("asdf123");
        let (input, result) = parse_identifier(prog).unwrap();

        assert_eq!(input.fragment(), &"123");
        assert_eq!(result.name, "asdf");
    }

    #[test]
    fn identifier_invalid_test() {
        let prog = Span::new("123");

        let result = parse_identifier(prog);

        assert!(result.is_err());
        println!("{:?}", result);
    }

    // Expr tests
    #[test]
    fn test_parse_expr_with_int_lit() {
        let input = Span::new("123");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(
            matches!(expr.kind, ExprKind::IntegerLiteral(123)),
            "Expected IntegerLiteral with value 123"
        );
    }

    #[test]
    fn test_parse_showtextln_fn() {
        let input = Span::new("showtextln(\"hello\")");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");

        assert_eq!(expr.to_string(), "showtextln(\"hello\")")
    }

    #[test]
    fn test_parse_expr_with_identifier_expr() {
        let input = Span::new("variableName");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(
            matches!(
                expr.kind,
                ExprKind::Identifier(Identifier {
                    name: "variableName",
                    ..
                })
            ),
            "Expected Identifier with name \"variableName\""
        );
    }

    #[test]
    fn test_parse_expr_with_func_call() {
        let input = Span::new("func(123, 111)");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(
            matches!(
                expr.kind,
                ExprKind::FunctionCall(FunctionCallData {
                    function_name: Identifier { name: "func", .. },
                    arguments: _,
                    ..
                })
            ),
            "Expected FunctionCall with name \"func\""
        );
    }

    #[test]
    fn test_parse_expr_with_func_call_with_string_arg() {
        let input = Span::new("func(123, \"hello\")");
        let result = parse_program(input);
        assert!(matches!(result, Err(_)))
    }

    #[test]
    fn test_parse_expr_with_binary_expr() {
        let input = Span::new("a + b");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(
            matches!(expr.kind, ExprKind::BinaryExpr(_, BinaryOp::Add, _)),
            "Expected BinaryExpr with Add operation"
        );
    }

    #[test]
    fn test_parse_expr_with_grouped_expr() {
        let input = Span::new("(a + b)");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(
            matches!(expr.kind, ExprKind::Grouped(_)),
            "Expected Grouped expression"
        );
    }

    #[test]
    fn test_parse_expr_with_nested_grouped_expr() {
        let input = Span::new("(a + (b * c))");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(
            matches!(expr.kind, ExprKind::Grouped(_)),
            "Expected nested Grouped expression"
        );
    }

    #[test]
    fn test_parse_expr_with_nested_func_call() {
        let input = Span::new("outer_func(inner_func(a, b), c)");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(
            matches!(
                expr.kind,
                ExprKind::FunctionCall(FunctionCallData {
                    function_name: Identifier {
                        name: "outer_func",
                        ..
                    },
                    arguments: _,
                    ..
                })
            ),
            "Expected nested FunctionCall with name \"outer_func\""
        );
    }

    #[test]
    fn test_parse_expr_with_multiple_binary_ops() {
        let input = Span::new("a * b + c / d");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(
            matches!(expr.kind, ExprKind::BinaryExpr(_, _, _)),
            "Expected expression with multiple Binary operations"
        );
    }

    #[test]
    fn test_parse_expr_with_equals_binary_op() {
        let input = Span::new("a == b");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(
            matches!(expr.kind, ExprKind::BinaryExpr(_, BinaryOp::Equals, _)),
            "Expected BinaryExpr with Equals operation"
        );
    }

    #[test]
    fn test_parse_expr_with_not_equals_binary_op() {
        let input = Span::new("a != b");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(
            matches!(expr.kind, ExprKind::BinaryExpr(_, BinaryOp::NotEqual, _)),
            "Expected BinaryExpr with NotEqual operation"
        );
    }
}
