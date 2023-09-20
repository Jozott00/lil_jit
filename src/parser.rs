use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case, take_until, take_while1};
use nom::character::complete::{char, digit1, multispace0, multispace1};
use nom::character::is_alphabetic;
use nom::combinator::{complete, eof, opt, value};
use nom::error::Error;
use nom::IResult;
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, tuple};
use nom_locate::LocatedSpan;
use crate::ast::{AstNode, BinaryOp, Expr, FuncDec, FunctionCallData, Identifier, Program, Stmt};
use crate::ast::Stmt::{Assignment, ExprStmt, For, If, Return};
use crate::location::Location;

type Span<'a> = LocatedSpan<&'a str>;

// FIXME: Maybe we should return a custom error here
pub fn parse_lil_program(source: &str) -> Result<Program, String> {
    let input = Span::new(source);
    match parse_program(input) {
        Ok((_, program)) => Ok(program),
        Err(e) => Err(e.to_string())
    }
}

fn parse_program(input: Span) -> IResult<Span, Program, Error<Span>> {
    let (input, _) = multispace0(input)?;
    let (input, funcs) = complete(many0(parse_function_declaration))(input)?;
    eof(input)?;

    let location = funcs.iter()
        .fold(Location::new(0, 0, 1), |acc, func| acc.merge(&func.location()));


    Ok((input, Program{
        functions: funcs,
        location,
    }))
}

fn parse_function_declaration(input: Span) -> IResult<Span, FuncDec> {
    let (input, (s, _, ident, _, _, _)) = tuple((tag("fn"), multispace0, parse_identifier, multispace0, tag("("), multispace0,))(input)?;
    let mut location = Location::from_span(&s);

    let (input, args) = separated_list0(tuple((multispace0, tag(","), multispace0)), parse_identifier)(input)?;

    let (input, (_, _, s, block)) = tuple((multispace0, tag(")"), multispace0, parse_block))(input)?;

    location = location.merge(&s.into());
    if let Some(l) = block.last() {
        location = location.merge(&l.location())
    }

    Ok((input, FuncDec{
        name: ident,
        params: args,
        body: block,
        location
    }))
}

fn parse_stmt(input: Span) -> IResult<Span, Stmt> {
    // So this function should not eat the trailing newline, because for likes to not need a
    // newline.
    let (input, stmt) = (alt((
        parse_assignment,
        parse_if,
        parse_for,
        parse_expr_stmt,
        parse_return,
        )))(input)?;

    Ok((input, stmt))
}

fn parse_assignment(input: Span) -> IResult<Span, Stmt> {
    let (input, decl) = opt(tuple((tag("let"), multispace1)))(input)?;
    let (input, (name, _, _, _, expr)) = tuple(( parse_identifier, multispace0, tag("="), multispace0, parse_expr))(input)?;

    let start = match decl {
        Some((kw, _)) => Location::from_span(&kw),
        None => name.location(),
    };

    let is_declaration = decl.is_some();
    let location = start.merge(&expr.location());

    Ok((input, Assignment(is_declaration, name, expr, location)))
}

fn parse_if(input: Span) -> IResult<Span, Stmt> {
    // FIXME: Also parse the else branch if it exists
    let (input, (start, _, cond, end, block)) = tuple((tag("if"), multispace0, parse_expr, multispace0, parse_block))(input)?;

    let mut location = Location::from_span(&start).merge(&end.into());
    if let Some(l) = block.last() {
        location = location.merge(&l.location())
    }

    Ok((
        input,
        If(
            cond,
            block,
            None,
            location
         )
    ))
}

fn parse_for(input: Span) -> IResult<Span, Stmt> {
    // FIXME: There must be betterway than just inserting a f*ck-load of multispace0
    let (input, (start, _, pre, _, _, _,  cond, _, _, _,post, end,  block)) = tuple((
        tag("for"),
        multispace0,
        parse_stmt,
        multispace0,
        tag(";"),
        multispace0,
        parse_expr,
        multispace0,
        tag(";"),
        multispace0,
        parse_stmt,
        multispace0,
        parse_block,
    ))(input)?;

    let mut location = Location::from_span(&start).merge(&end.into());
    if let Some(l) = block.last() {
        location = location.merge(&l.location())
    }

    Ok((
        input,
        For(
            Box::new(pre),
            cond,
            Box::new(post) ,
            block,
            location
        )
    ))
}

fn parse_return(input: Span) -> IResult<Span, Stmt> {
    let (input, (first, _, expr)) = tuple((tag("return"), multispace0, parse_expr))(input)?;
    let location = Location::from_span(&input).merge(&expr.location());

    Ok((input, Return(expr, location)))
}

fn parse_expr_stmt(input: Span) -> IResult<Span, Stmt> {
    let (input, expr) = parse_expr(input)?;
    let location = expr.location();
    Ok((input, ExprStmt(expr, location)))
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

fn parse_block(input: Span) -> IResult<Span, Vec<Stmt>> {
    // Single statement blocks
    // Example: if false: return 666
    if input.starts_with(":") {
        let (input, (_, _, stmt)) = tuple((tag(":"), multispace0, parse_stmt))(input)?;
        return Ok((input, vec!(stmt)))
    }

    // Multi statement blocks
    // Example: if false { return 666}
    let(input, _) = tuple((tag("{"), multispace0))(input)?;
    let(input, stmts) = many0(parse_stmt)(input)?;
    let(input, _) = tuple((tag("}"), multispace0))(input)?;

    Ok((input, stmts))
}

fn parse_showtext_call(input: Span) -> IResult<Span, Expr> {
    let (input, ident) = parse_identifier(input)?;
    let (input, (_,_,message,_,last)) = tuple((tag("("), multispace0, parse_string_lit, multispace0, tag(")")))(input)?;

    let location = ident.location().merge(&last.into());

    Ok((input, Expr::FunctionCall(
        FunctionCallData{function_name: ident, arguments: vec!(message), location: location.clone()},
        location.clone()
    )))
}

fn parse_func_call(input: Span) -> IResult<Span, Expr> {
    let (input, ident) = parse_identifier(input)?;
    let (input, args) = parse_args(input)?;

    let mut location = ident.location();
    if let Some(last) = args.last() {
        location = location.merge(&last.location())
    }

    Ok((input, Expr::FunctionCall(
        FunctionCallData{function_name: ident, arguments: args, location},
        location.clone()
    )))
}

fn parse_binary_expr<'a>(first: Expr<'a>) -> impl Fn(Span<'a>) -> IResult<Span<'a>, Expr<'a>> {
    move |input: Span<'a>| {
        let (input, (op, e2)) = tuple((parse_binary_op, parse_expr))(input)?;
        let location = first.location().merge(&e2.location());
        Ok((input, Expr::BinaryExpr(Box::new(first.clone()), op, Box::new(e2), location)))
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
    ))(input)?;

    Ok((input, binary_op))
}

fn parse_args(input: Span) -> IResult<Span, Vec<Expr>> {
    delimited(char('('), separated_list0(char(','), parse_expr),char(')'))(input)
}

fn parse_grouped_expr(input: Span) -> IResult<Span, Expr> {
    let (input, (start, expr, end)) = tuple((tag("("), parse_expr, tag(")")))(input)?;
    let location = Location::from_span(&start).merge(&end.into());
    Ok((input, Expr::Grouped(Box::new(expr), location)))
}

fn parse_int_lit(input: Span) -> IResult<Span, Expr> {
    let (input, num_span) = digit1(input)?;
    let num = num_span.fragment().parse::<i64>().unwrap();
    Ok((input, Expr::IntegerLiteral(num, Location::from_span(&num_span))))
}

fn parse_string_lit(input: Span) -> IResult<Span, Expr> {
    let (input, str) = delimited(char('\"'), take_until("\""), char('\"'))(input)?;
    Ok((input, Expr::StringLiteral(str.fragment(), Location::from_span(&str))))
}

fn parse_identifier_expr(input: Span) -> IResult<Span, Expr> {
    let (input, ident) = parse_identifier(input)?;
    let location = ident.location();
    Ok((input, Expr::Identifier(ident, location)))
}


fn parse_identifier(input: Span) -> IResult<Span, Identifier> {
    let (input, name) = take_while1(|c: char| c.is_alphabetic() || c == '_')(input)?;
    Ok((input, Identifier{ name: name.fragment(), location: Location::from_span(&name) }))
}


#[cfg(test)]
mod tests {
    use std::str::Matches;
    use nom::ExtendInto;
    use super::*;

    #[test]
    fn empty_prog_test() {
        let prog = Span::new("");
        let (input, result) = parse_program(prog).unwrap();

        assert_eq!(input.fragment(), &"");
        assert_eq!(result.functions, vec![]);
    }


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
        assert!(matches!(expr, Expr::IntegerLiteral(123, _)), "Expected IntegerLiteral with value 123");
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
        assert!(matches!(expr, Expr::Identifier(Identifier { name: "variableName", .. }, _)), "Expected Identifier with name \"variableName\"");
    }

    #[test]
    fn test_parse_expr_with_func_call() {
        let input = Span::new("func(123, 111)");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(matches!(expr, Expr::FunctionCall(FunctionCallData { function_name: Identifier { name: "func", .. }, arguments: _, .. }, _)), "Expected FunctionCall with name \"func\"");
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
        assert!(matches!(expr, Expr::BinaryExpr(_, BinaryOp::Add, _, _)), "Expected BinaryExpr with Add operation");
    }

    #[test]
    fn test_parse_expr_with_grouped_expr() {
        let input = Span::new("(a + b)");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(matches!(expr, Expr::Grouped(_, _)), "Expected Grouped expression");
    }

    #[test]
    fn test_parse_expr_with_nested_grouped_expr() {
        let input = Span::new("(a + (b * c))");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(matches!(expr, Expr::Grouped(_, _)), "Expected nested Grouped expression");
    }

    #[test]
    fn test_parse_expr_with_nested_func_call() {
        let input = Span::new("outer_func(inner_func(a, b), c)");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(matches!(expr, Expr::FunctionCall(FunctionCallData { function_name: Identifier { name: "outer_func", .. }, arguments: _, .. }, _)), "Expected nested FunctionCall with name \"outer_func\"");
    }

    #[test]
    fn test_parse_expr_with_multiple_binary_ops() {
        let input = Span::new("a * b + c / d");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(matches!(expr, Expr::BinaryExpr(_, _, _, _)), "Expected expression with multiple Binary operations");
    }

    #[test]
    fn test_parse_expr_with_equals_binary_op() {
        let input = Span::new("a == b");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(matches!(expr, Expr::BinaryExpr(_, BinaryOp::Equals, _, _)), "Expected BinaryExpr with Equals operation");
    }

    #[test]
    fn test_parse_expr_with_not_equals_binary_op() {
        let input = Span::new("a != b");
        let result = parse_expr(input);

        let (rest_input, expr) = result.unwrap();
        assert_eq!(rest_input.fragment(), &"");
        assert!(matches!(expr, Expr::BinaryExpr(_, BinaryOp::NotEqual, _, _)), "Expected BinaryExpr with NotEqual operation");
    }

}