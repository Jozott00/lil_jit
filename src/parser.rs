use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::multispace0;
use nom::combinator::{complete, eof};
use nom::error::Error;
use nom::IResult;
use nom::multi::{many0, separated_list0};
use nom::sequence::tuple;
use nom_locate::LocatedSpan;
use crate::ast::{Expr, FuncDec, Identifier, Program, Stmt};
use crate::ast::Expr::IdentifierExpr;
use crate::ast::Stmt::{Assignment, ExprStmt, For, If};
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
    let (input, funcs) = many0(parse_function_declaration)(input)?;
    eof(input)?;

    Ok((input, Program{
        functions: funcs,
    }))
}

fn parse_function_declaration(input: Span) -> IResult<Span, FuncDec> {
    let (input, (_, _, ident, _, _, _)) = tuple((tag("fn"), multispace0, parse_identifier, multispace0, tag("("), multispace0,))(input)?;

    let (input, args) = separated_list0(tuple((multispace0, tag(","), multispace0)), parse_identifier)(input)?;

    let (input, (_, _, _, block)) = tuple((multispace0, tag(")"), multispace0, parse_block))(input)?;

    Ok((input, FuncDec{
        name: ident,
        params: args,
        body: block,
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
        )))(input)?;

    Ok((input, stmt))
}

fn parse_expr_stmt(input: Span) -> IResult<Span, Stmt> {
    let (input, expr) = parse_expr(input)?;

    Ok((input, ExprStmt(expr)))
}

fn parse_assignment(input: Span) -> IResult<Span, Stmt> {
    let (input, (start, _, name, _, _, _, expr)) = tuple((tag("let"), multispace0, parse_identifier, multispace0, tag("="), multispace0, parse_expr))(input)?;

    start.fragment();

    Ok((input, Assignment(name, expr)))
}

fn parse_if(input: Span) -> IResult<Span, Stmt> {
    // FIXME: Also parse the else branch if it exists
    let (input, (_, _, cond, _, block)) = tuple((tag("if"), multispace0, parse_expr, multispace0, parse_block))(input)?;

    Ok((
        input,
        If(cond, block, None)
    ))
}

fn parse_for(input: Span) -> IResult<Span, Stmt> {
    // FIXME: There must be betterway than just inserting a f*ck-load of multispace0
    let (input, (_, _, pre, _, _, _,  cond, _, _, _,post, _,  block)) = tuple((
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

    Ok((
        input,
        For(Box::new(pre), cond, Box::new(post) , block)
    ))
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

    return Ok((input, stmts))
}

fn parse_expr(input: Span) -> IResult<Span, Expr> {
    let(input, expr) = alt((
        parse_identifier_expr,
        ))(input)?;

    return Ok((input, expr))
}

fn parse_identifier_expr(input: Span) -> IResult<Span, Expr> {
    let (input, ident) = parse_identifier(input)?;
    Ok((input, IdentifierExpr(ident)))
}

fn parse_identifier(input: Span) -> IResult<Span, Identifier> {
    let (input, name) = take_while1(|c: char| c.is_alphabetic())(input)?;
    Ok((input, Identifier{ name: name.fragment(), location: Location::from_span(&name) }))
}