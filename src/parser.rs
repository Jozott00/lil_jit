use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::multispace0;
use nom::character::is_alphabetic;
use nom::error::Error;
use nom::IResult;
use nom::multi::{many0, separated_list0};
use nom::sequence::tuple;
use crate::ast::{Expr, FuncDec, Identifier, Program, Stmt};
use crate::ast::Stmt::{Assignment, ExprStmt, For, If};

pub fn parse_program(input: &str) -> IResult<&str, Program, Error<&str>> {
    let (input, funcs) = many0(parse_function_declaration)(input)?;
    
    // FIXME: it doesn't feel right to return the string here
    Ok((input, Program{
        functions: funcs,
    }))
}

fn parse_function_declaration(input: &str) -> IResult<&str, FuncDec> {
    let (input, (_, _, ident, _, _, _)) = tuple((tag("fun"), multispace0, parse_identifier, multispace0, tag("("), multispace0,))(input)?;

    let (input, args) = separated_list0(tag(","), parse_identifier)(input)?;

    let (input, (_, _, _, block)) = tuple((multispace0, tag(")"), multispace0, parse_block))(input)?;

    Ok((input, FuncDec{
        name: ident,
        params: args,
        body: block,
    }))
}

fn parse_stmt(input: &str) -> IResult<&str, Stmt> {
    // So this function should not eat the trailing newline, because for likes to not need a
    // newline.
    let (input, stmt) = (alt((
        parse_assignment,
        parse_if,
        parse_for,
        parse_expr_stmt,
        ))(input)?;

    Ok((input, stmt))
}

fn parse_expr_stmt(input: &str) -> IResult<&str, Stmt> {
    let (input, expr) = parse_expr(input)?;

    Ok((input, ExprStmt(expr)))
}

fn parse_assignment(input: &str) -> IResult<&str, Stmt> {
    let (input, (_, _, name, _, _, _, expr)) = tuple((tag("let"), multispace0, parse_identifier, multispace0, tag("="), multispace0, parse_expr))(input)?;

    Ok((input, Assignment(name, expr)))
}

fn parse_if(input: &str) -> IResult<&str, Stmt> {
    // FIXME: Also parse the else branch if it exists
    let (input, (_, _, cond, _, block)) = tuple((tag("if"), multispace0, parse_expr, multispace0, parse_block))(input)?;

    Ok((
        input,
        If(cond, block, None)
    ))
}

fn parse_for(input: &str) -> IResult<&str, Stmt> {
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
        For(pre, cond, post , block)
    ))
}


fn parse_expr(input: &str) -> IResult<&str, Expr> {
    todo!;
}

fn parse_block(input: &str) -> IResult<&str, Vec<Stmt>> {
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


fn parse_identifier(input: &str) -> IResult<&str, Identifier> {
    let (input, name) = take_while1(is_alphabetic)(input)?;
    Ok((input, Identifier{ name }))
}