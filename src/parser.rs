use nom::bytes::complete::tag;
use nom::character::complete::multispace0;
use nom::error::Error;
use nom::IResult;
use nom::multi::many0;
use nom::sequence::tuple;
use crate::ast::{FuncDec, Identifier};

pub fn parse_program(input: &str) -> IResult<&str, Vec<FuncDec>, Error<&str>> {
    many0(parse_function_declaration)(input)
}

fn parse_function_declaration(input: &str) -> IResult<&str, FuncDec> {
    let (input, (_, _, ident, _, _, _)) = tuple((tag("fun"), multispace0, parse_identifier, multispace0, tag("("), multispace0,))(input)?;


    (input, _) = tag("fun")(input)?;

    Ok((input, FuncDec{
        name: identifier,
    }))
}

fn parse_identifier(input: &str) -> IResult<&str, Identifier> {

}