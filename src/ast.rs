use std::panic::Location;

#[derive(Debug, PartialEq)]
pub struct Program<'a> {
    functions: Vec<FuncDec<'a>>
}

#[derive(Debug, PartialEq)]
pub struct FuncDec<'a> {
    pub name: Identifier<'a>,
    pub params: Vec<Identifier<'a>>,
    pub body: Vec<Stmt<'a>>
}

#[derive(Debug, PartialEq)]
pub enum Stmt<'a> {
    Assignment(Identifier<'a>, Expr<'a>),
    If(Expr<'a>, Vec<Stmt<'a>>, Option<Vec<Stmt<'a>>>),
    ExprStmt(Expr<'a>),
}


#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    IntegerLiteral(i64, Location<'a>),
    StringLiteral(&'a str, Location<'a>),
    FunctionCall(FunctionCallData<'a>, Location<'a>),
    BinaryExpr(Box<Expr<'a>>, BinaryOp, Box<Expr<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Minus,
    Multi,
    Divide,
    Equals,
    NotEqual,
}

#[derive(Debug, PartialEq)]
pub struct FunctionCallData<'a> {
    function_name: Identifier<'a>,
    arguments: Vec<Expr<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Identifier<'a> {
    name: &'a str,
    location: Location<'a>,
}
