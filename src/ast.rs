use std::fmt::{Binary, Display, format, Formatter};
use std::fmt;
use crate::location::Location;

pub trait AstNode: ToString {
    fn location(&self) -> Location;
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program<'a> {
    pub(crate) functions: Vec<FuncDec<'a>>,
    pub location: Location
}

impl<'a> AstNode for Program<'a> {
    fn location(&self) -> Location {
        self.location.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncDec<'a> {
    pub name: Identifier<'a>,
    pub params: Vec<Identifier<'a>>,
    pub body: Vec<Stmt<'a>>,
    pub location: Location,
}

impl<'a> AstNode for FuncDec<'a> {
    fn location(&self) -> Location {
        self.location.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt<'a> {
    // The bool indicates if the assignment is a declaration or not
    Assignment(bool, Identifier<'a>, Expr<'a>, Location),

    If(Expr<'a>, Vec<Stmt<'a>>, Option<Vec<Stmt<'a>>>, Location),

    // FIXME: the pre and post stmt should be optional
    For(Box<Stmt<'a>>, Expr<'a>, Box<Stmt<'a>>, Vec<Stmt<'a>>, Location),
    ExprStmt(Expr<'a>, Location),

    Return(Expr<'a>, Location)
}


impl<'a> AstNode for Stmt<'a> {
    fn location(&self) -> Location {
        match self {
            Stmt::Assignment(_, _, _, location)
            | Stmt::If(_, _, _, location)
            | Stmt::For(_, _, _, _, location)
            | Stmt::ExprStmt(_, location)
            | Stmt::Return(_, location) => location.clone(),
        }
    }
}


#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    IntegerLiteral(i64, Location),
    StringLiteral(&'a str, Location),
    FunctionCall(FunctionCallData<'a>, Location),
    BinaryExpr(Box<Expr<'a>>, BinaryOp, Box<Expr<'a>>, Location),
    Identifier(Identifier<'a>, Location),
    Grouped(Box<Expr<'a>>, Location)
}

impl<'a> AstNode for Expr<'a> {
    fn location(&self) -> Location {
        match self {
            Expr::IntegerLiteral(_, loc)
            | Expr::StringLiteral(_, loc)
            | Expr::FunctionCall(_, loc)
            | Expr::BinaryExpr(_, _, _, loc)
            | Expr::Identifier(_, loc)
            | Expr::Grouped(_, loc) => loc.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Add,
    Minus,
    Multi,
    Divide,
    Equals,
    NotEqual,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCallData<'a> {
    pub function_name: Identifier<'a>,
    pub arguments: Vec<Expr<'a>>,
    pub location: Location
}


#[derive(Debug, PartialEq, Clone)]
pub struct Identifier<'a> {
    pub name: &'a str,
    pub location: Location
}

impl<'a> AstNode for Identifier<'a> {
    fn location(&self) -> Location {
        self.location.clone()
    }
}



impl<'a> fmt::Display for Program<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", join_custom(&self.functions, "\n"))
    }
}

impl<'a> fmt::Display for FuncDec<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = join_custom(&self.params, ", ");
        let body = join_custom(&self.body, "\n   ");
        write!(f, "fn {}({}) {{\n  {}\n}}\n", self.name, params, body)
    }
}

impl<'a> fmt::Display for Stmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Assignment(decl, ident, expr, _) => {
                let decl_str = if *decl { "let " } else { "" };
                write!(f, "{}{} = {};", decl_str, ident, expr)
            },
            Stmt::If(condition, if_block, else_block, _) => {
                let if_b = join_custom(&if_block, "\n");
                let else_b = join_custom(&else_block.as_ref().unwrap_or(&vec![]), "\n");
                write!(f, "if {} {{ {} }} else {{ {} }}", condition, if_b, else_b)
            },
            _ => todo!()
        }
    }
}

impl<'a> fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::IntegerLiteral(i, _) => write!(f, "{}", i),
            Expr::StringLiteral(s, _) => write!(f, "\"{}\"", s),
            Expr::FunctionCall(d, _) => write!(f, "{}({})", d.function_name, join_custom(&d.arguments, ", ")),
            Expr::BinaryExpr(first, op, s, _) => write!(f, "{} {} {}", first, op, s),
            Expr::Identifier(i, _) => write!(f, "{}", i),
            Expr::Grouped(expr, _) => write!(f, "({})", expr),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            BinaryOp::Add => "+",
            BinaryOp::Minus => "-",
            BinaryOp::Multi => "*",
            BinaryOp::Divide => "/",
            BinaryOp::Equals => "==",
            BinaryOp::NotEqual => "!=",
        })
    }
}

impl<'a> fmt::Display for Identifier<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

// Helper functions

fn join_custom<T: ToString>(vec: &[T], sep: &str) -> String {
    vec.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(sep)
}