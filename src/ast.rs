use crate::location::Location;
use std::fmt;

pub trait AstNode: fmt::Display {
    fn location(&self) -> Location;
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program<'a> {
    pub(crate) functions: Vec<FuncDec<'a>>,
    pub location: Location,
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
    pub body: Stmt<'a>,
    pub location: Location,
}

impl<'a> AstNode for FuncDec<'a> {
    fn location(&self) -> Location {
        self.location.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Stmt<'a> {
    pub kind: StmtKind<'a>,
    pub location: Location,
}

impl<'a> AstNode for Stmt<'a> {
    fn location(&self) -> Location {
        return self.location;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StmtKind<'a> {
    // The bool indicates if the assignment is a declaration or not
    Assignment(bool, Identifier<'a>, Expr<'a>),

    Block(Vec<Stmt<'a>>),
    If(Expr<'a>, Box<Stmt<'a>>, Option<Box<Stmt<'a>>>),

    // FIXME: the pre and post stmt should be optional
    For(
        Option<Box<Stmt<'a>>>,
        Expr<'a>,
        Option<Box<Stmt<'a>>>,
        Box<Stmt<'a>>,
    ),
    ExprStmt(Expr<'a>),

    Return(Expr<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr<'a> {
    pub(crate) kind: ExprKind<'a>,
    pub(crate) location: Location,
}

impl<'a> AstNode for Expr<'a> {
    fn location(&self) -> Location {
        return self.location;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprKind<'a> {
    IntegerLiteral(i64),
    StringLiteral(&'a str),
    FunctionCall(FunctionCallData<'a>),
    BinaryExpr(Box<Expr<'a>>, BinaryOp, Box<Expr<'a>>),
    Identifier(Identifier<'a>),
    Grouped(Box<Expr<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Add,
    Minus,
    Multi,
    Divide,
    Equals,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCallData<'a> {
    pub function_name: Identifier<'a>,
    pub arguments: Vec<Expr<'a>>,
    pub location: Location,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier<'a> {
    pub name: &'a str,
    pub location: Location,
}

impl<'a> AstNode for Identifier<'a> {
    fn location(&self) -> Location {
        self.location
    }
}

// Display implementations

impl<'a> fmt::Display for Program<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", join_custom(&self.functions, "\n"))
    }
}

impl<'a> fmt::Display for FuncDec<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = join_custom(&self.params, ", ");
        write!(f, "fn {}({}) {{\n  {}\n}}\n", self.name, params, self.body)
    }
}

impl<'a> fmt::Display for Stmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            StmtKind::Assignment(decl, ident, expr) => {
                let decl_str = if *decl { "let " } else { "" };
                write!(f, "{}{} = {};", decl_str, ident, expr)
            }
            StmtKind::If(condition, if_block, else_block) => {
                let else_b = else_block
                    .as_ref()
                    .map_or("".to_string(), |e| e.to_string());
                write!(
                    f,
                    "if {} {{ {} }} else {{ {} }}",
                    condition, if_block, else_b
                )
            }
            StmtKind::Block(stmts) => {
                let block = join_custom(stmts, "\n");
                write!(f, "{}", block)
            }
            StmtKind::Return(expr) => {
                write!(f, "return {}", expr)
            }
            _ => todo!(),
        }
    }
}

impl<'a> fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ExprKind::IntegerLiteral(i) => write!(f, "{}", i),
            ExprKind::StringLiteral(s) => write!(f, "\"{}\"", s),
            ExprKind::FunctionCall(d) => write!(
                f,
                "{}({})",
                d.function_name,
                join_custom(&d.arguments, ", ")
            ),
            ExprKind::BinaryExpr(first, op, s) => write!(f, "{} {} {}", first, op, s),
            ExprKind::Identifier(i) => write!(f, "{}", i),
            ExprKind::Grouped(expr) => write!(f, "({})", expr),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOp::Add => "+",
                BinaryOp::Minus => "-",
                BinaryOp::Multi => "*",
                BinaryOp::Divide => "/",
                BinaryOp::Equals => "==",
                BinaryOp::NotEqual => "!=",
                BinaryOp::Greater => ">",
                BinaryOp::GreaterEqual => ">=",
                BinaryOp::Less => "<",
                BinaryOp::LessEqual => "<=",
            }
        )
    }
}

impl<'a> fmt::Display for Identifier<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

// Helper functions

fn join_custom<T: ToString>(vec: &[T], sep: &str) -> String {
    vec.iter()
        .map(|e| e.to_string())
        .collect::<Vec<String>>()
        .join(sep)
}
