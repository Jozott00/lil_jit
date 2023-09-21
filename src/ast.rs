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

    /*fn accept<T>(&self, mut visitor: &mut dyn NodeVisitor<T>) -> T {
        visitor.visit_prog(self)
    }*/
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

    /*fn accept<T>(&self, mut visitor: &mut dyn NodeVisitor<T>) -> T {
        visitor.visit_funcdec(self)
    }*/
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

    If(Expr<'a>, Vec<Stmt<'a>>, Option<Vec<Stmt<'a>>>),

    // FIXME: the pre and post stmt should be optional
    For(Box<Stmt<'a>>, Expr<'a>, Box<Stmt<'a>>, Vec<Stmt<'a>>),
    ExprStmt(Expr<'a>),

    Return(Expr<'a>),
}

/*
impl<'a> AstNode for StmtKind<'a> {
    fn location(&self) -> Location {
        match self {
            StmtKind::Assignment(_, _, _, location)
            | StmtKind::If(_, _, _, location)
            | StmtKind::For(_, _, _, _, location)
            | StmtKind::ExprStmt(_, location)
            | StmtKind::Return(_, location) => location.clone(),
        }
    }
    fn accept<T>(&self, visitor: &mut dyn NodeVisitor<T>) -> T {
        match self {
            StmtKind::Assignment(..) => visitor.visit_assign(self),
            StmtKind::If(..) => visitor.visit_if(self),
            StmtKind::For(..) => visitor.visit_for(self),
            StmtKind::ExprStmt(..) => visitor.visit_expr_stmt(self),
            StmtKind::Return(..) => visitor.visit_return(self),
        }
    }
}

 */

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

/*
impl<'a> AstNode for ExprKind<'a> {
    fn location(&self) -> Location {
        match self {
            ExprKind::IntegerLiteral(_, loc)
            | ExprKind::StringLiteral(_, loc)
            | ExprKind::FunctionCall(_, loc)
            | ExprKind::BinaryExpr(_, _, _, loc)
            | ExprKind::Identifier(_, loc)
            | ExprKind::Grouped(_, loc) => loc.clone(),
        }
    }
    fn accept<T>(&self, mut visitor: &mut dyn NodeVisitor<T>) -> T {
        match self {
           ExprKind::IntegerLiteral(..)  => visitor.visit_int_literal(self),
            ExprKind::StringLiteral(..)  => visitor.visit_str_literal(self),
            ExprKind::FunctionCall(..)  => visitor.visit_func_call(self),
            ExprKind::BinaryExpr(..)  => visitor.visit_binary(self),
            ExprKind::Identifier(..)  => visitor.visit_identifier(self),
            ExprKind::Grouped(..)  => visitor.visit_grouped(self),
        }
    }
}

 */

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
        let body = join_custom(&self.body, "\n   ");
        write!(f, "fn {}({}) {{\n  {}\n}}\n", self.name, params, body)
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
                let if_b = join_custom(&if_block, "\n");
                let else_b = join_custom(&else_block.as_ref().unwrap_or(&vec![]), "\n");
                write!(f, "if {} {{ {} }} else {{ {} }}", condition, if_b, else_b)
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
