//! This module provides an interface and functions to walk over or
//! 'visit' nodes in an abstract syntax tree (AST).

use crate::ast::{Expr, ExprKind, FuncDec, Program, Stmt, StmtKind};

/// The NodeVisitor trait is implemented by the components that
/// need to 'visit' each node of the AST and possibly transform it.
pub trait NodeVisitor<'ast>: Sized {
    fn visit_prog(&mut self, node: &'ast Program) {
        walk_prog(self, node)
    }

    fn visit_funcdec(&mut self, node: &'ast FuncDec) {
        walk_funcdec(self, node)
    }

    fn visit_expr(&mut self, node: &'ast Expr) {
        walk_expr(self, node);
    }
    fn visit_stmt(&mut self, node: &'ast Stmt) {
        walk_stmt(self, node);
    }
}

pub fn walk_stmt<'a, V: NodeVisitor<'a>>(visitor: &mut V, node: &'a Stmt) {
    match &node.kind {
        StmtKind::Assignment(_, _, expr) => visitor.visit_expr(expr),
        StmtKind::If(cond, then, other) => {
            visitor.visit_expr(cond);
            visitor.visit_stmt(then);
            if let Some(other_exprs) = other {
                visitor.visit_stmt(other_exprs);
            }
        }
        StmtKind::For(pre, cond, post, body) => {
            if let Some(pre) = pre {
                visitor.visit_stmt(pre);
            }

            visitor.visit_expr(cond);

            if let Some(post) = post {
                visitor.visit_stmt(post);
            }

            visitor.visit_stmt(body);
        }
        StmtKind::ExprStmt(expr) => visitor.visit_expr(expr),
        StmtKind::Return(expr) => visitor.visit_expr(expr),
        StmtKind::Block(exprs) => walk_stmt_list(visitor, exprs),
    }
}

pub fn walk_stmt_list<'a, V: NodeVisitor<'a>>(visitor: &mut V, nodes: &'a Vec<Stmt>) {
    for stmt in nodes {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_expr<'a, V: NodeVisitor<'a>>(visitor: &mut V, node: &'a Expr) {
    match &node.kind {
        ExprKind::IntegerLiteral(_) => {}
        ExprKind::StringLiteral(_) => {}
        ExprKind::FunctionCall(data) => walk_expr_list(visitor, &data.arguments),
        ExprKind::BinaryExpr(left, _, right) => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
        }
        ExprKind::Identifier(_) => {}
        ExprKind::Grouped(inner) => visitor.visit_expr(inner),
    }
}

pub fn walk_expr_list<'a, V: NodeVisitor<'a>>(visitor: &mut V, expressions: &'a Vec<Expr>) {
    for expr in expressions {
        visitor.visit_expr(expr);
    }
}

pub fn walk_funcdec<'a, V: NodeVisitor<'a>>(visitor: &mut V, funcdec: &'a FuncDec) {
    visitor.visit_stmt(&funcdec.body);
}

pub fn walk_prog<'a, V: NodeVisitor<'a>>(visitor: &mut V, prog: &'a Program) {
    for funcdec in &prog.functions {
        visitor.visit_funcdec(&funcdec)
    }
}
