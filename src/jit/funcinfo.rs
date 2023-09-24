use crate::ast::FuncDec;

#[derive(Debug)]
pub struct FuncInfo<'a> {
    pub name: &'a str,
    pub ast: &'a FuncDec<'a>,
}
