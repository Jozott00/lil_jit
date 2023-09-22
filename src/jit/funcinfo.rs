use crate::ast::FuncDec;

pub struct FuncInfo<'a> {
    pub name: &'a str,
    pub ast: &'a FuncDec<'a>,
}
