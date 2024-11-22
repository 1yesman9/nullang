use super::expression::{Call, Expression};

#[derive(Debug, Hash)]
pub struct Type<'a> {
    pub name: &'a str,
    pub generics: Option<Vec<Type<'a>>>
}

#[derive(Debug)]
pub struct Program<'a> {
    pub name: &'a str,
    pub definitions: Vec<Definition<'a>>
}
#[derive(Debug)]
pub enum Definition<'a> {
    Sig(Signature<'a>),
    Record(Record<'a>),
    Import(Import<'a>),
    Function(Function<'a>),
    Program(Box<Program<'a>>)
}

#[derive(Debug)]
pub struct Signature<'a> {
    pub name: &'a str,
    pub generics: Option<Vec<Type<'a>>>,
    pub params: Vec<&'a str>,
    pub ret: &'a str
}
#[derive(Debug)]
pub struct Record<'a> {
    pub name: &'a str,
    pub params: Vec<(Type<'a>, &'a str)>,
}

#[derive(Debug)]
pub struct Import<'a> {
    pub def_path: Vec<&'a str>,
    pub method_flag: bool,
    pub file_path: Vec<&'a str>
}

#[derive(Debug, Hash)]
pub struct Function<'a> {
    pub name: &'a str,
    pub params: Vec<(Type<'a>, &'a str)>,
    pub statements: Vec<Statement<'a>>,
    pub return_typ: Option<Type<'a>>
}

#[derive(Debug, Hash)]
pub enum Statement<'a> {
    Variable(Variable<'a>),
    If(If<'a>),
    While(While<'a>),
    Return(Return<'a>),
    Assignment(Assignment<'a>),
    CallStatement(CallStatement<'a>)
}

#[derive(Debug, Hash)]
pub struct If<'a> {
    pub branches: Vec<(Expression<'a>, Block<'a>)>,
    pub else_branch: Option<Block<'a>>
}

#[derive(Debug, Hash)]
pub struct While<'a> {
    pub cond: Expression<'a>,
    pub block: Block<'a>
}

#[derive(Debug, Hash)]
pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>
}

#[derive(Debug, Hash)]
pub struct Variable<'a> {
    pub typ: Type<'a>,
    pub name: &'a str,
    pub expr: Expression<'a>
}

#[derive(Debug, Hash)]
pub struct Assignment<'a> {
    pub l_value: Expression<'a>,
    pub r_value: Expression<'a>
}
#[derive(Debug, Hash)]
pub struct Return<'a> {
    pub expr: Expression<'a>
}

#[derive(Debug, Hash)]
pub struct CallStatement<'a> {
    pub call: Call<'a>
}