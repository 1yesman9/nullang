use crate::lexical_analysis::Token;
use crate::syntax_analysis::ast::program::{Definition, Variable};

pub trait Operand<'a> {
    fn operation(tok: Token<'a>, lhs: Self, rhs: Self) -> Self;
}

#[derive(Debug, Hash)]
pub enum Expression<'a> {
    Unit(Unit<'a>),
    BinOp(Box<BinOp<'a>>),
}

impl<'a> Operand<'a> for Expression<'a> {
    fn operation(tok: Token<'a>, lhs: Self, rhs: Self) -> Self {
       Expression::BinOp(Box::new(BinOp {
            op: tok.into(),
            lhs,
            rhs
       }))
    }
}

impl<'a> From<Unit<'a>> for Expression<'a> {
    fn from(value: Unit<'a>) -> Self {
        Expression::Unit(value)
    }
}

#[derive(Debug, Hash)]
pub enum Operation { Sum, Difference, Product, Lt, Gt }

impl<'a> From<Token<'a>> for Operation {
    fn from(value: Token<'a>) -> Self {
        match value {
            Token::Plus => Operation::Sum,
            Token::Minus => Operation::Difference,
            Token::Star => Operation::Product,
            Token::Lt => Operation::Lt,
            Token::Gt => Operation::Gt,
            _ => panic!("attempted to convert invalid token to operation")
        }
    }
}

#[derive(Debug, Hash)]
pub struct BinOp<'a> {
    pub op: Operation,
    pub lhs: Expression<'a>,
    pub rhs: Expression<'a>
}

impl<'a> BinOp<'a> {
    pub fn name(&self) -> &'static str {
        match self.op {
            Operation::Sum => "add",
            Operation::Difference => "sub",
            Operation::Product => "mul",
            Operation::Lt => "lt",
            Operation::Gt => "gt"
        }
    }
}

#[derive(Debug, Hash)]
pub enum Unit<'a> {
    Identifier(&'a str),
    Integer(&'a str),
    String(&'a str),
    Bool(bool),
    Call(Call<'a>),
    SCall(SCall<'a>),
    Index(Index<'a>),
    Path(Vec<&'a str>),
    Array(Array<'a>),
    DotIndex(DotIndex<'a>),
    Construction(Construction<'a>)
}

impl<'a> Unit<'a> {
    pub fn to_path(&self) -> Vec<&'a str> {
        match self {
            Unit::Identifier(idn) => vec![*idn],
            Unit::Path(path) => path.clone(),
            _ => panic!("invalid path")
        }
    }
}

#[derive(Debug, Hash)]
pub struct Array<'a> {
    pub elements: Vec<Expression<'a>>
}

#[derive(Debug, Hash)]
pub struct Call<'a> {
    pub expr: Box<Unit<'a>>,
    pub args: Vec<Expression<'a>>
}

#[derive(Debug, Hash)]
pub struct SCall<'a> {
    pub id: u64,
    pub args: Vec<Expression<'a>>
}

#[derive(Debug, Hash)]
pub struct Construction<'a> {
    pub path: Vec<&'a str>,
    pub args: Vec<(&'a str, Expression<'a>)>
}

#[derive(Debug, Hash)]
pub struct Index<'a> {
    pub expr: Box<Expression<'a>>,
    pub value: Box<Expression<'a>>
}

#[derive(Debug, Hash)]
pub struct DotIndex<'a> {
    pub expr: Box<Expression<'a>>,
    pub index: &'a str
}


impl<'a> std::convert::From<Token<'a>> for Unit<'a> {
    fn from(value: Token<'a>) -> Self {
        match value {
            Token::Identifier(identifier) => Unit::Identifier(identifier),
            Token::Integer(integer) => Unit::Integer(integer),
            Token::String(string) => Unit::String(string),
            Token::True => Unit::Bool(true),
            Token::False => Unit::Bool(false),

            _ => panic!("attempted to cast non-literal token to literal")
        }
    }
}
