use crate::util::iter::Peek;
use crate::lexical_analysis::Token;

pub mod ast;
pub mod scanner;
pub mod examplewalker;

use ast::program::*;
use ast::expression::*;

use crate::compiler::syntax_analysis::ParseErr::{ExpectStmtExpr, Null};

macro_rules! ok_or_null {
    ($e: expr) => {
        match $e {
            Ok(definition) => Ok(definition),
            Err(definition) =>  if let ParseErr::Null = definition {
                Err(ParseErr::Null)
            } else {
                return Err(definition);
            }
        }
    }
}

macro_rules! list {
    ($e: expr) => {{
        let mut elements = Vec::new();
        while let Ok(element) = $e { elements.push(element); }
        elements
    }}
}

macro_rules! del_list {
    ($e: expr, $del: expr) => {{
        let mut elements = Vec::new();
        if let Ok(element) = $e {
            elements.push(element);
            while let Ok(element) = $e {
                elements.push(element);
                elements.push()
            }
        }
        elements
    }}
}

pub struct Parser<'a, I: Iterator<Item=Token<'a>>> {
    iter: Peek<I, 3>
}

#[derive(Debug)]
pub enum ParseErr<'a> {
    Null,
    Expect(Token<'a>, Token<'a>),
    ExpectStmtExpr
}

pub type Parse<'a, T> = Result<T, ParseErr<'a>>;

impl<'a, I: Iterator<Item=Token<'a>>> Parser<'a, I> {
    fn expect(&mut self, token: Token<'a>) -> Result<Token<'a>, ParseErr<'a>> {
        let tok = self.expect_peek(token)?;
        self.iter.next();
        Ok(tok)
    }

    fn expect_idn(&mut self) -> Result<&'a str, ParseErr<'a>> {
        self.expect(Token::Identifier("a")).map(|idn| idn.inner_idn().unwrap())
    }

    fn expect_int(&mut self) -> Result<u64, ParseErr<'a>> {
        self.expect(Token::Integer("0")).map(|idn| idn.inner_int().unwrap())
    }

    fn expect_peek(&mut self, token: Token<'a>) -> Result<Token<'a>, ParseErr<'a>> {
        let tok = self.iter.peek(0).copied().unwrap_or(Token::Eof);
        if std::mem::discriminant(&tok) == std::mem::discriminant(&token) {
            Ok(tok)
        } else {
            Err(ParseErr::Expect(token, tok))
        }
    }

    fn mtch(&mut self, tokens: &[Token<'a>]) -> bool {
        for (i, token) in tokens.into_iter().enumerate() {
            if std::mem::discriminant(self.iter.peek(i).unwrap_or(&Token::Eof)) != std::mem::discriminant(token) {
                return false
            }
        }

        true
    }

    fn del_list<T, K>(&mut self,
        element: impl Fn(&mut Self) -> Result<T, ParseErr<'a>>,
        delimeter: impl Fn(&mut Self) -> Result<K, ParseErr<'a>>
    ) -> Parse<'a, Vec<T>> {
        let mut list = Vec::new();

        if let Ok(e) = element(self) {
            list.push(e);
            while let Ok(_) = delimeter(self) {
                list.push(element(self)?);
            }
        }

        Ok(list)
    }

    fn comma_list<T>(&mut self, element: impl Fn(&mut Self) -> Result<T, ParseErr<'a>>) -> Parse<'a, Vec<T>> {
        self.del_list(element, |s| s.expect(Token::Comma))
    }

    fn idn_list(&mut self) -> Parse<'a, Vec<&'a str>> {
        self.comma_list(|s| s.expect_idn())
    }

    fn generics(&mut self) -> Parse<'a, Vec<Type<'a>>> {
        if let Ok(_) = self.expect(Token::Lt) {
            let list = self.comma_list(|s| ok_or_null!(s.typ()))?;
            self.expect(Token::Gt)?;
            Ok(list)
        } else {
            Err(ParseErr::Null)
        }
    }

    fn param(&mut self) -> Parse<'a, (Type<'a>, &'a str)> {
        Ok((self.typ()?, self.expect_idn()?))
    }

    fn signature(&mut self) -> Parse<'a, Signature<'a>> {
        self.expect(Token::Sig)?;
        let name = self.expect_idn()?;
        let generics = self.generics().ok();
        self.expect(Token::OpenP)?;
        let params = self.idn_list()?;
        self.expect(Token::ClosedP)?;
        let ret = self.expect_idn()?;

        Ok(Signature { name, generics, params, ret })
    }

    fn record(&mut self) -> Parse<'a, Record<'a>> {
        self.expect(Token::Record)?;
        let name = self.expect_idn()?;
        self.expect(Token::OpenC)?;
        let params = self.comma_list(|s| s.param())?;
        self.expect(Token::ClosedC)?;

        Ok(Record { name, params })
    }

    fn import(&mut self) -> Parse<'a, Import<'a>> {
        self.expect(Token::Use)?;
        let def_path = self.del_list(|s| s.expect_idn(), |s| s.expect(Token::Colon))?;
        let method_flag = self.expect(Token::Dot).is_ok();
        self.expect(Token::From)?;
        let file_path = self.del_list(|s| s.expect_idn(), |s| s.expect(Token::Slash))?;
        Ok(Import { def_path, method_flag, file_path })
    }

    fn definition(&mut self) -> Parse<'a, Definition<'a>> {
        let definition = match self.iter.peek(0) {
            Some(Token::Sig) => Definition::Sig(self.signature()?),
            Some(Token::Record) => Definition::Record(self.record()?),
            Some(Token::Use) => Definition::Import(self.import()?),
            Some(Token::Impl) => Definition::Function(self.function()?),
            Some(Token::Module) => Definition::Program(Box::new(self.module()?)),
            _ => return Err(ParseErr::Null)
        };

        Ok(definition)
    }

    //technically, the term literal includes array & object literals, which themselves may contain identifiers.
    //integer, string, bool, etc...
    fn literal(&mut self) -> Parse<'a, Unit<'a>> {
        if let Ok(_) = self.expect(Token::OpenB) {
            let elements = self.comma_list(|s| ok_or_null!(s.expression()))?;
            self.expect(Token::ClosedB)?;
            return Ok(Unit::Array(Array { elements }))
        }

        let literal = self.expect(Token::Integer(""))
            .or(self.expect(Token::String("")))
            .or(self.expect(Token::True))
            .or(self.expect(Token::False))
            .or(Err(ParseErr::Null))?;

        Ok(literal.into())
    }

    fn s_call(&mut self) -> Parse<'a, Unit<'a>> {
        self.expect(Token::Call)?;
        self.expect(Token::OpenP)?;
        let id = self.expect_int()?;
        let args = if let Ok(_) = self.expect(Token::Comma) {
            self.comma_list(|s| s.expression())?
        } else {
            Vec::new()
        };
        self.expect(Token::ClosedP)?;

        Ok(Unit::SCall(SCall { id, args }))
    }

    fn path(&mut self) -> Parse<'a, Unit<'a>> {
        let mut identifier = Unit::Identifier(self.expect_idn().map_err(|_| ParseErr::Null)?);

        //provided we have an identifier, we may have a path
        if let Ok(_) = self.expect(Token::DoubleColon) {
            identifier = Unit::Path(self.del_list(|s| s.expect_idn(), |s| s.expect(Token::DoubleColon))?);
        }

        Ok(identifier)
    }

    fn trailing(&mut self, mut identifier: Unit<'a>) -> Parse<'a, Unit<'a>> {
        if let Ok(_) = self.expect(Token::OpenP) {
            identifier = Unit::Call(Call {
                expr: Box::new(identifier),
                args: self.comma_list(|s| s.expression())?,
            });
            self.expect(Token::ClosedP)?;
        } else if let Ok(_) = self.expect(Token::OpenB) {
            identifier = Unit::Index(Index {
                expr: Box::new(Expression::Unit(identifier)),
                value: Box::new(self.expression()?)
            });
            self.expect(Token::ClosedB)?;
        } else if let Ok(_) = self.expect(Token::Dot) {
            identifier = Unit::DotIndex(DotIndex {
                expr: Box::new(Expression::Unit(identifier)),
                index: self.expect_idn()?
            })
        } else {
            return Ok(identifier);
        }

        self.trailing(identifier)
    }

    fn constr_arg(&mut self) -> Parse<'a, (&'a str, Expression<'a>)> {
        let name = self.expect_idn()?;
        self.expect(Token::Eq)?;
        let expr = self.expression()?;
        Ok((name, expr))
    }

    fn unit(&mut self) -> Parse<'a, Unit<'a>> {
        //a literal is a valid unit
        if let Ok(literal) = self.literal() { return Ok(literal) }

        //a call is a valid unit
        if let Ok(s_call) = self.s_call() { return Ok(s_call) }

        //otherwise, we at least need an identifier/path
        let mut identifier = self.path()?;

        //if we have a path, we may have a construction
        if let Ok(_) = self.expect(Token::OpenC) {
            let result = Ok(Unit::Construction(Construction {
                path: identifier.to_path(),
                args: self.comma_list(|s| ok_or_null!(s.constr_arg()) )?
            }));
            self.expect(Token::ClosedC)?;
            return result
        }

        Ok(self.trailing(identifier)?)
    }

    fn op<Op, InnerOp>(
        &mut self,
        mut lhs: Option<(Token<'a>, Op)>,
        tokens: &[Token<'a>],
        inner: impl Fn(&mut Self, Option<InnerOp>) -> Parse<'a, InnerOp>
    ) -> Parse<'a, Op>
    where
        Op: From<InnerOp> + Operand<'a>
    {
        let (tok, lhs) = if let Some(lhs) = lhs {
            lhs
        } else {
            //no lhs, so parse it myself
            let expr = inner(self, None)?;

            //also parse the operator after it
            let mut operator = None;

            for tok in tokens {
                if let Ok(tok) = self.expect(*tok) {
                    operator = Some(tok);
                    break
                }
            };

            if let None = operator {
                return Ok(expr.into());
            }

            (operator.unwrap(), expr.into())
        };

        //rhs
        let rhs = inner(self, None)?;
        let operation = Op::operation(tok, lhs, rhs.into());

        //recurse
        for tok in tokens {
            if let Ok(_) = self.expect(*tok) {
                return self.op(Some((*tok, operation)), tokens, inner);
            }
        }

        Ok(operation)
    }

    fn product(&mut self, mut lhs: Option<Expression<'a>>) -> Parse<'a, Expression<'a>> {
        //product
        let operators = &[Token::Star][..];
        let a: Expression<'a> = self.op(None, operators, |s, lhs| s.unit())?;
        Ok(a.into())
    }

    fn sum(&mut self, mut lhs: Option<Expression<'a>>) -> Parse<'a, Expression<'a>> {
        //sum
        let operators = &[Token::Plus, Token::Minus][..];
        let a: Expression<'a> = self.op(None, operators, |s, lhs| s.product(None))?;
        Ok(a.into())
    }
    fn expression(&mut self) -> Parse<'a, Expression<'a>> {
        //sum
        let operators = &[Token::Lt, Token::Gt][..];
        let a: Expression<'a> = self.op(None, operators, |s, lhs| s.sum(None))?;
        Ok(a.into())
    }

    /*
    fn opt<T>(&mut self, production: impl Fn(&mut Self) -> Parse<'a, T>) -> Parse<'a, Option<T>> {

    }
     */

    fn typ(&mut self) -> Parse<'a, Type<'a>> {
        let name = self.expect_idn()?;
        let generics = self.generics().ok();


        Ok(Type { name, generics })
    }

    fn variable(&mut self) -> Parse<'a, Variable<'a>> {
        let typ = self.typ()?;
        let name = self.expect_idn()?;
        self.expect(Token::Eq)?;
        let expr = self.expression()?;

        Ok(Variable { typ, name, expr })
    }

    fn follow_up_statement(&mut self) -> Parse<'a, Statement<'a>> {
        let l_value = self.expression()?;
        let l_value = match l_value {
            Expression::Unit(unit) => match unit {
                Unit::Index(ind) => Unit::Index(ind),
                Unit::DotIndex(dot_ind) => Unit::DotIndex(dot_ind),
                Unit::Identifier(idn) => Unit::Identifier(idn),
                Unit::Path(path) => Unit::Path(path),
                Unit::Call(call) => return Ok(Statement::CallStatement(CallStatement { call })),
                _ => {
                    println!("no l_value1");
                    return Err(ExpectStmtExpr)
                }
            },
            _ => {
                println!("no l_value2");
                return Err(ExpectStmtExpr)
            }
        };

        //may e assignment or call
        if let Ok(_) = self.expect(Token::Eq) {
            let r_value = self.expression()?;
            Ok(Statement::Assignment(Assignment { l_value: Expression::Unit(l_value), r_value }))
        } else {
            Err(ExpectStmtExpr)
        }

    }

    fn ret(&mut self) -> Parse<'a, Return<'a>> {
        self.expect(Token::Return)?;
        let expr = self.expression()?;
        Ok(Return { expr })
    }

    fn block(&mut self) -> Parse<'a, Block<'a>> {
        self.expect(Token::OpenC)?;
        let statements = list!(ok_or_null!(self.statement()));
        self.expect(Token::ClosedC)?;
        Ok(Block { statements })
    }

    fn branch(&mut self) -> Parse<'a, (Expression<'a>, Block<'a>)> {
        Ok((self.expression()?, self.block()?))
    }

    fn if_stmt(&mut self) -> Parse<'a, If<'a>> {
        self.expect(Token::If)?;

        //branches
        let mut branches = vec![self.branch()?];
        while let Ok(_) = self.expect(Token::ElseIf) {
            branches.push(self.branch()?)
        }

        //else branch
        let else_branch = if let Ok(_) = self.expect(Token::Else) {
            Some(self.block()?)
        } else {
            None
        };

        Ok(If { branches, else_branch })
    }

    fn while_stmt(&mut self) -> Parse<'a, While<'a>> {
        self.expect(Token::While)?;
        let cond = self.expression()?;
        let block = self.block()?;

        Ok(While { cond, block })
    }

    fn statement(&mut self) -> Parse<'a, Statement<'a>> {
        let statement = if
            self.mtch(&[Token::Identifier(""), Token::Identifier(""), Token::Eq]) ||
            self.mtch(&[Token::Identifier(""), Token::Lt])
        {
            Statement::Variable(self.variable()?)
        } else if self.mtch(&[Token::Return]) {
            Statement::Return(self.ret()?)
        } else if self.expect_peek(Token::If).is_ok() {
            Statement::If(self.if_stmt()?)
        } else if self.expect_peek(Token::While).is_ok() {
            Statement::While(self.while_stmt()?)
        } else {
            self.follow_up_statement()?
        };

        Ok(statement)
    }

    fn function(&mut self) -> Parse<'a, Function<'a>> {
        self.expect(Token::Impl)?;
        let name = self.expect_idn()?;
        self.expect(Token::OpenP)?;
        let params = self.comma_list(|s| s.param())?;
        self.expect(Token::ClosedP)?;
        let return_typ = self.typ().ok();
        self.expect(Token::OpenC)?;
        let statements = list!(ok_or_null!(self.statement()));
        self.expect(Token::ClosedC)?;

        Ok(Function { name, params, statements, return_typ })
    }

    fn module(&mut self) -> Parse<'a, Program<'a>> {
        self.expect(Token::Module)?;
        let name = self.expect_idn()?;
        self.expect(Token::OpenC)?;
        let program = self.program(name)?;
        self.expect(Token::ClosedC)?;
        Ok(program)
    }

    fn program(&mut self, name: &'a str) -> Parse<'a, Program<'a>> {
        Ok(Program { name, definitions: list!(ok_or_null!(self.definition())) })
    }

    pub fn new(iter: I) -> Self {
        Parser {
            iter: Peek::from_iter(iter)
        }
    }

    pub fn execute(&mut self) -> Parse<'a, Program<'a>> {
        self.program("main")
    }
}