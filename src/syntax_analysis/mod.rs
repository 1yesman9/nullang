use crate::util::iter::Peek;
use crate::lexical_analysis::Token;

pub mod ast;
use ast::program::Program;

pub struct Parser<I: Iterator<Item=Token>> {
    tokens: Peek<I, 1>
}

impl<'a, I: Iterator<Item=Token>> Parser<I> {
    fn program() -> Program<'a> {

    }

    fn new(tokens: &[Token<'a>]) -> Self {
        Parser {
            tokens: Peek::from_iter(tokens.iter())
        }
    }
}