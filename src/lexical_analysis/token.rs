use std::collections::HashMap;
use crate::util::tree::Trie;

#[derive(Debug, Copy, Clone)]
pub enum Token<'a> {
    Integer(&'a str),
    String(&'a str),
    Identifier(&'a str),
    Comment(&'a str),

    //Keywords
    Sig, Record, F, Impl, Use, //def
    From,
    Int, //types

    //Delimeter
    OpenP, ClosedP, OpenC, ClosedC,
    Comma,
    Colon, Dot,

    //Math
    Lt, Gt, Plus,

    //Assignment
    Eq
}

impl<'a> Token<'a> {
    pub fn symbols() -> Trie<Token<'a>> {
        (&[
            //Delimeter
            ("(", Token::OpenP), (")", Token::ClosedP), ("{", Token::OpenC), ("}", Token::ClosedC),
            (",", Token::Comma),
            (":", Token::Colon), (".", Token::Dot),

            //Math
            ("<", Token::Lt), (">", Token::Gt), ("+", Token::Plus),

            //Assignment
            ("=", Token::Eq)
        ][..]).into()
    }

    pub fn keywords() -> HashMap<&'a str, Token<'a>> {
        let mut keywords = HashMap::new();
        keywords.insert("sig", Token::Sig);
        keywords.insert("record", Token::Record);
        keywords.insert("f", Token::F);
        keywords.insert("impl", Token::Impl);
        keywords.insert("use", Token::Use);

        keywords.insert("from", Token::From);

        keywords.insert("int", Token::Int);
        keywords
    }
}