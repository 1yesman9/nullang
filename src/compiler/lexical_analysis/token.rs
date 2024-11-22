use std::collections::HashMap;
use crate::util::tree::Trie;

#[derive(Debug, Copy, Clone)]
pub enum Token<'a> {
    Integer(&'a str),
    String(&'a str),
    Identifier(&'a str),
    Comment(&'a str),

    //Keywords
    Module, Sig, Record, F, Impl, Use, //def
    From,
    If, While, ElseIf, Else,
    True, False,
    Return, Call,

    //Delimeter
    OpenP, ClosedP, OpenC, ClosedC, OpenB, ClosedB,
    Comma,
    DoubleColon, Colon, Dot,

    //Math
    Lt, Gt, Plus, Minus, Star, Slash,

    //Assignment
    Eq,

    //
    Eof
}

impl<'a> Token<'a> {
    pub fn symbols() -> Trie<Token<'a>> {
        (&[
            //Delimeter
            ("(", Token::OpenP), (")", Token::ClosedP), ("{", Token::OpenC), ("}", Token::ClosedC),
            ("[", Token::OpenB), ("]", Token::ClosedB),

            (",", Token::Comma),
            (":", Token::Colon), ("::", Token::DoubleColon), (".", Token::Dot),

            //Math
            ("<", Token::Lt), (">", Token::Gt),
            ("+", Token::Plus), ("-", Token::Minus),
            ("*", Token::Star), ("/", Token::Slash),

            //Assignment
            ("=", Token::Eq)
        ][..]).into()
    }

    pub fn keywords() -> HashMap<&'a str, Token<'a>> {
        let mut keywords = HashMap::new();
        keywords.insert("module", Token::Module);
        keywords.insert("sig", Token::Sig);
        keywords.insert("record", Token::Record);
        keywords.insert("f", Token::F);
        keywords.insert("impl", Token::Impl);
        keywords.insert("use", Token::Use);

        keywords.insert("from", Token::From);

        keywords.insert("if", Token::If);
        keywords.insert("elseif", Token::ElseIf);
        keywords.insert("else", Token::Else);
        keywords.insert("while", Token::While);

        keywords.insert("true", Token::True);
        keywords.insert("false", Token::False);

        keywords.insert("return", Token::Return);
        keywords.insert("CALL", Token::Call);
        keywords
    }

    pub fn inner_idn(self) -> Result<&'a str, &'static str> {
        if let Token::Identifier(inner) = self {
            Ok(inner)
        } else {
            Err("ran inner identifier on non-identifier token")
        }
    }

    pub fn inner_int(self) -> Result<u64, &'static str> {
        if let Token::Integer(inner) = self {
            Ok(inner.parse::<u64>().map_err(|_| "expected int")?)
        } else {
            Err("ran inner integer on non-integer token")
        }
    }
}