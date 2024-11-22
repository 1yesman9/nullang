//
mod token;

//
pub use token::Token;
use crate::util::{iter::Peek, tree::Trie};


pub struct Lexer<'a> {
    source: &'a str,
    iter: Peek<std::str::CharIndices<'a>, 2>,
    start: usize,
    end: usize
}

//
fn is_ident_lead(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

impl<'a> Lexer<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            source: source_code,
            iter: Peek::from_iter(source_code.char_indices()),
            start: 0,
            end: 0
        }
    }

    fn peek_is(&self, predicate: &impl Fn(char) -> bool) -> bool {
        self.iter.peek(0).copied().is_some_and(|(_, c)| predicate(c))
    }

    fn match_str(&self, string: &str) -> bool {
        for (i, c) in string.chars().enumerate() {
            if !self.iter.peek(i).is_some_and(|(_, target)| *target == c) { return false }
        }
        true
    }

    fn next(&mut self) {
        self.end = self.iter.next().unwrap().0;
    }

    fn consume(&mut self, n: usize) {
        for i in 0..n {
            self.next()
        }
    }

    fn consume_while(&mut self, predicate: impl Fn(char) -> bool) {
        while self.peek_is(&predicate) {
           self.next()
        }
    }

    fn parse(&mut self, predicate: impl Fn(char) -> bool) {
        self.next();
        self.consume_while(predicate)
    }

    fn substring(&self) -> &'a str {
        &self.source[self.start..=self.end]
    }

    pub fn execute(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        let symbol_trie = Token::symbols();
        let keyword_map = Token::keywords();

        while let Some((index, char)) = self.iter.peek(0).copied() {
            self.start = index;

            //parse integer
            if is_ident_lead(char) {
                self.parse(|c| is_ident_lead(c) || c.is_ascii_digit());
                tokens.push(keyword_map.get(self.substring()).copied().unwrap_or(Token::Identifier(self.substring())));

            } else if char.is_ascii_digit() {
                self.parse(|c| c.is_ascii_digit());
                tokens.push(Token::Integer(self.substring()));

            } else if self.match_str("//") {
                self.next();
                self.parse(|c| c != '\n' && c != '\r');

            } else if self.match_str("/*") {
                self.consume(2);
                while self.iter.peek(0).is_some() && !self.match_str("*/") { self.next() }
                if self.iter.peek(0).is_some() { self.consume(2) }

            } else if char == '"' || char == '\'' {
                self.parse(|c| c != char && c != '\n' && c != '\r');
                if self.peek_is(&|c| c == char) {
                    self.next();
                    tokens.push(Token::String(self.substring()));
                }

            } else if symbol_trie.children.contains_key(&char) {
                let mut trie_ref = &symbol_trie;
                while let Some((_, c)) = self.iter.peek(0).copied().filter(|(_, c)| trie_ref.children.contains_key(&c)) {
                    self.next();
                    trie_ref = trie_ref.children.get(&c).unwrap();
                }
                if let Some(tok) = trie_ref.value { tokens.push(tok); }

            } else {
                self.iter.next();

            }
        }

        tokens
    }
}