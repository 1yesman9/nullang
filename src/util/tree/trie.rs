use std::collections::HashMap;

use crate::lexical_analysis::Token;

#[derive(Debug)]
pub struct Trie<V> {
    pub value: Option<V>,
    pub children: HashMap<char, Trie<V>>
}

impl<'a> std::convert::From<&[(&'a str, Token<'a>)]> for Trie<Token<'a>> {
    fn from(value: &[(&'a str, Token<'a>)]) -> Self {
        let mut trie = Trie {
            value: None,
            children: HashMap::new()
        };


        for (string, tok) in value {
            let mut trie_ref = &mut trie;

            let len = string.len();
            for (index, char) in string.chars().enumerate() {
                if !trie_ref.children.contains_key(&char) {
                    trie_ref.children.insert(char, Trie {
                        value: None,
                        children: HashMap::with_capacity(len-index-1)
                    });
                };

                trie_ref = trie_ref.get_mut(char).unwrap();
                if index == len-1 { trie_ref.value = Some(*tok) }
            }
        }

        trie
    }
}

impl<V> Trie<V> {
    fn get_mut(&mut self, c: char) -> Option<&mut Trie<V>> {
        self.children.get_mut(&c)
    }
}