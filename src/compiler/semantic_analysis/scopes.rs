use std::collections::HashMap;
use crate::semantic_analysis::ir_gen::TypeMeta;
//scopes

#[derive(Default, Debug)]
pub struct Scope<'a> {
    pub bytes: i16,
    pub map: HashMap<&'a str, (i16, TypeMeta<'a>)>
}

#[derive(Default, Debug)]
pub struct Scopes<'a> {
    pub scopes: Vec<Scope<'a>>
}
impl<'a> Scopes<'a> {
    pub fn insert(&mut self, name: &'a str, typ: TypeMeta<'a>) {
        let cur = self.scopes.last_mut().unwrap();
        let size = typ.size();
        cur.map.insert(name, (cur.bytes, typ));
        cur.bytes += size as i16;
    }
    pub fn get(&self, idn: &str) -> Option<&(i16, TypeMeta<'a>)> {
        self.scopes.last().unwrap().map.get(idn)
    }
    pub fn push(&mut self) {
        self.scopes.push(Scope::default())
    }
    pub fn pop(&mut self) {
        self.scopes.pop();
    }
}
