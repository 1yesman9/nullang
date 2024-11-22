use crate::syntax_analysis::{
    scanner::Walker,
    ast::expression::{Unit, Call}
};
use crate::syntax_analysis::ast::expression::{Expression};

pub struct Example;

impl<'a> Walker<'a> for Example {
    fn expression(&mut self, node: Expression<'a>) -> Expression<'a> {
        match node {
            Expression::Unit(unit) => Expression::Unit(self.unit(unit)),
            Expression::BinOp(binop) => {
                //binary operator desugaring: 1 + 1 -> add(1, 1)
                let binop = self.binop(*binop);
                Expression::Unit(Unit::Call(Call {
                    expr: Box::new(Unit::Path(vec!["std","ops",binop.name()])),
                    args: vec![binop.lhs, binop.rhs]
                }))
            }
        }
    }
}