use crate::compiler::syntax_analysis::ast::program::CallStatement;
use crate::syntax_analysis::ast::expression::{
    BinOp, Call, SCall, Expression, Unit, Array, Index, DotIndex, Construction
};
use crate::syntax_analysis::ast::program::{
    Definition, Function, Program, Statement,
    Variable, Assignment, Return, If, While, Block
};

pub trait Walker<'a>: Sized {
    fn program(&mut self, node: Program<'a>) -> Program<'a> {
        Program {
            name: node.name,
            definitions: node.definitions.into_iter().map(|v| self.definition(v)).collect()
        }
    }
    fn definition(&mut self, node: Definition<'a>) -> Definition<'a> {
        match node {
            Definition::Function(f) => Definition::Function(Self::function(self, f)),
            Definition::Program(module) => Definition::Program(Box::new(Self::program(self, *module))),
            a => a
        }
    }

    fn function(&mut self, node: Function<'a>) -> Function<'a> {
        Function {
            name: node.name,
            params: node.params,
            statements: node.statements.into_iter().map(|v| Self::statement(self, v)).collect(),
            return_typ: node.return_typ
        }
    }
    fn statement(&mut self, node: Statement<'a>) -> Statement<'a> {
        match node {
            Statement::Variable(variable) => Statement::Variable(Self::variable(self, variable)),
            Statement::Return(ret) => Statement::Return(Self::ret(self, ret)),
            Statement::If(if_stmt) => Statement::If(self.if_stmt(if_stmt)),
            Statement::While(while_stmt) => Statement::While(self.while_stmt(while_stmt)),
            Statement::Assignment(assignment) => Statement::Assignment(self.assignment(assignment)),
            Statement::CallStatement(call_statement) => Statement::CallStatement(self.call_statement(call_statement))
        }
    }

    fn if_stmt(&mut self, node: If<'a>) -> If<'a> {
        If {
            branches: node.branches.into_iter().map(|(cond, branch)| (self.expression(cond), self.block(branch))).collect(),
            else_branch: node.else_branch.map(|branch| self.block(branch))
        }
    }

    fn while_stmt(&mut self, node: While<'a>) -> While<'a> {
        While {
            cond: self.expression(node.cond),
            block: self.block(node.block)
        }
    }

    fn block(&mut self, node: Block<'a>) -> Block<'a> {
        Block {
            statements: self.statements(node.statements).collect()
        }
    }

    fn call_statement(&mut self, node: CallStatement<'a>) -> CallStatement<'a> {
        CallStatement {
            call: self.call(node.call)
        }
    }

    fn statements(&mut self, list: Vec<Statement<'a>>) -> impl Iterator<Item=Statement<'a>> {
        list.into_iter().map(|v| self.statement(v))
    }

    fn expressions(&mut self, list: Vec<Expression<'a>>) -> impl Iterator<Item=Expression<'a>> {
        list.into_iter().map(|v| self.expression(v))
    }

    fn variable(&mut self, node: Variable<'a>) -> Variable<'a> {
        Variable {
            name: node.name,
            typ: node.typ,
            expr: Self::expression(self, node.expr)
        }
    }

    fn assignment(&mut self, node: Assignment<'a>) -> Assignment<'a> {
        Assignment {
            l_value: self.expression(node.l_value), //maybe l_values shouldn't be desugared?
            r_value: Self::expression(self, node.r_value)
        }
    }

    fn ret(&mut self, node: Return<'a>) -> Return<'a> {
        Return {
            expr: Self::expression(self, node.expr)
        }
    }
    fn expression(&mut self, node: Expression<'a>) -> Expression<'a> {
        match node {
            Expression::Unit(unit) => Expression::Unit(Self::unit(self, unit)),
            Expression::BinOp(binop) => Expression::BinOp(Box::new(Self::binop(self, *binop)))
        }
    }

    fn binop(&mut self, node: BinOp<'a>) -> BinOp<'a> {
        BinOp {
            op: node.op,
            lhs: Self::expression(self, node.lhs),
            rhs: Self::expression(self, node.rhs),
        }
    }

    fn unit(&mut self, node: Unit<'a>) -> Unit<'a> {
        match node {
            Unit::Call(call) => Unit::Call(self.call(call)),
            Unit::Identifier(idn) => Unit::Identifier(self.identifier(idn)),
            Unit::SCall(s_call) => Unit::SCall(self.s_call(s_call)),
            Unit::Array(array) => Unit::Array(self.array(array)),
            Unit::Index(index) => Unit::Index(self.index(index)),
            Unit::DotIndex(dot_index) => Unit::DotIndex(self.dot_index(dot_index)),
            a => a
        }
    }

    fn call(&mut self, node: Call<'a>) -> Call<'a> {
        Call {
            expr: node.expr,
            args: self.expressions(node.args).collect()
        }
    }
    fn s_call(&mut self, node: SCall<'a>) -> SCall<'a> {
        SCall {
            id: node.id,
            args: node.args.into_iter().map(|v| self.expression(v)).collect()
        }
    }

    fn array(&mut self, node: Array<'a>) -> Array<'a> {
        Array {
            elements: self.expressions(node.elements).collect()
        }
    }

    fn index(&mut self, node: Index<'a>) -> Index<'a> {
        Index {
            expr: Box::new(self.expression(*node.expr)),
            value: Box::new(self.expression(*node.value))
        }
    }

    fn dot_index(&mut self, node: DotIndex<'a>) -> DotIndex<'a> {
        DotIndex {
            expr: Box::new(self.expression(*node.expr)),
            index: node.index
        }
    }

    fn construction(&mut self, node: Construction<'a>) -> Construction<'a> {
        Construction {
            path: node.path,
            args: node.args.into_iter().map(|(idn, expr)| (idn, self.expression(expr))).collect()
        }
    }

    fn identifier(&mut self, idn: &'a str) -> &'a str { idn }
}