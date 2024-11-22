use std::collections::HashMap;
use std::iter::Map;
use std::slice::Iter;

use crate::syntax_analysis::{
    scanner::{Walker},
    ast::program::*,
    ast::expression::*
};

use crate::semantic_analysis::{
    scopes::{Scope, Scopes}
};


//size information, for a given type ( produced by type checker, used by ir generator )
#[derive(Debug, Clone)]
pub enum TypeMeta<'a> {
    Int, Bool,
    Arr(u8),
    Record(Vec<&'a str>)
}

impl<'a> TypeMeta<'a> {
    pub fn from(typ: &Type<'a>) -> Self {
        match typ.name {
            "int" => TypeMeta::Int,
            "bool" => TypeMeta::Bool,
            "Arr" => TypeMeta::Arr(TypeMeta::from(&typ.generics.as_ref().unwrap()[0]).size()),
            _ => TypeMeta::Record(vec![typ.name])
        }
    }

    pub fn from_expr(value: &Expression<'a>, scopes: &Scopes<'a>) -> Self {
        match value {
            Expression::Unit(unit) => match unit {
                Unit::Integer(_) => TypeMeta::Int,
                Unit::Bool(_) => TypeMeta::Bool,
                Unit::Array(_) => {
                    //TODO: we can figure this out by running from_value on the first argument.
                    //if the first argument is an array itself, we know the interior is 8
                    //if the first argument is missing, we don't know the type from the value. it must be inferred based on other information ( return err? )
                    TypeMeta::Arr(0) //for now we give array(0), cause the interior type size info isn't always needed
                },
                Unit::Identifier(identifier) => {
                    //we can find the size by looking in the scopes for an identifier
                    let (_, typ) = scopes.get(identifier).unwrap();
                    typ.clone()
                },
                _ => unimplemented!()
            },
            _ => unimplemented!()
        }
    }

    pub fn size(&self) -> u8 {
        use TypeMeta::*;
        match self {
            Bool => 1,
            Int => 4,
            _ => 8
        }
    }
}

#[derive(Debug)]
pub enum Literal {
    Integer(u32),
    Bool(bool)
}

#[derive(Debug)]
pub enum Value {
    Literal(Literal),
    Ptr(usize),
}

//in the future, i can impl a trait on value which uses an associated type to tell it the underlying datatype
//and generically use sizeof on that associated type.
impl Value {
    fn bytes_size(&self) -> (u8, Vec<u8>) {
        match self {
            Value::Literal(literal) => match literal {
                Literal::Integer(int) => (std::mem::size_of::<u32>() as u8, int.to_le_bytes().to_vec()),
                Literal::Bool(bool) => (1, vec![if *bool { 1 } else { 0 }])
            },
            Value::Ptr(ptr) => (std::mem::size_of::<usize>() as u8, ptr.to_le_bytes().to_vec())
        }
    }

}

#[derive(Debug)]
pub enum Instruction {
    Print,
    Push(Value),
    Pop(u8),
    Copy(i16, u8),
    Set(i16, u8),
    Jump(usize),
    CondJump(usize),
    Call(u8),
    Return(u8, u8),
    AddInt,
    SubInt,
    GtInt,
    LtInt,
    Alloc(u8),
    Index(u8),
    NewIndex(u8),
    OffsetIndex(u8),
    NewOffsetIndex(u8),
    MulInt,
    DivInt,
    AllocEmpty,
}

impl Instruction {
    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            Instruction::Print => vec![0],
            Instruction::Push(value) => {
                let mut bytes = vec![1];
                let (size, le_bytes) = value.bytes_size();
                bytes.extend_from_slice(&[size]);
                bytes.extend_from_slice(&le_bytes);
                bytes
            },
            Instruction::Pop(size) => {
                let mut bytes = vec![2];
                bytes.extend_from_slice(&size.to_le_bytes());
                bytes
            },
            Instruction::Copy(offset, size) => {
                let mut bytes = vec![3];
                bytes.extend_from_slice(&offset.to_le_bytes());
                bytes.extend_from_slice(&size.to_le_bytes());
                bytes
            },
            Instruction::Set(offset, size) => {
                let mut bytes = vec![4];
                bytes.extend_from_slice(&offset.to_le_bytes());
                bytes.extend_from_slice(&size.to_le_bytes());
                bytes
            },
            Instruction::Jump(ip) => {
                let mut bytes = vec![5];
                bytes.extend_from_slice(&ip.to_le_bytes());
                bytes
            },
            Instruction::CondJump(ip) => {
                let mut bytes = vec![6];
                bytes.extend_from_slice(&ip.to_le_bytes());
                bytes
            },
            Instruction::Call(size) => {
                let mut bytes = vec![7];
                bytes.extend_from_slice(&size.to_le_bytes());
                bytes
            },
            Instruction::Return(size, param_size) => {
                let mut bytes = vec![8];
                bytes.extend_from_slice(&size.to_le_bytes());
                bytes.extend_from_slice(&param_size.to_le_bytes());
                bytes
            },
            Instruction::AddInt => vec![9],
            Instruction::SubInt => vec![10],
            Instruction::GtInt => vec![11],
            Instruction::LtInt => vec![12],
            Instruction::Alloc(size) => {
                let mut bytes = vec![13];
                bytes.extend_from_slice(&size.to_le_bytes());
                bytes
            },
            Instruction::Index(size) => {
                let mut bytes = vec![14];
                bytes.extend_from_slice(&size.to_le_bytes());
                bytes
            },
            Instruction::NewIndex(size) => {
                let mut bytes = vec![15];
                bytes.extend_from_slice(&size.to_le_bytes());
                bytes
            },
            Instruction::OffsetIndex(size) => {
                let mut bytes = vec![16];
                bytes.extend_from_slice(&size.to_le_bytes());
                bytes
            },
            Instruction::NewOffsetIndex(size) => {
                let mut bytes = vec![17];
                bytes.extend_from_slice(&size.to_le_bytes());
                bytes
            },
            Instruction::MulInt => {
                let mut bytes = vec![18];
                bytes
            },
            Instruction::DivInt => {
                let mut bytes = vec![19];
                bytes
            },
            Instruction::AllocEmpty => {
                let mut bytes = vec![20];
                bytes
            },
        }
    }
}

//meta used for address resolution and the like
#[derive(Default, Debug)]
struct FunctionMeta {
    addr: usize,
    ret_size: u8,
    param_size: u8 //param size probably can't be a u8. imagine u have 32, 256 bit integer params,
    //that's 32 * 8, or size 256
}

#[derive(Default, Debug)]
struct FunctionMetaSpace<'a> {
    map: HashMap<Vec<&'a str>, FunctionMeta>
}

impl<'a> FunctionMetaSpace<'a> {
    fn get_f_meta(&self, path: &Vec<&'a str>) -> &FunctionMeta {
        self.map.get(path).unwrap()
    }
    fn get_addr(&self, path: &Vec<&'a str>) -> usize {
        self.get_f_meta(path).addr
    }
    fn get_ret_size(&self, path: &Vec<&'a str>) -> u8 {
        self.get_f_meta(path).ret_size
    }
    fn get_param_size(&self, path: &Vec<&'a str>) -> u8 {
        self.get_f_meta(path).param_size
    }
}

#[derive(Default, Debug)]
struct RecordMeta<'a> {
    scopes: Scopes<'a>
}

#[derive(Default, Debug)]
struct RecordMetaSpace<'a> {
    map: HashMap<Vec<&'a str>, RecordMeta<'a>>
}

impl<'a> RecordMetaSpace<'a> {
    fn get_idn(&self, path: &Vec<&'a str>, idn: &'a str) -> &(i16, TypeMeta) {
        self.map.get(path)
            .and_then(|record_meta| record_meta.scopes.get(idn))
            .expect(&format!("invalid record field {:?} for {:?}", idn, path))
    }

    fn get_size(&self, path: &Vec<&'a str>) -> i16 {
        self.map.get(path)
            .and_then(|record_meta| record_meta.scopes.scopes.last())
            .map(|v| v.bytes)
            .unwrap()
    }
}

#[derive(Default)]
struct CallResolution<'a> {
    buffer: Vec<(usize, Vec<&'a str>)>
}

impl<'a> CallResolution<'a> {
    fn queue(&mut self, ip: usize, path: Vec<&'a str>) {
        self.buffer.push((ip, path))
    }
}

pub struct IrGen<'a> {
    call_resolution: CallResolution<'a>,
    func_meta: FunctionMetaSpace<'a>,
    cw_path: Vec<&'a str>,
    scopes: Scopes<'a>,
    record_meta: RecordMetaSpace<'a>,
    pub instructions: Vec<Instruction>,
    pub bytecode: Vec<u8>,
    pub start: usize
}

impl<'a> IrGen<'a> {
    fn definitions<'b, 'c>(&'c mut self, list: &'b [Definition<'a>]) -> Map<Iter<'b, Definition<'a>>, impl FnMut(&'b Definition<'a>) -> &'b Definition<'a> + 'c> {
        list.iter().map(|v| { self.definition(v); v })
    }

    fn statements<'b, 'c>(&'c mut self, list: &'b [Statement<'a>]) -> Map<Iter<'b, Statement<'a>>, impl FnMut(&'b Statement<'a>) -> &'b Statement<'a> + 'c> {
        list.iter().map(|v| { self.statement(v); v })
    }

    pub fn program(&mut self, program: &Program<'a>) {
        if program.name != "main" { self.cw_path.push(program.name); }
        for definition in self.definitions(&program.definitions) {}
        if program.name != "main" { self.cw_path.pop(); }
    }

    fn definition(&mut self, definition: &Definition<'a>) {
        match definition {
            Definition::Function(f) => self.function(f),
            Definition::Program(m) => self.program(m),
            Definition::Record(r) => self.record(r),
            _ => {}
        };
    }

    fn record(&mut self, record: &Record<'a>) {
        //populate record meta
        //in future this is done during static analysis
        self.cw_path.push(record.name);
        let mut scopes = Scopes::default();
        scopes.push();
        for (typ, idn) in record.params.iter() { scopes.insert(idn, TypeMeta::from(&typ)) }
        self.record_meta.map.insert(self.cw_path.clone(), RecordMeta { scopes });
        self.cw_path.pop();
    }

    fn function(&mut self, function: &Function<'a>) {
        self.cw_path.push(function.name);
        self.scopes.push();

        //
        let ret_size: u8 = function.return_typ.as_ref().map(|r| TypeMeta::from(r)).unwrap_or(TypeMeta::Int).size();
        let param_size: u8 = function.params.iter().map(|(typ, idn)|
            TypeMeta::from(typ).size()
        ).sum();

        //push each parameter to scope
        let param_start: i16 = -(param_size as i16) -16 -ret_size as i16;
        self.scopes.scopes.last_mut().unwrap().bytes = param_start;
        for (typ, id) in function.params.iter() {
            self.scopes.insert(id, TypeMeta::from(typ));
        }
        self.scopes.scopes.last_mut().unwrap().bytes = 0;

        //function meta
        self.func_meta.map.insert(self.cw_path.clone(), FunctionMeta {
            addr: self.bytecode.len(), ret_size, param_size
        });

        //handle statements
        for statement in self.statements(&function.statements) {

        }

        self.scopes.pop();
        self.cw_path.pop();
    }

    fn statement(&mut self, statement: &Statement<'a>) {
        match statement {
            Statement::Variable(variable) => self.variable(variable),
            Statement::Return(ret) => self.ret(ret),
            Statement::If(if_stmt) => self.if_stmt(if_stmt),
            Statement::While(while_stmt) => self.while_stmt(while_stmt),
            Statement::Assignment(assignment) => self.assignment(assignment),
            Statement::CallStatement(call_statement) => self.call_statement(call_statement)
        }
    }

    fn variable(&mut self, variable: &Variable<'a>) {
        self.expression(&variable.expr);
        self.scopes.insert(variable.name, TypeMeta::from(&variable.typ));
    }

    fn call_statement(&mut self, statement: &CallStatement<'a>) {
        //in the future, i must resolve the expr of a call statement if it is not an identifier/path
        self.call(&statement.call);

        //again, presuming it's a path
        let call_statement_path = statement.call.expr.as_ref().to_path();
        let ret_size = self.func_meta.get_ret_size(&call_statement_path);
        self.push_instruction(Instruction::Pop(ret_size));
    }

    fn assignment(&mut self, assignment: &Assignment<'a>) {
        match &assignment.l_value {
            Expression::Unit(Unit::Identifier(name)) => {
                self.expression(&assignment.r_value);
                let (loc, typ) = self.scopes.get(name).unwrap();
                self.push_instruction(Instruction::Set(*loc, typ.size()));
            },
            Expression::Unit(Unit::Index(index)) => {
                self.expression(&index.expr);
                self.expression(&index.value);
                self.expression(&assignment.r_value);
                self.push_instruction(Instruction::NewIndex(TypeMeta::Int.size()));
            },
            Expression::Unit(Unit::DotIndex(index)) => {
                self.expression(&index.expr);

                //push offset
                let typ = TypeMeta::from_expr(&index.expr, &self.scopes);
                let path = if let TypeMeta::Record(path) = typ { path } else { panic!("") };
                let (loc, typ) = self.record_meta.get_idn(&path, index.index);
                let size = typ.size();
                self.push_instruction(Instruction::Push(Value::Literal(Literal::Integer(*loc as u32))));

                //value
                self.expression(&assignment.r_value);
                self.push_instruction(Instruction::NewOffsetIndex(size));
            }
            _ => unimplemented!()
        }


    }

    fn ret(&mut self, ret: &Return<'a>) {
        self.expression(&ret.expr);
        let param_size = self.func_meta.get_param_size(&self.cw_path);
        let return_typ_size = self.func_meta.get_ret_size(&self.cw_path);
        self.push_instruction(Instruction::Return(return_typ_size, param_size))
    }

    fn if_stmt(&mut self, if_stmt: &If<'a>) {
        let mut last_ip = 0;
        let mut end_ips = Vec::new();

        for (cond, branch) in &if_stmt.branches {
            self.expression(cond);
            last_ip = self.bytecode.len() + 1; //this is the location where the ptr for the jump is stored
            self.push_instruction(Instruction::CondJump(0)); //jump if top of stack is tre
            self.block(branch);

            //push jump to end instruction, and store the ip so that it can be correctly updated later
            end_ips.push(self.bytecode.len()+1);
            self.push_instruction(Instruction::Jump(0));

            //update the cond jump instruction to jump to after the completion of this block
            for (i, v) in self.bytecode.len().to_le_bytes().into_iter().enumerate() {
                self.bytecode[last_ip+i] = v;
            }
        }

        if let Some(branch) = &if_stmt.else_branch {
            self.block(branch);
        }

        //update the end ips to point to the end of the if statement
        for ip in end_ips.into_iter() {
            let buffer = self.bytecode.len().to_le_bytes();
            for (i, v) in buffer.iter().enumerate() {
                self.bytecode[ip+i] = *v;
            }
        }

    }

    fn while_stmt(&mut self, if_stmt: &While<'a>) {
        let start = self.bytecode.len();
        self.expression(&if_stmt.cond);
        let ip = self.bytecode.len() + 1; //this is the location where the ptr for the jump is stored
        self.push_instruction(Instruction::CondJump(0)); //jump if top of stack is tre
        self.block(&if_stmt.block);
        self.push_instruction(Instruction::Jump(start));
        //make the cond jump actually jump past the statements that have been pushed
        for (i, v) in self.bytecode.len().to_le_bytes().into_iter().enumerate() {
            self.bytecode[ip+i] = v;
        }
    }

    fn block(&mut self, block: &Block<'a>) {
        self.statements(&block.statements).for_each(|_|{});
    }
    fn expression(&mut self, expression: &Expression<'a>) {
        match expression {
            Expression::Unit(unit) => match unit {
                Unit::Identifier(idn) => self.identifier(idn),
                Unit::Integer(num) => self.number(num),
                Unit::Bool(bool) => self.bool(*bool),
                Unit::Call(call) => self.call(call),
                Unit::SCall(call) => self.s_call(call),
                Unit::Array(array) => self.array(array),
                Unit::Index(index) => self.index(index),
                Unit::DotIndex(dot_index) => self.dot_index(dot_index),
                Unit::Construction(construction) => self.construction(construction),

                _ => {}
            },
            _ => {}
        };
    }
    fn s_call(&mut self, s_call: &SCall<'a>) {
        let size = 8;
        self.expressions(&s_call.args).for_each(|_|{});

        match s_call.id {
            1 => self.push_instruction(Instruction::AddInt),
            2 => self.push_instruction(Instruction::SubInt),
            3 => self.push_instruction(Instruction::MulInt),
            4 => self.push_instruction(Instruction::DivInt),
            5 => self.push_instruction(Instruction::LtInt),
            6 => self.push_instruction(Instruction::GtInt),
            7 => self.push_instruction(Instruction::Print),
            8 => self.push_instruction(Instruction::AllocEmpty),
            _ => unimplemented!()
        }
    }

    fn construction(&mut self, construction: &Construction<'a>) {
        //expressions
        for (_, arg) in construction.args.iter(){
            self.expression(arg);
        }

        let size = self.record_meta.get_size(&construction.path);
        self.push_instruction(Instruction::Alloc(size as u8));
    }

    fn expressions<'b, 'c>(&'c mut self, list: &'b [Expression<'a>]) -> Map<Iter<'b, Expression<'a>>, impl FnMut(&'b Expression<'a>) -> &'b Expression<'a> + 'c> {
        list.iter().map(|v| { self.expression(v); v })
    }

    fn call(&mut self, call: &Call<'a>) {
        //args
        self.expressions(&call.args).for_each(|_|{});

        //call resolution
        //for now we can assume that the lhs of all calls are paths & identifiers
        //expressions can be handled whenever i implement callbacks
        self.call_resolution.queue(self.bytecode.len(), (&call.expr).to_path().clone());

        //call instruction
        self.push_instruction(Instruction::Push(Value::Ptr(0)));
        self.push_instruction(Instruction::Call(0));
    }

    fn array(&mut self, array: &Array<'a>) {
        //args
        /*
            additionally, static analysis may explicitly provide me with information on the type of every literal
            the way static analysis figures it out is:
            1. if we're directly in an assignment, check the type of the variable
            2. if we're directly in an argument, check the type of the function
            3. if we're directly in a return statement, check the type of the enclosing function
         */
        let mut array_element_size = 4; //until static analysis is started, we'll just assume it's an int
        let array_size = array_element_size * array.elements.len();

        //expressions
        for expression in self.expressions(&array.elements) { }

        self.push_instruction(Instruction::Alloc(array_size as u8))
    }

    fn index(&mut self, index: &Index<'a>) {
        self.expression(&index.expr);
        self.expression(&index.value);
        self.push_instruction(Instruction::Index(4))
    }

    fn dot_index(&mut self, index: &DotIndex<'a>) {
        let typ = TypeMeta::from_expr(&index.expr, &self.scopes);

        if let TypeMeta::Record(path) = typ {
            self.expression(&index.expr);
            let (loc, typ) = self.record_meta.get_idn(&path, index.index);
            let size = typ.size();
            self.push_instruction(Instruction::Push(Value::Literal(Literal::Integer(*loc as u32))));
            self.push_instruction(Instruction::OffsetIndex(size))
        } else {
            panic!("index into non-record?")
        }
    }

    fn identifier(&mut self, idn: &'a str) {
        let (loc, typ) = self.scopes.get(idn).unwrap(); //type checker prevents err
        let size = typ.size();
        self.push_instruction(Instruction::Copy(*loc, size))
    }

    fn number(&mut self, number: &'a str) {
        self.push_instruction(Instruction::Push(Value::Literal(Literal::Integer(number.parse::<u32>().unwrap()))))
    }

    fn bool(&mut self, bool: bool) {
        self.push_instruction(Instruction::Push(Value::Literal(Literal::Bool(bool))))
    }
    pub fn new() -> Self {
        Self {
            call_resolution: CallResolution::default(),
            func_meta: FunctionMetaSpace::default(),
            record_meta: RecordMetaSpace::default(),
            cw_path: Vec::new(),
            scopes: Scopes::default(),
            bytecode: Vec::new(),
            instructions: Vec::new(),
            start: 0,
        }
    }

    pub fn execute(&mut self, program: &Program<'a>) {
        self.program(program);

        //call main function
        //handle call resolutions
        for (ip, path) in &self.call_resolution.buffer {
            //resolve the call address
            let addr = self.func_meta.get_addr(&path).to_le_bytes();
            let call_addr_loc = ip + 2;
            for i in 0..8 { self.bytecode[call_addr_loc+i] = addr[i]; }

            //resolve the return type size
            let ret_size = self.func_meta.get_ret_size(&path);
            let ret_size_addr_loc = ip + 2 + 8 + 1;
            self.bytecode[ret_size_addr_loc] = ret_size;

        }

        //push main()
        self.start = self.bytecode.len();
        let main_path = &vec!["main"];
        let return_size = self.func_meta.get_ret_size(main_path);
        self.push_instruction(Instruction::Push(Value::Ptr(self.func_meta.get_addr(main_path))));
        self.push_instruction(Instruction::Call(return_size));
        self.push_instruction(Instruction::Pop(return_size));

    }

    fn push_instruction(&mut self, instruction: Instruction) {
        self.bytecode.extend_from_slice(&instruction.to_bytes());
        self.instructions.push(instruction)
    }
}
