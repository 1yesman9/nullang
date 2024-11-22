#![feature(associated_type_defaults)]

mod util;
mod compiler;
mod virtualmachine;

use compiler::{syntax_analysis, semantic_analysis, lexical_analysis};
use syntax_analysis::{scanner::{Walker}, ast::program::Program};
use syntax_analysis::examplewalker::Example;
use semantic_analysis::ir_gen::IrGen;
use util::arena::Arena;
use virtualmachine::objects::Objects;
use crate::virtualmachine::objects::Object;

fn main() {
    let std = std::fs::read_to_string("std.nul").unwrap();
    let source_code = std::fs::read_to_string("example.nul").unwrap();
    let source_code = format!("{}\n{}", std, source_code);
    //println!("{:}", source_code);

    //lexical analysis
    let mut lexer = lexical_analysis::Lexer::new(&source_code);
    let tokens = lexer.execute();
    //println!("{:?}", tokens);

    //syntax analysis
    let mut parser = syntax_analysis::Parser::new(tokens.into_iter());
    let ast = parser.execute().unwrap();

    //desugaring ( idk what to call this phase, or what broader phase it should be apart of
    let mut example = Example;
    let new_ast = example.program(ast);
    //println!("{:#?}", new_ast);

    //ir generation
    let mut ir_generator = IrGen::new();
    ir_generator.execute(&new_ast);
    let bytecode = &ir_generator.bytecode;

    //println!("{:?}", ir_generator.instructions);
    //println!("{:?}", ir_generator.bytecode);


    let mut vm = virtualmachine::VirtualMachine::new();
    vm.execute(&bytecode, ir_generator.start).unwrap();


}