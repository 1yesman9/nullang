pub mod objects;
use objects::Objects;

use std::time::Instant;

/*
    Our stack is an array of bytes.
    Primitive values are stored in little-endian

    Variables and array elements are stored in the order they're defined

    Primitives:
        int: 4 byte number
        void: 1 byte zero
        ptr<T>: 8 byte number

    Composite Types:
        Array<T>: contiguous sequence of T
        Struct<Tn,Kn,...>: 1 instance of each type, contiguously laid in memory

   NEW 5

   value
   key
   ptr
   NEWINDEX SIZE
*/

use crate::compiler::semantic_analysis::ir_gen::Instruction;

const PTR_SIZE: usize = std::mem::size_of::<usize>();
const MAX_STACK: usize = 10000;

//
fn le_bytes_to_usize(bytes: &[u8]) -> usize {
    let mut buffer = [0; 8];
    bytes.into_iter().enumerate().for_each(|(i, v)| buffer[i] = *v);
    usize::from_le_bytes(buffer)
}

#[derive(Debug)]
pub struct RuntimeError {
    msg: String
}

impl RuntimeError {
    fn new(msg: String) -> Self {
        Self { msg }
    }
}

struct InstructionParser<'a> {
    ip: usize,
    bytecode: &'a [u8],
    bytes_consumed: usize
}

impl<'a> InstructionParser<'a> {
    fn new(ip: usize, bytecode: &'a [u8]) -> Self {
        Self { ip, bytecode, bytes_consumed: 0 }
    }

    fn consume(&mut self, size: usize) -> &'a [u8] {
        let result = &self.bytecode[self.ip..self.ip+size];
        self.ip += size;
        self.bytes_consumed += size;
        result
    }

    fn consume_ptr(&mut self) -> usize {
        le_bytes_to_usize(self.consume(PTR_SIZE))
    }
}

pub struct VirtualMachine {
    sp: usize,
    ip: usize,
    bp: usize,
    pub stack: [u8; MAX_STACK],
    objects: Objects
}


impl VirtualMachine {
    pub fn new() -> Self {
        Self {
            ip: 0,
            sp: 0,
            bp: 0,
            stack: [0u8; MAX_STACK],
            objects: Objects::new()
        }
    }

    //benchmarking
    fn get(&mut self, size: usize) -> Result<&[u8], RuntimeError> {
        self.stack.get(self.sp..self.sp + size).ok_or_else(|| RuntimeError::new(String::from("out of bounds stack get")))
    }

    //instructions
    fn push(&mut self, bytes: &[u8]) -> Result<bool, RuntimeError> {
        for byte in bytes {
            //println!("{:?}", self.sp);
            let location = self.stack.get_mut(self.sp).ok_or_else(|| RuntimeError::new(String::from("stack overflow")))?;
            *location = *byte;
            self.sp += 1;
        }
        //println!("pushed {:?}", bytes);
        Ok(true)
    }

    fn copy(&mut self, offset: i16, size: u8) -> Result<bool, RuntimeError> {
        let size = size as usize;
        let base = (self.bp as i64 + offset as i64) as usize;
        let (mut left, mut right) = self.stack.split_at_mut(self.sp);
        &mut right[0..size].copy_from_slice(&left[base..base+size]);
        self.sp += size;
        Ok(true)
    }

    fn set(&mut self, offset: i16, size: u8) -> Result<bool, RuntimeError> {
        let size = size as usize;
        let base = (self.bp as i64 + offset as i64) as usize;

        //feed what we pop from the stack into the place we're setting
        let start = self.sp-size;
        for i in 0..size {
            self.stack[base+i] = self.stack[start+i];
            self.sp -= 1;
        }

        //println!("setting {:?} to {:?}", base, &self.stack[start..start+size]);

        Ok(true)
    }

    fn pop(&mut self, container: &mut [u8], size: usize)  {
        container.copy_from_slice(&self.stack[self.sp-size..self.sp]);
        self.sp = self.sp - size;
    }

    //don't support this in future as an optimization
    fn pop_dyn(&mut self, size: usize) -> Vec<u8> {
        let mut v = Vec::with_capacity(size);
        for i in 0..size { v.push(0) }
        self.pop(&mut v[..], size);
        v
    }

    fn read(&mut self, container: &mut [u8], offset: isize, size: usize)  {
        for i in 0..size {
            container[i] = self.stack[(self.bp as isize+offset) as usize + i];
        }
    }

    fn call(&mut self, size: u8) -> Result<bool, RuntimeError> {
        //pop the fptr from the stack;
        let mut ptr = [0; PTR_SIZE];
        self.pop(&mut ptr, PTR_SIZE);
        let fptr = usize::from_le_bytes(ptr);

        //println!("call {:?}", fptr);

        //make room for the return value on the stack frame
        for i in 0..size { self.sp += 1; }

        //push the bp to the stack frame
        self.push(&self.bp.to_le_bytes())?;

        //push the return address to the stack frame ( instruction after call instruction )
        self.push(&(self.ip+2).to_le_bytes())?;

        //update ip and bp
        self.jump(fptr)?;
        self.bp = self.sp; //set the base ptr

        Ok(false)
    }

    fn ret(&mut self, size: u8, param_size: u8) -> Result<bool, RuntimeError> {
        let offset = -16 -size as i16 - param_size as i16;
        self.set(offset, size)?;

        //pop the stack frame
        self.sp = self.bp;

        //pop return address from the stack frame
        let mut return_address = [0; PTR_SIZE];
        self.pop(&mut return_address, PTR_SIZE);
        let return_address = usize::from_le_bytes(return_address);

        //pop the bp from the stack
        let mut bp = [0; PTR_SIZE];
        self.pop(&mut bp, PTR_SIZE);
        let bp = usize::from_le_bytes(bp);

        self.sp = self.sp - param_size as usize; //the return value is located at param. additional space is allocated to make sure we have enough for return value
        self.jump(return_address)?;
        self.bp = bp;

        //println!("return address{:?}", return_address);

        Ok(false)
    }

    fn cond_jump(&mut self, iptr: usize) -> Result<bool, RuntimeError> {
        //pop bool at top of stack
        let mut bool = [0; 1];
        self.pop(&mut bool, 1);
        let bool = bool[0];
        if bool == 0 { self.jump(iptr) } else { Ok(true) }
    }

    fn jump(&mut self, iptr: usize) -> Result<bool, RuntimeError> {
        self.ip = iptr;
        Ok(false)
    }

    fn pop_int(&mut self) -> Result<u32, RuntimeError> {
        let mut a = [0; 4];
        self.pop(&mut a, 4);
        Ok(u32::from_le_bytes(a))
    }

    fn pop_ptr(&mut self) -> Result<usize, RuntimeError> {
        let mut a = [0; 8];
        self.pop(&mut a, 8);
        Ok(usize::from_le_bytes(a))
    }

    fn add_int(&mut self) -> Result<bool, RuntimeError> {
        let (b, a) = (self.pop_int()?, self.pop_int()?);
        self.push(&(a+b).to_le_bytes())?;
        Ok(true)
    }

    fn sub_int(&mut self) -> Result<bool, RuntimeError> {
        let (b, a) = (self.pop_int()?, self.pop_int()?);
        self.push(&(a-b).to_le_bytes())?;
        Ok(true)
    }

    fn mul_int(&mut self) -> Result<bool, RuntimeError> {
        let (b, a) = (self.pop_int()?, self.pop_int()?);
        self.push(&(a*b).to_le_bytes())?;
        Ok(true)
    }

    fn div_int(&mut self) -> Result<bool, RuntimeError> {
        let (b, a) = (self.pop_int()?, self.pop_int()?);
        self.push(&(a/b).to_le_bytes())?;
        Ok(true)
    }

    fn gt_int(&mut self) -> Result<bool, RuntimeError> {
        let (b, a) = (self.pop_int()?, self.pop_int()?);
        self.push(&(if a > b { [1] } else { [0] }))?;
        Ok(true)
    }

    fn lt_int(&mut self) -> Result<bool, RuntimeError> {
        let (b, a) = (self.pop_int()?, self.pop_int()?);
        self.push(&(if a < b { [1] } else { [0] }))?;
        Ok(true)
    }

    fn alloc(&mut self, size: usize) -> Result<bool, RuntimeError> {
        //make tmp obj on stack
        let container = self.pop_dyn(size);

        //allocate onto heap
        let ptr = self.objects.allocate(container)
            .map_err(|e| RuntimeError::new(e.to_owned()))?;
        let ptr = ptr.to_le_bytes();

        //push ptr to stack
        self.push(&ptr)?;

        Ok(true)
    }

    fn alloc_empty(&mut self) -> Result<bool, RuntimeError> {
        //make tmp obj on stack
        let size = self.pop_int()?;
        let mut container = Vec::with_capacity(size as usize);
        for i in 0..size { container.push(0) }

        //allocate onto heap
        let ptr = self.objects.allocate(container)
            .map_err(|e| RuntimeError::new(e.to_owned()))?;
        let ptr = ptr.to_le_bytes();

        //push ptr to stack
        self.push(&ptr)?;

        Ok(true)
    }

    fn heap_index(&mut self, ptr: usize, offset: usize, size: usize) -> Result<bool, RuntimeError> {
        //index value
        let obj = self.objects.arena.get(ptr)
            .map_err(|v| RuntimeError::new(String::from("index nonexistant object")))?;
        let value = obj.index(offset, size);
        let mut container = Vec::from(value); //TODO: optimize container so that doesn't allocate on heap

        //push
        self.push(&container)?;
        Ok(true)
    }

    fn heap_new_index(&mut self, value: &[u8], ptr: usize, offset: usize, size: usize) -> Result<bool, RuntimeError> {
        //index value
        let obj = self.objects.arena.get_mut(ptr)
            .map_err(|v| RuntimeError::new(String::from("index nonexistant object")))?;
        let obj_value = obj.index_mut(offset, size);
        obj_value.copy_from_slice(value);
        Ok(true)
    }

    fn index(&mut self, size: usize) -> Result<bool, RuntimeError> {
        //stack args: obj[index]
        let index = self.pop_int()?;
        let ptr = self.pop_ptr()?;
        self.heap_index(ptr, index as usize * size, size)?;
        Ok(true)
    }

    fn offset_index(&mut self, size: usize) -> Result<bool, RuntimeError> {
        //stack args: obj[index]
        let offset = self.pop_int()?;
        let ptr = self.pop_ptr()?;
        self.heap_index(ptr, offset as usize, size)?;
        Ok(true)
    }
    fn new_index(&mut self, size: usize) -> Result<bool, RuntimeError> {
        //stack args: obj[index]
        let mut value = self.pop_dyn(size);
        let index = self.pop_int()?;
        let ptr = self.pop_ptr()?;

        //index value
        self.heap_new_index(&value, ptr, index as usize * size, size)?;
        Ok(true)
    }

    fn new_offset_index(&mut self, size: usize) -> Result<bool, RuntimeError> {

        //stack args: obj[index]
        let mut value = self.pop_dyn(size);
        let offset = self.pop_int()?;
        let ptr = self.pop_ptr()?;

        //index value
        self.heap_new_index(&value, ptr, offset as usize, size)?;
        Ok(true)
    }

    /*
    fn read(&mut self, offset: usize, size: usize) -> Result<&[u8], RuntimeError> {
        Ok(&self.stack[self.bp-offset..self.bp-offset+size])
    }
    */

    pub fn execute(&mut self, bytecode: &Vec<u8>, start: usize) -> Result<(), RuntimeError> {
        /*
            bottom of bytecode looks like:
            ...
            Push(Main.main) // OPCODE 8 X X X X X X X X = 10 BYTES
            Call(1) // OPCODE 1 = 2 BYTES
            Pop(1) // OPCODE 1 = 2 BYTES

            Thus, the start instruction is the bottom of bytecode - 14
         */

        self.ip = start;
        let mut c = 0;

        loop {
            if self.ip >= bytecode.len() { break }
            let mut instruction = InstructionParser::new(self.ip, bytecode);

            let opcode = instruction.consume(1)[0];
            //match instruction

            //println!("executing ip: {:?}, opcode: {:?}", self.ip, opcode);
            let increment = match opcode {
                0 => { //print debug
                    //println!("{:?} {:?} {:?}", self.sp, self.bp, &self.stack[0..100]);
                    println!("{:?}", self.pop_int()?);
                    Ok(true)
                },
                1 => { //push
                    //println!("push");
                    let size = instruction.consume(1)[0] as usize;
                    self.push(instruction.consume(size))
                },
                2 => { //pop
                    //println!("pop");
                    let size = instruction.consume(1)[0] as usize;
                    self.sp -= size;
                    Ok(true)
                },
                3 => { //copy
                    //println!("copy");
                    let mut offset = [0; 2];
                    offset.copy_from_slice(instruction.consume(2));
                    let size = instruction.consume(1);
                    self.copy(i16::from_le_bytes(offset), size[0])
                },
                4 => { //set
                    //println!("set");
                    let mut offset = [0; 2];
                    offset.copy_from_slice(instruction.consume(2));
                    let size = instruction.consume(1);
                    self.set(i16::from_le_bytes(offset), size[0])
                },
                5 => { //jump
                    //println!("jump");
                    let mut iptr = [0; PTR_SIZE];
                    iptr.copy_from_slice(instruction.consume(PTR_SIZE));
                    let iptr = usize::from_le_bytes(iptr);
                    self.jump(iptr)
                },
                6 => { //cond jump
                    //println!("cond jump");
                    let mut iptr = [0; PTR_SIZE];
                    iptr.copy_from_slice(instruction.consume(PTR_SIZE));
                    let iptr = usize::from_le_bytes(iptr);
                    self.cond_jump(iptr)
                }
                7 => { //call
                    c += 1;
                    let size = instruction.consume(1)[0];
                    self.call(size)
                },
                8 => { //return
                    //println!("return");
                    let size = instruction.consume(1);
                    let param_size = instruction.consume(1);
                    self.ret(size[0], param_size[0])
                },
                9 => self.add_int(), //add
                10 => self.sub_int(), //sub
                11 => self.gt_int(), //greater than
                12 => self.lt_int(), //less than
                13 => {
                    let size = instruction.consume(1);
                    self.alloc(size[0] as usize)
                },
                14 => {
                    let size = instruction.consume(1);
                    self.index(size[0] as usize)
                },
                15 => {
                    let size = instruction.consume(1);
                    self.new_index(size[0] as usize)
                }
                16 => {
                    let size = instruction.consume(1);
                    self.offset_index(size[0] as usize)
                },
                17 => {
                    let size = instruction.consume(1);
                    self.new_offset_index(size[0] as usize)
                },
                18 => self.mul_int(), //mul
                19 => self.div_int(), //div
                20 => self.alloc_empty(),

                a => Err(RuntimeError { msg: format!("invalid instruction {:?}", a) })
            }?;
            //println!("stack: {:?}", &self.stack[0..50]);

            self.ip += if increment { instruction.bytes_consumed } else { 0 };
        }

        Ok(())
    }
}

/*

*/

