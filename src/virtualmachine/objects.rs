use crate::util::arena::Arena;

#[derive(Debug)]
pub enum Color { White, Grey, Black }

#[derive(Debug)]
pub struct Object {
    mark: Color,
    pub value: Box<[u8]>
}
impl Object {
    pub fn index(&self, offset: usize, size: usize) -> &[u8] {
        &self.value[offset..offset+size]
    }

    pub fn index_mut(&mut self, offset: usize, size: usize) -> &mut [u8] {
        &mut self.value[offset..offset+size]
    }
}
#[derive(Debug)]
pub struct Objects {
    pub arena: Arena<Object>
}

impl Objects {
    pub fn new() -> Self {
        Self {
            arena: Arena::new()
        }
    }
    pub fn allocate(&mut self, value: Vec<u8>) -> Result<usize, &'static str> {
        self.arena.insert(Object { mark: Color::White, value: value.into_boxed_slice()})
            .map_err(|_| "heap max")
    }

    pub fn mark(&mut self, index: usize, mark: Color) -> Result<(), &'static str> {
        self.arena.get_mut(index)
            .map_err(|_| "attempted to deference null ptr")?
            .mark = mark;

        Ok(())
    }

    //sweep unreachable objects
    pub fn sweep(&mut self) -> Result<(), &'static str> {
        let mut sweep_queue = Vec::new();

        for (index, object) in self.arena.iter() {
            if let Color::White = object.mark {
                sweep_queue.push(index);
            }
        }

        for index in sweep_queue {
            self.arena.remove(index).map_err(|_| "double free")?
        }

        Ok(())
    }
}

