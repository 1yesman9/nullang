/*
    an arena
    o(1) insert, o(1) deletion
    constant size, stack allocated
    only iterates through the occupied slots.

    downsides: occupied slots can be non-contiguous, leading to cache locality issues. mem overhead on stack
    upsides: no realloc, no heap access
 */

#[derive(Debug)]
pub enum ArenaError {
    OOM,
    DoubleFree,
    NullDeref
}

#[derive(Debug)]
pub enum Slot<T> {
    Taken { value: T, parent: Option<usize>, child: Option<usize> },
    Free { next_free: usize }
}

#[derive(Debug)]
pub struct Arena<T> {
    buffer: Vec<Slot<T>>,
    next_free: usize,
    head: Option<usize>
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self {
            buffer: Vec::new(),
            next_free: 0,
            head: None
        }
    }

    pub fn insert(&mut self, value: T) -> Result<usize, ArenaError> {
        if self.next_free >= self.buffer.len() {
            self.buffer.push(Slot::Free { next_free: self.next_free + 1});
        }

        let next_occupied = self.next_free;

        if let Slot::Free { next_free } = self.buffer[self.next_free] {
            //prepend occupied list
            if let Some(usize) = self.head {
                if let Slot::Taken { value, parent, child } = &mut self.buffer[usize] {
                    *parent = Some(next_occupied)
                }
            }
            self.buffer[next_occupied] = Slot::Taken { value, parent: None, child: self.head };
            self.head = Some(self.next_free);

            //pop_front free list
            self.next_free = next_free;
            Ok(next_occupied)
        } else {
            Err(ArenaError::OOM)
        }
    }

    pub fn parent_child(&self, index: usize) -> Option<(Option<usize>, Option<usize>)> {
        if let Slot::Taken { value, parent, child } = &self.buffer[index] {
            Some((*parent, *child))
        } else {
            None
        }
    }

    pub fn remove(&mut self, index: usize) -> Result<(), ArenaError> {
        if let Slot::Free { next_free } = &self.buffer[index] {
            return Err(ArenaError::DoubleFree)
        }

        //remove from middle of linked list
        //grandparent adopts child
        if let Some((grand_parent, grand_child)) = self.parent_child(index) {
            //if the child exists, give him new parent
            if let Some(child) = grand_child {
                if let Slot::Taken { value, parent, child } = &mut self.buffer[child] {
                    *parent = grand_parent;
                    if let None = grand_parent {
                        self.head = grand_child;
                    }
                }
            }

            //if the parent exists, give him a new child
            if let Some(parent) = grand_parent {
                if let Slot::Taken { value, parent, child } = &mut self.buffer[parent] {
                    *child = grand_child;
                }
            }
        }

        self.buffer[index] = Slot::Free { next_free: self.next_free };
        self.next_free = index;
        Ok(())
    }

    pub fn get_mut(&mut self, index: usize) -> Result<&mut T, ArenaError> {
        if let Slot::Taken { value, parent, child } = &mut self.buffer[index] {
            Ok(value)
        } else {
            Err(ArenaError::NullDeref)
        }
    }

    pub fn get(&self, index: usize) -> Result<&T, ArenaError> {
        if let Slot::Taken { value, parent, child } = &self.buffer[index] {
            Ok(value)
        } else {
            Err(ArenaError::NullDeref)
        }
    }

    pub fn iter(&self) -> ArenaIter<T> {
        ArenaIter {
            arena: self,
            index: self.head
        }
    }
}

pub struct ArenaIter<'a, T> {
    arena: &'a Arena<T>,
    index: Option<usize>
}

impl<'a, T> Iterator for ArenaIter<'a, T> {
    type Item = (usize, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(index) = self.index {
            if let Slot::Taken { value, parent, child } = &self.arena.buffer[index] {
                self.index = *child;
                Some((index, value))
            } else {
                None
            }
        } else {
            None
        }
    }
}
