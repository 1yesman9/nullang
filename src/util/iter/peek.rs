use std::iter::Iterator;

//iterator which allows arbitrary lookahead.
pub struct Peek<I: Iterator, const N: usize> {
    iter: I,
    buffer: [Option<I::Item>; N]
}

impl<I: Iterator, const N: usize>  Peek<I, N> {
    pub fn from_iter(iter: I) -> Self {
        let mut iterator = Self { iter, buffer: std::array::from_fn(|_| None) };
        for i in 0..N { iterator.push(); }
        iterator
    }

    pub fn peek(&self, n: usize) -> Option<&I::Item> {
        self.buffer.get(self.buffer.len()-1-n).unwrap_or(&None).as_ref()
    }

    fn push(&mut self) -> Option<I::Item> {
        self.buffer.rotate_right(1);
        std::mem::replace(
            self.buffer.first_mut()?,
            self.iter.next()
        )
    }
}

impl<I: Iterator, const N: usize> Iterator for Peek<I, N> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.push()
    }
}