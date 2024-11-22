## Optimizations

In the future we'll implement many optimizations


### Data Alignment
Data should be aligned, so that memory doesn't fall between two
blocks fetched by the CPU. Blocks are always fetched at addresses
which are multiples of the block size. 

If blocks are word-length, they're 8 bytes in a 64-bit machine.
In that case, blocks are fetched at loc 0, 8, 16, etc...

In that case, for any datum with size 2^n < 8, it's sufficient to ensure that the address
of the datum is a multiple of it's size.
For example, if the addr of a 2-byte value is a multiple of 2, then it will never be odd,
which is required for it to span two blocks.
If the addr of a 4-byte value is a multiple of 4, it will either be at the start or middle
of a block.
1 byte values cannot span two blocks, because taking up multiple locations are required to span two blocks.

#### Data Structure Padding

Data structure members must be padded, and the addr's of the data structures must be selected
intelligently so that data alignment is ensured.

### Garbage Collection

Garbage collectors contribute significantly to the runtime of a language. It's important
to optimize the standard mark-sweep algorithm used to implement a tracing garbage collector.

#### Generational Collection

Objects can be assigned to generations, depending on how many collection cycle's they've survived.
It's useful to defer attempting to collect older gen objects.

#### Concurrent Collection

It's useful to run the garbage collector in parallel.

#### Escape analysis

We can avoid heap allocating objects when they can be proven to not escape.

