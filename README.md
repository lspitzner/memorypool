# memorypool

A very basic memory pool imlemented in haskell.

The core idea is that the pool allocates large chunks of memory that are
some power-of-two factor (e.g. 256) of some base size (e.g. 10k).
The user of the pool allocates chunks of a power-of-two factor of the base
size (i.e. 10k, 20k, 40k, ..). This scheme avoids fragmentation due to
weirdly-sized holes, but keep in mind that no compaction takes place, so
this kind of fragmentation must be worked around manually if necessary.

The pool internally allocates memory on the C heap, i.e. outside of any
haskell/GC heap.

Uses a buddy allocation strategy internally.
