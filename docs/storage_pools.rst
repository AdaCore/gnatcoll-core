************************************************
**Storage Pools**: controlling memory management
************************************************

Ada gives full control to the user for memory management. That allows for
a number of optimization in your application. For instance, if you need to
allocate a lot of small chunks of memory, it is generally more efficient
to allocate a single large chunk, which is later divided into smaller
chunks. That results in a single system call, which speeds up your
application.

This can of course be done in most languages. However, that generally
means you have to remember not to use the standard memory allocations
like `malloc` or `new`, and instead call one of your
subprograms. If you ever decide to change the allocation strategy, or
want to experiment with several strategies, that means updating your
code in several places.

In Ada, when you declare the type of your data, you also specify through
a `'Storage_Pool` attribute how the memory for instances of that
type should be allocated. And that's it. You then use the usual
`new` keyword to allocate memory.

GNATColl provides a number of examples for such storage pools,
with various goals. There is also one advanced such pool in the GNAT
run-time itself, called `GNAT.Debug_Pools`, which allows you to
control memory leaks and whether all accesses do reference valid memory
location (and not memory that has already been deallocated).

In GNATColl, you will find the following storage pools:

*`GNATCOLL.Storage_Pools.Alignment`*
  This pool gives you full control over the alignment of your data. In
  general, Ada will only allow you to specify alignments up to a limited
  number of bytes, because the compiler must only accept alignments
  that can be satisfied in all contexts, in particular on the stack.

  This package overcomes that limitation, by allocating larger chunks
  of memory than needed, and returning an address within that chunk which
  is properly aligned.
