***********************************
**Memory**: Monitoring memory usage
***********************************

.. highlight:: ada

The GNAT compiler allocates and deallocates all memory either through
type-specific debug pools that you have defined yourself, or defaults to
the standard malloc and free system calls. However, it calls those through
an Ada proxy, in the package `System.Memory` that you can also
replace in your own application if need be.

Like this::

   procedure Ada

`gnatcoll` provides such a possible replacement. Its implementation
is also based on `malloc` and `free`, but if you so chose you
can activate extra monitoring capabilities to help you find out which parts
of your program is allocating the most memory, or where memory is allocated
at any moment in the life of your application.

This package is called `GNATCOLL.Memory`. To use it requires a bit of
preparation in your application:

* You need to create your own version of :file:`s-memory.adb` with the
  template below, and put it somewhere in your source path. This file should
  contain the following bit of code::

    with GNATCOLL.Memory;

    package body System.Memory is
       package M renames GNATCOLL.Memory;

       function Alloc (Size : size_t) return System.Address is
       begin
          return M.Alloc (M.size_t (Size));
       end Alloc;

       procedure Free (Ptr : System.Address) renames M.Free;

       function Realloc
         (Ptr  : System.Address;
          Size : size_t)
          return System.Address is
       begin
          return M.Realloc (Ptr, M.size_t (Size));
       end Realloc;
    end;

* You then need to compile your application with the extra switch
  `-a` passed to `gnatmake` or `gprbuild`, so that this
  file is appropriately compiled and linked with your application

* If you only do this, the monitor is disabled by default. This
  basically has zero overhead for your application (apart from the initial
  small allocation of some internal data). When you call the procedure
  `GNATCOLL.Memory.Configure` to activate the monitor, each memory
  allocation or deallocation will result in extra overhead that will slow
  down your application a bit. But at that point you can then get access
  to the information stored in the monitor

We actually recommend that the activation of the monitor be based on an
environment variable or command line switch of your application, so that
you can decide at any time to rerun your application with the monitor
activated, rather than have to go through an extra recompilation.

All allocations and deallocations are monitor automatically when this
module is activated. However, you can also manually call
`GNATCOLL.Memory.Mark_Traceback` to add a dummy entry in the
internal tables that matches the current stack trace. This is helpful
for instance if you want to monitor the calls to a specific subprogram,
and know both the number of calls, and which callers executed it how
many times. This can help find hotspots in your application to optimize
the code.

The information that is available through the monitor is the list of
all chunks of memory that were allocated in Ada (this does not include
allocations done in other languages like C). These chunks are grouped
based on the stack trace at the time of their invocation, and this
package knows how many times each stack trace executed each allocation.

As a result, you can call the function `GNATCOLL.Memory.Dump` to
dump on the standard output various types of data, sorted. To limit the
output to a somewhat usable format, `Dump` asks you to specify
how many blocks it should output.

*Memory usage*
  Blocks are sorted based on the amount of memory they have allocated and
  is still allocated. This helps you find which part of your application
  is currently using the most memory.

*Allocations count*
  Blocks are sorted based on the number of allocation that are still
  allocated. This helps you find which part of your application has done
  the most number of allocations (since malloc is a rather slow system
  call, it is in general a good idea to try and reduce the number of
  allocations in an application).

*Total number of allocations*
  This is similar to the above, but includes all allocations ever done
  in this block, even if memory has been deallocated since then.

*Marked blocks*
  These are the blocks that were created through your calls to
  `GNATCOLL.Memory.Mark_Traceback`. They are sorted by the number
  of allocation for that stacktrace, and also shows you the total number
  of such allocations in marked blocks. This is useful to monitor and
  analyze calls to specific places in your code
