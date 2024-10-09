.. highlight:: ada

*************************************
**Strings**: high-performance strings
*************************************

The generic package :file:`GNATCOLL.Strings_Impl` (and its default
instantiation in :file:`GNATCOLL.Strings`) provides a high-performance
strings implementation.

It comes in addition to Ada's own `String` and `Unbounded_String` types,
although it attempts to find a middle ground in between (flexibility
vs performance).

GNATCOLL.Strings therefore provides strings (named `XString`, as in
extended-strings) that can grow as needed (up to `Natural'Last`, like standard
strings), yet are faster than unbounded strings. They also come with an
extended API, which includes all primitive operations from unbounded strings,
in addition to some subprograms inspired from GNATCOLL.Utils and the python and
C++ programming languages.

Small string optimization
=========================

GNATCOLL.Strings uses a number of tricks to improve on the efficiency.  The
most important one is to limit the number of memory allocations.  For this, we
use a trick similar to what all C++ implementations do nowadays, namely the
small string optimization.

The idea is that when a string is short, we can avoid all memory allocations
altogether, while still keeping the string type itself small. We therefore
use an Unchecked_Union, where a string can be viewed in two ways::

    Small string

      [f][s][ characters of the string 23 bytes               ]
         f  = 1 bit for a flag, set to 0 for a small string
         s  = 7 bits for the size of the string (i.e. number of significant
              characters in the array)

    Big string

      [f][c      ][size     ][data      ][first     ][pad    ]
         f = 1 bit for a flag, set to 1 for a big string
         c = 31 bits for half the capacity. This is the size of the buffer
             pointed to by data, and which contains the actual characters of
             the string.
         size = 32 bits for the size of the string, i.e. the number of
             significant characters in the buffer.
         data = a pointer (32 or 64 bits depending on architecture)
         first = 32 bits, see the handling of substrings below
         pad = 32 bits on a 64 bits system, 0 otherwise.
             This is because of alignment issues.

So in the same amount of memory (24 bytes), we can either store a small string
of 23 characters or less with no memory allocations, or a big string that
requires allocation. In a typical application, most strings are smaller than 23
bytes, so we are saving very significant time here.

This representation has to work on both 32 bits systems and 64 bits systems, so
we have careful representation clauses to take this into account.  It also
needs to work on both big-endian and little-endian systems. Thanks to Ada's
representation clauses, this one in fact relatively easy to achieve (well,
okay, after trying a few different approaches to emulate what's done in C++,
and that did not work elegantly). In fact, emulating via bit-shift operations
ended up with code that was less efficient than letting the compiler do it
automatically because of our representation clauses.

Character types
===============

Applications should be able to handle the whole set of Unicode characters. In
Ada, these are represented as the Wide_Character type, rather than Character,
and stored on 2 bytes rather than 1. Of course, for a lot of applications it
would be wasting memory to always store 2 bytes per character, so we want to
give flexibility to users here.

So the package GNATCOLL.Strings_Impl is a generic. It has several formal
parameters, among which:

   * Character_Type is the type used to represent each character. Typically,
     it will be Character, Wide_Character, or even possibly
     Wide_Wide_Character. It could really be any scalar type, so for instance
     we could use this package to represent DNA with its 4-valued nucleobases.

   * Character_String is an array of these characters, as would be
     represented in Ada. It will typically be a String or a Wide_String. This
     type is used to make this package work with the rest of the Ada world.

Note about Unicode: we could also always use a Character, and use UTF-8
encoding internally. But this makes all operations (from taking the length to
moving the next character) slower, and more fragile. We must make sure not to
cut a string in the middle of a multi-byte sequence. Instead, we manipulate a
string of code points (in terms of Unicode). A similar choice is made in Ada
(String vs Wide_String), Python and C++.

Configuring the size of small strings
=====================================

The above is what is done for most C++ implementations nowadays.  The maximum
23 characters we mentioned for a small string depends in fact on several
criteria, which impact the actual maximum size of a small string:

   * on 32 bits system, the size of the big string is 16 bytes, so the maximum
     size of a small string is 15 bytes.
   * on 64 bits system, the size of the big string is 24 bytes, so the maximum
     size of a small string is 23 bytes.
   * If using a Character as the character type, the above are the actual
     number of characters in the string. But if you are using a
     Wide_Character, this is double the maximum length of the string, so a
     small string is either 7 characters or 11 characters long.

This is often a reasonable number, and given that applications mostly use small
strings, we are already saving a lot of allocations. However, in some cases we
know that the typical length of strings in a particular context is different.
For instance, GNATCOLL.Traces builds messages to output in the log file. Such
messages will typically be at most 100 characters, although they can of course
be much larger sometimes.

We have added one more formal parameter to GNATCOLL.Strings_Impl to control the
maximum size of small strings. If for instance we decide that a "small" string
is anywhere from 1 to 100 characters long (i.e. we do not want to allocate
memory for those strings), it can be done via this parameter.

Of course, in such cases the size of the string itself becomes much larger.
In this example it would be 101 bytes long, rather than the 24 bytes.  Although
we are saving on memory allocations, we are also spending more time copying
data when the string is passed around, so you'll need to measure the
performance here.

The maximum size for the small string is 127 bytes however, because this size
and the 1-bit flag need to fit in 1 bytes in the representation clauses we
showed above. We tried to make this more configurable, but this makes things
significantly more complex between little-endian and big-endian systems, and
having large "small" strings would not make much sense in terms of performance
anyway.

Typical C++ implementations do not make this small size configurable.

Task safety
===========

Just like unbounded strings, the strings in this package are not thread safe.
This means that you cannot access the same string (read or write) from two
different threads without somehow protecting the access via a protected type,
locks,...

In practice, sharing strings would rarely be done, so if the package itself
was doing its own locking we would end up with very bad performance in all
cases, for a few cases where it might prove useful.

As we'll discuss below, it is possible to use two different strings that
actually share the same internal buffer, from two different threads. Since this
is an implementation detail, this package takes care of guaranteeing the
integrity of the shared data in such a case.

Copy on write
=============

There is one more formal parameter, to configure whether this package should
use copy-on-write or not. When copy on write is enabled, you can have multiple
strings that internally share the same buffer of characters. This means that
assigning a string to another one becomes a reasonably fast operation (copy a
pointer and increment a refcount). Whenever the string is modified, a copy of
the buffer is done so that other copies of the same string are not impacted.

But in fact, there is one drawback with this scheme: we need reference counting
to know when we can free the shared data, or when we need to make a copy of it.
This reference counting must be thread safe, since users might be using two
different strings from two different threads, but they share data internally.

Thus the reference counting is done via atomic operations, which have some
impact on performance. Since multiple threads try to access the same memory
addresses, this is also a source of contention in multi-threaded applications.

For this reason, the current C++ standard prevents the use of copy-on-write
for strings.

In our case, we chose to make this configurable in the generic, so that users
can decide whether to pay the cost of the atomic operations, but save on the
number of memory allocations and copy of the characters.  Sometimes it is
better to share the data, sometimes to systematically copy it.
Again, actual measurements of the performance are needed for your specific
application.

Growth strategy
===============

When the current size of the string becomes bigger than the available allocated
memory (for instance because you are appending characters), this package needs
to reallocate memory. There are plenty of strategies here, from allocating only
the exact amount of memory needed (which saves on memory usage, but is very bad
in terms of performance), to doubling the current size of the string until we
have enough space, as currently done in the GNAT unbounded strings
implementation.

The latter approach would therefore allocate space for two characters, then
for 4, then 8 and so on.

This package has a slightly different strategy. Remember that we only start
allocating memory past the size of small strings, so we will for instance first
allocate 24 bytes. When more memory is needed, we multiply this size by 1.5,
which some researchers have found to be a good comprise between waste of memory
and number of allocations. For very large strings, we always allocate multiples
of the memory page size (4096 bytes), since this is what the system will make
available anyway. So we will basically allocate the following: 24, 36, 54, 82,
122,...

An additional constraint is that we only ever allocate even number of bytes.
This is called the capacity of the string. In the layout of the big string,
as shown above, we store half that capacity, which saves one bit that we
use for the flag.

Substrings
==========

One other optimization performed by this package (which is not done for
unbounded strings or various C++ implementations) is to optimize substrings
when also using copy-on-write.

We simply store the index of the first character of the string within the
shared buffer, instead of always starting at the first.

From the user's point of view, this is an implementation detail. Strings
are always indexed from 1, and internally we convert to an actual position
in the buffer. This means that if we need to reallocate the buffer, for
instance when the string is modified, we transparently change the index
of the first character, but the indexes the user was using are still valid.

This results in very significant savings, as shown below in the timings
for Trim for instance. Also, we can do an operation like splitting a
string very efficiently.

For instance, the following code doesn't allocate any memory, beside
setting the initial value of the string. It parses a file containing
some "key=value" lines, with optional spaces, and possibly empty lines::

     declare
        S, Key, Value : XString;
        L             : XString_Array (1 .. 2);
        Last          : Natural;
     begin
        S.Set (".......");

        --  Get each line
        for Line in S.Split (ASCII.LF) loop

           --  Split into at most two substrings
           Line.Split ('=', Into => L, Last => Last);

           if Last = 2 then
              Key := L (1);
              Key.Trim;    --  Removing leading and trailing spaces

              Value := L (2);
              Value.Trim;

           end if;
        end loop;
     end;

API
===

This package provides a very extensive set of API that apply to `XString`,
please check the spec in :file:`gnatcoll-strings_impl.ads` for a fully
documented list.
