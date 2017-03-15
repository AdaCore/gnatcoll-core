.. _Reading_and_Writing_Files:

***********************************
**Mmap**: Reading and Writing Files
***********************************

.. index:: mmap

.. highlight:: ada

Most applications need to efficiently read files from the disk. Some also
need in addition to modify them and write them back. The Ada run-time
profiles several high-level functions to do so, most notably in the
:file:`Ada.Text_IO` package. However, these subprograms require a lot of
additional housekeeping in the run-time, and therefore tend to be slow.

GNAT provides a number of low-level functions in its :file:`GNAT.OS_Lib`
package. These are direct import of the usual C system calls `read()`,
`write()` and `open()`. These are much faster, and suitable for
most applications.

However, if you happen to manipulate big files (several megabytes and much
more), these functions are still slow. The reason is that to use `read`
you basically need a few other system calls: allocate some memory to
temporarily store the contents of the file, then read the whole contents of
the file (even if you are only going to read a small part of it, although
presumably you would use `lseek` in such a case).

On most Unix systems, there exists an additional system call `mmap()`
which basically replaces `open`, and makes the contents of the file
immediately accessible, in the order of a few micro-seconds. You do not
need to allocate memory specifically for that purpose. When you access
part of the file, the actual contents is temporarily mapped in memory
by the system. To modify the file, you just modify the contents of the
memory, and do not worry about writing the file back to the disk.

When your application does not need to read the whole contents of the file,
the speed up can be several orders of magnitude faster than `read()`.
Even when you need to read the whole contents, using `mmap()` is
still two or three times faster, which is especially interesting on big
files.

GNATColl's `GNATCOLL.Mmap` package provides a high-level abstraction
on top of the `mmap` system call. As for most other packages in
GNATColl, it also nicely handles the case where your system does not
actually support `mmap`, and will in that case fallback on using
`read` and `write` transparently. In such a case, your application
will perform a little slower, but you do not have to modify your code to
adapt it to the new system.

Due to the low-level C API that is needed underneath, the various subprograms
in this package do not directly manipulate Ada strings with valid bounds.
Instead, a new type `Str_Access` was defined. It does not contain the
bounds of the string, and therefore you cannot use the usual
`'First` and `'Last` attributes on that string. But there are other
subprograms that provide those values.

Here is how to read a whole file at once. This is what your code will use
in most cases, unless you expect to read files bigger than `Integer'Last`
bytes long. In such cases you need to read chunks of the file separately.
The `mmap` system call is such that its performance does not depend on
the size of the file your are mapping. Of course, this could be a problem if
`GNATCOLL.Mmap` falls back on calling `read`, since in that case it
needs to allocate as much memory as your file. Therefore in some cases you
will also want to only read chunks of the file at once::

  declare
     File : Mapped_File;
     Str  : Long.Str_Access;
  begin
     File := Open_Read ("/tmp/file_on_disk");
     Read (File);  *--  read the whole file*
     Str := Long.Data (File);
     for S in 1 .. Long.Last (File) loop
         Put (Str (S));
     end loop;
     Close (File);
  end;

The above example works for files larger than 2Gb, on 64 bits system
(up to a petabyte in fact), on systems that support the `mmap` system
call.

To read only a chunk of the file, your code would look like the following.
At the low-level, the system call will always read chunks multiple of a
size called the page_size. Although `GNATCOLL.Mmap` takes care of rounding
the numbers appropriately, it is recommended that you pass parameters that
are multiples of that size. That optimizes the number of system calls you
will need to do, and therefore speeds up your application somewhat::

  declare
     File   : Mapped_File;
     Str    : Str_Access;
     Offs   : Long_Integer := 0;
     Page   : constant Integer := Get_Page_Size;
  begin
     File := Open_Read ("/tmp/file_on_disk");
     while Offs < Length (File) loop
         Read (File, Offs, Length => Long_Integer (Page) * 4);
         Str := Data (File);

         *--  Print characters for this chunk:*
         for S in Integer (Offs - Offset (File)) + 1 .. Last (File) loop
            Put (Str (S));
         end loop;

         Offs := Offs + Long_Integer (Last (File));
     end loop;
     Close (File);
  end;

There are a number of subtle details in the code above. Since the system call
only manipulates chunk of the file on boundaries multiple of the code size,
there is no guarantee that the part of the file we actually read really starts
exactly at `Offs`. If could in fact start before, for rounding issues.
Therefore when we loop over the contents of the buffer, we make sure to
actually start at the `Offs`-th character in the file.

In the particular case of this code, we make sure we only manipulate multiples
of the page_size, so we could in fact replace the loop with the simpler::

   for S in 1 .. Last (File) loop 
  
If you intend to modify the contents of the file, not that `GNATCOLL.Mmap`
currently gives you no way to change the size of the file. The only difference
compared to the code used for reading the file is the call to open the file,
which should be::

   File := Open_Write ("/tmp/file_on_disk");
  
Modifications to Str are automatically reflected in the file. However, there
is no guarantee this saving is done immediately. It could be done only when
you call `Close`. This is in particular always the case when your system
does not support `mmap` and `GNATCOLL.Mmap` had to fallback on calls to
`read`.
