------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides memory mapping of files
--
--  Example of use for this package, when reading a file that can be fully
--  mapped
--
--  declare
--     File : Mapped_File;
--     Str  : Str_Access;
--  begin
--     File := Open_Read ("/tmp/file_on_disk");
--     Read (File);  --  read the whole file
--     Str := Data (File);
--     for S in 1 .. Last (File) loop
--         Put (Str (S));
--     end loop;
--     Close (File);
--  end;
--
--  When the file is big, or you only want to access part of it at a given
--  time, you can use the following type of code.

--  declare
--     File   : Mapped_File;
--     Str    : Str_Access;
--     Offs   : File_Size := 0;
--     Page   : constant Integer := Get_Page_Size;
--  begin
--     File := Open_Read ("/tmp/file_on_disk");
--     while Offs < Length (File) loop
--         Read (File, Offs, Length => Long_Integer (Page) * 4);
--         Str := Data (File);
--
--         --  Print characters for this chunk:
--         for S in Integer (Offs - Offset (File)) + 1 .. Last (File) loop
--            Put (Str (S));
--         end loop;
--
--         --  Since we are reading multiples of Get_Page_Size, we can simplify
--         --  with
--         --    for S in 1 .. Last (File) loop ...
--
--         Offs := Offs + Long_Integer (Last (File));
--     end loop;

with GNAT.OS_Lib;
with GNAT.Strings;
with System;
with Interfaces.C;

package GNATCOLL.Mmap is

   type Mapped_File is private;
   --  A representation of (part of) a file in memory.
   --  This package will use the fastest possible algorithm to load the file in
   --  memory. On systems that support it, the file is not really loaded in
   --  memory. Instead, a call to the mmap() system call (or
   --  CreateFileMapping()) will keep the file on disk, but make it accessible
   --  as if it was in memory.
   --  When the system does not support it, the file is actually loaded in
   --  memory through calls to read(), and written back with write() when you
   --  close it. This is of course much slower

   Invalid_Mapped_File : constant Mapped_File;

   type Unconstrained_String is new String (Positive);
   type Str_Access is access all Unconstrained_String;
   pragma No_Strict_Aliasing (Str_Access);

   type File_Size is new Interfaces.C.size_t;

   function To_Str_Access
     (Str : GNAT.Strings.String_Access) return Str_Access;
   --  Convert Str. The returned value points to the same memory block, but no
   --  longer includes the bounds, which you need to manage yourself

   function Open_Read
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return Mapped_File;
   --  Open a file for reading. The same file can be shared by multiple
   --  processes, that will see each others's changes as they occur.
   --  Any attempt to write the data might result in a segmentation fault,
   --  depending on how the file is open.
   --  Name_Error is raised if the file does not exist.
   --  Filename should be compatible with the filesystem.

   function Open_Write
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return Mapped_File;
   --  Open a file for writing.
   --  You cannot change the length of the file.
   --  Name_Error is raised if the file does not exist
   --  Filename should be compatible with the filesystem.

   procedure Close (File : in out Mapped_File);
   --  Close the file, and unmap the memory that is used.
   --  If the system does not support the unmmap() system call or equivalent,
   --  or these were not available for the file itself, then the file is
   --  written back to the disk if it was opened for writing.

   procedure Read
     (File   : in out Mapped_File;
      Offset : File_Size := 0;
      Length : File_Size := 0);
   --  Read a specific part of the file in memory. Offset is the number of
   --  bytes since the beginning of the file at which we should start reading.
   --  Length is the number of bytes that should be read. If set to 0, as much
   --  of the file as possible is read (presumably the whole file unless you
   --  are reading a _huge_ file).
   --  Nothing is done if that part of the file is already available through
   --  File.
   --  If the file was opened for writing, any modification you do to the data
   --  stored in File will be stored on disk (either immediately when the file
   --  is opened through a mmap() system call, or when the file is closed
   --  otherwise).
   --  There is no guarantee that the data read will actually start at Offset,
   --  or be only Length characters in length, since at the system level the
   --  offset must be a multiple of the page size on your system. So you should
   --  always use the functions below to get information on what exactly was
   --  mapped.

   function Offset (File : Mapped_File) return File_Size;
   --  Return the offset, in the physical file on disk, corresponding to the
   --  region mapped in File.

   function Last (File : Mapped_File) return Integer;
   --  Return the number of bytes mapped in File.
   --  It is erroneous to access Data for indices outside of 1 .. Last (File);
   --  such accesses may cause Storage_Error to be raised.

   function Length (File : Mapped_File) return File_Size;
   --  Size of the file on the disk

   function Data (File : Mapped_File) return Str_Access;
   --  The data read from the file. The result is an unconstrained string, so
   --  you cannot use the usual 'First and 'Last attributes. Instead, these are
   --  respectively 1 and Last (File).

   function Is_Mmapped (File : Mapped_File) return Boolean;
   --  Whether the file was opened through an mmap() system call or equivalent.
   --  This is in general irrelevant to your application, unless the file can
   --  be accessed by multiple concurrent processes or tasks. In such a case,
   --  and if the file is indeed mmap-ed, then the various parts of the file
   --  can be written simulatenously, and thus you cannot ensure the integrity
   --  of the file. If the file is not mmapped, the latest process to Close it
   --  overwrite what other processes have done.

   function Get_Page_Size return Integer;
   --  Returns the number of bytes in a page. Once a file is mapped from the
   --  disk, its offset and Length should be multiples of this page size (which
   --  is ensured by this package in any case). Knowing this page size allows
   --  you to map as much memory as possible at once, thus potentially reducing
   --  the number of system calls to read the file by chunks.

   function Read_Whole_File
     (Filename           : String;
      Empty_If_Not_Found : Boolean := False) return GNAT.Strings.String_Access;
   --  Returns the whole contents of the file.
   --  The returned string must be freed by the user.
   --  This is a convenience function, which is of course slower than the ones
   --  above since we also need to allocate some memory, actually read the file
   --  and copy the bytes.
   --  If the file does not exist, null is returned. However, if
   --  Empty_If_Not_Found is True, then the empty string is returned instead.
   --  Filename should be compatible with the filesystem.

private
   pragma Inline (Data, Length, Last, Offset, Is_Mmapped, To_Str_Access);

   --  ??? The components of the record below must be documented!

   type Mapped_File is record
      Data               : Str_Access;
      Buffer             : GNAT.Strings.String_Access;
      Offset             : File_Size;
      Last               : Integer;
      Length             : File_Size;
      Write              : Boolean;
      Mapped             : Boolean;
      Fd                 : GNAT.OS_Lib.File_Descriptor;
      Page_Size          : File_Size;
      --  Win32 specific handle below
      Handle, Map_Handle : System.Address;
   end record;
   --  Fd is either a file descriptor on Unix systems or a Handle on Windows

   Invalid_Mapped_File : constant Mapped_File :=
     (null, null, 0, 0, 0, False, False,
      GNAT.OS_Lib.Invalid_FD, 0, System.Null_Address, System.Null_Address);

end GNATCOLL.Mmap;
