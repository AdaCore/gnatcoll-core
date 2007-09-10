-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2007, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

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
--     Offs   : Long_Integer := 0;
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

package GNAT.Mmap is

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
   type Str_Access is access Unconstrained_String;

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
   --  Name_Error is raised if the file does not exist

   function Open_Write
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return Mapped_File;
   --  Open a file for writting.
   --  You cannot change the length of the file.
   --  Name_Error is raised if the file does not exist

   procedure Close (File : in out Mapped_File);
   --  Close the file, and unmap the memory that is used.
   --  If the system does not support the unmmap() system call or equivalent,
   --  or these were not available for the file itself, then the file is
   --  written back to the disk if it was opened for writting.

   procedure Read
     (File   : in out Mapped_File;
      Offset : Long_Integer := 0;
      Length : Long_Integer := 0);
   --  Read a specific part of the file in memory. Offset is the number of
   --  bytes since tbe beginning of the file at which we should start reading.
   --  Length is the number of bytes that should be read. If set to 0, as much
   --  of the file as possible is read (presumably the whole file unless you
   --  are reading a _huge_ file).
   --  Nothing is done if that part of the file is already available through
   --  File.
   --  If the file was opened for writting, any modification you do to the data
   --  stored in File will be stored on disk (either immediately when the file
   --  is opened through a mmap() system call, or when the file is closed
   --  otherwise).
   --  There is no guarantee that the data read will actually start at Offset,
   --  or be only Length characters in length, since at the system level these
   --  must be multiples of the page_size on your system. So you should always
   --  use the functions below to get information on what exactly what mapped.

   function Offset (File : Mapped_File) return Long_Integer;
   --  Return the offset, in the physical file on disk, corresponding to the
   --  region mapped in File.

   function Last (File : Mapped_File) return Integer;
   --  Return the number of bytes mapped in File.
   --  In Data, you can only access bytes 1 .. Last (File), otherwise
   --  storage_errors will occur.

   function Length (File : Mapped_File) return Long_Integer;
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
   --  Returns the number of bytes in a page. One a file is mapped from the
   --  disk, its offset and Length should be multiples of this page size (which
   --  is ensures by this package in any case). Knowing this page size allows
   --  you to map as much memory as possible at once, thus potentially reducing
   --  the number of system calls to read the file by chunks.

private
   pragma Inline (Data, Length, Last, Offset, Is_Mmapped, To_Str_Access);

   type Mapped_File is record
      Data      : Str_Access;
      Buffer    : GNAT.Strings.String_Access;
      Offset    : Long_Integer;
      Last      : Integer;
      Length    : Long_Integer;
      Write     : Boolean;
      Mapped    : Boolean;
      Fd        : GNAT.OS_Lib.File_Descriptor;
      Handle    : System.Address;
      Page_Size : Long_Integer;
   end record;
   --  Fd is either a file descriptor on Unix systems or a Handle on Windows.

   Invalid_Mapped_File : constant Mapped_File :=
     (null, null, 0, 0, 0, False, False,
      GNAT.OS_Lib.Invalid_FD, System.Null_Address, 0);

   pragma Import (C, Get_Page_Size, "getpagesize");
end GNAT.Mmap;
