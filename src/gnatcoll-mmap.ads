------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2007-2017, AdaCore                     --
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

--  This package provides memory mapping of files. Depending on your operating
--  system, this might provide a more efficient method for accessing the
--  contents of files.
--  A description of memory-mapping is available on the sqlite page, at:
--      http://www.sqlite.org/mmap.html
--
--  The traditional method for reading a file is to allocate a buffer in the
--  application address space, then open the file and copy its contents. When
--  memory mapping is available though, the application asks the operating
--  system to return a pointer to the requested page, if possible. If the
--  requested page has been or can be mapped into the application address
--  space, the system returns a pointer to that page for the application to
--  use without having to copy anything. Skipping the copy step is what makes
--  memory mapped I/O faster.
--
--  When memory mapping is not available, this package automatically falls
--  back to the traditional copy method.
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

with Ada.Unchecked_Conversion;
with Interfaces.C;
with GNAT.Strings;                use GNAT.Strings;
with GNATCOLL.Strings;
with System;

package GNATCOLL.Mmap is

   type Mapped_File is private;
   --  File to be mapped in memory.

   --  This package will use the fastest possible algorithm to load the
   --  file in memory. On systems that support it, the file is not really
   --  loaded in memory. Instead, a call to the mmap() system call (or
   --  CreateFileMapping()) will keep the file on disk, but make it
   --  accessible as if it was in memory.

   --  When the system does not support it, the file is actually loaded in
   --  memory through calls to read(), and written back with write() when you
   --  close it. This is of course much slower.

   --  Legacy: each mapped file has a "default" mapped region in it.

   type Mapped_Region is private;
   --  A representation of part of a file in memory. Actual reading/writing
   --  is done through a mapped region. After being returned by Read, a mapped
   --  region must be free'd when done. If the original Mapped_File was open
   --  for reading, it can be closed before the mapped region is free'd.

   Invalid_Mapped_File : constant Mapped_File;
   Invalid_Mapped_Region : constant Mapped_Region;

   type File_Size is new Interfaces.C.size_t;

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
   --  Close the file, and unmap the memory that is used for the region
   --  contained in File. If the system does not support the unmmap() system
   --  call or equivalent, or these were not available for the file itself,
   --  then the file is written back to the disk if it was opened for writing.

   procedure Free (Region : in out Mapped_Region);
   --  Unmap the memory that is used for this region and deallocate the region

   type Use_Advice is
      (Use_Normal,
       Use_Random,
       Use_Sequential);
   for Use_Advice use
      (Use_Normal      => 1,
       Use_Random      => 2,
       Use_Sequential  => 4);
   --  This type can be used to provide advice to some operation systems on
   --  how a mapped page will be used.
   --
   --  If you specify Use_Sequential, you are telling the system that the
   --  contents of the page will be read sequentially from lower to higher
   --  address, and therefore the system should use prefetching aggressively.
   --
   --  If you specify Use_Random, the page will be accessed in a
   --  non-sequential manner.
   --
   --  This advice might be ignored by the system (depending on whether the
   --  madvise() system call is supported). It will always be ignored for
   --  systems that do not support mmap.

   procedure Read
     (File    : Mapped_File;
      Region  : in out Mapped_Region;
      Offset  : File_Size := 0;
      Length  : File_Size := 0;
      Mutable : Boolean := False;
      Advice  : Use_Advice := Use_Normal);
   --  Read a specific part of File and set Region to the corresponding mapped
   --  region, or re-use it if possible.
   --  Offset is the number of bytes since the beginning of the file at which
   --  we should start reading. Length is the number of bytes that should be
   --  read. If set to 0, as much of the file as possible is read (presumably
   --  the whole file unless you are reading a _huge_ file).
   --  Note that no (un)mapping is is done if that part of the file is already
   --  available through Region.
   --  If the file was opened for writing, any modification you do to the
   --  data stored in File will be stored on disk (either immediately when the
   --  file is opened through a mmap() system call, or when the file is closed
   --  otherwise).
   --  Mutable is processed only for reading files. If set to True, the
   --  data can be modified, even through it will not be carried through the
   --  underlying file, nor it is guaranteed to be carried through remapping.
   --  This function takes care of page size alignment issues. The accessors
   --  below only expose the region that has been requested by this call, even
   --  if more bytes were actually mapped by this function.
   --
   --  TODO??? Enable to have a private copy for readable files
   --
   --  Operating systems generally limit the number of open file descriptors
   --  that an application can have at one time (typically 1024 or 2048).
   --  They however often have a much higher limit on the number of mapped
   --  regions (65535 for instance). If you hitting the first limit, you
   --  could use the following workflow:
   --
   --         File := Open_Read ("filename.txt");
   --         Region := Read (File);
   --         Close (File);   --  release the file descriptor
   --         ...
   --         Free (Region);  --  release the mapped file

   function Read
     (File    : Mapped_File;
      Offset  : File_Size := 0;
      Length  : File_Size := 0;
      Mutable : Boolean := False;
      Advice  : Use_Advice := Use_Normal) return Mapped_Region;
   --  Likewise, return a new mapped region

   procedure Read
     (File    : Mapped_File;
      Offset  : File_Size := 0;
      Length  : File_Size := 0;
      Mutable : Boolean := False)
     with Obsolescent;
   --  Likewise, use the legacy "default" region in File

   function Length (File : Mapped_File) return File_Size
      with Inline;
   --  Size of the file on the disk

   function Offset (Region : Mapped_Region) return File_Size
      with Inline;
   --  Return the offset, in the physical file on disk, corresponding to the
   --  requested mapped region. The first byte in the file has offest 0.

   function Offset (File : Mapped_File) return File_Size
      with Inline, Obsolescent;
   --  Likewise for the region contained in File

   function Data_Address (Region : Mapped_Region) return System.Address
      with Inline;
   function Data_Address (File : Mapped_File) return System.Address
      with Inline, Obsolescent;
   --  Return the address of the internal buffer.
   --  Do not use this function directly, but via an instance of the
   --  package Data_Getters below.

   function Data_Size (Region : Mapped_Region) return File_Size
      with Inline;
   function Data_Size (File : Mapped_File) return File_Size
      with Inline, Obsolescent;
   --  Full size of the mapped region.
   --  Better to use one of the instances of Data_Getters instead.

   generic
      type Index_Type is range <>;
      --  The type of indexes used when mapping the file to memory.
      --  Typical values are 'Positive' when you want to read files less than
      --  2Gb in size, although you might want to use
      --  System.Storage_Elements.Storage_Offset or Long_Long_Integer on
      --  64 bits system supporting the mmap system call (which will allow
      --  you to manipulate Petabytes files...)

      type Base_Unconstrained_String is
         array (Index_Type range <>) of Character;
      --  How is memory represented.
      --  For small strings, it is recommended to use the String type
      --  directly for ease of use for the user.

   package Data_Getters is
      pragma Compile_Time_Error
         (Index_Type'First /= 1, "Wrong index type");

      subtype Extended_Index_Type is Index_Type'Base
         range 0 .. Index_Type'Last;

      subtype Unconstrained_String is Base_Unconstrained_String (Index_Type);
      type Str_Access is access all Unconstrained_String;
      pragma No_Strict_Aliasing (Str_Access);
      --  We do not use a String, which would limit the index to Integer and
      --  not allow us to load files larger than 2Gb.
      --  We also do not systematically use a
      --  System.Storage_Elements.Storage_Array, since it is easier for users
      --  if we directly have Character elements rather than Storage_Element.

      function Convert is new Ada.Unchecked_Conversion
         (System.Address, Str_Access);

      function To_Str_Access
        (Str : GNAT.Strings.String_Access) return Str_Access
        is (if Str = null then null else Convert (Str.all'Address));
      --  Convert Str. The returned value points to the same memory block,
      --  but no longer includes the bounds, which you need to manage yourself

      function Last (Region : Mapped_Region) return Extended_Index_Type
         is (Extended_Index_Type (Data_Size (Region)));
      --  Return the number of requested bytes mapped in this region. It is
      --  erroneous to access Data for indices outside 1 .. Last (Region).
      --  Such accesses may cause Storage_Error to be raised.
      --
      --  A constraint error is raised if the size of the region is larger
      --  than can be represented by Index_Type. So you need to pass a
      --  compatible Length parameter in your call to Open_Read.

      function Last (File : Mapped_File) return Extended_Index_Type
         is (Extended_Index_Type (Data_Size (File)))
         with Obsolescent;
      --  Return the number of requested bytes mapped in the region contained
      --  in File. It is erroneous to access Data for indices outside
      --  of 1 .. Last (File); such accesses may cause Storage_Error to
      --  be raised.

      function Data (Region : Mapped_Region) return Str_Access
         is (Convert (Data_Address (Region)));
      --  The data mapped in Region as requested. The result is an
      --  unconstrained string, so you cannot use the usual 'First and
      --  'Last attributes. Instead, these are respectively 1 and Size.

      function Data (File : Mapped_File) return Str_Access
         is (Convert (Data_Address (File)))
         with Obsolescent;
      --  Likewise for the region contained in File

   end Data_Getters;

   package Short is new Data_Getters (Positive, String);
   --  This package can be used when mapping files less than 2Gb.
   --  A range of the result of Data can be converted to a String, as in:
   --      S : constant String := String (Data (Region) (1 .. Last (Region)));

   subtype Long_Index is Long_Long_Integer range 1 .. Long_Long_Integer'Last;
   type Large_Unconstrained_String is array (Long_Index range <>) of Character;
   package Long is new Data_Getters (Long_Index, Large_Unconstrained_String);
   --  This package can be used when mapping files up to a petabyte.
   --  The whole data cannot be represented as a single string, so you'll
   --  need to iterate on it.

   subtype Str_Access is Short.Str_Access;
   function "=" (Left, Right : Str_Access) return Boolean
      renames Short."=";
   function Last (Region : Mapped_Region) return Positive
      renames Short.Last;
   function Last (File : Mapped_File) return Positive
      renames Short.Last;
   function Data (Region : Mapped_Region) return Str_Access
      renames Short.Data;
   function Data (File : Mapped_File) return Str_Access
      renames Short.Data;
   --  Convenient renamings, for backward compatibility.
   --  These functions only work for files up to 2Gb. For larger sizes,
   --  you should use Long.Str_Access, Long.Last and Long.Data instead.

   function Is_Mutable (Region : Mapped_Region) return Boolean;
   --  Return whether it is safe to change bytes in Data (Region). This is true
   --  for regions from writeable files, for regions mapped with the "Mutable"
   --  flag set, and for regions that are copied in a buffer. Note that it is
   --  not specified whether empty regions are mutable or not, since there is
   --  no byte no modify.

   function Is_Mmapped (File : Mapped_File) return Boolean
      with Inline;
   --  Whether regions for this file are opened through an mmap() system call
   --  or equivalent. This is in general irrelevant to your application, unless
   --  the file can be accessed by multiple concurrent processes or tasks. In
   --  such a case, and if the file is indeed mmap-ed, then the various parts
   --  of the file can be written simulatenously, and thus you cannot ensure
   --  the integrity of the file. If the file is not mmapped, the latest
   --  process to Close it overwrite what other processes have done.

   function Get_Page_Size return Positive;
   --  Returns the number of bytes in a page. Once a file is mapped from the
   --  disk, its offset and Length should be multiples of this page size (which
   --  is ensured by this package in any case). Knowing this page size allows
   --  you to map as much memory as possible at once, thus potentially reducing
   --  the number of system calls to read the file by chunks.

   function Read_Whole_File
     (Filename           : String;
      Empty_If_Not_Found : Boolean := False) return GNAT.Strings.String_Access;
   function Read_Whole_File
     (Filename           : String)
     return GNATCOLL.Strings.XString;
   --  Returns the whole contents of the file.
   --  The returned string must be freed by the user.
   --  This is a convenience function, which is of course slower than the ones
   --  above since we also need to allocate some memory, actually read the file
   --  and copy the bytes.
   --  If the file does not exist, null is returned. However, if
   --  Empty_If_Not_Found is True, then the empty string is returned instead.
   --  Filename should be compatible with the filesystem.
   --
   --  This function only works for files smaller than 2Gb.

private
   type Mapped_File_Record;
   type Mapped_File is access Mapped_File_Record;

   type Mapped_Region_Record;
   type Mapped_Region is access Mapped_Region_Record;

   Invalid_Mapped_File   : constant Mapped_File := null;
   Invalid_Mapped_Region : constant Mapped_Region := null;

end GNATCOLL.Mmap;
