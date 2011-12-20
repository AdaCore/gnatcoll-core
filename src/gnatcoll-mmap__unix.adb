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

with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Ada.Unchecked_Conversion;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Interfaces.C;             use Interfaces.C;
with System;                   use System;

package body GNATCOLL.Mmap is

   type Mmap_Prot is mod Interfaces.C.int'Last;
   for Mmap_Prot'Size use Interfaces.C.int'Size;
--     PROT_NONE  : constant Mmap_Prot := 16#00#;
--     PROT_EXEC  : constant Mmap_Prot := 16#04#;
   PROT_READ  : constant Mmap_Prot := 16#01#;
   PROT_WRITE : constant Mmap_Prot := 16#02#;

   type Mmap_Flags is mod Interfaces.C.int'Last;
   for Mmap_Flags'Size use Interfaces.C.int'Size;
--     MAP_NONE    : constant Mmap_Flags := 16#00#;
--     MAP_FIXED   : constant Mmap_Flags := 16#10#;
   MAP_SHARED  : constant Mmap_Flags := 16#01#;
   MAP_PRIVATE : constant Mmap_Flags := 16#02#;

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Str_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Str_Access, System.Address);

   function Mmap (Start  : System.Address := System.Null_Address;
                  Length : File_Size;
                  Prot   : Mmap_Prot := PROT_READ;
                  Flags  : Mmap_Flags := MAP_PRIVATE;
                  Fd     : GNAT.OS_Lib.File_Descriptor;
                  Offset : File_Size := 0) return System.Address;
   pragma Import (C, Mmap, "gnatcoll_mmap");

   function Munmap (Start : System.Address;
                    Length : File_Size) return Integer;
   pragma Import (C, Munmap, "gnatcoll_munmap");

   function Has_Mmap return Integer;
   pragma Import (C, Has_Mmap, "gnatcoll_has_mmap");

   procedure From_Disk (File : in out Mapped_File);
   --  Read a file from the disk

   procedure To_Disk (File : in out Mapped_File);
   --  Write the file back to disk if necessary, and free memory

   ---------------
   -- From_Disk --
   ---------------

   procedure From_Disk (File : in out Mapped_File) is
   begin
      File.Buffer := new String (1 .. File.Last);
      --  ??? Lseek offset should be a size_t instead of a Long_Integer.
      Lseek (File.Fd, Long_Integer (File.Offset), Seek_Set);

      if Read (File.Fd, File.Buffer.all'Address, File.Last) /= File.Last then
         GNAT.Strings.Free (File.Buffer);
         Close (File.Fd);
         raise Device_Error;
      else
         File.Mapped := False;
      end if;
   end From_Disk;

   -------------
   -- To_Disk --
   -------------

   procedure To_Disk (File : in out Mapped_File) is
   begin
      if File.Write and then File.Buffer /= null then
         Lseek (File.Fd, Long_Integer (File.Offset), Seek_Set);

         if Write (File.Fd, File.Buffer.all'Address, File.Last) /=
           File.Last
         then
            GNAT.Strings.Free (File.Buffer);
            Close (File.Fd);
            raise Device_Error;
         end if;
      end if;

      GNAT.Strings.Free (File.Buffer);
      File.Buffer := null;
   end To_Disk;

   -------------------
   -- Get_Page_Size --
   -------------------

   function Get_Page_Size return Integer is
      function Internal return Integer;
      pragma Import (C, Internal, "getpagesize");
   begin
      return Internal;
   end Get_Page_Size;

   ---------------
   -- Open_Read --
   ---------------

   function Open_Read
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return Mapped_File
   is
      Fd : File_Descriptor;
   begin
      Fd := Open_Read (Filename, Binary);
      if Fd = Invalid_FD then
         raise Name_Error with "Cannot open " & Filename;
      end if;

      return Mapped_File'
        (Data       => null,
         Buffer     => null,
         Offset     => 0,
         Last       => 0,
         --  ??? File_Length should return a size_t instead of a Long_Integer.
         Length     => File_Size (File_Length (Fd)),
         Write      => False,
         Mapped     => Use_Mmap_If_Available and (Has_Mmap = 1),
         Page_Size  => File_Size (Get_Page_Size),
         Fd         => Fd,
         Handle     => System.Null_Address,
         Map_Handle => System.Null_Address);
   end Open_Read;

   ----------------
   -- Open_Write --
   ----------------

   function Open_Write
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return Mapped_File
   is
      Fd : File_Descriptor;
   begin
      Fd := Open_Read_Write (Filename, Binary);
      if Fd = Invalid_FD then
         raise Name_Error;
      end if;

      return Mapped_File'
        (Data       => null,
         Buffer     => null,
         Offset     => 0,
         Last       => 0,
         Length     => File_Size (File_Length (Fd)),
         Write      => True,
         Mapped     => Use_Mmap_If_Available and (Has_Mmap = 1),
         Page_Size  => File_Size (Get_Page_Size),
         Fd         => Fd,
         Handle     => System.Null_Address,
         Map_Handle => System.Null_Address);
   end Open_Write;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out Mapped_File) is
      Ignored : Integer;
      pragma Unreferenced (Ignored);
   begin
      if File.Mapped then
         Ignored := Munmap (Convert (File.Data), File_Size (File.Last));
      end if;

      To_Disk (File);

      if File.Fd /= Invalid_FD then
         Close (File.Fd);
      end if;

      File := Invalid_Mapped_File;
   end Close;

   ----------
   -- Read --
   ----------

   procedure Read
     (File   : in out Mapped_File;
      Offset : File_Size := 0;
      Length : File_Size := 0)
   is
      Prot    : Mmap_Prot;
      Flags   : Mmap_Flags;
      Ignored : Integer;

      Previous_Last : constant Integer := File.Last;

      Map_Length : File_Size;
      --  When using mmap(2), the offset and mapping length must be multiples
      --  of the page size, so they may be different from the requested Offset
      --  and Length.

      pragma Unreferenced (Ignored);
   begin
      --  If Length is 0 or goes beyoned file size, map till end of file

      if Length = 0 or else Length > File.Length - Offset then
         Map_Length := File.Length - Offset;
      else
         Map_Length := Length;
      end if;

      --  If the requested section is already mapped, nothing to do

      if Offset >= File.Offset
        and then Offset + Map_Length <= File.Offset + File_Size (File.Last)
        and then (File.Data /= null or else File.Buffer /= null)
      then
         return;
      end if;

      File.Last   := Integer (Map_Length);
      File.Offset := Offset;

      --  mmap() will sometimes return NULL when the file exists but is empty,
      --  which is not what we want, so in the case of a zero length file we
      --  fall back to read(2)/write(2)-based mode.

      if File.Length > 0 and then File.Mapped then

         --  Unmap previous memory if necessary

         if File.Data /= null then
            Ignored := Munmap (Convert (File.Data), File_Size (Previous_Last));
            File.Data := null;
         end if;

         if File.Write then
            Prot  := PROT_WRITE;
            Flags := MAP_SHARED;
         else
            Prot  := PROT_READ;
            Flags := MAP_PRIVATE;
         end if;

         --  Adjust offset and mapping length to account for required alignment
         --  of offset on page boundary. Note that there is no requirement for
         --  File.Last to be rounded up to a multiple of page size.

         File.Offset := File.Offset - File.Offset mod File.Page_Size;
         File.Last   := File.Last + Integer (Offset - File.Offset);

         if Map_Length > File_Size (Integer'Last) then
            raise Device_Error;

         else
            File.Data := Convert
                           (Mmap (Offset => File.Offset,
                                  Length => File_Size (File.Last),
                                  Prot   => Prot,
                                  Flags  => Flags,
                                  Fd     => File.Fd));
         end if;

      else
         if File.Buffer /= null then
            To_Disk (File);
         end if;

         From_Disk (File);
      end if;
   end Read;

   ------------
   -- Offset --
   ------------

   function Offset (File : Mapped_File) return File_Size is
   begin
      return File.Offset;
   end Offset;

   ----------
   -- Last --
   ----------

   function Last (File : Mapped_File) return Integer is
   begin
      return File.Last;
   end Last;

   ------------
   -- Length --
   ------------

   function Length (File : Mapped_File) return File_Size is
   begin
      return File.Length;
   end Length;

   -------------------
   -- To_Str_Access --
   -------------------

   function To_Str_Access
     (Str : GNAT.Strings.String_Access) return Str_Access is
   begin
      if Str = null then
         return null;
      else
         return Convert (Str.all'Address);
      end if;
   end To_Str_Access;

   ----------
   -- Data --
   ----------

   function Data (File : Mapped_File) return Str_Access is
   begin
      if File.Mapped then
         return File.Data;
      else
         return To_Str_Access (File.Buffer);
      end if;
   end Data;

   ----------------
   -- Is_Mmapped --
   ----------------

   function Is_Mmapped (File : Mapped_File) return Boolean is
   begin
      return File.Mapped;
   end Is_Mmapped;

   ---------------------
   -- Read_Whole_File --
   ---------------------

   function Read_Whole_File
     (Filename           : String;
      Empty_If_Not_Found : Boolean := False) return GNAT.Strings.String_Access
   is
      File   : Mapped_File;
      Result : String_Access;
   begin
      File := Open_Read (Filename);
      Read (File);

      if File.Data /= null then
         Result := new String'(String (File.Data (1 .. File.Last)));

      elsif File.Buffer /= null then
         Result := File.Buffer;
         File.Buffer := null;  --  So that it is not deallocated
      end if;

      Close (File);

      return Result;

   exception
      when Name_Error =>
         if Empty_If_Not_Found then
            return new String'("");
         else
            return null;
         end if;

      when others =>
         Close (File);
         return null;
   end Read_Whole_File;

end GNATCOLL.Mmap;
