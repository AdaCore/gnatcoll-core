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

with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Ada.Unchecked_Conversion;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Interfaces.C;             use Interfaces.C;
with System;                   use System;

package body GNAT.Mmap is

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
                  Length : Long_Integer;
                  Prot   : Mmap_Prot := PROT_READ;
                  Flags  : Mmap_Flags := MAP_PRIVATE;
                  Fd     : GNAT.OS_Lib.File_Descriptor;
                  Offset : Long_Integer := 0) return System.Address;
   pragma Import (C, Mmap, "gnatlib_mmap");

   function Munmap (Start : System.Address;
                    Length : Long_Integer) return Integer;
   pragma Import (C, Munmap, "gnatlib_munmap");

   function Has_Mmap return Integer;
   pragma Import (C, Has_Mmap, "gnatlib_has_mmap");

   procedure From_Disk (File : in out Mapped_File);
   --  Read a file from the disk

   procedure To_Disk (File : in out Mapped_File);
   --  Write the file back to disk if necessary, and free memory

   ---------------
   -- From_Disk --
   ---------------

   procedure From_Disk (File : in out Mapped_File) is
   begin
      File.Buffer   := new String (1 .. File.Last);
      Lseek (File.Fd, File.Offset, Seek_Set);

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
         Lseek (File.Fd, File.Offset, Seek_Set);
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

   ---------------
   -- Open_Read --
   ---------------

   function Open_Read
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return Mapped_File
   is
      Fd   : File_Descriptor;
   begin
      Fd := Open_Read (Filename, Binary);
      if Fd = Invalid_FD then
         raise Name_Error;
      end if;

      return Mapped_File'
        (Data      => null,
         Buffer    => null,
         Offset    => 0,
         Last      => 0,
         Length    => File_Length (Fd),
         Write     => False,
         Mapped    => Use_Mmap_If_Available and (Has_Mmap = 1),
         Page_Size => Long_Integer (Get_Page_Size),
         Fd        => Fd,
         Handle    => System.Null_Address);
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
        (Data      => null,
         Buffer    => null,
         Offset    => 0,
         Last      => 0,
         Length    => File_Length (Fd),
         Write     => True,
         Mapped    => Use_Mmap_If_Available and (Has_Mmap = 1),
         Page_Size => Long_Integer (Get_Page_Size),
         Fd        => Fd,
         Handle    => System.Null_Address);
   end Open_Write;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out Mapped_File) is
      Ignored : Integer;
      pragma Unreferenced (Ignored);
   begin
      if File.Mapped then
         Ignored := Munmap (Convert (File.Data), Long_Integer (File.Last));
      else
         To_Disk (File);
      end if;

      if File.Fd /= Invalid_FD then
         Close (File.Fd);
      end if;

      File.Fd     := Invalid_FD;
      File.Data   := null;
      File.Buffer := null;
      File.Handle := System.Null_Address;
      File.Offset := 0;
      File.Length := 0;
      File.Last   := 0;
   end Close;

   ----------
   -- Read --
   ----------

   procedure Read
     (File   : in out Mapped_File;
      Offset : Long_Integer := 0;
      Length : Long_Integer := 0)
   is
      Len     : Long_Integer := Length;
      Extra   : Long_Integer;
      Prot    : Mmap_Prot;
      Flags   : Mmap_Flags;
      Ignored : Integer;
      Tmp     : Long_Integer;
      pragma Unreferenced (Ignored);
   begin
      --  If that part of the file is already readable, don't do anything

      if Len = 0 then
         Len := File.Length - Offset;
      end if;

      if Offset >= File.Offset
        and then Offset + Len <= File.Offset + Long_Integer (File.Last)
      then
         return;
      end if;

      if File.Mapped then
         --  Unmap previous memory if necessary
         if File.Data /= null then
            Ignored := Munmap (Convert (File.Data), Long_Integer (File.Last));
            File.Data := null;
         end if;

         Extra := (Offset mod File.Page_Size);

         if File.Write then
            Prot  := PROT_WRITE;
            Flags := MAP_SHARED;
         else
            Prot  := PROT_READ;
            Flags := MAP_PRIVATE;
         end if;

         Tmp := Len + Extra;
         if Tmp mod File.Page_Size /= 0 then
            Tmp :=
              (Len + Extra + File.Page_Size) / File.Page_Size * File.Page_Size;
         end if;
         if Tmp > Long_Integer (Integer'Last) then
            raise Device_Error;
         else
            File.Offset := Offset - Extra;

            if File.Offset + Tmp - 1 > File.Length then
               Tmp := File.Length - File.Offset;
            end if;

            File.Last   := Integer (Tmp);
            File.Data := Convert
              (Mmap (Offset => File.Offset,
                     Length => Long_Integer (File.Last),
                     Prot   => Prot,
                     Flags  => Flags,
                     Fd     => File.Fd));
         end if;

      else
         if File.Buffer /= null then
            To_Disk (File);
         end if;

         File.Offset := Offset;
         Tmp         := Len;
         if File.Offset + Tmp - 1 > File.Length then
            Tmp  := File.Length - File.Offset;
         end if;

         File.Last   := Integer (Tmp);

         From_Disk (File);
      end if;
   end Read;

   ------------
   -- Offset --
   ------------

   function Offset (File : Mapped_File) return Long_Integer is
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

   function Length (File : Mapped_File) return Long_Integer is
   begin
      return File.Length;
   end Length;

   -------------------
   -- To_Str_Access --
   -------------------

   function To_Str_Access
     (Str : GNAT.Strings.String_Access) return Str_Access
   is
   begin
      return Convert (Str (Str'First)'Address);
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
      else
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
   end Read_Whole_File;

end GNAT.Mmap;
