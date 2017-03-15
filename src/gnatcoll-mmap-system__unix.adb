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

with Ada.IO_Exceptions;
with System; use System;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body GNATCOLL.Mmap.System is

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

   function Mmap (Start  : Standard.System.Address := Null_Address;
                  Length : File_Size;
                  Prot   : Mmap_Prot := PROT_READ;
                  Flags  : Mmap_Flags := MAP_PRIVATE;
                  Fd     : GNAT.OS_Lib.File_Descriptor;
                  Offset : File_Size := 0) return Standard.System.Address;
   pragma Import (C, Mmap, "gnatcoll_mmap");

   function Munmap (Start  : Standard.System.Address;
                    Length : File_Size) return Integer;
   pragma Import (C, Munmap, "gnatcoll_munmap");

   procedure Madvise (Addr   : Standard.System.Address;
                      Length : File_Size;
                      Advice : Use_Advice);
   pragma Import (C, Madvise, "gnatcoll_madvise");
   --  Allows a process that has knowledge of its memory behavior to
   --  describe it to the system. This advice applies to the mapped
   --  region at address Addr, and for the given Length. If Length
   --  is 0, this applies to the whole region.

   function Align
     (Addr : File_Size) return File_Size;
   --  Align some offset/length to the lowest page boundary

   function Is_Mapping_Available return Boolean;
   --  Wheter memory mapping is actually available on this system. It is an
   --  error to use Create_Mapping and Dispose_Mapping if this is False.

   ---------------
   -- Open_Read --
   ---------------

   function Open_Read
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return System_File is
      Fd : constant File_Descriptor :=
        Open_Read (Filename, Binary);
   begin
      if Fd = Invalid_FD then
         raise Ada.IO_Exceptions.Name_Error
           with "Cannot open " & Filename;
      end if;
      return
        (Fd     => Fd,
         Mapped => Use_Mmap_If_Available and then Is_Mapping_Available,
         Write  => False,
         Length => File_Size (File_Length (Fd)));
   end Open_Read;

   ----------------
   -- Open_Write --
   ----------------

   function Open_Write
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return System_File is
      Fd : constant File_Descriptor :=
        Open_Read_Write (Filename, Binary);
   begin
      if Fd = Invalid_FD then
         raise Ada.IO_Exceptions.Name_Error
         with "Cannot open " & Filename;
      end if;
      return
        (Fd     => Fd,
         Mapped => Use_Mmap_If_Available and then Is_Mapping_Available,
         Write  => True,
         Length => File_Size (File_Length (Fd)));
   end Open_Write;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out System_File) is
   begin
      Close (File.Fd);
      File.Fd := Invalid_FD;
   end Close;

   --------------------
   -- Read_From_Disk --
   --------------------

   function Read_From_Disk
     (File           : System_File;
      Offset, Length : File_Size) return GNAT.Strings.String_Access
   is
      Buffer : GNAT.Strings.String_Access :=
         new String (1 .. Integer (Length));
   begin
      --  ??? Lseek offset should be a size_t instead of a Long_Integer

      Lseek (File.Fd, Long_Integer (Offset), Seek_Set);
      if GNAT.OS_Lib.Read (File.Fd, Buffer.all'Address, Integer (Length))
        /= Integer (Length)
      then
         GNAT.Strings.Free (Buffer);
         raise Ada.IO_Exceptions.Device_Error;
      end if;
      return Buffer;
   end Read_From_Disk;

   -------------------
   -- Write_To_Disk --
   -------------------

   procedure Write_To_Disk
     (File           : System_File;
      Offset, Length : File_Size;
      Buffer         : GNAT.Strings.String_Access) is
   begin
      pragma Assert (File.Write);
      Lseek (File.Fd, Long_Integer (Offset), Seek_Set);
      if GNAT.OS_Lib.Write (File.Fd, Buffer.all'Address, Integer (Length))
        /= Integer (Length)
      then
         raise Ada.IO_Exceptions.Device_Error;
      end if;
   end Write_To_Disk;

   --------------------
   -- Create_Mapping --
   --------------------

   procedure Create_Mapping
     (File           : System_File;
      Offset, Length : in out File_Size;
      Mutable        : Boolean;
      Mapping        : out System_Mapping;
      Advice         : Use_Advice := Use_Normal)
   is
      Prot  : Mmap_Prot;
      Flags : Mmap_Flags;
   begin
      if File.Write then
         Prot  := PROT_READ or PROT_WRITE;
         Flags := MAP_SHARED;
      else
         Prot := PROT_READ;
         if Mutable then
            Prot := Prot or PROT_WRITE;
         end if;
         Flags := MAP_PRIVATE;
      end if;

      --  Adjust offset and mapping length to account for the required
      --  alignment of offset on page boundary.

      declare
         Queried_Offset : constant File_Size := Offset;
      begin
         Offset := Align (Offset);

         --  First extend the length to compensate the offset shift, then align
         --  it on the upper page boundary, so that the whole queried area is
         --  covered.

         Length := Length + Queried_Offset - Offset;
         Length := Align (Length + Get_Page_Size - 1);
      end;

      Mapping :=
        (Address => Mmap
           (Offset => Offset,
            Length => Length,
            Prot   => Prot,
            Flags  => Flags,
            Fd     => File.Fd),
         Length  => Length);

      if Advice /= Use_Normal then
         Madvise (Mapping.Address, Length => 0, Advice => Advice);
      end if;
   end Create_Mapping;

   ---------------------
   -- Dispose_Mapping --
   ---------------------

   procedure Dispose_Mapping
     (Mapping : in out System_Mapping)
   is
      Ignored : Integer;
      pragma Unreferenced (Ignored);
   begin
      Ignored := Munmap (Mapping.Address, Mapping.Length);
      Mapping := Invalid_System_Mapping;
   end Dispose_Mapping;

   --------------------------
   -- Is_Mapping_Available --
   --------------------------

   function Is_Mapping_Available return Boolean is
      function Has_Mmap return Integer;
      pragma Import (C, Has_Mmap, "gnatcoll_has_mmap");
   begin
      return Has_Mmap /= 0;
   end Is_Mapping_Available;

   -------------------
   -- Get_Page_Size --
   -------------------

   function Get_Page_Size return File_Size is
      function Internal return Integer;
      pragma Import (C, Internal, "getpagesize");
   begin
      return File_Size (Internal);
   end Get_Page_Size;

   -----------
   -- Align --
   -----------

   function Align
     (Addr : File_Size) return File_Size is
   begin
      return Addr - Addr mod Get_Page_Size;
   end Align;

end GNATCOLL.Mmap.System;
