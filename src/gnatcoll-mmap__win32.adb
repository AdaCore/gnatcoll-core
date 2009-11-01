-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                  Copyright (C) 2008-2009, AdaCore                 --
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
with System.Win32;             use System; use System.Win32;

with GNAT.OS_Lib;              use GNAT.OS_Lib;
with GNATCOLL.IO.Native;       use GNATCOLL.IO.Native;

package body GNATCOLL.Mmap is

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Str_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Str_Access, System.Address);

   procedure From_Disk (File : in out Mapped_File);
   --  Read a file from the disk

   procedure To_Disk (File : in out Mapped_File);
   --  Write the file back to disk if necessary, and free memory

   function To_Handle is
     new Ada.Unchecked_Conversion (System.Address, HANDLE);
   function To_Address is
     new Ada.Unchecked_Conversion (HANDLE, System.Address);

   --  ??? This package should be removed when GNAT 6.3 and GNAT GPL 2010
   --  are out. Definitions from System.Win32 are to be used instead. Make
   --  sure that any Windows definitions added here are also copied into
   --  System.Win32.

   package Win is
      INVALID_FILE_SIZE : constant := 16#FFFFFFFF#;
   end Win;

   -------------------
   -- Get_Page_Size --
   -------------------

   function Get_Page_Size return Integer is
      SystemInfo : aliased SYSTEM_INFO;
   begin
      GetSystemInfo (SystemInfo'Unchecked_Access);
      return Integer (SystemInfo.dwAllocationGranularity);
   end Get_Page_Size;

   ---------------
   -- From_Disk --
   ---------------

   procedure From_Disk (File : in out Mapped_File) is
      Pos    : DWORD;
      Res    : BOOL;
      NbRead : aliased DWORD;
      pragma Unreferenced (Pos, Res);
   begin
      if File.Buffer /= null then
         GNAT.Strings.Free (File.Buffer);
      end if;

      File.Buffer := new String (1 .. File.Last);

      Pos := SetFilePointer
        (To_Handle (File.Handle), LONG (File.Offset), null, FILE_BEGIN);

      --  Take care of the case where File.Last is 0, as this
      --  causes a SEGV in call to ReadFile.
      if File.Last > 0
        and then ReadFile
          (To_Handle (File.Handle), File.Buffer (1)'Address,
           DWORD (File.Last), NbRead'Unchecked_Access, null) = Win32.FALSE
      then
         GNAT.Strings.Free (File.Buffer);
         Res := CloseHandle (To_Handle (File.Handle));
         raise Device_Error;
      else
         File.Mapped := False;
      end if;
   end From_Disk;

   -------------
   -- To_Disk --
   -------------

   procedure To_Disk (File : in out Mapped_File) is
      Pos     : DWORD;
      Res     : BOOL;
      Written : aliased DWORD;
      pragma Unreferenced (Pos, Res);
   begin
      if File.Write and then File.Buffer /= null then

         Pos := SetFilePointer
           (To_Handle (File.Handle), LONG (File.Offset), null, FILE_BEGIN);

         if WriteFile
           (To_Handle (File.Handle), File.Buffer.all'Address,
            DWORD (File.Last), Written'Unchecked_Access, null) = Win32.FALSE
         then
            GNAT.Strings.Free (File.Buffer);
            Res := CloseHandle (To_Handle (File.Handle));
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
      W_File : constant Wide_String :=
                 Codec.From_Utf8 (Filename) & Wide_Character'Val (0);
      H      : constant HANDLE :=
                 CreateFile
                   (W_File'Address, GENERIC_READ, FILE_SHARE_READ,
                    null, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      SizeH  : aliased DWORD;
      Size   : File_Size;

   begin
      if H = INVALID_HANDLE_VALUE then
         raise Name_Error;
      end if;

      Size := File_Size (GetFileSize (H, SizeH'Access));

      if Size = Win.INVALID_FILE_SIZE then
         raise Use_Error;
      end if;

      if SizeH /= 0 and then File_Size'Size > 32 then
         Size := Size + (File_Size (SizeH) * 16#FFFF_FFFF#);
      end if;

      return Mapped_File'
        (Data       => null,
         Buffer     => null,
         Offset     => 0,
         Last       => 0,
         Length     => Size,
         Write      => False,
         Mapped     => Use_Mmap_If_Available,
         Page_Size  => File_Size (Get_Page_Size),
         Fd         => Invalid_FD,
         Handle     => To_Address (H),
         Map_Handle => System.Null_Address);
   end Open_Read;

   ----------------
   -- Open_Write --
   ----------------

   function Open_Write
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return Mapped_File
   is
      W_File : constant Wide_String :=
                 Codec.From_Utf8 (Filename) & Wide_Character'Val (0);
      H      : constant HANDLE :=
                 CreateFile
                   (W_File'Address, GENERIC_READ + GENERIC_WRITE, 0,
                    null, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      SizeH  : aliased DWORD;
      Size   : File_Size;
   begin
      if H = INVALID_HANDLE_VALUE then
         raise Name_Error;
      end if;

      Size := File_Size (GetFileSize (H, SizeH'Access));

      if Size = Win.INVALID_FILE_SIZE then
         raise Use_Error;
      end if;

      if SizeH /= 0 and then File_Size'Size > 32 then
         Size := Size + (File_Size (SizeH) * 16#FFFF_FFFF#);
      end if;

      return Mapped_File'
        (Data       => null,
         Buffer     => null,
         Offset     => 0,
         Last       => 0,
         Length     => Size,
         Write      => True,
         Mapped     => Use_Mmap_If_Available,
         Page_Size  => File_Size (Get_Page_Size),
         Fd         => Invalid_FD,
         Handle     => To_Address (H),
         Map_Handle => System.Null_Address);
   end Open_Write;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out Mapped_File) is
      Res : BOOL;
      pragma Unreferenced (Res);
   begin
      if File.Mapped then
         Res := UnmapViewOfFile (Convert (File.Data));
      else
         To_Disk (File);
      end if;

      if To_Handle (File.Handle) /= INVALID_HANDLE_VALUE then
         Res := CloseHandle (To_Handle (File.Map_Handle));
         Res := CloseHandle (To_Handle (File.Handle));
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
      Len       : File_Size := Length;
      Extra     : File_Size;
      Flags     : DWORD;
      PageFlags : DWORD;
      Res       : BOOL;
      Tmp       : File_Size;
      pragma Unreferenced (Res);
      use type System.Address;
   begin
      --  If that part of the file is already readable, don't do anything

      if Len = 0 then
         Len := File.Length - Offset;
      end if;

      --  If the requested block is already in memory, do nothing

      if Offset >= File.Offset
        and then Offset + Len <= File.Offset + File_Size (File.Last)
        and then (File.Data /= null or else File.Buffer /= null)
      then
         return;
      end if;

      if File.Length > 0 and then File.Mapped then

         --  Unmap previous memory if necessary

         if File.Data /= null then
            Res := UnmapViewOfFile (Convert (File.Data));
            File.Data := null;
         end if;

         Extra := Offset mod File.Page_Size;

         if File.Write then
            Flags := FILE_MAP_WRITE;
            PageFlags := PAGE_READWRITE;
         else
            Flags := FILE_MAP_READ;
            PageFlags := PAGE_READONLY;
         end if;

         Tmp := Len + Extra;

         if Tmp mod File.Page_Size /= 0 then
            Tmp :=
              (Len + Extra + File.Page_Size) / File.Page_Size * File.Page_Size;
         end if;

         if Tmp > File_Size (Integer'Last) then
            raise Device_Error;

         else
            File.Offset := Offset - Extra;

            if File.Offset + Tmp > File.Length then
               Tmp := File.Length - File.Offset;
            end if;

            File.Last := Integer (Tmp);

            if File.Map_Handle = System.Null_Address then
               File.Map_Handle := To_Address
                 (CreateFileMapping
                    (To_Handle (File.Handle), null, PageFlags,
                     0, DWORD (File.Length), System.Null_Address));
            end if;

            File.Data := Convert
              (MapViewOfFile
                 (To_Handle (File.Map_Handle), Flags,
                  0, DWORD (File.Offset), DWORD (File.Last)));
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
   end Read_Whole_File;

end GNATCOLL.Mmap;
