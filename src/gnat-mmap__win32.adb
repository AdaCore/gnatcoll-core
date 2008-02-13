-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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
pragma Warnings (Off);
with System.Win32;             use System; use System.Win32;
pragma Warnings (On);

package body GNAT.Mmap is

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

   --  The Win package contains copy of definition found in recent System.Win32
   --  unit provided with the GNAT compiler. The copy is needed to be able to
   --  compile this unit with older compilers. Note that this internal Win
   --  package can be removed when GNAT 6.2.0 and GNAT GPL 2009 will be out.
   --  ??? TO BE REMOVED

   package Win is

      INVALID_HANDLE_VALUE  : constant HANDLE := -1;
      FILE_BEGIN            : constant := 0;
      FILE_SHARE_READ       : constant := 16#00000001#;
      FILE_ATTRIBUTE_NORMAL : constant := 16#00000080#;

      function GetFileSize
        (HFile : HANDLE; LpFileSizeHigh : access DWORD) return BOOL;
      pragma Import (Stdcall, GetFileSize, "GetFileSize");

      function SetFilePointer
        (HFile                : HANDLE;
         LDistanceToMove      : LONG;
         LpDistanceToMoveHigh : access LONG;
         DwMoveMethod         : DWORD) return DWORD;
      pragma Import (Stdcall, SetFilePointer, "SetFilePointer");

      function CreateFileMapping
        (HFile                : HANDLE;
         LpSecurityAttributes : access SECURITY_ATTRIBUTES;
         FlProtect            : DWORD;
         DwMaximumSizeHigh    : DWORD;
         DwMaximumSizeLow     : DWORD;
         LpName               : Address) return HANDLE;
      pragma Import (Stdcall, CreateFileMapping, "CreateFileMappingA");

      function MapViewOfFile
        (HFileMappingObject   : HANDLE;
         DwDesiredAccess      : DWORD;
         DwFileOffsetHigh     : DWORD;
         DwFileOffsetLow      : DWORD;
         DwNumberOfBytesToMap : DWORD) return System.Address;
      pragma Import (Stdcall, MapViewOfFile, "MapViewOfFile");

      function UnmapViewOfFile (LpBaseAddress : System.Address) return BOOL;
      pragma Import (Stdcall, UnmapViewOfFile, "UnmapViewOfFile");

   end Win;

   ---------------
   -- From_Disk --
   ---------------

   procedure From_Disk (File : in out Mapped_File) is
      Pos : DWORD;
      Res : BOOL;
      pragma Unreferenced (Pos, Res);
   begin
      File.Buffer := new String (1 .. File.Last);

      Pos := Win.SetFilePointer
        (To_Handle (File.Handle), LONG (File.Offset), null, Win.FILE_BEGIN);

      if ReadFile
        (To_Handle (File.Handle), File.Buffer.all'Address,
         DWORD (File.Last), null, null) = Win32.FALSE
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
      Pos : DWORD;
      Res : BOOL;
      pragma Unreferenced (Pos, Res);
   begin
      if File.Write and then File.Buffer /= null then

         Pos := Win.SetFilePointer
           (To_Handle (File.Handle), LONG (File.Offset), null, Win.FILE_BEGIN);

         if WriteFile
           (To_Handle (File.Handle), File.Buffer.all'Address,
            DWORD (File.Last), null, null) = Win32.FALSE
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
      C_File : constant String := Filename & ASCII.NUL;
      H      : constant HANDLE :=
                 CreateFile
                   (C_File'Address, GENERIC_READ, Win.FILE_SHARE_READ,
                    null, OPEN_EXISTING, Win.FILE_ATTRIBUTE_NORMAL, 0);
      Size   : aliased DWORD;
   begin
      if H = Win.INVALID_HANDLE_VALUE then
         raise Name_Error;

      elsif Win.GetFileSize (H, Size'Access) = Win32.FALSE then
         raise Use_Error;
      end if;

      return Mapped_File'
        (Data       => null,
         Buffer     => null,
         Offset     => 0,
         Last       => 0,
         Length     => Long_Integer (Size),
         Write      => False,
         Mapped     => Use_Mmap_If_Available,
         Page_Size  => Long_Integer (Get_Page_Size),
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
      C_File : constant String := Filename & ASCII.NUL;
      H      : constant HANDLE :=
                 CreateFile
                   (C_File'Address, GENERIC_WRITE, 0,
                    null, OPEN_EXISTING, Win.FILE_ATTRIBUTE_NORMAL, 0);
      Size   : aliased DWORD;
   begin
      if H = Win.INVALID_HANDLE_VALUE then
         raise Name_Error;

      elsif Win.GetFileSize (H, Size'Access) = Win32.FALSE then
         raise Use_Error;
      end if;

      return Mapped_File'
        (Data       => null,
         Buffer     => null,
         Offset     => 0,
         Last       => 0,
         Length     => Long_Integer (Size),
         Write      => False,
         Mapped     => Use_Mmap_If_Available,
         Page_Size  => Long_Integer (Get_Page_Size),
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
         Res := Win.UnmapViewOfFile (Convert (File.Data));
      else
         To_Disk (File);
      end if;

      if To_Handle (File.Handle) /= Win.INVALID_HANDLE_VALUE then
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
      Offset : Long_Integer := 0;
      Length : Long_Integer := 0)
   is
      Len   : Long_Integer := Length;
      Extra : Long_Integer;
      Flags : DWORD;
      Res   : BOOL;
      Tmp   : Long_Integer;
      pragma Unreferenced (Res);
   begin
      --  If that part of the file is already readable, don't do anything

      if Len = 0 then
         Len := File.Length - Offset;
      end if;

      --  If the requested block is already in memory, do nothing

      if Offset >= File.Offset
        and then Offset + Len <= File.Offset + Long_Integer (File.Last)
        and then (File.Data /= null or else File.Buffer /= null)
      then
         return;
      end if;

      if File.Length > 0 and then File.Mapped then

         --  Unmap previous memory if necessary

         if File.Data /= null then
            Res := Win.UnmapViewOfFile (Convert (File.Data));
            File.Data := null;
         end if;

         Extra := Offset mod File.Page_Size;

         if File.Write then
            Flags := FILE_MAP_WRITE;
         else
            Flags := FILE_MAP_READ;
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

            File.Last := Integer (Tmp);

            File.Map_Handle := To_Address
              (Win.CreateFileMapping
                 (To_Handle (File.Handle), null, PAGE_READONLY,
                  0, DWORD (File.Length), System.Null_Address));

            File.Data := Convert
              (Win.MapViewOfFile
                 (To_Handle (File.Map_Handle), Flags, 0, 0, 0));
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

end GNAT.Mmap;
