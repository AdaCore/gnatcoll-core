------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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

   --  The Win package contains copy of definition found in recent System.Win32
   --  unit provided with the GNAT compiler. The copy is needed to be able to
   --  compile this unit with older compilers. Note that this internal Win
   --  package can be removed when GNAT 6.1.0 is not supported anymore.

   package Win is

      subtype PVOID is System.Address;

      type HANDLE is new Interfaces.C.ptrdiff_t;

      type WORD   is new Interfaces.C.unsigned_short;
      type DWORD  is new Interfaces.C.unsigned_long;
      type LONG   is new Interfaces.C.long;
      type SIZE_T is new Interfaces.C.size_t;

      type BOOL   is new Interfaces.C.int;
      for BOOL'Size use Interfaces.C.int'Size;

      FALSE : constant := 0;

      GENERIC_READ  : constant := 16#80000000#;
      GENERIC_WRITE : constant := 16#40000000#;
      OPEN_EXISTING : constant := 3;

      type OVERLAPPED is record
         Internal     : DWORD;
         InternalHigh : DWORD;
         Offset       : DWORD;
         OffsetHigh   : DWORD;
         hEvent       : HANDLE;
      end record;

      type SECURITY_ATTRIBUTES is record
         nLength             : DWORD;
         pSecurityDescriptor : PVOID;
         bInheritHandle      : BOOL;
      end record;

      type SYSTEM_INFO is record
         dwOemId : DWORD;
         dwPageSize : DWORD;
         lpMinimumApplicationAddress : PVOID;
         lpMaximumApplicationAddress : PVOID;
         dwActiveProcessorMask       : PVOID;
         dwNumberOfProcessors        : DWORD;
         dwProcessorType             : DWORD;
         dwAllocationGranularity     : DWORD;
         wProcessorLevel             : WORD;
         wProcessorRevision          : WORD;
      end record;
      type LP_SYSTEM_INFO is access all SYSTEM_INFO;

      INVALID_HANDLE_VALUE  : constant HANDLE := -1;
      FILE_BEGIN            : constant := 0;
      FILE_SHARE_READ       : constant := 16#00000001#;
      FILE_ATTRIBUTE_NORMAL : constant := 16#00000080#;
      FILE_MAP_READ         : constant := 4;
      FILE_MAP_WRITE        : constant := 2;
      PAGE_READONLY         : constant := 16#0002#;
      PAGE_READWRITE        : constant := 16#0004#;
      INVALID_FILE_SIZE     : constant := 16#FFFFFFFF#;

      function CreateFile
        (lpFileName            : System.Address;
         dwDesiredAccess       : DWORD;
         dwShareMode           : DWORD;
         lpSecurityAttributes  : access SECURITY_ATTRIBUTES;
         dwCreationDisposition : DWORD;
         dwFlagsAndAttributes  : DWORD;
         hTemplateFile         : HANDLE) return HANDLE;
      pragma Import (Stdcall, CreateFile, "CreateFileW");

      function WriteFile
        (hFile                  : HANDLE;
         lpBuffer               : System.Address;
         nNumberOfBytesToWrite  : DWORD;
         lpNumberOfBytesWritten : access DWORD;
         lpOverlapped           : access OVERLAPPED) return BOOL;
      pragma Import (Stdcall, WriteFile, "WriteFile");

      function ReadFile
        (hFile                : HANDLE;
         lpBuffer             : System.Address;
         nNumberOfBytesToRead : DWORD;
         lpNumberOfBytesRead  : access DWORD;
         lpOverlapped         : access OVERLAPPED) return BOOL;
      pragma Import (Stdcall, ReadFile, "ReadFile");

      function CloseHandle (hObject : HANDLE) return BOOL;
      pragma Import (Stdcall, CloseHandle, "CloseHandle");

      function GetFileSize
        (hFile : HANDLE; lpFileSizeHigh : access DWORD) return DWORD;
      pragma Import (Stdcall, GetFileSize, "GetFileSize");

      function SetFilePointer
        (hFile                : HANDLE;
         lDistanceToMove      : LONG;
         lpDistanceToMoveHigh : access LONG;
         dwMoveMethod         : DWORD) return DWORD;
      pragma Import (Stdcall, SetFilePointer, "SetFilePointer");

      function CreateFileMapping
        (hFile                : HANDLE;
         lpSecurityAttributes : access SECURITY_ATTRIBUTES;
         flProtect            : DWORD;
         dwMaximumSizeHigh    : DWORD;
         dwMaximumSizeLow     : DWORD;
         lpName               : System.Address) return HANDLE;
      pragma Import (Stdcall, CreateFileMapping, "CreateFileMappingW");

      function MapViewOfFile
        (hFileMappingObject   : HANDLE;
         dwDesiredAccess      : DWORD;
         dwFileOffsetHigh     : DWORD;
         dwFileOffsetLow      : DWORD;
         dwNumberOfBytesToMap : SIZE_T) return System.Address;
      pragma Import (Stdcall, MapViewOfFile, "MapViewOfFile");

      function UnmapViewOfFile (lpBaseAddress : System.Address) return BOOL;
      pragma Import (Stdcall, UnmapViewOfFile, "UnmapViewOfFile");

      procedure GetSystemInfo (lpSystemInfo : LP_SYSTEM_INFO);
      pragma Import (Stdcall, GetSystemInfo, "GetSystemInfo");

   end Win;

   use Win;

   function To_Handle is
     new Ada.Unchecked_Conversion (System.Address, HANDLE);
   function To_Address is
     new Ada.Unchecked_Conversion (HANDLE, System.Address);

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

      Pos := Win.SetFilePointer
        (To_Handle (File.Handle), LONG (File.Offset), null, Win.FILE_BEGIN);

      --  Take care of the case where File.Last is 0, as this
      --  causes a SEGV in call to ReadFile.
      if File.Last > 0
        and then ReadFile
          (To_Handle (File.Handle), File.Buffer (1)'Address,
           DWORD (File.Last), NbRead'Unchecked_Access, null) = Win.FALSE
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

         Pos := Win.SetFilePointer
           (To_Handle (File.Handle), LONG (File.Offset), null, Win.FILE_BEGIN);

         if WriteFile
           (To_Handle (File.Handle), File.Buffer.all'Address,
            DWORD (File.Last), Written'Unchecked_Access, null) = Win.FALSE
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
                 Codec.From_UTF8 (Filename) & Wide_Character'Val (0);
      H      : constant HANDLE :=
                 CreateFile
                   (W_File'Address, GENERIC_READ, Win.FILE_SHARE_READ,
                    null, OPEN_EXISTING, Win.FILE_ATTRIBUTE_NORMAL, 0);
      SizeH  : aliased DWORD;
      Size   : File_Size;

   begin
      if H = Win.INVALID_HANDLE_VALUE then
         raise Name_Error;
      end if;

      Size := File_Size (Win.GetFileSize (H, SizeH'Access));

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
                 Codec.From_UTF8 (Filename) & Wide_Character'Val (0);
      H      : constant HANDLE :=
                 CreateFile
                   (W_File'Address, GENERIC_READ + GENERIC_WRITE, 0,
                    null, OPEN_EXISTING, Win.FILE_ATTRIBUTE_NORMAL, 0);
      SizeH  : aliased DWORD;
      Size   : File_Size;
   begin
      if H = Win.INVALID_HANDLE_VALUE then
         raise Name_Error;
      end if;

      Size := File_Size (Win.GetFileSize (H, SizeH'Access));

      if Size = INVALID_FILE_SIZE then
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
            Res := Win.UnmapViewOfFile (Convert (File.Data));
            File.Data := null;
         end if;

         Extra := Offset mod File.Page_Size;

         if File.Write then
            Flags := Win.FILE_MAP_WRITE;
            PageFlags := Win.PAGE_READWRITE;
         else
            Flags := Win.FILE_MAP_READ;
            PageFlags := Win.PAGE_READONLY;
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
                 (Win.CreateFileMapping
                    (To_Handle (File.Handle), null, PageFlags,
                     0, DWORD (File.Length), System.Null_Address));
            end if;

            File.Data := Convert
              (Win.MapViewOfFile
                 (To_Handle (File.Map_Handle), Flags,
                  0, DWORD (File.Offset), SIZE_T (File.Last)));
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
