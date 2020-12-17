------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

--  Win32 Files related APIs (see Microsoft MSDN for more information).
with GNATCOLL.OS.FS;

package GNATCOLL.OS.Win32.Files is

   package FS renames GNATCOLL.OS.FS;

   type Open_Mode is new UINT;

   O_RDONLY      : constant Open_Mode := 16#0000#;
   O_WRONLY      : constant Open_Mode := 16#0001#;
   O_RDWR        : constant Open_Mode := 16#0002#;
   O_APPEND      : constant Open_Mode := 16#0008#;
   O_CREAT       : constant Open_Mode := 16#0100#;
   O_TRUNC       : constant Open_Mode := 16#0200#;
   O_EXCL        : constant Open_Mode := 16#0400#;
   O_TEXT        : constant Open_Mode := 16#4000#;
   O_BINARY      : constant Open_Mode := 16#8000#;
   O_WTEXT       : constant Open_Mode := 16#10000#;
   O_U16TEXT     : constant Open_Mode := 16#20000#;
   O_U8TEXT      : constant Open_Mode := 16#40000#;
   O_NOINHERIT   : constant Open_Mode := 16#0080#;
   O_TEMPORARY   : constant Open_Mode := 16#0040#;
   O_SHORT_LIVED : constant Open_Mode := 16#1000#;
   O_SEQUENTIAL  : constant Open_Mode := 16#0020#;
   O_RANDOM      : constant Open_Mode := 16#0010#;

   type Permission_Mode is new UINT;

   S_IREAD : constant Permission_Mode := 16#0100#;
   S_IWRITE : constant Permission_Mode := 16#0080#;

   subtype ACCESS_MASK is DWORD;
   subtype SHARE_ACCESS is ULONG;
   subtype FILE_ATTRIBUTE is ULONG;

   DIRECTORY     : constant FILE_ATTRIBUTE := 16#00000010#;
   REPARSE_POINT : constant FILE_ATTRIBUTE := 16#00000400#;

   FILE_READ  : constant ACCESS_MASK := 1;
   FILE_WRITE : constant ACCESS_MASK := 2;

   SHARE_READ   : constant SHARE_ACCESS := 1;
   SHARE_WRITE  : constant SHARE_ACCESS := 2;
   SHARE_DELETE : constant SHARE_ACCESS := 4;
   SHARE_ALL    : constant SHARE_ACCESS := 7;

   type OBJECT_ATTRIBUTES is record
      Length                   : ULONG;
      RootDirectory            : HANDLE;
      ObjectName               : PUNICODE_STRING;
      Attributes               : ULONG;
      SecurityDescriptor       : LPVOID;
      SecurityQualityOfService : LPVOID;
   end record
   with Convention => C_Pass_By_Copy;

   type POBJECT_ATTRIBUTES is access all OBJECT_ATTRIBUTES;

   type FILE_OBJECT_ATTRIBUTES is record
      OA     : OBJECT_ATTRIBUTES;
      Path   : UNICODE_PATH;
   end record;

   procedure Initialize (FA   : in out FILE_OBJECT_ATTRIBUTES;
                         Name : String);

   type IO_STATUS_BLOCK is record
      Information : ULONG;
   end record
   with Convention => C_Pass_By_Copy;

   type PIO_STATUS_BLOCK is access all IO_STATUS_BLOCK;

   function NtOpenFile
     (FileHandle       : out HANDLE;
      DesiredAccess    : ACCESS_MASK;
      ObjectAttributes : POBJECT_ATTRIBUTES;
      IoStatusBlock    : out IO_STATUS_BLOCK;
      ShareAccess      : ULONG;
      OpenOptions      : ULONG)
      return NTSTATUS
    with Import => True,
        Convention => Stdcall,
        External_Name => "NtOpenFile";

   function NtCreateFile
     (FileHandle        : in out HANDLE;
      DesiredAccess     : ACCESS_MASK;
      ObjectAttributes  : POBJECT_ATTRIBUTES;
      IoStatusBlock     : in out IO_STATUS_BLOCK;
      AllocationSize    : PLARGE_INTEGER;
      FileAttributes    : ULONG;
      ShareAccess       : ULONG;
      CreateDisposition : ULONG;
      CreateOptions     : ULONG;
      EaBuffer          : LPVOID := NULL_LPVOID;
      EaLength          : ULONG  := 0)
      return NTSTATUS
   with Import => True,
        Convention => Stdcall,
        External_Name => "NtCreateFile";

   subtype FILE_INFORMATION_CLASS is unsigned;
   FileBasicInformation : constant FILE_INFORMATION_CLASS := 4;

   function NtQueryInformationFile
     (hFile : HANDLE;
      io    : PIO_STATUS_BLOCK;
      ptr   : LPVOID;
      len   : ULONG;
      FileInformationClass : FILE_INFORMATION_CLASS)
   return NTSTATUS
   with Import => True,
        Convention => Stdcall,
        External_Name => "NtQueryInformationFile";

   type FILE_BASIC_INFORMATION is record
      CreationTime   : LARGE_INTEGER;
      LastAccessTime : LARGE_INTEGER;
      LastWriteTime  : LARGE_INTEGER;
      ChangeTime     : LARGE_INTEGER;
      FileAttributes : FILE_ATTRIBUTE;
   end record
   with Convention => C_Pass_By_Copy;

   function NtQueryAttributesFile
      (ObjectAttributes : in out OBJECT_ATTRIBUTES;
       Information      : out FILE_BASIC_INFORMATION)
   return NTSTATUS
   with Import => True,
        Convention => Stdcall,
        External_Name => "NtQueryAttributesFile";

   function Open
      (Filename : C_WString;
       Flags    : Open_Mode;
       Mode     : Permission_Mode)
      return FS.File_Descriptor
   with Import => True,
        Convention => Stdcall,
        External_Name => "_wopen";

   function CreatePipe
      (ReadPipe       : out HANDLE;
       WritePipe      : out HANDLE;
       PipeAttributes : LPSECURITY_ATTRIBUTES;
       Size           : DWORD) return BOOL
   with Import        => True,
        Convention    => Stdcall,
        External_Name => "CreatePipe";

   function GetOSFHandle (FD : FS.File_Descriptor) return HANDLE
   with Import        => True,
        Convention    => C,
        External_Name => "_get_osfhandle";

   function OpenOSFHandle
      (Object : HANDLE; Flags : Integer := 0) return FS.File_Descriptor
   with Import        => True,
        Convention    => C,
        External_Name => "_open_osfhandle";

   type Handle_Flag is new DWORD;

   HANDLE_FLAG_INHERIT            : constant Handle_Flag := 16#01#;
   HANDLE_FLAG_PROTECT_FROM_CLOSE : constant Handle_Flag := 16#02#;

   function SetHandleInformation
      (Object : HANDLE; Mask : Handle_Flag; Flags : Handle_Flag)
      return BOOL
   with Import        => True,
        Convention    => Stdcall,
        External_Name => "SetHandleInformation";

end GNATCOLL.OS.Win32.Files;
