------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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
   subtype OPEN_OPTION is ULONG;

   READONLY      : constant FILE_ATTRIBUTE := 16#00000001#;
   HIDDEN        : constant FILE_ATTRIBUTE := 16#00000002#;
   DIRECTORY     : constant FILE_ATTRIBUTE := 16#00000010#;
   NORMAL        : constant FILE_ATTRIBUTE := 16#00000080#;
   REPARSE_POINT : constant FILE_ATTRIBUTE := 16#00000400#;

   FILE_READ            : constant ACCESS_MASK := 16#0001#;
   FILE_WRITE           : constant ACCESS_MASK := 16#0002#;
   FILE_READ_ATTRIBUTES : constant ACCESS_MASK := 16#0080#;
   FILE_LIST_DIRECTORY  : constant ACCESS_MASK := 16#0001#;
   SYNCHRONIZE          : constant ACCESS_MASK := 16#00100000#;

   SHARE_READ   : constant SHARE_ACCESS := 1;
   SHARE_WRITE  : constant SHARE_ACCESS := 2;
   SHARE_DELETE : constant SHARE_ACCESS := 4;
   SHARE_ALL    : constant SHARE_ACCESS := 7;

   FILE_OPEN_FOR_BACKUP_INTENT : constant OPEN_OPTION := 16#00004000#;
   FILE_SYNCHRONOUS_IO_NONALERT : constant OPEN_OPTION := 16#00000020#;

   type OBJECT_ATTRIBUTES is record
      Length                   : ULONG;
      RootDirectory            : HANDLE;
      ObjectName               : PUNICODE_STRING;
      Attributes               : ULONG;
      SecurityDescriptor       : LPVOID;
      SecurityQualityOfService : LPVOID;
   end record
   with Convention => C_Pass_By_Copy;

   type FILE_OBJECT_ATTRIBUTES is record
      OA     : OBJECT_ATTRIBUTES;
      Path   : UNICODE_PATH;
   end record
   with Convention => C_Pass_By_Copy;

   procedure Initialize (FA   : in out FILE_OBJECT_ATTRIBUTES;
                         Name : String);

   type IO_STATUS_BLOCK is record
      Pointer     : LPVOID;
      Information : ULONG;
   end record
   with Convention => C_Pass_By_Copy;

   function NtOpenFile
      (FileHandle     : in out HANDLE;
       Path           : in out UNICODE_STRING;
       DesiredAccess  : ACCESS_MASK;
       IoStatusBlock  : in out IO_STATUS_BLOCK;
       ShareAccess    : SHARE_ACCESS;
       OpenOptions    : ULONG)
       return NTSTATUS
   with Import => True,
        Convention => C,
        External_Name => "__gnatcoll_ntopenfile";

   function NtClose (FileHandle : HANDLE) return NTSTATUS
   with Import => True,
        Convention => Stdcall,
        External_Name => "NtClose";

   function NtCreateFile
     (FileHandle        : in out HANDLE;
      DesiredAccess     : ACCESS_MASK;
      ObjectAttributes  : in out OBJECT_ATTRIBUTES;
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
   FileDirectoryInformation : constant FILE_INFORMATION_CLASS := 1;
   FileBasicInformation     : constant FILE_INFORMATION_CLASS := 4;
   FileAllInformation       : constant FILE_INFORMATION_CLASS := 18;
   FileStatInformation      : constant FILE_INFORMATION_CLASS := 68;

   function NtQueryInformationFile
     (FileHandle           : HANDLE;
      IoStatusBlock        : in out IO_STATUS_BLOCK;
      FileInformation      : LPVOID;
      Length               : ULONG;
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

   type FILE_STANDARD_INFORMATION is record
      AllocationSize : LARGE_INTEGER;
      EndOfFile      : LARGE_INTEGER;
      NumberOfLinks  : ULONG;
      DeletePending  : BOOL;
      Directory      : BOOL;
   end record
   with Convention => C_Pass_By_Copy;

   type FILE_INTERNAL_INFORMATION is record
      IndexNumber : LARGE_INTEGER;
   end record
   with Convention => C_Pass_By_Copy;

   type FILE_EA_INFORMATION is record
      EaSize : ULONG;
   end record
   with Convention => C_Pass_By_Copy;

   type FILE_ACCESS_INFORMATION is record
      AccessFlags : ACCESS_MASK;
   end record
   with Convention => C_Pass_By_Copy;

   type FILE_POSITION_INFORMATION is record
      CurrentByteOffset : LARGE_INTEGER;
   end record
   with Convention => C_Pass_By_Copy;

   type FILE_MODE_INFORMATION is record
      Mode : ULONG;
   end record
   with Convention => C_Pass_By_Copy;

   type FILE_ALIGNMENT_INFORMATION is record
      AlignmentRequirement : ULONG;
   end record
   with Convention => C_Pass_By_Copy;

   type FILE_NAME_INFORMATION is record
      FileNameLength : ULONG;
      FileName       : Wide_String (1 .. 4096);
   end record
   with Convention => C_Pass_By_Copy;

   type FILE_ALL_INFORMATION is record
      BasicInformation     : FILE_BASIC_INFORMATION;
      StandardInformation  : FILE_STANDARD_INFORMATION;
      InternalInformation  : FILE_INTERNAL_INFORMATION;
      EaInformation        : FILE_EA_INFORMATION;
      AccessInformation    : FILE_ACCESS_INFORMATION;
      PositionInformation  : FILE_POSITION_INFORMATION;
      ModeInformation      : FILE_MODE_INFORMATION;
      AlignmentInformation : FILE_ALIGNMENT_INFORMATION;
      NameInformation      : FILE_NAME_INFORMATION;
   end record
   with Convention => C_Pass_By_Copy;

   type FILE_DIRECTORY_INFORMATION is record
      NextEntryOffset : ULONG;
      FileIndex       : ULONG;
      CreationTime    : LARGE_INTEGER;
      LastAccessTime  : LARGE_INTEGER;
      LastWriteTime   : LARGE_INTEGER;
      ChangeTime      : LARGE_INTEGER;
      EndOfFile       : LARGE_INTEGER;
      AllocationSize  : LARGE_INTEGER;
      FileAttributes  : FILE_ATTRIBUTE;
      FileNameLength  : ULONG;
      FileName        : Wide_String (1 .. 256);
   end record
   with Convention => C_Pass_By_Copy,
        Alignment  => 8;

   function NtQueryAttributesFile
      (ObjectAttributes : in out OBJECT_ATTRIBUTES;
       Information      : out FILE_BASIC_INFORMATION)
   return NTSTATUS
   with Import => True,
        Convention => Stdcall,
        External_Name => "NtQueryAttributesFile";

   function NtQueryDirectoryFile
      (FileHandle           : HANDLE;
       Event                : HANDLE;
       ApcRoutine           : LPVOID;
       ApcContext           : LPVOID;
       IoStatusBlock        : in out IO_STATUS_BLOCK;
       FileInformation      : LPVOID;
       Length               : ULONG;
       FileInformationClass : FILE_INFORMATION_CLASS;
       ReturnSingleEntry    : BOOL;
       FileName             : PUNICODE_STRING;
       RestartScan          : BOOL)
      return NTSTATUS
      with Import => True,
           Convention => Stdcall,
           External_Name => "NtQueryDirectoryFile";

   subtype ACCESS_MODE is DWORD;

   GENERIC_READ    : constant ACCESS_MODE := 16#8000_0000#;
   GENERIC_WRITE   : constant ACCESS_MODE := 16#4000_0000#;
   GENERIC_EXECUTE : constant ACCESS_MODE := 16#2000_0000#;
   GENERIC_ALL     : constant ACCESS_MODE := 16#1000_0000#;

   subtype CF_SHARE_MODE is DWORD;

   --  Note: FILE_SHARE_NONE does not exist, only the value 0 is given
   --  in the CreateFile documentation.
   CF_FILE_SHARE_NONE   : constant CF_SHARE_MODE := 16#0000_0000#;
   CF_FILE_SHARE_DELETE : constant CF_SHARE_MODE := 16#0000_0004#;
   CF_FILE_SHARE_READ   : constant CF_SHARE_MODE := 16#0000_0001#;
   CF_FILE_SHARE_WRITE  : constant CF_SHARE_MODE := 16#0000_0002#;

   subtype CF_CREATION_DISPOSITION is DWORD;

   CF_CREATE_ALWAYS     : constant CF_CREATION_DISPOSITION := 16#0000_0002#;
   CF_CREATE_NEW        : constant CF_CREATION_DISPOSITION := 16#0000_0001#;
   CF_OPEN_ALWAYS       : constant CF_CREATION_DISPOSITION := 16#0000_0004#;
   CF_OPEN_EXISTING     : constant CF_CREATION_DISPOSITION := 16#0000_0003#;
   CF_TRUNCATE_EXISTING : constant CF_CREATION_DISPOSITION := 16#0000_0005#;

   subtype CF_FILE_ATTRIBUTE is DWORD;

   CF_FILE_ATTRIBUTE_ARCHIVE     : constant CF_FILE_ATTRIBUTE := 16#0000_0020#;
   CF_FILE_ATTRIBUTE_ENCRYPTED   : constant CF_FILE_ATTRIBUTE := 16#0000_4000#;
   CF_FILE_ATTRIBUTE_HIDDEN      : constant CF_FILE_ATTRIBUTE := 16#0000_0002#;
   CF_FILE_ATTRIBUTE_NORMAL      : constant CF_FILE_ATTRIBUTE := 16#0000_0080#;
   CF_FILE_ATTRIBUTE_OFFLINE     : constant CF_FILE_ATTRIBUTE := 16#0000_1000#;
   CF_FILE_ATTRIBUTE_READONLY    : constant CF_FILE_ATTRIBUTE := 16#0000_0001#;
   CF_FILE_ATTRIBUTE_SYSTEM      : constant CF_FILE_ATTRIBUTE := 16#0000_0004#;
   CF_FILE_ATTRIBUTE_TEMPORARY   : constant CF_FILE_ATTRIBUTE := 16#0000_0100#;
   CF_FILE_FLAG_BACKUP_SEMANTICS : constant CF_FILE_ATTRIBUTE := 16#0200_0000#;

   function CreateFile
     (Filename            : OS.C_WString; DesiredAccess : ACCESS_MODE;
      ShareMode           : CF_SHARE_MODE;
      SecurityAttributes  : LPSECURITY_ATTRIBUTES := null;
      CreationDisposition : CF_CREATION_DISPOSITION;
      FlagsAndAttributes  : CF_FILE_ATTRIBUTE;
      TemplateFile        : HANDLE := NULL_HANDLE) return HANDLE with
     Import => True, Convention => Stdcall, External_Name => "CreateFileW";

   function Open
      (Filename : C_WString;
       Flags    : Open_Mode;
       Mode     : Permission_Mode)
      return FS.File_Descriptor
   with Import => True,
        Convention => C,
        External_Name => "_wopen";

   function CopyFile
     (Existing_File_Name : OS.C_WString; New_File_Name : OS.C_WString;
      Fail_If_Exists     : BOOL) return BOOL with
     Import => True, Convention => Stdcall, External_Name => "CopyFileW";

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
