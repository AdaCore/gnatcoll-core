------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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

with GNATCOLL.OS.Win32.Files; use GNATCOLL.OS.Win32.Files;
with GNATCOLL.OS.Win32; use GNATCOLL.OS.Win32;
with GNAT.OS_Lib;
with Ada.Calendar.Conversions;
with Interfaces.C;

separate (GNATCOLL.OS.Stat)
function Fstat (FD : FS.File_Descriptor) return File_Attributes
is
   Status    : NTSTATUS;
   Info      : FILE_ALL_INFORMATION;
   Result    : File_Attributes;
   WinHandle : HANDLE := NULL_HANDLE;
   IO        : IO_STATUS_BLOCK;
begin
   WinHandle := GetOSFHandle (FD);

   Status := NtQueryInformationFile (WinHandle, IO, LPVOID (Info'Address),
                                     FILE_ALL_INFORMATION'Size / 8,
                                     FileAllInformation);
   if Is_Success (Status) then
      Result.Exists := True;
      Result.Directory :=
         (DIRECTORY and Info.BasicInformation.FileAttributes) > 0;
      Result.Symbolic_Link :=
         (REPARSE_POINT and Info.BasicInformation.FileAttributes) > 0;
      Result.Regular :=
         not (Result.Directory or Result.Symbolic_Link);

      Result.Stamp := To_Unix_Nanoseconds (Info.BasicInformation.LastWriteTime);
      Result.Length := Info.StandardInformation.EndOfFile;
      Result.Executable := True;
      Result.Readable   := True;
      Result.Writable   := True;
   else
      Result.Exists := False;
   end if;

   return Result;
end Fstat;
