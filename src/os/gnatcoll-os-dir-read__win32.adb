------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                       Copyright (C) 2021-2022, AdaCore                   --
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

with GNATCOLL.OS.Win32; use GNATCOLL.OS.Win32;
with GNATCOLL.OS.Win32.Files; use GNATCOLL.OS.Win32.Files;
with GNATCOLL.OS.Win32.Strings; use GNATCOLL.OS.Win32.Strings;
with Ada.Calendar.Conversions;
with Interfaces.C;

separate (GNATCOLL.OS.Dir)
function Read
   (Handle          : Dir_Handle;
    Follow_Symlinks : Boolean := True)
   return Dir_Entry
is
   pragma Unreferenced (Follow_Symlinks);
   Dir_Info   : aliased FILE_DIRECTORY_INFORMATION;
   Result     : Dir_Entry;
   Status     : NTSTATUS;
   IO         : IO_STATUS_BLOCK;
   Is_Symlink : Boolean;
   Is_Dir     : Boolean;
   Ignore_Entry : Boolean := False;
begin
   loop
      Status := NtQueryDirectoryFile
         (FileHandle           => Handle.Handle,
          Event                => NULL_HANDLE,
          ApcRoutine           => NULL_LPVOID,
          ApcContext           => NULL_LPVOID,
          IoStatusBlock        => IO,
          FileInformation      => LPVOID (Dir_Info'Address),
          Length               => FILE_DIRECTORY_INFORMATION'Size / 8,
          FileInformationClass => FileDirectoryInformation,
          ReturnSingleEntry    => BOOL_TRUE,
          FileName             => null,
          RestartScan          => BOOL_FALSE);

      if Is_Success (Status) then
         Result.Name_Last := To_UTF8
            (Dir_Info.FileName (1 .. Integer (Dir_Info.FileNameLength / 2)),
             Result.Name_Buffer);
         if Result.Name_Buffer (1 .. Result.Name_Last) = "." or else
            Result.Name_Buffer (1 .. Result.Name_Last) = ".."
         then
            Ignore_Entry := True;
         else
            Ignore_Entry := False;
         end if;

         Is_Dir := (DIRECTORY and Dir_Info.FileAttributes) > 0;
         Is_Symlink := (REPARSE_POINT and Dir_Info.FileAttributes) > 0;
         Result.Info := Stat.New_File_Attributes
            (Exists        => True,
             Writable      => True,
             Readable      => True,
             Executable    => True,
             Symbolic_Link => Is_Symlink,
             Regular       => not (Is_Symlink or Is_Dir),
             Directory     => Is_Dir,
             Stamp         => Ada.Calendar.Conversions.To_Ada_Time
                (Interfaces.C.long
                   ((Dir_Info.LastWriteTime / 10000000) - Win32_Epoch_Offset)),
             Length        => Long_Long_Integer (Dir_Info.EndOfFile));
      else
         Result.Name_Last := 0;
         Ignore_Entry := False;
      end if;

      exit when not Ignore_Entry;
   end loop;

   return Result;
end Read;
