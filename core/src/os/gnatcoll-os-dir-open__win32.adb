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

with GNAT.OS_Lib;
with GNATCOLL.OS.Win32; use GNATCOLL.OS.Win32;
with GNATCOLL.OS.Win32.Files; use GNATCOLL.OS.Win32.Files;

separate (GNATCOLL.OS.Dir)
function Open (Path : UTF8.UTF_8_String) return Dir_Handle is
   Status     : NTSTATUS;
   Win_Handle : HANDLE := NULL_HANDLE;
   IO         : IO_STATUS_BLOCK;
   Result     : Dir_Handle;
   Full_Path  : constant UTF8.UTF_8_String :=
      GNAT.OS_Lib.Normalize_Pathname (Path);
   Win_Path   : UNICODE_PATH;
begin
   --  NtOPenFile requires an absolute normalized path
   Initialize (Win_Path, Full_Path);

   --  Open the file
   Status := NtOpenFile
      (Win_Handle,
       Win_Path.Str,
       FILE_READ_ATTRIBUTES or FILE_LIST_DIRECTORY or SYNCHRONIZE,
       IO,
       SHARE_ALL,
       FILE_OPEN_FOR_BACKUP_INTENT or FILE_SYNCHRONOUS_IO_NONALERT);

   --  Check for status
   if not Is_Success (Status) then
      raise OS_Error with
      "cannot open directory" & Full_Path & "(error: " & Status'Img & ")";
   end if;

   Result.Handle := Win_Handle;
   Result.Path_Last := Full_Path'Length;
   Result.Path (1 .. Result.Path_Last) := Full_Path;
   Result.Is_Opened := True;
   return Result;
end Open;
