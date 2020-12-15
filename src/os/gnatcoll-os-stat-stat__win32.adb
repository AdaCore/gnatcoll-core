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

separate (GNATCOLL.OS.Stat)
function Stat
   (Path : UTF8.UTF_8_String;
    Follow_Symlinks : Boolean := True)
   return File_Attributes is
   Attr        : FILE_OBJECT_ATTRIBUTES;
   Status      : NTSTATUS;
   Information : FILE_BASIC_INFORMATION;
   Result      : File_Attributes;

   pragma Unreferenced (Follow_Symlinks);

begin
   --  NtQueryAttributesFile requires an absolute path
   if GNAT.OS_Lib.Is_Absolute_Path (Path) then
      Initialize (Attr, Path);
   else
      Initialize (Attr, GNAT.OS_Lib.Normalize_Pathname (Path));
   end if;

   Status := NtQueryAttributesFile (Attr.OA, Information);

   if Is_Success (Status) then
      Result.Exists := True;
      Result.Directory := (DIRECTORY and Information.FileAttributes) > 0;
      Result.Symbolic_Link :=
         (REPARSE_POINT and Information.FileAttributes) > 0;
      Result.Regular :=
         not (Result.Directory or Result.Symbolic_Link);
   end if;

   return Result;
end Stat;
