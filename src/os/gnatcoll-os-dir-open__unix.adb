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

with GNATCOLL.OS.Libc.Dirent;
with GNATCOLL.String_Builders;
with GNAT.OS_Lib;

separate (GNATCOLL.OS.Dir)
function Open (Path : UTF8.UTF_8_String) return Dir_Handle is

   package SB renames GNATCOLL.String_Builders;
   package Dirent renames GNATCOLL.OS.Libc.Dirent;

   use all type Dirent.Dir_Handle;

   Result   : Dir_Handle;

   Abs_Path : constant UTF8.UTF_8_String :=
      GNAT.OS_Lib.Normalize_Pathname (Path, Resolve_Links => False);
   C_Path   : SB.Static_String_Builder (Abs_Path'Length + 1);
begin
   --  Keep track of the opened path
   Result.Path_Last := Abs_Path'Length;
   Result.Path (1 .. Abs_Path'Length) := Abs_Path;

   --  Open the directory
   SB.Append (C_Path, Abs_Path);
   Result.Handle := Dirent.Opendir (SB.As_C_String (C_Path));

   --  Check for errors
   if Result.Handle = Dirent.Invalid_Handle then
      raise OS_Error with "cannot open directory " & Abs_Path;
   end if;

   Result.Is_Opened := True;

   return Result;
end Open;
