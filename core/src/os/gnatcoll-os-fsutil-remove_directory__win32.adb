------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2020-2023, AdaCore                     --
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

with GNATCOLL.OS.Win32.Files;
with GNATCOLL.WString_List_Builders;

separate (GNATCOLL.OS.FSUtil)
function Remove_Directory (Path : UTF8.UTF_8_String) return Boolean is
   package SLB renames GNATCOLL.WString_List_Builders;
   package Win32 renames GNATCOLL.OS.Win32;

   use all type SLB.WString_List_Builder;
   C_Path : SLB.WString_List_Builder;

   File_Op_Struct : Win32.Files.SHFileOpStructW :=
     (Wnd                  => Win32.NULL_HANDLE, Func => Win32.Files.FO_DELETE,
      From                 => Null_C_WString, To => Null_C_WString,
      Flags                => Win32.Files.FOF_NOCONFIRMATION,
      AnyOperationsAborted => Win32.BOOL_FALSE,
      NameMappings => Win32.NULL_LPVOID, ProgressTitle => Null_C_WString);
begin
   Append (C_Path, Path);
   File_Op_Struct.From := As_C_WString (C_Path);

   return Win32.Files.SHFileOperation (File_Op_Struct) = 0;
end Remove_Directory;
