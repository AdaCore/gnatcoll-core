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
with GNATCOLL.WString_Builders;

separate (GNATCOLL.OS.FSUtil)
function Create_Directory (Path : UTF8.UTF_8_String) return Boolean is
   package SB renames GNATCOLL.WString_Builders;
   package Win32 renames GNATCOLL.OS.Win32;

   use all type SB.Static_WString_Builder;

   C_Path  : SB.Static_WString_Builder (Path'Length + 1);
   Success : Win32.BOOL;
begin

   Append (C_Path, Path);
   Success :=
     Win32.Files.CreateDirectory (As_C_WString (C_Path));

   if Success = Win32.BOOL_FALSE then
      return False;
   end if;

   return True;
end Create_Directory;
