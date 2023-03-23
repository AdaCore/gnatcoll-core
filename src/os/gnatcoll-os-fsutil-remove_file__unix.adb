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

with GNATCOLL.OS.Libc;
with GNATCOLL.String_Builders;

separate (GNATCOLL.OS.FSUtil)
function Remove_File (Path : UTF8.UTF_8_String) return Boolean is
   package SB renames GNATCOLL.String_Builders;
   package Libc renames GNATCOLL.OS.Libc;

   use all type SB.Static_String_Builder;

   C_Path : SB.Static_String_Builder (Path'Length + 1);
begin

   Append (C_Path, Path);
   return (Libc.Remove (As_C_String (C_Path)) = Libc.Success);

end Remove_File;
