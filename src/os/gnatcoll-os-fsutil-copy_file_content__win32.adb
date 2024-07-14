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

with GNATCOLL.OS.Win32;
with GNATCOLL.OS.Win32.Files;
with GNATCOLL.WString_Builders;

separate (GNATCOLL.OS.FSUtil)

function Copy_File_Content
  (Src : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String) return Boolean
is
   package SB renames GNATCOLL.WString_Builders;
   package Win32 renames GNATCOLL.OS.Win32;

   use all type SB.Static_WString_Builder;

   Src_C_Path : SB.Static_WString_Builder (Src'Length + 1);
   Dst_C_Path : SB.Static_WString_Builder (Dst'Length + 1);
begin
   Append (Src_C_Path, Src);
   Append (Dst_C_Path, Dst);

   return (Win32.Files.CopyFile
       (As_C_WString (Src_C_Path),
        As_C_WString (Dst_C_Path), Win32.BOOL_FALSE) =
     Win32.BOOL_TRUE);
end Copy_File_Content;
