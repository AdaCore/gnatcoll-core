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

with Interfaces.C;
with GNATCOLL.OS.Libc; use GNATCOLL.OS.Libc;
with GNATCOLL.String_Builders;
with Ada.Strings.Unbounded;

separate (GNATCOLL.OS.FSUtil)
function Read_Symbolic_Link
  (Link_Path : UTF8.UTF_8_String; Target_Path : out Unbounded_String)
   return Boolean
is
   package SB renames GNATCOLL.String_Builders;
   package Libc renames GNATCOLL.OS.Libc;
   package C renames Interfaces.C;

   use all type SB.Static_String_Builder;

   Link_C_Path : SB.Static_String_Builder (Link_Path'Length + 1);
   Bytes_Read  : Ssize_t;
   C_Buf       : C.char_array (1 .. 4096) := (others => C.nul);
   --  PATH_MAX is usually set to 4096 bytes, so we set the maximum
   --  target path length to this value.

begin
   Append (Link_C_Path, Link_Path);

   Bytes_Read :=
     Libc.ReadLink (As_C_String (Link_C_Path), C_Buf, C_Buf'Length);

   if Bytes_Read = -1 then
      return False;
   end if;

   Target_Path := To_Unbounded_String (C.To_Ada (C_Buf));

   return True;
end Read_Symbolic_Link;
