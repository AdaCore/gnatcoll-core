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

with GNATCOLL.OS.Libc; use GNATCOLL.OS.Libc;
with GNATCOLL.String_Builders;

separate (GNATCOLL.OS.Dir)
function Create (Path : UTF8.UTF_8_String) return Boolean is
   package SB renames GNATCOLL.String_Builders;
   package Libc renames GNATCOLL.OS.Libc;

   use all type SB.Static_String_Builder;

   C_Path : SB.Static_String_Builder (Path'Length + 1);
   --  All rights for the owner, and rw rights for groups and others.
   Perm   : constant Libc.File_Mode :=
     S_IRWXU or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH;
   Status : Libc.Libc_Status;

begin

   Append (C_Path, Path);

   Status := Libc.Mkdir (As_C_String (C_Path), Perm);

   if Status = Libc.Error then
      return False;
   else
      return True;
   end if;

end Create;
