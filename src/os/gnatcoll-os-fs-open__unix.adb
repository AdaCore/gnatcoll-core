------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

separate (GNATCOLL.OS.FS)
function Open
   (Path : UTF8.UTF_8_String;
    Mode : Open_Mode := Read_Mode)
   return File_Descriptor
is
   package SB renames GNATCOLL.String_Builders;
   package Libc renames GNATCOLL.OS.Libc;

   use all type SB.Static_String_Builder;

   C_Path : SB.Static_String_Builder (Path'Length + 1);
   Result : File_Descriptor;
   Perm   : File_Mode;
   O_Mode : Libc.Open_Mode;
begin

   Append (C_Path, Path);

   case Mode is
      when Read_Mode =>
         O_Mode := O_RDONLY or O_CLOEXEC;
         Perm   := 0;
      when Write_Mode =>
         O_Mode := O_WRONLY or O_CREAT or O_CLOEXEC;
         Perm   := S_IRUSR or S_IWUSR or S_IRGRP or
            S_IWGRP or S_IROTH or S_IWOTH;
      when Append_Mode =>
         O_Mode := O_WRONLY or O_CREAT or O_APPEND or O_CLOEXEC;
         Perm   := S_IRUSR or S_IWUSR or S_IRGRP or
            S_IWGRP or S_IROTH or S_IWOTH;
   end case;

   Result := Libc.Open (As_C_String (C_Path), O_Mode, Perm);

   if Result < 0 then
      return Invalid_FD;
   else
      return Result;
   end if;

end Open;
