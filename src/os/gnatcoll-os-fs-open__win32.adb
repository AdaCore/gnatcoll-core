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

separate (GNATCOLL.OS.FS)
function Open
   (Path : UTF8.UTF_8_String;
    Mode : Open_Mode := Read_Mode;
    Advise_Sequential : Boolean := False)
   return File_Descriptor
is
   package SB renames GNATCOLL.WString_Builders;
   package Win32 renames GNATCOLL.OS.Win32;

   use all type SB.Static_WString_Builder;
   use all type Win32.Files.Open_Mode;

   C_Path : SB.Static_WString_Builder (Path'Length + 1);
   Result : File_Descriptor;
   O_Mode : Win32.Files.Open_Mode;
   O_Perm : Win32.Files.Permission_Mode;
begin

   Append (C_Path, Path);

   case Mode is
      when Read_Mode =>
         O_Mode := Win32.Files.O_RDONLY
            or Win32.Files.O_NOINHERIT
            or Win32.Files.O_BINARY;
         O_Perm   := 0;
      when Write_Mode =>
         O_Mode := Win32.Files.O_WRONLY
            or Win32.Files.O_CREAT
            or Win32.Files.O_NOINHERIT
            or Win32.Files.O_BINARY
            or Win32.Files.O_TRUNC;
         O_Perm := Win32.Files.S_IWRITE;
      when Append_Mode =>
         O_Mode := Win32.Files.O_WRONLY
            or Win32.Files.O_CREAT
            or Win32.Files.O_APPEND
            or Win32.Files.O_NOINHERIT
            or Win32.Files.O_BINARY;
         O_Perm := Win32.Files.S_IWRITE;
   end case;

   if Advise_Sequential then
      O_Mode := O_Mode or Win32.Files.O_SEQUENTIAL;
   end if;

   Result := Win32.Files.Open (As_C_WString (C_Path), O_Mode, O_Perm);

   if Result < 0 then
      return Invalid_FD;
   else
      return Result;
   end if;

end Open;
