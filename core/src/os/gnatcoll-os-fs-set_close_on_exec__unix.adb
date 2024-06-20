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

with GNATCOLL.OS.Libc;

separate (GNATCOLL.OS.FS)
procedure Set_Close_On_Exec
  (FD            : File_Descriptor;
   Close_On_Exec : Boolean)
is
   package Libc renames GNATCOLL.OS.Libc;
   use type Libc.Uint;

   Flags  : Libc.Uint;
   Result : Integer;
begin
   Result := Libc.Fcntl (FD, Libc.F_GETFD, 0);
   if Result < 0 then
      raise OS_Error with "cannot get file descriptor flags";
   end if;
   Flags := Libc.Uint (Result);
   if Close_On_Exec then
      Flags := Flags or Libc.FD_CLOEXEC;
   else
      Flags := Flags and not Libc.FD_CLOEXEC;
   end if;

   Result := Libc.Fcntl (FD, Libc.F_SETFD, Integer (Flags));
   if Result < 0 then
      raise OS_Error with "cannot set close on exec";
   end if;
end Set_Close_On_Exec;
