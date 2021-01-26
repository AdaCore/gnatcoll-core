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
procedure Open_Pipe
  (Pipe_Input  : out File_Descriptor;
   Pipe_Output : out File_Descriptor)
is
   package Libc renames GNATCOLL.OS.Libc;

   Result : aliased Libc.Pipe_Type;
   Status : Libc.Libc_Status;
begin

   --  This implementation relies on the fact that pipe2 is used to open the
   --  pipe and flag set to O_CLOEXEC. Thus there is no need to call
   --  Set_Close_On_Exec.
   Status := Libc.Pipe (Result'Access);
   if Status = Libc.Error then
      raise OS_Error with "cannot open pipe";
   end if;

   Pipe_Input := Result.Input;
   Pipe_Output := Result.Output;

end Open_Pipe;
