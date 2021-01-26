------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

separate (GNATCOLL.OS.Process)
function Wait (H : Process_Handle) return Integer
is
   use all type uint_32;

   Unix_Pid : constant Integer := Integer (H);
   Finished : Integer;
   Status   : uint_32 := 0;
begin
   Finished := Waitpid (Unix_Pid, Status, 0);
   if Finished /= Unix_Pid or (Status and 16#7f#) /= 0 then
      return -1;
   end if;
   return Integer ((Status and 16#ff00#) / 256);
end Wait;
