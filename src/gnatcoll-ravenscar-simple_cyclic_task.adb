------------------------------------------------------------------------------
--                                                                          --
--                	        G N A T C O L L                             --
--                                                                          --
--                      Copyright (C) 2008-2014, AdaCore                    --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body GNATCOLL.Ravenscar.Simple_Cyclic_Task is

   task body Simple_Cyclic_Task is
      use Ada.Real_Time;
      Next_Time : Time := System_Start_Time + Milliseconds (Phase);
   begin
      loop
         delay until Next_Time;
         Cyclic_Operation;
         Next_Time := Next_Time + Milliseconds (Period);
      end loop;
   end Simple_Cyclic_Task;

end GNATCOLL.Ravenscar.Simple_Cyclic_Task;
