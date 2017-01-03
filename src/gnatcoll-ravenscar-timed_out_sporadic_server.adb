------------------------------------------------------------------------------
--                                                                          --
--                	        G N A T C O L L                             --
--                                                                          --
--                      Copyright (C) 2008-2017, AdaCore                    --
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

package body GNATCOLL.Ravenscar.Timed_Out_Sporadic_Server is

   procedure Put_Request (Par : Param) renames
     Timed_Out_Sporadic_Server.Put_Request;

   procedure Handler is
      use Ada.Real_Time;
   begin
      Ada.Synchronous_Task_Control.Set_True (Timer_Server_Suspender);
   end Handler;

   task body Timer_Server is
      use Ada.Real_Time;
   begin

      loop

         --  set to False the suspension object

         Ada.Synchronous_Task_Control.Set_False (Timer_Server_Suspender);

         --  wait on the suspension object to be set to true by the timing
         --  event

         Ada.Synchronous_Task_Control.Suspend_Until_True
           (Timer_Server_Suspender);

         --  when triggered, execute the handler

         Time_Out_Handler;

         --  and set the next release instant for the timer to Clock +
         --  Maximum_Interelease_Time

         My_Timer.Set
           (Clock + Milliseconds (Maximum_Interelease_Time),
            Handler_Access);

      end loop;

   end Timer_Server;

   procedure Timed_Out_Sporadic_Operation (Par : Param) is
      use Ada.Real_Time;
      Next_Time_Out_Instant : constant Ada.Real_Time.Time :=
         Clock + Milliseconds (Maximum_Interelease_Time);
   begin

      --  Set the timer to the next instant

      My_Timer.Set (Next_Time_Out_Instant, Handler_Access);

      --  execute the operation

      Sporadic_Operation (Par);

   end Timed_Out_Sporadic_Operation;

end GNATCOLL.Ravenscar.Timed_Out_Sporadic_Server;
