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

package body GNATCOLL.Ravenscar.Simple_Sporadic_Task is

   procedure Release is
   begin
      Protocol.Release;
   end Release;

   protected body Protocol is

      procedure Update_Barrier is
      begin
         Barrier := Pending > 0;
      end Update_Barrier;
      pragma Inline (Update_Barrier);

      procedure Release is
      begin
         Pending := Pending + 1;
         Update_Barrier;
      end Release;

      entry Wait (Release_Time : out Ada.Real_Time.Time) when Barrier is
      begin

         --  keep track of the release instant to guarantee a faithful
         --  interelease time

         Release_Time := Ada.Real_Time.Clock;
         Pending      := Pending - 1;
         Update_Barrier;
      end Wait;

   end Protocol;

   task body Simple_Sporadic_Task is
      use Ada.Real_Time;
      Next_Time    : Time := System_Start_Time;
      Release_Time : Time;
   begin
      loop
         delay until Next_Time;
         Protocol.Wait (Release_Time);
         Sporadic_Operation;
         Next_Time := Release_Time + Milliseconds (Minimum_Interelease_Time);
      end loop;
   end Simple_Sporadic_Task;

end GNATCOLL.Ravenscar.Simple_Sporadic_Task;
