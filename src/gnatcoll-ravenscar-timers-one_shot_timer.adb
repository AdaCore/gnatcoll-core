------------------------------------------------------------------------------
--                                                                          --
--                              G N A T C O L L                             --
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

package body GNATCOLL.Ravenscar.Timers.One_Shot_Timer is

   ---------
   -- Set --
   ---------

   procedure Set
     (Instant : Ada.Real_Time.Time;
      Action  : Timer_Action) is
   begin
      --  Set the timer

      Events.Set_Action (Instant, Action);
   end Set;

   ------------
   -- Cancel --
   ------------

   procedure Cancel (Success : out Boolean) is
   begin
      --  cancel the timer

      Ada.Real_Time.Timing_Events.Cancel_Handler (The_Event, Success);
   end Cancel;

   protected body Events is

      -------------
      -- Handler --
      -------------

      procedure Handler
        (Event : in out Ada.Real_Time.Timing_Events.Timing_Event)
      is
         pragma Unreferenced (Event);
      begin
         The_Action.all;
      end Handler;

      ----------------
      -- Set_Action --
      ----------------

      procedure Set_Action
        (Instant : Ada.Real_Time.Time;
         Action  : Timer_Action) is
      begin
         The_Action := Action;
         Ada.Real_Time.Timing_Events.Set_Handler
           (The_Event, Instant, Events_Handler);
      end Set_Action;

   end Events;

end GNATCOLL.Ravenscar.Timers.One_Shot_Timer;
