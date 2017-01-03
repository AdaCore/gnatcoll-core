------------------------------------------------------------------------------
--                                                                          --
--                              G N A T C O L L                             --
--                                                                          --
--                    Copyright (C) 2008-2017, AdaCore                      --
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

--   A Ravenscar-compliant one-shot timer

with System;
with Ada.Real_Time;
with Ada.Real_Time.Timing_Events;

generic
package GNATCOLL.Ravenscar.Timers.One_Shot_Timer is

   procedure Set
     (Instant : Ada.Real_Time.Time;
      Action  : Timer_Action);
   --  Set the timer to an absolute instant in time to execute a specific
   --  action.

   procedure Cancel (Success : out Boolean);
   --  cancel the timer

private

   The_Event : Ada.Real_Time.Timing_Events.Timing_Event;
   --  the timing event

   protected Events is
      --  the handler used to invoke the user-provided action (see procedure
      --  Set).
      pragma Priority (System.Any_Priority'Last);

      procedure Handler
        (Event : in out Ada.Real_Time.Timing_Events.Timing_Event);

      procedure Set_Action
        (Instant : Ada.Real_Time.Time;
         Action  : Timer_Action);

   private

      The_Action : GNATCOLL.Ravenscar.Timers.Timer_Action;

   end Events;

   Events_Handler : constant Ada.Real_Time.Timing_Events.Timing_Event_Handler
                      := Events.Handler'Access;

end GNATCOLL.Ravenscar.Timers.One_Shot_Timer;
