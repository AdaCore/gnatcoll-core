------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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
