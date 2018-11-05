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

--  A simple archetype of a Ravenscar-compliant cyclic task. The task is meant
--  to be released at a constant time interval and to execute at
--  a constant priority level (a part when subject to the immediate priority
--  ceiling protocol).
--
--  The task timing behavior can be analyzed with the most common timing
--  analysis techniques.
--
--  A typical example of usage is the following:
--
--  procedure Cyclic_Operation;
--  package My_Cyclic_Task is new GNATCOLL.Ravenscar.Simple_Cyclic_Task
--      (Task_Priority => 10,
--       Phase => 1_000,
--       Period => 1_000,
--       System_Start_Time => System_Properties.Start_UP_Time,
--       Cyclic_Operation => Cyclic_Operation);
--
--  The instantiation generates a cyclic task which executes at priority 10,
--  is released for the first time at System_UP_Time + Phase, executes
--  Cyclic_Operation, and suspends itself until Period milliseconds have passed
--  from the previous release.

with System;
with Ada.Real_Time;

generic

   Task_Priority : System.Priority;
   --  the task priority

   Phase : Millisecond;
   --  the task phase

   Period : Millisecond;
   --  the task period

   System_Start_Time : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   --  system-wide release instant

   with procedure Cyclic_Operation;
   --  the nominal operation

package GNATCOLL.Ravenscar.Simple_Cyclic_Task is

private

   task Simple_Cyclic_Task is
      pragma Priority (Task_Priority);
   end Simple_Cyclic_Task;

end GNATCOLL.Ravenscar.Simple_Cyclic_Task;
