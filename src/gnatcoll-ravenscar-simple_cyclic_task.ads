------------------------------------------------------------------------------
--                                                                          --
--                              G N A T C O L L                             --
--                                                                          --
--                      Copyright (C) 2008-2016, AdaCore                    --
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

--  A simple archetype of a Ravenscar-compliant cyclic task. The task is meant
--  to be released at a constant time interval and to execute at
--  a contant priority level (a part when subject to the immediate priority
--  ceiling protocol).
--
--  The task timing behaviour can be analyzed with the most common timing
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
   --  the task phse

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
