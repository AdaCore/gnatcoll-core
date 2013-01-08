------------------------------------------------------------------------------
--                                                                          --
--                            G N A T C O L L                               --
--                                                                          --
--                      Copyright (C) 2008-2013, AdaCore                    --
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

--  A simple archetype of a Ravenscar-compliant sporadic task. The task is
--  meant to enforce a minimum interelease interval and to execute at a contant
--  priority level (a part when subject to the immediate priority ceiling
--  protocol). The suspension/release mechanism is managed by a protected
--  object. In the worst case, the task behaviour is identical to the behaviour
--  of a cyclic task (GNAT.Ravenscar.Simple_Cyclic_Task) with Period =
--  Minimum_Interelease_Time.
--
--  The task timing behaviour can be analyzed with the most common timing
--  analysis techniques.
--
--  A typical example of usage is the following:
--
--  procedure Sporadic_Operation;
--  package My_Sporadic_Task is new GNATCOLL.Ravenscar.Simple_Sporadic_Task
--      (Task_Priority => 10,
--       Minimum_Interelease_Time => 1_000,
--       Protocol_Ceiling => 15,
--       System_Start_Time => System_Properties.Start_UP_Time,
--       Sporadic_Operation => Sporadic_Operation);
--
--  [...]
--  -- Release the task --
--  My_Sporadic_Task.Release;
--  [...]
--
--  The instantiation generates a sporadic task which executes at priority 10,
--  is ready to executed starting from System_UP_Time + Phase, executes
--  Sporadic_Operation, and enforce a minimum interelease time of 1000
--  milliseconds. Protocol_Ceiling is the ceiling priority of the protected
--  object managing the suspension/release mechanism of the task: the protected
--  object is accessed by both the clients (via Release) and the sporadic task
--  itself. Protocol_Ceiling must be equal to the priority of the client with
--  the highest priority, including the task itself (Task_Priority).

with Ada.Real_Time;
with System;

generic

   Task_Priority : System.Priority;
   --  The priority of the task

   Minimum_Interelease_Time : Millisecond;
   --  The minimum time between two consecutive releases

   System_Start_Time : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   --  the system-wide relase time

   Protocol_Ceiling : System.Any_Priority;
   --  the ceiling priority of the protected object used to post and fetch
   --  requests

   with procedure Sporadic_Operation;
   --  the nominal operation

package GNATCOLL.Ravenscar.Simple_Sporadic_Task is

   procedure Release;
   --  used by client to trigger the task

private

   protected Protocol is
      pragma Priority (Protocol_Ceiling);

      procedure Release;

      entry Wait (Release_Time : out Ada.Real_Time.Time);

   private
      Barrier : Boolean := False;
      Pending : Integer := 0;
   end Protocol;

   task Simple_Sporadic_Task is
      pragma Priority (Task_Priority);
   end Simple_Sporadic_Task;

end GNATCOLL.Ravenscar.Simple_Sporadic_Task;
