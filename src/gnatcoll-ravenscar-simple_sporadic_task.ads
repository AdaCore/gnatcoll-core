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

--  A simple archetype of a Ravenscar-compliant sporadic task. The task is
--  meant to enforce a minimum inter-release interval and to execute at a
--  constant priority level (a part when subject to the immediate priority
--  ceiling protocol). The suspension/release mechanism is managed by a
--  protected object. In the worst case, the task behavior is identical to the
--  behavior of a cyclic task (GNAT.Ravenscar.Simple_Cyclic_Task) with Period =
--  Minimum_Interelease_Time.
--
--  The task timing behavior can be analyzed with the most common timing
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
   --  the system-wide release time

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
