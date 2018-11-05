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

--  Ravenscar constraints prohibit the use of language-level timeout facilities
--  (via the select statement). This archetype manage to mimic the semantics of
--  a timeout within Ravenscar constraints: if the server is not released
--  (via Put_Request) within Maximum_Interelase_Time, it is automatically
--  released by the run-time and invokes an appropriate, user-specified handler
--
--  A typical example of usage is the following:
--
--  type Par is ...
--  procedure Sporadic_Operation(P : Par);
--  procedure Handler;
--  package My_Sporadic_Server is new Ravenscar.Timed_Out_Sporadic_Server
--      (Task_Priority => 10,
--       Minimum_Interelease_Time => 1_000,
--       Maximum_Interelease_Time => 2_000, -- wait at most 2 seconds
--       Protocol_Ceiling => 15,
--       System_Start_Time => System_Properties.Start_UP_Time,
--       QS => 4,
--       Param => Par,
--       Sporadic_Operation => Sporadic_Operation,
--       Time_Out_Handler => Handler);
--
--  [...]
--  declare
--    P : Par;
--  begin
--   -- Release the task --
--   My_Sporadic_Server.Put_Request(P);
--
--  BEHAVIOUR
--  If the time elapsed between two consecutive releases of the server is
--  greater then Maximimum_Interelease_Time, then Handler is invoked.
--
--  Explanations for GNAT.Ravenscar.Sporadic_Server still hold.
--
--  NOTE FOR THE ANALYSIS: the pattern is implemented as follows:
--  (1) A Timer is set to expires at each Maximum_Interelease_Time
--    (a) I can be deleted and re-set if the server is released
--    (b) If it expires, it release an additional task (see point 2)
--  (2) An additional sporadic task with minimum_interarrival_time =
--     maximum_interarrival_time is suspended waiting to be released by the
--     timer at point (1): this task executes the handler. It suspends on a
--     Suspension_Object.

with Ada.Real_Time;
with System;
with Ada.Synchronous_Task_Control;
with GNATCOLL.Ravenscar.Sporadic_Server;
with GNATCOLL.Ravenscar.Timers.One_Shot_Timer;

generic

   Task_Priority : System.Priority;
   --  The priority of the task

   Minimum_Interelease_Time : Millisecond;
   --  The minimum time between two consecutive releases

   Maximum_Interelease_Time : Millisecond;
   --  the maximum interelease time which triggers the automatic release
   --  of the server

   System_Start_Time : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   --  the system-wide release time

   Protocol_Ceiling : System.Any_Priority;
   --  the ceiling priority of the protected object used to post and fetch
   --  requests

   QS : Queue_Size;
   --  the maximum number of saved requests

   type Param is private;
   --  the request descriptor

   with procedure Sporadic_Operation (Par : Param);
   --  the procedure invoked when the server is released by the client

   with procedure Time_Out_Handler;
   --  the handler executed by the server when non released the maximum
   --  interrelease time

package GNATCOLL.Ravenscar.Timed_Out_Sporadic_Server is

   procedure Put_Request (Par : Param);
   --  invoked by the clients

private

   procedure Timed_Out_Sporadic_Operation (Par : Param);

   package Timed_Out_Sporadic_Server is new Sporadic_Server
     (Task_Priority,
      Minimum_Interelease_Time,
      System_Start_Time,
      Protocol_Ceiling,
      QS,
      Param,
      Timed_Out_Sporadic_Operation);
   --  The sporadic server

   Timer_Server_Suspender : Ada.Synchronous_Task_Control.Suspension_Object;
   --  A suspension object for the timer server (see below)

   task Timer_Server is
      pragma Priority (Task_Priority);
   end Timer_Server;
   --  The task which is triggered by the timer. We have an additional task
   --  to avoid having the timer itself to execute (it runs at interrupt
   --  priority).

   package My_Timer is new GNATCOLL.Ravenscar.Timers.One_Shot_Timer;
   --  the timer triggering the task if no request is posted within
   --  the maximum interelease time

   procedure Handler;

   Handler_Access : constant GNATCOLL.Ravenscar.Timers.Timer_Action :=
                      Handler'Access;

end GNATCOLL.Ravenscar.Timed_Out_Sporadic_Server;
