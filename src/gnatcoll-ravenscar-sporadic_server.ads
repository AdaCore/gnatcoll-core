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

--  A simple archetype of a Ravenscar-compliant sporadic server: it extends the
--  sporadic task archetype (GNAT.RAVENSCAR.Simple_Sporadic_Task) by permitting
--  to carry parameters for the release of the task. The sporadic operation is
--  of course called in a deferred way, so parameters are always IN by
--  construction.
--
--  A typical example of usage is the following:
--
--  type Par is ...
--  procedure Sporadic_Operation(P : Par);
--  package My_Sporadic_Server is new GNATCOLL.Ravenscar.Sporadic_Server
--      (Task_Priority => 10,
--       Minimum_Interelease_Time => 1_000,
--       Protocol_Ceiling => 15,
--       System_Start_Time => System_Properties.Start_UP_Time,
--       QS => 4,
--       Param => Par,
--       Sporadic_Operation => Sporadic_Operation);
--
--  [...]
--  declare
--    P : Par;
--  begin
--   -- Release the task --
--   My_Sporadic_Server.Put_Request(P);
--
--
--  Explanations for GNAT.Ravenscar.Sporadic_Task still hold; QS is the
--  maximum numbered of buffered requests, which are executed following a FIFO
--  policy.

with System;
with Ada.Real_Time;

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

   QS : Queue_Size;
   --  the maximum number of buffered requests

   type Param is private;
   --  the descriptor of the request to be fulfilled by the server

   with procedure Sporadic_Operation (Par : Param);
   --  the procedure to be executed by the server

package GNATCOLL.Ravenscar.Sporadic_Server is

   procedure Put_Request (Par : Param);
   --  Invoked by the client to post as request and trigger the server

private

   type Queue is array (1 .. QS) of Param;
   --  the queue containing reified requests

   protected Protocol is
      --  the protected object containing the request queue

      pragma Priority (Protocol_Ceiling);

      procedure Put_Request (Par : Param);

      entry Get_Request
        (Release_Time : out Ada.Real_Time.Time;
         Par        : out Param);

   private
      Barrier         : Boolean := False;
      Pending         : Integer := 0;
      Insert_Index    : Integer := 1;
      Extract_Index   : Integer := 1;
      Buffer          : Queue;
      Buffer_Overflow : Boolean := False;
   end Protocol;

   task Sporadic_Task is
      pragma Priority (Task_Priority);
   end Sporadic_Task;
   --  the sporadic server

end GNATCOLL.Ravenscar.Sporadic_Server;
