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

--  Ravenscar tasks always communicate via messages or shared resources. It is
--  thus not possible to have a deferred operation (an operation executed by a
--  dedicated task) which carries OUT parameters. This archetype shows a
--  solution to this expressive limit by extending
--  GNAT.Ravenscar.Sporadic_Server. Clients release the server passing IN
--  parameters and a pointer to a procedure: the latter is automatically
--  called passing the computed value of OUT parameters of the deferred
--  operation executed by the sporadic server.
--
--  A typical example of usage is the following:
--
--  type IN_Param is ...
--  type OUT_Param is ...
--  procedure Sporadic_Operation(In_P : IN_Param; OUT_P : out OUT_Param);
--  procedure CallBack (P : OUT_Param);
--  package My_Sporadic_Server is
--    new GNATCOLL.Ravenscar.Sporadic_Server_With_Callback
--       Task_Priority => 10,
--       Minimum_Interelease_Time => 1_000,
--       Protocol_Ceiling => 15,
--       System_Start_Time => System_Properties.Start_UP_Time,
--       QS => 4,
--       IN_Param_Type => IN_Param,
--       OUT_Param_Type => IOUT_Param,
--       Sporadic_Operation => Sporadic_Operation);
--
--  [...]
--  declare
--    P : IN_Param;
--  begin
--    -- Release the task --
--    My_Sporadic_Server.Put_Request(P, Callback'access);
--
--  BEHAVIOUR:
--  My_Sporadic_Server executes Sporadic_Operation and Callback passing as
--  parameters the values computed by Sporadic_Operation.
--
--  Additional explanations for GNAT.Ravenscar.Sporadic_Server still hold.

with System;
with Ada.Real_Time;
with GNATCOLL.Ravenscar.Sporadic_Server;

generic

   Task_Priority : System.Priority;
   --  the task priority

   Minimum_Interelease_Time : Millisecond;
   --  the minimum time between two consecutive releases

   System_Start_Time : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   --  the system-wide release instant

   Protocol_Ceiling : System.Any_Priority;
   --  the ceiling priority of the protected object used to post and fetch
   --  requests

   QS : Queue_Size;
   --  the maximum amount of saved requests

   type In_Param is private;
   --  the descriptor of IN parameters

   type Out_Param is private;
   --  the descriptor of OUT parameters

   with procedure Sporadic_Operation
     (In_Par  : In_Param;
      Out_par : out Out_Param);
   --  the nominal operation

package GNATCOLL.Ravenscar.Sporadic_Server_With_Callback is

   type Callback is access procedure (Out_Par : Out_Param);
   --  the type of the callback

   procedure Put_Request
     (In_Par : In_Param;
      CB       : Callback);
   --  invoked by clients to put requests (and corresponding callback)

private

   type Queue_Item is record
      In_Par : In_Param;
      CB : Callback;
   end record;
   --  a reifed request descriptor containing IN parameters and callback

   procedure Dispatch (Req : Queue_Item);
   --  the dispatch procedure first executes the posted request and then
   --  the callback

   package My_Sporadic_Task is new GNATCOLL.Ravenscar.Sporadic_Server
     (Task_Priority,
      Minimum_Interelease_Time,
      System_Start_Time,
      Protocol_Ceiling,
      QS,
      Queue_Item,
      Dispatch);
   --  a sporadic server to execute requests

end GNATCOLL.Ravenscar.Sporadic_Server_With_Callback;
