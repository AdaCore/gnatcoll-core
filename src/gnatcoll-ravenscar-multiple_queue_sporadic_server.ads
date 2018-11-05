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

--  This version of a Ravenscar-compliant sporadic server extends the behavior
--  of GNAT.Ravenscar.Sporadic_Server by accepting multiple types of requests
--  reified in a single variant type. It shares all basic properties of
--  GNAT.Ravenscar.Sporadic_Server.
--
--  A typical example of usage is the following:
--
--  type Req is (REQ1, REQ2, REQ3);
--  type Par (R : Req := REQ1) is
--    record
--      Req : Req := R;
--      case R is
--         when REQ1 =>
--            null;
--         when REQ2 =>
--             P1 : Type1;
--         when REQ3 =>
--             P2 : Type2;
--             P3 : Type3;
--      end case;
--    end record;
--
--  procedure Dispatch(P : Par) is
--  begin
--    case P.Req is
--       when REQ1 =>
--          Do_Something;
--       when REQ2 =>
--           Do_Something(P.P1);
--       when REQ3 =>
--           Do_Something(P.P2, P.P3);
--     end case;
--  end Dispatch;
--
--  package My_Sporadic_Server is
--    new GNAT.Ravenscar.Multiple_Queue_Sporadic_Server
--      (Task_Priority => 10,
--       Minimum_Interelease_Time => 1_000,
--       Protocol_Ceiling => 15,
--       System_Start_Time => System_Properties.Start_UP_Time,
--       QS => 4,
--       Request_Kind => Req,
--       Param => Par,
--       Dispatch => Dispatch);
--
--  [...]
--  declare
--    P : Par(REQ2);
--  begin
--    -- fill parameters
--    P.P2 := Val;
--    -- Release the task --
--    My_Sporadic_Server.Put_Request(P);

with System;
with Ada.Real_Time;

generic

   Task_Priority : System.Priority;
   --  the priority of the server

   Minimum_Interelease_Time : Millisecond;
   --  the minimum time between two consecutive releases

   System_Start_Time : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   --  the absolute instant in time for the release of the systems as a whole

   Protocol_Ceiling : System.Any_Priority;
   --  the ceiling priority of the protected object used to post and fetch
   --  requests

   QS : Queue_Size;
   --  the size of accepted requests

   type Request_Kind is (<>);
   --  an enumeration type identifying the possible kinds of request

   type Request is private;
   --  the reified request type

   with procedure Dispatch (Req : Request);
   --  the procedure invoked by the server to dispatch the fetched request

package GNATCOLL.Ravenscar.Multiple_Queue_Sporadic_Server is

   procedure Put_Request
     (Req      : Request;
      Kind : Request_Kind);
   --  Invoked by clients to post reified requests to be fetched and executed
   --  by the server

private

   type Request_Type_Ref is access all Request;
   --  pointer type for request

   type Request_Queue is array (1 .. QS) of aliased Request;
   --  physical queue for posted requests

   type All_Queue is array (Request_Kind'Range) of Request_Queue;
   --  the entire set of queues (one for each possible request)

   type All_Queue_Index is array (Request_Kind'Range) of Queue_Range;
   --  type to collect all indexes to access requests

   Pointer_Queue_Range_Max : constant Integer := QS;
   --  maximum index value

   type Pointer_Queue_Item is record
      Kind    : Request_Kind;
      Req : Request_Type_Ref;
   end record;
   --  reified request descriptor saved in a queue

   type Pointer_Queue_Item_Ref is access all Pointer_Queue_Item;
   --  pointer type to reified request descriptors

   type Pointer_Queue is
     array (Integer range 1 .. Pointer_Queue_Range_Max)
            of aliased Pointer_Queue_Item;
   --  logical queue of posted requests

   protected Protocol is
      --  the protected object used to post/fetch requests

      pragma Priority (Protocol_Ceiling);

      procedure Put
        (Req      : Request;
         Kind : Request_Kind);

      entry Get
        (Req      : out Request;
         Release_Time : out Ada.Real_Time.Time);

   private
      Barrier                     : Boolean              := False;
      Queues                      : All_Queue;
      Insert_Index                : All_Queue_Index      := (others => 1);
      Pending                     : Integer              := 0;
      Ptr_Queue                   : Pointer_Queue   :=
        (others => (Request_Kind'First, null));
      Pointer_Queue_Insert_Index  : Integer              := 1;
      Pointer_Queue_Extract_Index : Integer              := 1;
      Pointer_Queue_Overflow      : Boolean              := False;
   end Protocol;

   task Sporadic_Task is
      pragma Priority (Task_Priority);
   end Sporadic_Task;
   --  the cyclic server

end GNATCOLL.Ravenscar.Multiple_Queue_Sporadic_Server;
