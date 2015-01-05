------------------------------------------------------------------------------
--                                                                          --
--                         G N A T C O L L                                  --
--                                                                          --
--                      Copyright (C) 2008-2015, AdaCore                    --
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

--  This version of a Ravenscar-compliant cyclic server extends the behaviour
--  of GNAT.Ravenscar.Simple_Cyclic_Task by accepting multiple types of
--  requests reified in a single variant type; if no requests have been posted
--  during the previous period, the server executes its nominal operation
--  (Cyclic_Operation). It shares all basic properties of
--  GNAT.Ravenscar.Simple_Cyclic_Task.
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
--  procedure Cyclic_Operation;
--
--  package My_Cyclic_Server is new GNAT.Ravenscar.Multiple_Queue_Cyclic_Server
--      (Task_Priority => 10,
--       Period => 1_000,
--       Phase => 200,
--       System_Start_Time => System_Properties.Start_UP_Time,
--       Cyclic_Operation => Cyclic_Operation,
--       Protocol_Ceiling => 15,
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
--    My_Sporadic_Task.Put_Request(P);
--
--  BEHAVIOUR
--  If no Req is posted via Put_Request, My_Cyclic_Server executes
--  Cyclic_Operation at its frequency; otherwise it fulfill the posted Req.

with System;
with Ada.Real_Time;

generic

   Task_Priority : System.Priority;
   --  the priority of the server

   Period : Millisecond;
   --  the constant perios of the server

   System_Start_Time : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   --  the absolute instant in time for the release of the systems as a whole

   Phase : Millisecond;
   --  the phase of the server

   with procedure Cyclic_Operation;
   --  the nominal operation which is executed if no requestests have been
   --  posted within the previous cycle

   Protocol_Ceiling : System.Any_Priority;
   --  the ceiling priority of the protected object used to post and fetch
   --  requests

   QS : Queue_Size;
   --  the size of accepted requests

   type Request_Kind is (<>);
   --  an enumeration type identificng the possible kinds of request

   type Request is private;
   --  the reified request type

   with procedure Dispatch (Req : Request);
   --  the procedure invoked by the server to dispatch the fetched request

package GNATCOLL.Ravenscar.Multiple_Queue_Cyclic_Server is

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
      pragma Priority (Protocol_Ceiling);

      procedure Put (Req : Request; Kind : Request_Kind);

      procedure Get (Req : out Request; Has_Pending : out Boolean);

   private
      Queues                      : All_Queue;
      Insert_Index                : All_Queue_Index := (others => 1);
      Pending                     : Integer           := 0;
      Ptr_Queue                   : Pointer_Queue   :=
        (others => (Request_Kind'First, null));
      Pointer_Queue_Insert_Index  : Integer           := 1;
      Pointer_Queue_Extract_Index : Integer           := 1;
      Pointer_Queue_Overflow      : Boolean           := False;
   end Protocol;
   --  the protected object used to post/fetch requests

   task Cyclic_Task is
      pragma Priority (Task_Priority);
   end Cyclic_Task;
   --  the cyclic server

end GNATCOLL.Ravenscar.Multiple_Queue_Cyclic_Server;
