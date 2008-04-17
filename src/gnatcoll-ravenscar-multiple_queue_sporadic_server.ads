------------------------------------------------------------------------------
--                                                                          --
--                             G N A T C O L L                              --
--                                                                          --
--                      Copyright (C) 2008, AdaCore                         --
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

--  This version of a Ravenscar-compliant sporadic server extends the behaviour
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
   Minimum_Interelease_Time : Millisecond;
   System_Start_Time : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   Protocol_Ceiling : System.Any_Priority;
   QS : Queue_Size;
   type Request_Kind is (<>);
   type Request is private;
   with procedure Dispatch (Req : Request);
package GNATCOLL.Ravenscar.Multiple_Queue_Sporadic_Server is

   procedure Put_Request
     (Req      : Request;
      Kind : Request_Kind);

private

   type Request_Type_Ref is access all Request;

   type Request_Queue is array (1 .. QS) of aliased Request;

   type All_Queue is array (Request_Kind'Range) of Request_Queue;

   type All_Queue_Index is array (Request_Kind'Range) of Queue_Range;

   Pointer_Queue_Range_Max : constant Integer := QS;

   type Pointer_Queue_Item is record
      Kind    : Request_Kind;
      Req : Request_Type_Ref;
   end record;

   type Pointer_Queue_Item_Ref is access all Pointer_Queue_Item;

   type Pointer_Queue is
     array (Integer range 1 .. Pointer_Queue_Range_Max)
            of aliased Pointer_Queue_Item;

   protected Protocol is
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

end GNATCOLL.Ravenscar.Multiple_Queue_Sporadic_Server;
