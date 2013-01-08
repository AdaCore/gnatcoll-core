------------------------------------------------------------------------------
--                                                                          --
--                	        G N A T C O L L                             --
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

with GNATCOLL.Ravenscar.Utils;

package body GNATCOLL.Ravenscar.Multiple_Queue_Cyclic_Server is
   use GNATCOLL.Ravenscar.Utils;

   procedure Put_Request
     (Req      : Request;
      Kind : Request_Kind)
   is
   begin

      --  Simply delegates to Protocol.Put

      Protocol.Put (Req, Kind);

   end Put_Request;

   protected body Protocol is

      procedure Put (Req : Request; Kind : Request_Kind) is
         Ref : constant Request_Type_Ref :=
            Queues (Kind) (Insert_Index (Kind))'Access;
      begin

         --  put the Req in the appropriate queue

         Queues (Kind) (Insert_Index (Kind))   := Req;

         --  increase insert index

         Increase_Counter (Insert_Index (Kind), QS);
         if Pending < Pointer_Queue_Range_Max then
            Pending := Pending + 1;
         end if;

         --  save pointer to last Req on a queue

         Ptr_Queue (Pointer_Queue_Insert_Index) := (Kind, Ref);
         Increase_Counter
           (Pointer_Queue_Insert_Index,
            Pointer_Queue_Range_Max);

         --  If there has been an overflow, increase also the Extract index.
         --  This is because the insert index has surpassed the extract index,
         --  overwriting older request. It is thus necessary to increase the
         --  extract index to avoid to fetch a newly posted request instead
         --  of older ones.

         if Pointer_Queue_Overflow then
            Increase_Counter
              (Pointer_Queue_Extract_Index,
               Pointer_Queue_Range_Max);
         end if;

         --  Check if the Insert_Index is going to surpass the extract index

         if Pointer_Queue_Insert_Index = Pointer_Queue_Extract_Index then
            Pointer_Queue_Overflow := True;
         end if;

      end Put;

      procedure Get_Next_Request (Req : out Request) is
         Ref : Pointer_Queue_Item_Ref := null;
      begin

         --  If the insert and extract index have the same value, it is
         --  possible to avoid overflow if a request is fetched before a new
         --  one is posted: this case addresses exactly this.

         if Pointer_Queue_Insert_Index = Pointer_Queue_Extract_Index then
            Pointer_Queue_Overflow := False;
         end if;

         --  get the oldest Req

         Ref := Ptr_Queue (Pointer_Queue_Extract_Index)'Access;
         Increase_Counter
           (Pointer_Queue_Extract_Index,
            Pointer_Queue_Range_Max);

         Req     := Ref.Req.all;

         --  decrease number of pending requests

         Pending := Pending - 1;

      end Get_Next_Request;
      pragma Inline (Get_Next_Request);

      procedure Get (Req : out Request; Has_Pending : out Boolean) is
      begin

         --  check if there are pending requests

         Has_Pending := Pending > 0;
         if Pending > 0 then
            Get_Next_Request (Req);
         end if;

      end Get;

   end Protocol;

   task body Cyclic_Task is
      Req     : Request;
      Next_Time   : Ada.Real_Time.Time := System_Start_Time;
      Has_Pending : Boolean            := False;
      use Ada.Real_Time;
   begin

      Next_Time := Next_Time + Milliseconds (Phase);

      loop

         delay until Next_Time;

         Protocol.Get (Req, Has_Pending);

         if Has_Pending then
            Dispatch (Req);
         else
            Cyclic_Operation;
         end if;

         Next_Time := Next_Time + Milliseconds (Period);

      end loop;

   end Cyclic_Task;

end GNATCOLL.Ravenscar.Multiple_Queue_Cyclic_Server;
