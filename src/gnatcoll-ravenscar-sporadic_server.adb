------------------------------------------------------------------------------
--                                                                          --
--                	        G N A T C O L L                             --
--                                                                          --
--                      Copyright (C) 2008-2017, AdaCore                    --
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

package body GNATCOLL.Ravenscar.Sporadic_Server is
   use GNATCOLL.Ravenscar.Utils;

   procedure Put_Request (Par : Param) is
   begin
      --  just a delegation
      Protocol.Put_Request (Par);
   end Put_Request;

   protected body Protocol is

      procedure Update_Barrier is
      begin
         Barrier := Pending > 0;
      end Update_Barrier;
      pragma Inline (Update_Barrier);

      procedure Put_Request (Par : Param) is
      begin

         Buffer (Insert_Index) := Par;
         Increase_Counter (Insert_Index, QS);

         --  Check if with the posting of this message, the Insert_Index is
         --  greater of the extract index: if so, increase also the extract
         --  index to avoid fetching a newer request when older ones are
         --  pending

         if Buffer_Overflow then
            Increase_Counter (Extract_Index, QS);
         end if;

         --  increase the number of pending request but do not overcome the
         --  maximum

         if Pending < QS then
            Pending := Pending + 1;
         end if;

         --  If Insert_Index = Extract_Index, then at the next posting an
         --  overflow may occour

         if Insert_Index = Extract_Index then
            Buffer_Overflow := True;
         end if;

         Update_Barrier;

      end Put_Request;

      entry Get_Request
        (Release_Time : out Ada.Real_Time.Time;
         Par        : out Param) when Barrier
      is
      begin

         --  get the real release time

         Release_Time := Ada.Real_Time.Clock;

         --  cancel the overflow if fetching request

         if Extract_Index = Insert_Index then
            Buffer_Overflow := False;
         end if;

         Par        := Buffer (Extract_Index);

         Increase_Counter (Extract_Index, QS);

         Pending := Pending - 1;

         Update_Barrier;

      end Get_Request;

   end Protocol;

   task body Sporadic_Task is
      use Ada.Real_Time;
      Next_Time    : Time := System_Start_Time;
      Release_Time : Time;
      Par        : Param;
   begin
      loop
         delay until Next_Time;
         Protocol.Get_Request (Release_Time, Par);
         Sporadic_Operation (Par);
         Next_Time := Release_Time + Milliseconds (Minimum_Interelease_Time);
      end loop;
   end Sporadic_Task;

end GNATCOLL.Ravenscar.Sporadic_Server;
