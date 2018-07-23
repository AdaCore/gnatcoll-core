------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

package body GNATCOLL.Coders is

   -----------
   -- Flush --
   -----------

   procedure Flush
     (Coder    : in out Coder_Interface'Class;
      Out_Data :    out Stream_Element_Array;
      Out_Last :    out Stream_Element_Offset;
      Flush    :        Flush_Mode)
   is
      No_Data : constant Stream_Element_Array := (1 .. 0 => 0);
      Last    : Stream_Element_Offset;
   begin
      Transcode (Coder, No_Data, Last, Out_Data, Out_Last, Flush);
   end Flush;

   -----------
   -- Write --
   -----------

   procedure Write
     (Coder : in out Coder_Interface'Class;
      Item  :        Stream_Element_Array;
      Flush :        Flush_Mode := No_Flush)
   is
      Buffer   : Stream_Element_Array (1 .. Buffer_Size);
      In_Last  : Stream_Element_Offset;
      Out_Last : Stream_Element_Offset;
      In_First : Stream_Element_Offset := Item'First;
   begin
      if Item'Length = 0 and Flush = No_Flush then
         return;
      end if;

      loop
         Transcode
           (Coder    => Coder,
            In_Data  => Item (In_First .. Item'Last),
            In_Last  => In_Last,
            Out_Data => Buffer,
            Out_Last => Out_Last,
            Flush    => Flush);

         if Out_Last >= Buffer'First then
            Write (Buffer (1 .. Out_Last));
         end if;

         exit when In_Last = Item'Last or Finished (Coder);

         In_First := In_Last + 1;
      end loop;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (Coder : in out Coder_Interface'Class;
      Item  :    out Stream_Element_Array;
      Last  :    out Stream_Element_Offset;
      Flush :        Flush_Mode := No_Flush)
   is
      In_Last    : Stream_Element_Offset;
      Item_First : Stream_Element_Offset := Item'First;
      V_Flush    : Flush_Mode := Flush;

   begin
      pragma Assert (Rest_First in Buffer'First .. Buffer'Last + 1);
      pragma Assert (Rest_Last in Buffer'First - 1 .. Buffer'Last);

      loop
         if Rest_Last = Buffer'First - 1 then
            V_Flush := Finish;

         elsif Rest_First > Rest_Last then
            Read (Buffer, Rest_Last);
            Rest_First := Buffer'First;

            if Rest_Last < Buffer'First then
               V_Flush := Finish;
            end if;
         end if;

         Transcode
           (Coder    => Coder,
            In_Data  => Buffer (Rest_First .. Rest_Last),
            In_Last  => In_Last,
            Out_Data => Item (Item_First .. Item'Last),
            Out_Last => Last,
            Flush    => V_Flush);

         Rest_First := In_Last + 1;

         exit when Finished (Coder)
           or else Last = Item'Last
           or else (Last >= Item'First and then Allow_Read_Some);

         Item_First := Last + 1;
      end loop;
   end Read;

end GNATCOLL.Coders;
