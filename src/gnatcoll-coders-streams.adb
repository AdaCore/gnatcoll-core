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

package body GNATCOLL.Coders.Streams is

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      Buffer : constant access Stream_Element_Array :=
        Stream.Buffer.Reference.Element;

      procedure Read_Stream
        (Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset);

      -----------------
      -- Read_Stream --
      -----------------

      procedure Read_Stream
        (Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset) is
      begin
         Stream.Read_From.Read (Item, Last);

         if (Stream.Read_Ends = Partial_Read and then Last < Item'Last)
           or else (Stream.Read_Ends = Empty_Read and then Last < Item'First)
         then
            Stream.End_Of_Read := True;
         end if;
      end Read_Stream;

      procedure Read_Over is new Coders.Read
         (Read       => Read_Stream,
          Buffer     => Buffer.all,
          Rest_First => Stream.Rest_First,
          Rest_Last  => Stream.Rest_Last);

   begin
      Read_Over
        (Stream.Read_Coder.all, Item, Last,
         Flush => (if Stream.End_Of_Read then Finish else No_Flush));
   end Read;

   ------------------
   -- End_Of_Input --
   ------------------

   procedure End_Of_Input (Stream : in out Stream_Type) is
   begin
      Stream.End_Of_Read := True;
   end End_Of_Input;

   function End_Of_Input (Stream : Stream_Type) return Boolean is
   begin
      return Stream.End_Of_Read;
   end End_Of_Input;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Stream_Type; Item : Stream_Element_Array)
   is
      procedure Write_Stream (Item : Stream_Element_Array);

      -----------
      -- Write --
      -----------

      procedure Write_Stream (Item : Stream_Element_Array) is
      begin
         Stream.Write_To.Write (Item);
      end Write_Stream;

      procedure Write_Over is new Coders.Write
         (Write => Write_Stream, Buffer_Size => Stream.Buffer_Size);

   begin
      Write_Over (Stream.Write_Coder.all, Item, No_Flush);
   end Write;

   -----------
   -- Flush --
   -----------

   procedure Flush
     (Stream : in out Stream_Type;
      Mode   :        Flush_Mode := Sync_Flush)
   is
      Buffer : Stream_Element_Array (1 .. Stream.Buffer_Size);
      Last   : Stream_Element_Offset;
   begin
      loop
         Stream.Write_Coder.Flush (Buffer, Last, Mode);

         Stream.Write_To.Write (Buffer (1 .. Last));

         exit when Last < Buffer'Last;
      end loop;
   end Flush;

   ----------------
   -- Flush_Read --
   ----------------

   procedure Flush_Read
     (Stream : in out Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset;
      Mode   :        Flush_Mode := Sync_Flush) is
   begin
      Stream.Read_Coder.Flush (Item, Last, Mode);
   end Flush_Read;

   -------------------
   -- Read_Total_In --
   -------------------

   function Read_Total_In
     (Stream : Stream_Type) return Stream_Element_Count is
   begin
      return (if Stream.Read_Coder = null then 0
              else Stream.Read_Coder.Total_In);
   end Read_Total_In;

   --------------------
   -- Read_Total_Out --
   --------------------

   function Read_Total_Out
     (Stream : Stream_Type) return Stream_Element_Count is
   begin
      return (if Stream.Read_Coder = null then 0
              else Stream.Read_Coder.Total_Out);
   end Read_Total_Out;

   --------------------
   -- Write_Total_In --
   --------------------

   function Write_Total_In
     (Stream : Stream_Type) return Stream_Element_Count is
   begin
      return (if Stream.Write_Coder = null then 0
              else Stream.Write_Coder.Total_In);
   end Write_Total_In;

   ---------------------
   -- Write_Total_Out --
   ---------------------

   function Write_Total_Out
     (Stream : Stream_Type) return Stream_Element_Count is
   begin
      return (if Stream.Write_Coder = null then 0
              else Stream.Write_Coder.Total_Out);
   end Write_Total_Out;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Stream            : in out Stream_Type;
      Read_Coder        : access Coder_Interface'Class  := null;
      Write_Coder       : access Coder_Interface'Class  := null;
      Read_From         : access Root_Stream_Type'Class := null;
      Write_To          : access Root_Stream_Type'Class := null;
      Read_Ends_By      : End_Of_Input_Method           := Empty_Read;
      Read_Buffer_Size  : Stream_Element_Count          := Default_Buffer_Size;
      Write_Buffer_Size : Stream_Element_Count          := Default_Buffer_Size)
   is
   begin
      if (Read_Coder = null) /= (Read_From = null) then
         raise Constraint_Error with
           "Read coder and stream have to be either null or not null together";
      end if;

      if (Write_Coder = null) /= (Write_To = null) then
         raise Constraint_Error with
           "Write coder and stream have to be either null or not null" &
           " together";
      end if;

      if Write_To = null and then Read_From = null then
         raise Constraint_Error with
           "Either write or read coders and streams have to be defined";
      end if;

      Stream.Read_Coder  := Read_Coder;
      Stream.Write_Coder := Write_Coder;
      Stream.Read_From   := Read_From;
      Stream.Write_To    := Write_To;
      Stream.Read_Ends   := Read_Ends_By;
      Stream.End_Of_Read := False;

      if Read_Coder /= null then
         Stream.Buffer := SEA_Holders.To_Holder ((1 .. Read_Buffer_Size => 0));

         Stream.Rest_First := Read_Buffer_Size + 1;
         Stream.Rest_Last  := Read_Buffer_Size;
      end if;

      Stream.Buffer_Size := Write_Buffer_Size;
   end Initialize;

end GNATCOLL.Coders.Streams;
