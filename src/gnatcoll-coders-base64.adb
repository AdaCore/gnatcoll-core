------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
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

package body GNATCOLL.Coders.Base64 is

   --  The base64 character set

   Base64 : constant array (Base64_Mode) of aliased Base64_Encode_Array :=
             (MIME =>
              ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
               'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
               'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
               'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
               '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
               '+', '/'),
              URL =>
              ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
               'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
               'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
               'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
               '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
               '-', '_'));

   Shift_1_2 : constant array (1 .. 2, 1 .. 2) of Integer :=
     (1 => (1 => 4, 2 => 4), 2 => (1 => 2, 2 => 6));
   --  Shifts for 1 and 2 states of the decoding

   type Decoder_Array is array (Stream_Element) of Unsigned_8;

   function Make_Decoder return Decoder_Array;

   ------------------
   -- Make_Decoder --
   ------------------

   function Make_Decoder return Decoder_Array is
   begin
      return Result : Decoder_Array := (others => Unsigned_8'Last) do
         for J in Base64 (MIME)'Range loop
            Result (Character'Pos (Base64 (MIME) (J))) := J;
         end loop;

         for J in Base64_Encode_Array'Last - 1 .. Base64_Encode_Array'Last loop
            Result (Character'Pos (Base64 (URL) (J))) :=  J;
         end loop;
      end return;
   end Make_Decoder;

   Decoder : constant Decoder_Array := Make_Decoder;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Coder : in out Encoder_Type;
      Wrap  : Natural     := 0;
      Mode  : Base64_Mode := MIME) is
   begin
      Coder := Encoder_Type'
        (To_Char => Base64 (Mode)'Access,
         Align   => Mode = MIME,
         Wrap    => (if Wrap > 0 then Wrap + 1 else 0),
         others  => <>);
   end Initialize;

   ---------------
   -- Transcode --
   ---------------

   overriding procedure Transcode
     (Coder    : in out Encoder_Type;
      In_Data  :        Stream_Element_Array;
      In_Last  :    out Stream_Element_Offset;
      Out_Data :    out Stream_Element_Array;
      Out_Last :    out Stream_Element_Offset;
      Flush    :        Flush_Mode)
   is
      procedure Append (Item : Unsigned_16) with Inline;
      procedure Append (Item : Character);
      procedure Append_EOL;

      ------------
      -- Append --
      ------------

      procedure Append (Item : Character) is
      begin
         Out_Last := Out_Last + 1;
         Out_Data (Out_Last) := Character'Pos (Item);
         Coder.Out_Count := Coder.Out_Count + 1;
      end Append;

      procedure Append (Item : Unsigned_16) is
      begin
         Append (Coder.To_Char (Unsigned_8 (Item)));
      end Append;

      procedure Append_EOL is
      begin
         Out_Last := Out_Last + 1;
         Out_Data (Out_Last) := Character'Pos (ASCII.LF);
         Coder.Lines := Coder.Lines + 1;
      end Append_EOL;

   begin
      Out_Last := Out_Data'First - 1;
      In_Last  := In_Data'First  - 1;

      while Out_Last < Out_Data'Last loop
         if Coder.Wrap > 0 and then Coder.Out_Count > 0
           and then (Coder.Out_Count + Coder.Lines)
                     rem Stream_Element_Count (Coder.Wrap) = 0
         then
            Append_EOL;

         elsif Coder.Left_Bits >= 6 then
            Coder.Left_Bits := Coder.Left_Bits - 6;
            Append (Shift_Right (Coder.Left, Coder.Left_Bits) and 16#3F#);

         elsif Coder.Finish and then Coder.Left_Bits = 0 then
            if not Coder.Align
              or else Coder.Out_Count rem 4 not in 2 .. 3
            then
               if Coder.Wrap > 0 then
                  Append_EOL;
                  Coder.Wrap := 0;
               end if;

               exit;
            end if;

            Append ('=');

         elsif In_Last = In_Data'Last then
            exit when Coder.Finish or else Flush /= Finish;
            Coder.Finish := True;

            case Coder.Left_Bits is
               when 0 => null;
               when 2 => Append (Shift_Left (Coder.Left and 3, 4));
               when 4 => Append (Shift_Left (Coder.Left and 16#F#, 2));
               when others =>
                  raise Program_Error with "invalid Base64 encoder state";
            end case;

            Coder.Left_Bits := 0;

         elsif Coder.Left_Bits < 6 then
            In_Last := In_Last + 1;
            Coder.Left_Bits := Coder.Left_Bits + 8;
            Coder.Left := Shift_Left (Coder.Left, 8)
              or Unsigned_16 (In_Data (In_Last));
         end if;
      end loop;
   end Transcode;

   -------------
   -- Is_Open --
   -------------

   overriding function Is_Open (Coder : Encoder_Type) return Boolean is
   begin
      return Coder.To_Char /= null;
   end Is_Open;

   --------------
   -- Total_In --
   --------------

   overriding function Total_In
     (Coder : Coder_Type) return Stream_Element_Count is
   begin
      return Coder.In_Count;
   end Total_In;

   ---------------
   -- Total_Out --
   ---------------

   overriding function Total_Out
     (Coder : Encoder_Type) return Stream_Element_Count is
   begin
      return Coder.Out_Count + Coder.Lines - 1;
   end Total_Out;

   --------------
   -- Finished --
   --------------

   overriding function Finished (Coder : Encoder_Type) return Boolean is
   begin
      return Coder.Finish and then Coder.Left_Bits = 0 and then Coder.Wrap = 0
        and then (not Coder.Align or else Coder.Out_Count rem 4 not in 2 .. 3);
   end Finished;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Coder : in out Encoder_Type) is
   begin
      Coder.To_Char := null;
   end Close;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Coder : in out Decoder_Type) is
   begin
      Coder := Decoder_Type'(others => <>);
      Coder.Open := True;
   end Initialize;

   ---------------
   -- Transcode --
   ---------------

   overriding procedure Transcode
     (Coder    : in out Decoder_Type;
      In_Data  :        Stream_Element_Array;
      In_Last  :    out Stream_Element_Offset;
      Out_Data :    out Stream_Element_Array;
      Out_Last :    out Stream_Element_Offset;
      Flush    :        Flush_Mode)
   is
      Bits  : Unsigned_8;
      State : Integer;

      procedure Append (Item : Unsigned_8);

      ------------
      -- Append --
      ------------

      procedure Append (Item : Unsigned_8) is
      begin
         Out_Last := Out_Last + 1;
         Out_Data (Out_Last) := Stream_Element (Item);
         Coder.Out_Count := Coder.Out_Count + 1;
      end Append;

   begin
      Out_Last := Out_Data'First - 1;
      In_Last  := In_Data'First  - 1;

      while Out_Last < Out_Data'Last loop
         if In_Last = In_Data'Last then
            exit when Flush /= Finish or else Coder.Finish;

            Coder.Finish := True;

            if Coder.Has then
               Append (Coder.Bits);
               Coder.Has := False;
            end if;

            exit;

         else
            In_Last := In_Last + 1;
            Bits := Decoder (In_Data (In_Last));
         end if;

         if Bits /= Unsigned_8'Last then
            State := Integer (Coder.In_Count rem 4);

            case State is
               when 0 =>
                  Coder.Bits := Shift_Left (Bits, 2);
                  Coder.Has := True;

               when 1 | 2 =>
                  Append
                    (Coder.Bits or Shift_Right (Bits, Shift_1_2 (State, 1)));

                  Coder.Bits := Shift_Left (Bits, Shift_1_2 (State, 2));
                  Coder.Has := False;

               when others =>
                  Append (Coder.Bits or Bits);
                  Coder.Has := False;
            end case;

            Coder.In_Count := Coder.In_Count + 1;
         end if;
      end loop;
   end Transcode;

   -------------
   -- Is_Open --
   -------------

   overriding function Is_Open (Coder : Decoder_Type) return Boolean is
   begin
      return Coder.Open;
   end Is_Open;

   ---------------
   -- Total_Out --
   ---------------

   overriding function Total_Out
     (Coder : Decoder_Type) return Stream_Element_Count is
   begin
      return Coder.Out_Count;
   end Total_Out;

   --------------
   -- Finished --
   --------------

   overriding function Finished (Coder : Decoder_Type) return Boolean is
   begin
      return Coder.Finish and not Coder.Has;
   end Finished;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Coder : in out Decoder_Type) is
   begin
      Coder.Open := False;
   end Close;

end GNATCOLL.Coders.Base64;
