------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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
--
--  Test base64 encoder and decoder

with Ada.Streams;            use Ada.Streams;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Text_IO;            use Ada.Text_IO;
with GNATCOLL.Coders.Base64; use GNATCOLL.Coders;
with GNATCOLL.Email.Utils;   use GNATCOLL.Email.Utils;
with Test_Assert;
with GNAT.Random_Numbers;    use GNAT.Random_Numbers;

procedure TB64 is

   package A renames Test_Assert;

   Encoder   : Base64.Encoder_Type;
   Decoder   : Base64.Decoder_Type;
   Sample    : Stream_Element_Array (1 .. 4096);
   Text      : String (1 .. Sample'Length);
   for Text'Address use Sample'Address;
   Coded     : Stream_Element_Array (1 .. Sample'Last * 5 / 3 + 1);
   Coded_S   : String (1 .. Coded'Length);
   for Coded_S'Address use Coded'Address;
   Coded_U   : Unbounded_String;
   Result    : Stream_Element_Array (Sample'First .. Sample'Last + 1);
   Result_U  : Unbounded_String;
   In_Last   : Stream_Element_Offset;
   Cod_Last  : Stream_Element_Offset;
   Res_Last  : Stream_Element_Offset;
   Gen       : Generator;
   Printable : constant Boolean := False;

begin
   Reset (Gen);
   Coded (1) := Stream_Element'Val (Character'Pos ('a'));

   for J in Sample'Range loop
      if Printable then
         Sample (1 .. J) := Coded (1 .. J);
      else
         Sample (J) := Stream_Element
           (Stream_Element'Mod (Integer'(Random (Gen))));
      end if;

      Encoder.Initialize
        (Wrap => (if Integer'(Random (Gen)) rem 8 = 0 then 0
                  else Random (Gen) rem 70 + 20),
         Mode => Base64.Base64_Mode'Val
           (Integer'(Random (Gen))
            rem (Base64.Base64_Mode'Pos (Base64.Base64_Mode'Last) + 1)));
      Decoder.Initialize;

      Encoder.Transcode
        (In_Data  => Sample (1 .. J),
         In_Last  => In_Last,
         Out_Data => Coded,
         Out_Last => Cod_Last,
         Flush    => Finish);

      A.Assert (In_Last = J, "In_Last /= J");

      Decoder.Transcode
        (In_Data  => Coded (1 .. Cod_Last),
         In_Last  => In_Last,
         Out_Data => Result,
         Out_Last => Res_Last,
         Flush    => Finish);

      A.Assert (In_Last = Cod_Last, "In_Last /= Cod_Last");

      A.Assert (J = Res_Last, "J /= Res_Last");

      A.Assert (Sample (1 .. J) = Result (1 .. J), "Sample /= Result");

      Encoder.Close;
      Decoder.Close;

      Base64_Encode
        (Text (1 .. Integer (J)), "",
         Max_Block_Len => Random (Gen) rem 64 + 16,
         Result        => Coded_U);
      Base64_Decode (To_String (Coded_U), Result_U);

      A.Assert
        (Result_U = Text (1 .. Integer (J)),
         "Result_U /= Text (1 .. Integer (J))");
   end loop;
end TB64;
