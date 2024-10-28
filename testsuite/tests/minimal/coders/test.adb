------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
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
--  Test for GNATCOLL.Coders

with Ada.Streams;             use Ada.Streams;
with GNATCOLL.Coders.Streams; use GNATCOLL.Coders;
with GNATCOLL.Coders.Base64;
with Tb64;
with Test_Streams;
with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;

   Stream  : aliased Streams.Stream_Type;
   Encoder : aliased Base64.Encoder_Type;
   Decoder : aliased Base64.Decoder_Type;
   Buffer  : Stream_Element_Array (0 .. 511);

   function Test_Iteration
     (Read_Ends_By : Streams.End_Of_Input_Method;
      Tail         : Stream_Element_Offset) return Boolean;

   function Test_Iteration
     (Read_Ends_By : Streams.End_Of_Input_Method;
      Tail         : Stream_Element_Offset) return Boolean
   is
      subtype Middle_Range is Stream_Element_Offset range 241 .. 270;

      TS : aliased Test_Streams.Stream_Type;
      Last : Stream_Element_Offset;
   begin
      Encoder.Initialize (Wrap => 76);
      Decoder.Initialize;

      Stream.Initialize
        (Read_Coder   => Decoder'Unchecked_Access,
         Write_Coder  => Encoder'Unchecked_Access,
         Read_From    => TS'Unchecked_Access,
         Write_To     => TS'Unchecked_Access,
         Read_Ends_By => Read_Ends_By);

      for M in Middle_Range loop
         Stream.Write (Buffer (Buffer'First .. M));
         Stream.Write (Buffer (M + 1 .. Buffer'Last));
      end loop;
      Stream.Write (Buffer (Buffer'First .. Tail));

      Stream.Flush (Finish);

      declare
         Result : Stream_Element_Array (Buffer'Range);
      begin
         for M in reverse Middle_Range loop
            Stream.Read (Result (Result'First .. M), Last);
            if Last /= M then
               A.Assert (False, "Unexpected" & Last'Img & "  /= " & M'Img);
               return False;
            end if;
            Stream.Read (Result (M + 1 .. Result'Last), Last);

            if Last /= Buffer'Last then
               A.Assert
                 (False,
                  "Unexpected" & Last'Img & "  /= " & Buffer'Last'Img & " at"
                  & M'Img);
               return False;
            end if;

            if Result (Buffer'Range) /= Buffer then
               A.Assert
                 (False, "Result (Buffer'Range) /= Buffer at " & M'Img & ' '
                  & TS.Slice (1, 64));
               for J in Buffer'Range loop
                  if Result (J) /= Buffer (J) then
                     A.Assert
                       (Result (J)'Img, Buffer (J)'Img, "at index " & J'Img);
                     return False;
                  end if;
               end loop;

               return False;
            end if;
         end loop;

         Stream.Read (Result, Last);

         if Last /= Tail then
            A.Assert (False, "Unexpected" & Last'Img & "  /= " & Tail'Img);
            return False;
         end if;

         if Result (Result'First .. Tail) /= Buffer (Buffer'First .. Tail) then
            A.Assert (False, "Tail failure");
            for J in Result'First .. Tail loop
               if Result (J) /= Buffer (J) then
                  A.Assert
                    (Result (J)'Img, Buffer (J)'Img, "at index " & J'Img);
                  return False;
               end if;
            end loop;
            return False;
         end if;
      end;

      return True;
   end Test_Iteration;

begin
   for J in Buffer'Range loop
      Buffer (J) := Stream_Element'Mod (J);
   end loop;

   Main_Loop : for End_Mode in Streams.End_Of_Input_Method loop
      for Tail in Stream_Element_Offset range 1 .. 255 loop
         exit Main_Loop when not Test_Iteration (End_Mode, Tail);
      end loop;
   end loop Main_Loop;

   Tb64;

   return A.Report;
end Test;
