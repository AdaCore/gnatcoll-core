------------------------------------------------------------------------------
--                                                                          --
--                     G N A T C O L L   E X A M P L E S                    --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
--                                                                          --
-- This is free software;  you can redistribute it and/or modify it         --
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

--  This example encodes/decodes to/from base64 from standard input to
--  standard output.

with Ada.Streams;              use Ada.Streams;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO;
with GNATCOLL.Coders.Streams;  use GNATCOLL.Coders;
with GNATCOLL.Coders.Base64;

procedure Base64_Coder is

   Stream  : aliased Streams.Stream_Type;
   Encoder : aliased Base64.Encoder_Type;
   Decoder : aliased Base64.Decoder_Type;
   Buffer  : Stream_Element_Array (1 .. 1024);
   Last    : Stream_Element_Offset;
   Action  : constant String := (if Argument_Count = 0 then ""
                                 else Argument (1));
   Back_Input   : access Root_Stream_Type'Class;
   Back_Output  : access Root_Stream_Type'Class;
   Input        : access Root_Stream_Type'Class;
   Output       : access Root_Stream_Type'Class;
   Read_Coder   : access Coder_Interface'Class;
   Write_Coder  : access Coder_Interface'Class;
   Input_File   : File_Type;
   Input_Stream : access Root_Stream_Type'Class;

begin
   Encoder.Initialize (Wrap => 76);
   Decoder.Initialize;

   if Argument_Count > 1 then
      Open (Input_File, In_File, Argument (2));
      Input_Stream := Text_Streams.Stream (Input_File);
   else
      Input_Stream := Text_Streams.Stream (Current_Input);
   end if;

   if Action = "" then
      --  Encode, decode back, and print to output

      Back_Input  := Input_Stream;
      Back_Output := Text_Streams.Stream (Current_Output);
      Input       := Stream'Access;
      Output      := Stream'Access;
      Read_Coder  := Encoder'Unchecked_Access;
      Write_Coder := Decoder'Unchecked_Access;

   elsif Action = "-" then
      --  Decode, encode, and print to output

      Back_Input  := Input_Stream;
      Back_Output := Text_Streams.Stream (Current_Output);
      Input       := Stream'Access;
      Output      := Stream'Access;
      Read_Coder  := Decoder'Unchecked_Access;
      Write_Coder := Encoder'Unchecked_Access;

   elsif Action = "er" then
      --  Encode over read from stream

      Back_Input  := Input_Stream;
      Input       := Stream'Access;
      Output      := Text_Streams.Stream (Current_Output);
      Read_Coder  := Encoder'Unchecked_Access;

   elsif Action = "dr" then
      --  Decode over read from stream

      Back_Input  := Input_Stream;
      Input       := Stream'Access;
      Output      := Text_Streams.Stream (Current_Output);
      Read_Coder  := Decoder'Unchecked_Access;

   elsif Action = "ew" then
      --  Encode over write to stream

      Back_Output := Text_Streams.Stream (Current_Output);
      Output      := Stream'Access;
      Input       := Input_Stream;
      Write_Coder := Encoder'Unchecked_Access;

   elsif Action = "dw" then
      --  Decode over write to stream

      Back_Output := Text_Streams.Stream (Current_Output);
      Output      := Stream'Access;
      Input       := Input_Stream;
      Write_Coder := Decoder'Unchecked_Access;
   end if;

   Stream.Initialize
     (Read_Coder   => Read_Coder,
      Read_From    => Back_Input,
      Write_Coder  => Write_Coder,
      Write_To     => Back_Output,
      Read_Ends_By => Streams.Partial_Read);

   loop
      Input.Read (Buffer, Last);
      Output.Write (Buffer (1 .. Last));
      exit when Last < Buffer'Last;
   end loop;

   if Output = Stream'Access then
      Stream.Flush (Finish);
   end if;

end Base64_Coder;
