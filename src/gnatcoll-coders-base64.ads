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

--  This package provides Base64 encoding/decoding

with Interfaces;

package GNATCOLL.Coders.Base64 is

   type Base64_Mode is (MIME, URL);
   --  Base64 encoding variants for encoding routines,
   --  RFC4648
   --  MIME - section 4
   --  URL  - section 5

   type Coder_Type is abstract new Coder_Interface with private;
   --  Common code and data for base64 encoder/decoder

   overriding function Total_In
     (Coder : Coder_Type) return Stream_Element_Count;
   --  Returns total amount of input data sent into the coder

   type Encoder_Type is new Coder_Type with private;
   --  Base64 encoder

   procedure Initialize
     (Coder : in out Encoder_Type;
      Wrap    : Natural     := 0;
      Mode    : Base64_Mode := MIME);
   --  Initialize base64 encoder.
   --  Wrap defines line length in encoded output.
   --  Mode MIME mean base64 encoding defined in RFC 2045 section 6.8.
   --  Mode URL mean base64 encoding defined in RFC 4648 section 4.

   overriding procedure Transcode
     (Coder    : in out Encoder_Type;
      In_Data  :        Stream_Element_Array;
      In_Last  :    out Stream_Element_Offset;
      Out_Data :    out Stream_Element_Array;
      Out_Last :    out Stream_Element_Offset;
      Flush    :        Flush_Mode)
     with Pre => In_Data'First > Stream_Element_Offset'First
       and then Out_Data'First > Stream_Element_Offset'First;
   --  Encode data to base64 from In_Data to Out_Date.
   --  In_Last is the index of last element from In_Data accepted by
   --  the Coder.
   --  Out_Last is the index of the last element written to the Out_Data.

   overriding function Is_Open (Coder : Encoder_Type) return Boolean;
   --  Indicates that encoder is ready for data processing

   overriding function Total_Out
     (Coder : Encoder_Type) return Stream_Element_Count;
   --  Returns total amount of output data taken from the coder

   overriding function Finished (Coder : Encoder_Type) return Boolean;
   --  Indicates that incoming data stream finished and all internally
   --  processed data is out of coder.

   overriding procedure Close (Coder : in out Encoder_Type);
   --  Close encoding

   type Decoder_Type is new Coder_Type with private;
   --  Base64 decoder

   procedure Initialize (Coder : in out Decoder_Type);
   --  Initialize base64 decoder

   overriding procedure Transcode
     (Coder    : in out Decoder_Type;
      In_Data  :        Stream_Element_Array;
      In_Last  :    out Stream_Element_Offset;
      Out_Data :    out Stream_Element_Array;
      Out_Last :    out Stream_Element_Offset;
      Flush    :        Flush_Mode)
     with Pre => In_Data'First > Stream_Element_Offset'First
       and then Out_Data'First > Stream_Element_Offset'First;
   --  Decode base64 data from In_Data to Out_Date.

   overriding function Is_Open (Coder : Decoder_Type) return Boolean;
   --  Indicates that decoder is ready for data processing

   overriding function Total_Out
     (Coder : Decoder_Type) return Stream_Element_Count;
   --  Returns total amount of output data taken from the decoder

   overriding function Finished (Coder : Decoder_Type) return Boolean;
   --  Indicates that incoming data stream finished and all internally
   --  processed data is out of decoder.

   overriding procedure Close (Coder : in out Decoder_Type);
   --  Close decoding

private

   use Interfaces;

   type Base64_Encode_Array is array (Unsigned_8 range 0 .. 63) of Character;

   type Coder_Type is abstract new Coder_Interface with record
      In_Count  : Stream_Element_Count := 0;
      Out_Count : Stream_Element_Count := 0;
      Finish    : Boolean              := False;
   end record;

   type Encoder_Type is new Coder_Type with record
      To_Char   : access constant Base64_Encode_Array;
      Lines     : Stream_Element_Count := 1; -- Number of lines
      Left      : Unsigned_16          := 0;
      Left_Bits : Integer              := 0;
      Align     : Boolean              := False;
      Wrap      : Natural;
   end record;

   type Decoder_Type is new Coder_Type with record
      Bits  : Unsigned_8 := 0;
      Has   : Boolean    := False;
      Open  : Boolean    := False;
   end record;

end GNATCOLL.Coders.Base64;
