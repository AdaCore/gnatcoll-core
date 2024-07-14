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

--  This package declares the interface for different types of coders, such
--  as compressors/decompressors, encoders/decoders, and any other kind of
--  streaming data transformation.

with Ada.Streams; use Ada.Streams;

package GNATCOLL.Coders is

   type Coder_Interface is limited interface;

   type Coder_Access is access all Coder_Interface'Class;

   type Flush_Mode is
     (No_Flush,
      --  Regular way for coding, no flush

      Sync_Flush,
      --  All pending output is flushed to the output buffer and the output
      --  is aligned on a byte boundary, so that the decoder can get all
      --  input data available so far. (In particular In_Last = In_Data'Last
      --  after the call to Transcode if enough output space has been provided
      --  before the call). Flushing may degrade compression for some
      --  compression algorithms and so it should be used only when necessary.

      Full_Flush,
      --  All output is flushed as with Synch_Flush, and the coding state is
      --  reset so that decoding can restart from this point if previous
      --  compressed data has been damaged or if random access is desired.
      --  Using Full_Flush too often can seriously degrade the compression kind
      --  of coding.

      Finish);
      --  Just to tell the coder that input data is complete

   function Is_Open (Coder : Coder_Interface) return Boolean is abstract;
   --  Indicates that coder is ready to transcode data.
   --  Initialization procedure has to be implemented in the descendant with
   --  parameters appropriate to the transcoding algorithm.

   procedure Transcode
     (Coder    : in out Coder_Interface;
      In_Data  :        Stream_Element_Array;
      In_Last  :    out Stream_Element_Offset;
      Out_Data :    out Stream_Element_Array;
      Out_Last :    out Stream_Element_Offset;
      Flush    :        Flush_Mode) is abstract
     with Pre'Class => In_Data'First > Stream_Element_Offset'First
       and then Out_Data'First > Stream_Element_Offset'First;
   --  Transcodes data from In_Data to Out_Date.
   --  In_Last is the index of last element from In_Data accepted by
   --  the Coder.
   --  Out_Last is the index of the last element written to the Out_Data.
   --  To tell the Coder that incoming data is complete, pass Finish as the
   --  Flush parameter and call Transcoder with empty In_Data until Finished
   --  routine indicates end of stream.

   procedure Flush
     (Coder    : in out Coder_Interface'Class;
      Out_Data :    out Stream_Element_Array;
      Out_Last :    out Stream_Element_Offset;
      Flush    :        Flush_Mode) with Inline;
   --  Flushes the data from coder

   function Total_In
     (Coder : Coder_Interface) return Stream_Element_Count is abstract;
   --  Returns total amount of input data sent into the coder

   function Total_Out
     (Coder : Coder_Interface) return Stream_Element_Count is abstract;
   --  Returns total amount of output data taken from the coder

   function Finished (Coder : Coder_Interface) return Boolean is abstract;
   --  Indicates that incoming data stream finished and all internally
   --  processed data is out of coder.

   procedure Close (Coder : in out Coder_Interface) is abstract;
   --  Frees all internal coder memory allocations

private

   --------------------------------------------------------------------------
   -- Generic Read/Write routines helpers to implement streaming interface --
   --------------------------------------------------------------------------

   generic
      with procedure Write (Item : Stream_Element_Array);
      --  User should provide this routine to accept transcoded data

      Buffer_Size : Stream_Element_Offset;
      --  Buffer size for Write user routine

   procedure Write
     (Coder : in out Coder_Interface'Class;
      Item  :        Stream_Element_Array;
      Flush :        Flush_Mode := No_Flush);
   --  Transcodes data from Item and put it to the generic parameter procedure
   --  Write. Output buffer size could be set in Buffer_Size generic parameter.

   generic
      with procedure Read
        (Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset);
      --  User should provide data to transcode through this routine

      Buffer : in out Stream_Element_Array;
      --  Buffer to keep remaining data from the previous back read

      Rest_First, Rest_Last : in out Stream_Element_Offset;
      --  Rest_First have to be initialized to Buffer'Last + 1
      --  Rest_Last have to be initialized to Buffer'Last
      --  before usage.

      Allow_Read_Some : Boolean := False;
      --  Is it allowed to return Last < Item'Last before end of data

   procedure Read
     (Coder : in out Coder_Interface'Class;
      Item  :    out Stream_Element_Array;
      Last  :    out Stream_Element_Offset;
      Flush :        Flush_Mode := No_Flush);
   --  Transcodes data from generic parameter procedure Read to the Item.
   --  User should provide Buffer and initialized Rest_First, Rest_Last
   --  indicators. If Allow_Read_Some is True, Read routines could return
   --  Last < Item'Last only at end of stream.

end GNATCOLL.Coders;
