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

--  This package provides Ada stream interface to the coders implemented the
--  parent package abstraction Coder_Interface. Example of using this package
--  can be found in examples/base64_coder.adb.

with Ada.Containers.Indefinite_Holders;

package GNATCOLL.Coders.Streams is

   type Stream_Type is new Root_Stream_Type with private;

   type Stream_Access is access all Root_Stream_Type'Class;

   type End_Of_Input_Method is (Empty_Read, Partial_Read, Explicit);
   --  Method to determine end of Read_From stream (See Initialize parameters
   --  below).
   --  Empty_Read means that the end of input stream is determined by last
   --  read from Read_From stream giving an empty result.
   --  Partial_Read means that the end of input stream is determined by last
   --  read from Read_From stream giving Partial result (Last < Item'Last).
   --  Explicit means that the end of input stream is determined by explicitly
   --  calling End_Of_Input. Note that a call to End_Of_Input procedure
   --  indicates the end of input in any case, independent of Read_Ends_By
   --  parameter of Initialize routine.

   Default_Buffer_Size : constant := 4096;
   --  Default buffer size for Read and Write operations.

   overriding procedure Read
     (Stream : in out Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Stream_Type; Item : Stream_Element_Array);

   procedure Initialize
     (Stream            : in out Stream_Type;
      Read_Coder        : access Coder_Interface'Class  := null;
      Write_Coder       : access Coder_Interface'Class  := null;
      Read_From         : access Root_Stream_Type'Class := null;
      Write_To          : access Root_Stream_Type'Class := null;
      Read_Ends_By      : End_Of_Input_Method           := Empty_Read;
      Read_Buffer_Size  : Stream_Element_Count := Default_Buffer_Size;
      Write_Buffer_Size : Stream_Element_Count := Default_Buffer_Size);
   --  Sets read and/or write streams and coders for them.
   --  If Read pair is defined then Read operation is available.
   --  If Write pair is defined then Write operation is available.

   procedure Flush
     (Stream : in out Stream_Type; Mode : Flush_Mode := Sync_Flush);
   --  Flushes the written data to the Write_To stream,
   --  All data placed to the Write_Coder is flushed to the Write_To stream.
   --  Should not be used unless necessary, as it may e.g. degrade the
   --  compression quality in case when coder is compressor.

   procedure Flush_Read
     (Stream : in out Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset;
      Mode   :        Flush_Mode := Sync_Flush);
   --  Flushes read data from the Read_Coder and returns it in Item and Last
   --  out parameters.

   function Read_Total_In
     (Stream : Stream_Type) return Stream_Element_Count with Inline;
   --  Returns the total number of bytes read from Read_From stream so far

   function Read_Total_Out
     (Stream : Stream_Type) return Stream_Element_Count with Inline;
   --  Returns the total number of bytes read so far

   function Write_Total_In
     (Stream : Stream_Type) return Stream_Element_Count with Inline;
   --  Returns the total number of bytes written so far

   function Write_Total_Out
     (Stream : Stream_Type) return Stream_Element_Count with Inline;
   --  Returns the total number of bytes written to the Write_To stream

   procedure End_Of_Input (Stream : in out Stream_Type);
   --  Declares that input data is completed. Read routine is not going to
   --  read more data from Read_From stream.

   function End_Of_Input (Stream : Stream_Type) return Boolean;
   --  Returns True if data from Read_From stream is finished.

private

   package SEA_Holders is
     new Ada.Containers.Indefinite_Holders (Stream_Element_Array);

   type Stream_Type is new Root_Stream_Type with record
      Read_Coder  : access Coder_Interface'Class;
      Write_Coder : access Coder_Interface'Class;
      Read_From   : access Root_Stream_Type'Class;
      Write_To    : access Root_Stream_Type'Class;
      Read_Ends   : End_Of_Input_Method := Empty_Read;
      End_Of_Read : Boolean := False;

      Buffer      : SEA_Holders.Holder;
      Rest_First  : Stream_Element_Offset;
      Rest_Last   : Stream_Element_Offset;
      --  Buffer for Read operation.
      --  We need to have this buffer in the record because all read data
      --  from Read_From stream may not be processed during the read operation.

      Buffer_Size : Stream_Element_Offset;
      --  Buffer size for write operation.
      --  We do not need to have this buffer in the record because all data
      --  can be processed in the write operation.
   end record;

end GNATCOLL.Coders.Streams;
