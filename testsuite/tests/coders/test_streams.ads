with Ada.Streams;      use Ada.Streams;
with GNATCOLL.Strings; use GNATCOLL.Strings;

package Test_Streams is

   type Stream_Type is new Root_Stream_Type with private;
   --  Stream reading the data which was wrote there before

   overriding procedure Read
     (Stream : in out Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Stream_Type;
      Item   :        Stream_Element_Array);

   procedure Reset (Stream : in out Stream_Type);
   --  Reset read position to the start of data

   procedure Clear (Stream : in out Stream_Type);
   --  Clear all internal written data from stream

   function Slice
     (Stream : Stream_Type; Low : Positive; High : Natural) return String;

private

   type Stream_Type is new Root_Stream_Type with record
      Position : Natural := 0;
      Buffer   : XString;
   end record;

end Test_Streams;
