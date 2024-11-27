package body Test_Streams is

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      Length : constant Integer :=
        Natural'Min (Item'Length, Stream.Buffer.Length - Stream.Position);
      Target : String (1 .. Integer'Max (Length, 0));
      for Target'Address use Item'Address;
   begin
      if Target = "" then
         Last := Item'First - 1;
         return;
      end if;

      Target := To_String
        (Stream.Buffer.Slice
           (Stream.Position + 1, Stream.Position + Length));
      Stream.Position := Stream.Position + Length;
      Last := Item'First + Stream_Element_Offset (Length) - 1;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Stream_Type;
      Item   :        Stream_Element_Array)
   is
      Source : String (1 .. Item'Length);
      for Source'Address use Item'Address;
   begin
      Stream.Buffer.Append (Source);
   end Write;

   -----------
   -- Clear --
   -----------

   procedure Clear (Stream : in out Stream_Type) is
   begin
      Stream.Buffer.Clear;
      Stream.Reset;
   end Clear;

   -----------
   -- Reset --
   -----------

   procedure Reset (Stream : in out Stream_Type) is
   begin
      Stream.Position := 0;
   end Reset;

   -----------
   -- Slice --
   -----------

   function Slice
     (Stream : Stream_Type; Low : Positive; High : Natural) return String is
   begin
      return To_String (Stream.Buffer.Slice (Low, High));
   end Slice;

end Test_Streams;
