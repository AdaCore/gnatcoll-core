with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Text_IO;
with GNATCOLL.JSON;
with GNAT.OS_Lib;

function Test return Integer is

   package JSON renames GNATCOLL.JSON;
   package IO renames Ada.Text_IO;

   function Read_File (Filename : String) return Unbounded_String;

   function Read_File (Filename : String) return Unbounded_String is
      use GNAT.OS_Lib;
      F : constant File_Descriptor := Open_Read (Filename, Binary);
      Expected_Bytes_Read : Integer;
      Bytes_Read : Integer;
   begin
      Expected_Bytes_Read := Integer (File_Length (F));

      declare
         Buffer_Str : aliased String (1 .. Expected_Bytes_Read);
      begin
         Bytes_Read := Read (F, Buffer_Str'Address, Expected_Bytes_Read);
         pragma Assert (Bytes_Read = Expected_Bytes_Read);
         Close (F);

         return To_Unbounded_String (Buffer_Str);
      end;
   end Read_File;

   --  Read json filename passed as first argument
   Filename  : constant String := Ada.Command_Line.Argument (1);
   JSON_Data : constant Unbounded_String := Read_File (Filename);
begin

   --  Parse the json
   declare
      Value : constant JSON.JSON_Value := JSON.Read
         (Strm     => JSON_Data, Filename => Filename);
   begin
      --  Dump JSON back to stdout for validation by the python json
      --  parser
      declare
         New_JSON_Data : constant Unbounded_String :=
           JSON.Write (Item => Value, Compact => False);
      begin
         IO.Put_Line (To_String (New_JSON_Data));
      end;
   end;
   return 0;
end Test;
