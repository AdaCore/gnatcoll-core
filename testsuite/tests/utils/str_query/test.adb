with GNATCOLL.Utils;
with GNAT.Strings;              use GNAT.Strings;

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Characters.Latin_1;    use Ada.Characters.Latin_1;

with System.Assertions;

with Test_Assert;

with Ada.Text_IO;


function Test return Integer is

   package A renames Test_Assert;
   package IO renames Ada.Text_IO;

   procedure Test_Image is
       S1   : constant String := "005";
       S2   : constant String := '+' & S1;
       S3   : constant String := "-00000" & S1;
       Int1 : constant Integer := 5;
       Int2 : constant Integer := -5;

       S4   : constant String := "-    455";
       Int3 : constant Integer := -455;

   begin
       IO.Put_Line ("Test Image");
       A.Assert (GNATCOLL.Utils.Image (Int1,
                                       Int1'Image'Length,
                                       Padding => ' ') = Int1'Image,
                 Msg => "positive, mimic attribute Image");
       A.Assert (GNATCOLL.Utils.Image (Int1, S1'Length) = S1,
                 Msg => "positive, several 0 (default padding)");
       A.Assert (GNATCOLL.Utils.Image (Int1,
                                       S1'Length,
                                       Force_Sign => True) = S2,
                 Msg => "positive, several 0, force sign");
       A.Assert (GNATCOLL.Utils.Image (Int2,
                                       S3'Length -1) = S3,
                 Msg => "negative, several 0");
       A.Assert (GNATCOLL.Utils.Image (Int3,
                                       S4'Length -1,
                                       Padding => ' ',
                                       Force_Sign => True) = S4,
                 Msg => "negative, several ' ', force sign");

   end Test_Image;

   procedure Test_Find_Char is
       Str  : String (1 .. Character'Pos(Character'Last) + 1);
       Str2 : constant string := "AAAAAAAAAAAAAAAAAAA";
   begin
       for Char in Character loop
           Str(Character'Pos(Char) + 1) := Char;
       end loop;

       for Char in Character loop
            A.Assert (GNATCOLL.Utils.Find_Char (Str, Char) =
                      Character'Pos(Char) + 1,
                      Msg => "find all char in in all char string");
       end loop;

       A.Assert (GNATCOLL.Utils.Find_Char (Str2, 'B') = Str2'Last + 1,
                 Msg => "not found");

       A.Assert (GNATCOLL.Utils.Find_Char (Str2, 'A') = Str2'First,
                 Msg => "several occurence => take first");


   end Test_Find_Char;

begin
   IO.New_Line;
   Test_Image;
   IO.New_Line;
   Test_Find_Char;

   return A.Report;

end Test;
