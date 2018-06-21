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


   procedure Test_Equality is
       S1 : constant String := "abcdefg";
       S2 : constant String := To_Upper(S1);
       S3 : constant String := "hijklmn";
       S4 : constant String := S1 & S3;
       Empty : constant String := "";
   begin

       for CS in Boolean loop
            IO.Put_Line ("Test Equal with case sensitity set to: " & CS'Image);
            A.Assert (GNATCOLL.Utils.Equal (Empty, "", Case_Sensitive => CS)
                      = True,
                      Msg => "empty string");
            A.Assert (GNATCOLL.Utils.Equal (S1, S1, Case_Sensitive => CS)
                      = True,
                      Msg => "exact same string");
            A.Assert (GNATCOLL.Utils.Equal (S1, S2, Case_Sensitive => CS)
                      = not CS,
                      Msg => "compare to To_Upper copy");
            A.Assert (GNATCOLL.Utils.Equal (S1, S3, Case_Sensitive => CS)
                      = False,
                      Msg => "differnet strings");
            A.Assert (GNATCOLL.Utils.Equal (S4, S1, Case_Sensitive => CS)
                      = False,
                      Msg => "S4 compare to its substring S1");
       end loop;

       IO.Put_Line ("Test Case_Insensitive_Equal");
       A.Assert (GNATCOLL.Utils.Case_Insensitive_Equal (S1, S1) = True,
                 Msg => "exact same string");
       A.Assert (GNATCOLL.Utils.Case_Insensitive_Equal (S1, S2) = True,
                 Msg => "compare to To_Upper copy");
       A.Assert (GNATCOLL.Utils.Case_Insensitive_Equal (S1, S3) = False,
                 Msg => "different string");

   end Test_Equality;

   procedure Test_Is_Whitespace is

       subtype Whitespace_Type is Character with
         Static_Predicate => Whitespace_Type in ' ' | LF | CR;
   begin
       IO.Put_Line ("Test Is_Whitespace");

       for Car in Whitespace_Type loop
           A.Assert (GNATCOLL.Utils.Is_Whitespace (Car) = True,
                     Msg => Character'Image(Car) & " is whitespace");
       end loop;

       A.Assert (GNATCOLL.Utils.Is_Whitespace ('a') = False,
                 Msg => "is whitespace false");
       A.Assert (GNATCOLL.Utils.Is_Whitespace (NUL) = False,
                 Msg => "is whitespace empty");
   end Test_Is_Whitespace;

   procedure Test_Starts_With is
   begin
       IO.Put_Line ("Test Starts_With");
       A.Assert (GNATCOLL.Utils.Starts_With ("ABCD", "ABC") = True,
                 Msg => "starts_with true");
       A.Assert (GNATCOLL.Utils.Starts_With ("ABCD", "") = True,
                 Msg => "starts_with empty => always true");

       A.Assert (GNATCOLL.Utils.Starts_With ("ABCD", "BCD") = False,
                 Msg => "starts_with false");
       A.Assert (GNATCOLL.Utils.Starts_With ("ABCD", "BCDEFGH") = False,
                 Msg => "starts_with false : pattern too long");
   end Test_Starts_With;

   procedure Test_Ends_With is
   begin
       IO.Put_Line ("Test Ends_With");
       A.Assert (GNATCOLL.Utils.Ends_With ("ABCD", "BCD") = True,
                 Msg => "ends_with true");
       A.Assert (GNATCOLL.Utils.Ends_With ("ABCD", "") = True,
                 Msg => "ends_with empty => always true");

       A.Assert (GNATCOLL.Utils.Ends_With ("ABCD", "ABC") = False,
                 Msg => "ends_with false");
       A.Assert (GNATCOLL.Utils.Ends_With ("ABCD", "ABCDEFG") = False,
                 Msg => "ends_with false : pattern too long");
   end Test_Ends_With;

begin
   IO.New_Line;
   Test_Equality;
   IO.New_Line;
   Test_Is_Whitespace;
   IO.New_Line;
   Test_Starts_With;
   IO.New_Line;
   Test_Ends_With;

   return A.Report;

end Test;
