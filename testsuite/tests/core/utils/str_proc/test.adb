with GNATCOLL.Utils;
with GNAT.Strings;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with System.Assertions;
with Test_Assert;
with Ada.Text_IO;

function Test return Integer is

   package A renames Test_Assert;
   package IO renames Ada.Text_IO;

   procedure Test_Replace;
   procedure Test_Capitalize;
   procedure Test_Join;

   procedure Test_Replace is
      S  : constant String :=
           "This adacore is adacore a adacore String";
      SS : constant String :=
           "This something is something a something String";
      S2 : Unbounded_String := To_Unbounded_String (S);
      S3 : constant String := "This is a String";
   begin

      -- Test Replace with String --
      IO.Put_Line ("Test Replace with Strings");
      declare
         S4 : constant String := GNATCOLL.Utils.Replace (S, "adacore ", "");
         S5 : constant String := GNATCOLL.Utils.Replace
            (S, "adacore", "something");
         S6 : constant String := GNATCOLL.Utils.Replace (S, "something",
                                                         "something_else");
      begin
         A.Assert (S4 = S3, Msg => "replace valid pattern into empty string");
         A.Assert (S5 = SS, Msg => "replace valid pattern");
         A.Assert (S6 = S, Msg => "replace 'absent' pattern into other word");

         declare
            pragma Warnings (Off);
            S7 : String (1 .. S'Length);
            pragma Warning (On);
         begin
            S7 := GNATCOLL.Utils.Replace (S, "", " something");
            A.Assert (False,
                    Msg => "replace failed raising exception or pre-cond");
         exception
            when System.Assertions.Assert_Failure =>
               A.Assert (True,
                         Msg => "replace raised assertion failure");

            when Ada.Strings.Pattern_Error =>
               A.Assert (True,
                         Msg => "replace raised exception (no assertions)");

         end;
      end;

      -- Test Replace with Unbounded String --
      IO.Put_Line ("Test Replace with Unbounded Strings");
      GNATCOLL.Utils.Replace (S2, "adacore ", "");
      A.Assert (To_String (S2) = S3,
                Msg => "replace valid pattern into empty string");

      GNATCOLL.Utils.Replace (S2, "something", "something_else");
      A.Assert (To_String (S2) = S3,
                Msg => "replace valid pattern");

      declare
      begin
         GNATCOLL.Utils.Replace (S2, "", " something");
         A.Assert (False,
                   Msg => "replace failed raising exception or pre-cond");
      exception
         when System.Assertions.Assert_Failure =>
            A.Assert (True,
                      Msg => "replace raised assertion failure");
         when Ada.Strings.Pattern_Error =>
            A.Assert (True,
                      Msg => "replace raised exception (no assertions)");

      end;

   end Test_Replace;

   procedure Test_Capitalize is
      S1 : constant String := "abcdefg";
      S2 : constant String := To_Upper (S1);
      S3 : constant String := "Abcdefg";
      Empty : constant String := "";
   begin
      IO.Put_Line ("Test Capitalize");
      A.Assert (GNATCOLL.Utils.Capitalize (Empty) = "",
           Msg => "empty string");
      A.Assert (GNATCOLL.Utils.Capitalize (S1) = S3,
           Msg => "capitalize from all lower");
      A.Assert (GNATCOLL.Utils.Capitalize (S2) = S3,
           Msg => "capitalize from all upper");
      A.Assert (GNATCOLL.Utils.Capitalize ("abCD__ef___GHi_j_k__l")
           = "Abcd_Ef_Ghi_J_K_L",
           Msg => "capitalize when several '_'");
      A.Assert (GNATCOLL.Utils.Capitalize ("abCD/ef//GHi/j/k////l")
           = "Abcd_Ef__Ghi_J_K____L",
           Msg => "capitalize when several '/'");
      A.Assert (GNATCOLL.Utils.Capitalize ("c++abcd") = "Cppabcd",
           Msg => "capitalize with '+'");
      A.Assert (GNATCOLL.Utils.Capitalize ("c--abcd") = "C__Abcd",
           Msg => "capitalize with '-'");
      A.Assert (GNATCOLL.Utils.Capitalize ("abc'def''ghijkl'''mno")
           = "Abc_Def__Ghijkl___Mno",
           Msg => "capitalize with ");
      A.Assert (GNATCOLL.Utils.Capitalize ("\abc?def?\?g\hijkl???mno")
           = "\abcUdefU\Ug\hijklUUUmno",
           Msg => "capitalize with '?' and '\'");

   end Test_Capitalize;

   procedure Test_Join is
      S1 : constant String := ",";
      S2 : aliased String := "adacore";
      S  : constant String := S2 & S1 & S2 & S1 & S2 & S1 & S2 & S1 & S2;
      SL : GNAT.Strings.String_List (1 .. 5) :=
         (others => S2'Unchecked_Access);
   begin
      A.Assert (GNATCOLL.Utils.Join (S1, SL) = S,
                Msg => "simple join");

   end Test_Join;

begin
   IO.New_Line;
   IO.Put_Line ("GNATCOLL.UTILS");
   IO.New_Line;
   Test_Replace;
   IO.New_Line;
   Test_Capitalize;
   IO.New_Line;
   Test_Join;

   return A.Report;
end Test;
