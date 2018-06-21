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

   procedure Test_Replace is
      S  : String :=
           "This adacore is adacore a adacore String";
      SS  : String :=
           "This something is something a something String";
      S2 : Unbounded_String := To_Unbounded_String (S);
      S3 : constant String := "This is a String";
   begin

      -- Test Replace with String --
       IO.Put_Line ("Test Replace with Strings");
      declare
         S4 : String := GNATCOLL.Utils.Replace (S, "adacore ", "");
         S5 : String := GNATCOLL.Utils.Replace (S, "adacore", "something");
         S6 : String := GNATCOLL.Utils.Replace (S, "something",
                                                   "something_else");
      begin
          A.Assert (S4 = S3, Msg => "replace valid pattern into empty string");
          A.Assert (S5 = SS, Msg => "replace valid pattern");
          A.Assert (S6 = S, Msg => "replace 'absent' pattern into other word");

          declare
              S7 : String (1 .. S'Length);
          begin
              S7 := GNATCOLL.Utils.Replace (S, "", " something");
              A.Assert (False,
                        Msg => "replace failed raising exception of pre-cond");
          exception
              when SYSTEM.ASSERTIONS.ASSERT_FAILURE =>
                   A.Assert (True,
                             Msg => "replace raising exception of pre-cond");
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
                    Msg => "replace failed raising exception of pre-cond");
      exception
          when SYSTEM.ASSERTIONS.ASSERT_FAILURE =>
               A.Assert (True,
                         Msg => "replace raising exception of pre-cond");
      end;


   end Test_Replace;

   procedure Test_Capitalize is
       S1 : constant String := "abcdefg";
       S2 : constant String := To_Upper(S1);
       S3 : constant String := "Abcdefg";
       Empty : constant String := "";

       S4 : constant String := "ab_cd++e+fg_____defG\H\\I?jk???lm/noPQ_rStuv w xyZ'abc-def=gh";
       S5 : constant String := "Ab_Cdppepfg_Defg\h\\iUjkUUUlm_Nopq_Rstuv_W_Xyz_Abc_Def=gh";

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

begin
   IO.New_Line;
   IO.Put_Line ("GNATCOLL.UTILS");
   IO.New_Line;
   Test_Replace;
   IO.New_Line;
   Test_Capitalize;

   return A.Report;
end Test;
