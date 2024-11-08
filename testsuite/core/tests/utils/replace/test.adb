with GNATCOLL.Utils;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Test_Assert;

function Test return Integer is
   Blank : constant String := " ";
   Str   : Unbounded_String := To_Unbounded_String (Blank);
   Exp   : constant String := "Some";
begin
   GNATCOLL.Utils.Replace (Str, " ", Exp);
   Test_Assert.Assert
      (To_String (Str) = GNATCOLL.Utils.Replace (Blank, " ", Exp),
       "Replacing blank string");
   return Test_Assert.Report;
end Test;
