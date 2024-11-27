with GNATCOLL.Utils;
with Test_Assert;
with GNAT.OS_Lib;

function Test return Integer is
   package GU renames GNATCOLL.Utils;
   package A renames Test_Assert;

   Sep : constant Character := GNAT.OS_Lib.Directory_Separator;
begin
   A.Assert (GU.Join_Path ("/dummy", "lib"),
             "/dummy" & Sep & "lib");
   A.Assert (GU.Join_Path ("/dummy", "/foo", "bin"),
             "/foo" & Sep & "bin");
   A.Assert (GU.Join_Path ("/foo", "bar1", "bar2", "bar3", "bar4"),
             "/foo" & Sep & "bar1" & Sep & "bar2" & Sep &
             "bar3" & Sep & "bar4");
   return A.Report;
end Test;
