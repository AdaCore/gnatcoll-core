with GNATCOLL.Utils;
with Test_Assert;
with Ada.Environment_Variables;
with GNAT.OS_Lib;

function Test return Integer is
   package GU renames GNATCOLL.Utils;
   package A renames Test_Assert;
   package Env renames Ada.Environment_Variables;
   package OS renames GNAT.OS_Lib;

   Sep : constant Character := OS.Path_Separator;
begin
   Env.Clear ("GNATCOLL_PATH");
   GU.Add_Search_Path ("GNATCOLL_PATH", "/dummy");
   A.Assert (Env.Value ("GNATCOLL_PATH", ""), "/dummy");
   GU.Add_Search_Path ("GNATCOLL_PATH", "/dummy2");
   A.Assert (Env.Value ("GNATCOLL_PATH", ""),
             "/dummy2" & OS.Path_Separator & "/dummy");
   GU.Add_Search_Path ("GNATCOLL_PATH", "/dummy");
   A.Assert (Env.Value ("GNATCOLL_PATH", ""),
             "/dummy" & OS.Path_Separator & "/dummy2");
   Env.Set ("GNATCOLL_PATH", "." & Sep & Sep);
   GU.Add_Search_Path ("GNATCOLL_PATH", "/dummy");
   A.Assert (Env.Value ("GNATCOLL_PATH", ""),
             "/dummy" & OS.Path_Separator & "." & Sep & Sep);
   GU.Add_Search_Path ("GNATCOLL_PATH", "");
   A.Assert (Env.Value ("GNATCOLL_PATH", ""),
             "/dummy" & OS.Path_Separator & "." & Sep & Sep);
   return A.Report;
end Test;
