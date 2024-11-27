with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Projects.Aux;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Test_Assert;

function Test return Integer is
   PT  : Project_Tree;
   Env : Project_Environment_Access;

   procedure Errors (S : String);
   procedure Errors (S : String) is
   begin
      Test_Assert.Assert
        (Index (S, "prj.gpr:3:09: unknown package ""custom""") > 0,
         "check that error message is emmitted and correct");
   end Errors;
begin
   Initialize (Env);
   PT.Load
     (GNATCOLL.VFS.Create ("prj.gpr"), Env,
      Errors => Errors'Unrestricted_Access);

   Test_Assert.Assert (False, "check that project is rejected");

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   Unload (PT);
   Free (Env);

   return Test_Assert.Report;

exception
   when GNATCOLL.Projects.Invalid_Project =>
      Test_Assert.Assert (True, "check that project is rejected");
      GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
      Unload (PT);
      Free (Env);
      return Test_Assert.Report;

end Test;
