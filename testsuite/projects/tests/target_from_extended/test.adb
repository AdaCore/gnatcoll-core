with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Projects.Aux;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Test_Assert;

function Test return Integer is
   PT  : Project_Tree;
   Env : Project_Environment_Access;

   procedure Err (S : String);

   procedure Err (S : String) is
   begin
      Test_Assert.Assert
        (S, "Could not locate exec powerpc-doesntexist-gnatls",
         "check that target from extended project is taken into account");
   end Err;
begin
   Initialize (Env);
   PT.Load
     (GNATCOLL.VFS.Create ("main.gpr"), Env,
      Errors => Err'Unrestricted_Access);

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   Unload (PT);
   Free (Env);

   return Test_Assert.Report;
end Test;
