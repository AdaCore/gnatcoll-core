with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Projects.Aux;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Test_Assert;

function Test return Integer is
   PT : Project_Tree;
   Env : Project_Environment_Access;

   procedure Errors (S : String);

   procedure Errors (S : String) is
   begin
      Test_Assert.Assert (False, S);
   end Errors;
begin
   --  Createdirectories for native cases
   Make_Dir (Create ("src_x86-windows"));
   Make_Dir (Create ("src_x86_64-windows"));
   Make_Dir (Create ("src_x86-linux"));
   Make_Dir (Create ("src_x86_64-linux"));

   Initialize (Env);
   PT.Load
     (GNATCOLL.VFS.Create ("foo.gpr"),
      Env,
      Errors => Errors'Unrestricted_Access,
      Report_Missing_Dirs => True);

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   Unload (PT);
   Free (Env);

   return Test_Assert.Report;
end Test;
