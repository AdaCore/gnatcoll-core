with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Projects.Aux;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Test_Assert;

function Test return Integer is
   PT : Project_Tree;
   Env : Project_Environment_Access;
begin

   Initialize (Env);
   PT.Load (GNATCOLL.VFS.Create ("r.gpr"), Env);

   Test_Assert.Assert
     (PT.Root_Project.Is_Main_File ("main.ada2"), "check Ada main");
   Test_Assert.Assert
     (PT.Root_Project.Is_Main_File ("main2.c2"), "check C main");

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   Unload (PT);
   Free (Env);
   return Test_Assert.Report;

end Test;
