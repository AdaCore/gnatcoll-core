with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Projects.Aux;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Test_Assert;

function Test return Integer is
   PT : Project_Tree;
   Env : Project_Environment_Access;

   Files : File_Array_Access;
begin
   Initialize (Env);
   PT.Load (GNATCOLL.VFS.Create ("test_project.gpr"), Env);

   Files := PT.Root_Project.Source_Files;

   Test_Assert.Assert
     (Files.all'Length = 2,
      "checking that source dependant on Canonocal_Target is added");

   Unchecked_Free (Files);

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   Unload (PT);
   Free (Env);

   return Test_Assert.Report;
end Test;
