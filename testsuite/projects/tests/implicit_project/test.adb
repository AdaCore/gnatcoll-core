with GNATCOLL.Projects;  use GNATCOLL.Projects;
with GNATCOLL.VFS;       use GNATCOLL.VFS;
with GNATCOLL.Projects.Aux;
with Test_Assert;

function Test return Integer is
   PT      : Project_Tree;
   Env     : Project_Environment_Access;
   Sources : File_Array_Access;
begin
   Initialize (Env);

   GNATCOLL.Projects.Load_Implicit_Project (PT, Env);

   Sources  := PT.Root_Project.Source_Files;
   Test_Assert.Assert (Sources'Length = 1, "check that sources are present");
   Test_Assert.Assert
     (Sources (Sources.all'First).Display_Base_Name,
      "test.adb",
      "check source file name");
   Unchecked_Free (Sources);

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   Unload (PT);
   Free (Env);

   return Test_Assert.Report;

end Test;
