with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects.Aux;
with Test_Assert;

function Test return Integer is
   PT      : Project_Tree;
   Env     : Project_Environment_Access;
   Sources : File_Array_Access;
begin
   Initialize (Env);

   GNATCOLL.Projects.Load
     (PT,
      Root_Project_Path => Create ("foo.gpr"),
      Env               => Env);

   Sources := PT.Root_Project.Source_Files (Recursive => True);
   Test_Assert.Assert
     (Sources'Length = 2,
      "check ammount of sources");
   Test_Assert.Assert
     (Sources (1).Display_Full_Name < Sources (2).Display_Full_Name,
      "check correct order of sources");
   Unchecked_Free (Sources);

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   Unload (PT);
   Free (Env);

   return Test_Assert.Report;

end Test;
