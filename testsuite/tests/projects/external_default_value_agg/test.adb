with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects.Aux;
with Test_Assert;

function Test return Integer is
   PT : Project_Tree;
   Env : Project_Environment_Access;
   UV : Untyped_Variable;
begin
   Initialize (Env);
   PT.Load (GNATCOLL.VFS.Create ("agg.gpr"), Env);

   UV := PT.Get_Untyped_Variable ("VAL");
   Test_Assert.Assert
     (UV /= No_Untyped_Variable, "check that variable is detected");
   Test_Assert.Assert
     (External_Default (UV), "val", "check external default value");

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   PT.Unload;
   Free (Env);

   return Test_Assert.Report;
end Test;
