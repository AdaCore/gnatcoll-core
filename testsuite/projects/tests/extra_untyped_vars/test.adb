with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Projects.Aux;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Test_Assert;

function Test return Integer is
   PT : Project_Tree;
   Env : Project_Environment_Access;

   UV : Untyped_Variable;
begin

   Initialize (Env);
   Env.Change_Environment ("objects", "obj");
   PT.Load (GNATCOLL.VFS.Create ("p.gpr"), Env, Report_Missing_Dirs => False);

   UV := PT.Get_Untyped_Variable ("sources");
   Test_Assert.Assert
     (UV /= No_Untyped_Variable,
     "check 'sources' variable is obtainable");
   Test_Assert.Assert
     (Value (UV), ".",
      "check 'sources' variable value (left as is)");

   UV := PT.Get_Untyped_Variable ("objects");
   Test_Assert.Assert
     (UV /= No_Untyped_Variable,
     "check 'objects' variable is obtainable");
   Test_Assert.Assert
     (Value (UV), "obj",
      "check 'objects' variable value (overwritten in environment)");

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   Unload (PT);
   Free (Env);

   return Test_Assert.Report;
end Test;
