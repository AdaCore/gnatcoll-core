with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects.Aux;
with Test_Assert;

function Test return Integer is
   PT  : Project_Tree;
   Env : Project_Environment_Access;
begin
   Initialize (Env);
   PT.Load (GNATCOLL.VFS.Create ("p.gpr"), Env);

   Test_Assert.Assert
     (String (Build_Tree_Dir (Env.all)), "",
      "Wrong Build_Tree_Dir after load");
   Test_Assert.Assert
     (String (Root_Dir (Env.all)), "",
      "Wrong Root_Dir after load");

   Set_Build_Tree_Dir (Env.all, "..");

   Test_Assert.Assert
     (String (Build_Tree_Dir (Env.all)), "..",
      "Wrong Build_Tree_Dir after Set_Build_Tree_Dir");
   Test_Assert.Assert
     (String (Root_Dir (Env.all)), "",
      "Wrong Root_Dir after Set_Build_Tree_Dir");

   Set_Root_Dir (Env.all, "..");

   Test_Assert.Assert
     (String (Build_Tree_Dir (Env.all)), "..",
      "Wrong Build_Tree_Dir after Set_Root_Dir");
   Test_Assert.Assert
     (String (Root_Dir (Env.all)), "..",
      "Wrong Root_Dir after Set_Root_Dir");

   Set_Build_Tree_Dir (Env.all, "");
   Set_Root_Dir (Env.all, "");

   Test_Assert.Assert
     (String (Build_Tree_Dir (Env.all)), "",
      "Wrong Build_Tree_Dir after setting to default");
   Test_Assert.Assert
     (String (Root_Dir (Env.all)), "",
      "Wrong Root_Dir after setting to default");

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   PT.Unload;
   Free (Env);

   return Test_Assert.Report;
end Test;
