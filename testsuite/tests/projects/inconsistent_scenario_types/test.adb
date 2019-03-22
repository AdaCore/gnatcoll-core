with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects.Aux;

with Test_Assert;

function Test return Integer is
   Env  : GNATCOLL.Projects.Project_Environment_Access;
   Tree : GNATCOLL.Projects.Project_Tree;

begin
   Initialize (Env);
   Load (Tree, Root_Project_Path => Create (+"p.gpr"),
         Env    => Env);

   declare
      SVs : constant Scenario_Variable_Array := Tree.Scenario_Variables;
      UVs : constant Untyped_Variable_Array  := Tree.Untyped_Variables;
   begin
      Test_Assert.Assert
        (SVs'Length = 0, "check Scenario Variables amount");
      Test_Assert.Assert
        (UVs'Length = 2, "check Untyped Variables amount");
   end;

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (Tree.Root_Project);

   Tree.Unload;
   Free (Env);
   return Test_Assert.Report;

end Test;
