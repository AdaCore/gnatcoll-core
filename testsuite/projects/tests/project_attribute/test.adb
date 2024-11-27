with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Test_Assert;

function Test return Integer is
   PT  : Project_Tree;
   Env : Project_Environment_Access;
begin
   Initialize (Env);
   Env.Set_Target_And_Runtime (Target => "x86_64-pc-linux-gnu");

   GNATCOLL.Projects.Load
     (PT,
      Root_Project_Path => Create ("main_x86.gpr"),
      Env => Env);

   --  Check that the object directory name is as expected

   Test_Assert.Assert
     (PT.Root_Project.Object_Dir.Base_Dir_Name = "obj-x86");

   PT.Unload;
   Free (Env);
   return Test_Assert.Report;
end Test;
