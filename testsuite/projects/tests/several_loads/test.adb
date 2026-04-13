with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Test_Assert;       use Test_Assert;

function Test return Integer is
   Env : Project_Environment_Access;
   PT  : GNATCOLL.Projects.Project_Tree;
begin
   Initialize (Env);
   PT.Load (GNATCOLL.VFS.Create (+"prj.gpr"), Env);
   Assert (PT.Root_Project.Name, "Prj", "Check that loading worked");
   PT.Unload;

   PT.Load (GNATCOLL.VFS.Create (+"prj.gpr"), Env);
   PT.Load (GNATCOLL.VFS.Create (+"prj.gpr"), Env);
   Assert (PT.Root_Project.Name, "Prj", "Check that loading worked");
   PT.Unload;

   Free (Env);

   return Test_Assert.Report;
end Test;
