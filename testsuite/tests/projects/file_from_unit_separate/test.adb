with Text_IO; use Text_IO;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Test_Assert;

function Test return Integer is
   PT : Project_Tree;
   U_Name : constant String := "Foo.Bar.Baz";
begin
   PT.Load (GNATCOLL.VFS.Create ("prj.gpr"));

   Test_Assert.Assert
     (+GNATCOLL.Projects.File_From_Unit
        (Project         => PT.Root_Project,
         Unit_Name       => U_Name,
         Part            => Unit_Separate,
         Language        => "Ada",
         File_Must_Exist => False),
      "foo.bar.baz.3.ada",
      "check file name for separate");

   return Test_Assert.Report;
end Test;
