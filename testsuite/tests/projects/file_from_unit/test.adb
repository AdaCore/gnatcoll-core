with Gnat.Strings;      use Gnat.Strings;
with Gnatcoll.Projects; use Gnatcoll.Projects;
with Gnatcoll.VFS;      use Gnatcoll.VFS;

with Test_Assert;

function Test return Integer is
   Env  : Gnatcoll.Projects.Project_Environment_Access;
   Tree : Gnatcoll.Projects.Project_Tree;
begin
   Initialize (Env);
   Load (Tree, Env => Env, Root_Project_Path => Create (+"test1.gpr"));

   declare
      File_Name : constant String :=
        +File_From_Unit (Project   => Root_Project (Tree),
                        Unit_Name       => "PrOc",
                        Part            => Unit_Body,
                        Language        => "ada",
                        File_Must_Exist => False);
   begin
      Test_Assert.Assert
        (File_Name, "my__proc.txt", "Wrong file name for unit Proc");
   end;

   Tree.Unload;
   Free (Env);
   return Test_Assert.Report;
end Test;
