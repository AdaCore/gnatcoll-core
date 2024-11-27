with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Test_Assert;

function Test return Integer is
   Env  : GNATCOLL.Projects.Project_Environment_Access;
   Tree : GNATCOLL.Projects.Project_Tree;
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
