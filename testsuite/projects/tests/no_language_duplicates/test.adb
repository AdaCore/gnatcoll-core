with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects.Aux;
with Test_Assert;

with GNAT.Strings;      use GNAT.Strings;

function Test return Integer is
   PT    : Project_Tree;
   Env   : Project_Environment_Access;
   Langs : GNAT.Strings.String_List_Access;
begin
   Initialize (Env);

   GNATCOLL.Projects.Load
     (PT,
      Root_Project_Path => Create ("t2.gpr"),
      Env               => Env);

   Langs := new String_List'(PT.Root_Project.Languages (Recursive => True));
   Test_Assert.Assert (Langs'Length = 2, "there must be two languages");
   Test_Assert.Assert (Langs (1).all = "Ada", "check Ada casing");
   Test_Assert.Assert (Langs (2).all = "C", "check C casing");

   Free (Langs);
   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   Unload (PT);
   Free (Env);

   return Test_Assert.Report;

end Test;
