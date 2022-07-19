with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Projects.Aux;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Test_Assert;

with Ada.Environment_Variables;

function Test return Integer is
   PT : Project_Tree;
   Env : Project_Environment_Access;

   UV : Untyped_Variable;

   Flags : constant String := "-gnat05 -gnatwe";
begin

   Ada.Environment_Variables.Set ("ADAFLAGS", Flags);

   Initialize (Env);

   PT.Load (GNATCOLL.VFS.Create ("abst.gpr"), Env);

   UV := PT.Get_Untyped_Variable ("ADAFLAGS");
   Test_Assert.Assert
     (UV /= No_Untyped_Variable,
      "check that external_as_list variable is found");
   Test_Assert.Assert
     (Value (UV), Flags,
      "check value passed to external_as_list");

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   Unload (PT);
   Free (Env);

   return Test_Assert.Report;
end Test;
