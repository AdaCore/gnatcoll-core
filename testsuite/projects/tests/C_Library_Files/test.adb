with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Projects.Aux;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Test_Assert;

function Test return Integer is

   Env      : Project_Environment_Access;
   Prj_Tree : Project_Tree;
   LI_Infos : Library_Info_List;

begin
   Initialize (Env);
   Prj_Tree.Load (Create (+"my_test.gpr"), Env);
   Prj_Tree.Root_Project.Library_Files (List => LI_Infos, ALI_Ext => ".gli");

   --  Check that all <basename>.c.gli files are attributed to the coresponding
   --  <basename>.c file and not to <basename>.h.
   for LI of LI_Infos loop
      Test_Assert.Assert
        (+LI.Source.File.File_Extension, ".c", "Check file extension");
   end loop;

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (Prj_Tree.Root_Project);
   Unload (Prj_Tree);
   Free (Env);

   return Test_Assert.Report;
end Test;
