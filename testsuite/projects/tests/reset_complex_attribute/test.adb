with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;
with GNATCOLL.Projects.Aux;
with Test_Assert;

with Ada.Text_IO; use Ada.Text_IO;

function Test return Integer is
   PT    : Project_Tree;
   Env   : Project_Environment_Access;
   Dummy : Boolean;

   F : File_Type;

begin
   Initialize (Env);
   PT.Load (GNATCOLL.VFS.Create ("p.gpr"), Env, Report_Missing_Dirs => False);

   PT.Root_Project.Set_Attribute (Obj_Dir_Attribute, "Bye");
   PT.Root_Project.Set_Attribute (Exec_Dir_Attribute, "FooBar");
   PT.Root_Project.Set_Modified (True);
   PT.Recompute_View;
   Dummy := PT.Root_Project.Save (Force => True);

   if not Dummy then
      raise Program_Error;
   end if;

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   PT.Unload;
   Free (Env);

   Open (F, In_File, "p.gpr");
   Skip_Line (F, 2);
   Test_Assert.Assert
     (Get_Line (F),
      "   for Object_Dir use ""Bye"";",
      "checking first edited line");
   Test_Assert.Assert
     (Get_Line (F),
      "   for Exec_Dir use ""FooBar"";",
      "checking second edited line");
   Close (F);

   return Test_Assert.Report;
end Test;
