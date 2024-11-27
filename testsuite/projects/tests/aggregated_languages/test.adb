with GNAT.Strings;               use GNAT.Strings;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.Projects.Aux;
with Test_Assert;

function Test return Integer is
   Env : Project_Environment_Access;
   PT  : GNATCOLL.Projects.Project_Tree;

   procedure Check_Language (Lang_List : String_List; Name : String);
   --  Verify that Name is present in Lang_List

   --------------------
   -- Check_Language --
   --------------------

   procedure Check_Language (Lang_List : String_List; Name : String) is
   begin
      for L of Lang_List loop
         if L.all = Name then
            return;
         end if;
      end loop;
      Test_Assert.Assert
        (False, "Missing language in the language list: " & Name);
   end Check_Language;
begin
   Initialize (Env);
   PT.Load (GNATCOLL.VFS.Create (+"aggr.gpr"), Env);

   declare
      Langs : String_List := PT.Root_Project.Languages;
   begin
      Check_Language (Langs, "Ada");
      Check_Language (Langs, "C");
      Check_Language (Langs, "C++");
      for L of Langs loop
         Free (L);
      end loop;
   end;

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   PT.Unload;
   Free (Env);

   return Test_Assert.Report;

end Test;
