with GNATCOLL.Projects;       use GNATCOLL.Projects;
with GNATCOLL.Projects.Aux;
with GNATCOLL.VFS;            use GNATCOLL.VFS;
with Test_Assert;

with GNAT.Strings;            use GNAT.Strings;
with Ada.Characters.Handling; use Ada.Characters.Handling;

function Test return Integer is
   PT  : Project_Tree;
   Env : Project_Environment_Access;

   Ada_Present : Boolean := False;
   C_Present   : Boolean := False;
begin
   Initialize (Env);
   PT.Load (GNATCOLL.VFS.Create ("my_test.gpr"), Env);

   declare
      Langs : String_List := PT.Root_Project.Languages (True);
   begin
      for Lang of Langs loop
         if To_Lower (Lang.all) = "ada" then
            Ada_Present := True;
         elsif To_Lower (Lang.all) = "c" then
            C_Present := True;
         end if;
         Free (Lang);
      end loop;
   end;

   Test_Assert.Assert (C_Present, "check C");
   Test_Assert.Assert (Ada_Present, "check Ada");

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   Unload (PT);
   Free (Env);

   return Test_Assert.Report;
end Test;
