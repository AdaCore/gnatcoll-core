with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Projects.Aux;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Test_Assert;

with Ada.Containers.Indefinite_Ordered_Sets;

function Test return Integer is
   PT : Project_Tree;
   Env : Project_Environment_Access;

   Files : File_Array_Access;

   package Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);
   use Sets;

   Set : Sets.Set;
begin
   Initialize (Env);
   PT.Load (GNATCOLL.VFS.Create ("p.gpr"), Env);

   Files := PT.Root_Project.Source_Files (Recursive => True);

   for F of Files.all loop
      if Set.Contains (F.Display_Base_Name) then
         Test_Assert.Assert
           (False, "duplicating source: " & F.Display_Base_Name);
      else
         Set.Insert (F.Display_Base_Name);
      end if;
   end loop;

   Unchecked_Free (Files);

   Set.Clear;
   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   Unload (PT);
   Free (Env);

   return Test_Assert.Report;
end Test;
