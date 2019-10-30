with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Projects.Aux;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Test_Assert;

with Ada.Containers.Indefinite_Ordered_Multisets;
with Ada.Containers; use Ada.Containers;

function Test return Integer is
   PT : Project_Tree;
   Env : Project_Environment_Access;

   Files : File_Array_Access;

   package Sets is new Ada.Containers.Indefinite_Ordered_Multisets (String);
   use Sets;

   Set : Sets.Set;
begin
   Make_Dir (Create ("obj_p"));
   Make_Dir (Create ("obj_q"));
   Make_Dir (Create ("obj_r"));

   Initialize (Env);
   PT.Load (GNATCOLL.VFS.Create ("p.gpr"), Env);

   Files := PT.Root_Project.Extended_Projects_Source_Files;

   for F of Files.all loop
      Set.Insert (F.Display_Base_Name);
   end loop;
   Unchecked_Free (Files);

   Test_Assert.Assert (Set.Length = 3, "check ammount of sources");
   Test_Assert.Assert (Set.Contains ("a.ads"), "check source a.ads");
   Test_Assert.Assert (Set.Contains ("b.ads"), "check source b.ads");
   Test_Assert.Assert (Set.Contains ("c.ads"), "check source c.ads");

   Set.Clear;
   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   Unload (PT);
   Free (Env);

   return Test_Assert.Report;
end Test;
