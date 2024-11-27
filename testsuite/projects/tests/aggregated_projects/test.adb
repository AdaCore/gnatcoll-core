with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.Projects.Aux;
with Test_Assert;                use Test_Assert;

function Test return Integer is
   Env : Project_Environment_Access;
   PT  : GNATCOLL.Projects.Project_Tree;

   Aggregated : Project_Array_Access;
begin
   Initialize (Env);
   PT.Load (GNATCOLL.VFS.Create (+"aggr.gpr"), Env);

   Aggregated :=
     PT.Root_Project.Aggregated_Projects (Unwind_Aggregated => True);
   Test_Assert.Assert
     (Aggregated'Length = 4, "check list length Unwind_Aggregated = True");
   declare
      Files : File_Array (1 .. 4);
   begin
      for I in 1 .. 4 loop
         Files (I) := Aggregated (I).Project_Path;
      end loop;
      Sort (Files);

      Assert (Files (1).Display_Base_Name, "a.gpr", "check 1st project");
      Assert (Files (2).Display_Base_Name, "b.gpr", "check 2nd project");
      Assert (Files (3).Display_Base_Name, "d.gpr", "check 3rd project");
      Assert (Files (4).Display_Base_Name, "e.gpr", "check 4th project");
   end;
   Unchecked_Free (Aggregated);

   Aggregated :=
     PT.Root_Project.Aggregated_Projects (Unwind_Aggregated => False);
   Test_Assert.Assert
     (Aggregated'Length = 3, "check list length Unwind_Aggregated = False");
   declare
      Files : File_Array (1 .. 3);
   begin
      for I in 1 .. 3 loop
         Files (I) := Aggregated (I).Project_Path;
      end loop;
      Sort (Files);

      Assert (Files (1).Display_Base_Name, "a.gpr", "check 1st project");
      Assert (Files (2).Display_Base_Name, "b.gpr", "check 2nd project");
      Assert
        (Files (3).Display_Base_Name, "nested_aggr.gpr", "check 3rd project");
   end;
   Unchecked_Free (Aggregated);

   Aggregated :=
     PT.Project_From_Name ("c").Aggregated_Projects;
   Test_Assert.Assert
     (Aggregated.all = Empty_Project_Array,
      "check empty list for non aggregate prpject");
   Unchecked_Free (Aggregated);

   Test_Assert.Assert
     (PT.Project_From_Name ("abst").Is_Abstract_Project,
      "check that abstract project is abstract");

   Test_Assert.Assert
     (not PT.Project_From_Name ("c").Is_Abstract_Project,
      "check that not abstract project is not abstract");

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   PT.Unload;
   Free (Env);

   return Test_Assert.Report;

end Test;
