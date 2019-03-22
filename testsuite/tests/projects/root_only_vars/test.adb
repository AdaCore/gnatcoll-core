with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Projects.Aux;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Test_Assert;

function Test return Integer is
   PT : Project_Tree;
   Env : Project_Environment_Access;
begin
   Initialize (Env);
   PT.Load (GNATCOLL.VFS.Create ("a.gpr"), Env);

   declare
      SVs : constant Scenario_Variable_Array := PT.Scenario_Variables;
      UVs : constant Untyped_Variable_Array  := PT.Untyped_Variables;
   begin
      Test_Assert.Assert
        (SVs'Length = 2, "checking initial quantity of SVs");
      Test_Assert.Assert
        (UVs'Length = 2, "checking initial quantity of UVs");
   end;

   declare
      SVs : constant Scenario_Variable_Array := PT.Scenario_Variables (True);
      UVs : constant Untyped_Variable_Array  := PT.Untyped_Variables (True);
   begin
      Test_Assert.Assert
        (SVs'Length = 1, "checking root-only quantity of SVs");
      Test_Assert.Assert
        (UVs'Length = 1, "checking root-only quantity of UVs");
   end;

   declare
      SVs : constant Scenario_Variable_Array := PT.Scenario_Variables;
      UVs : constant Untyped_Variable_Array  := PT.Untyped_Variables;
   begin
      Test_Assert.Assert
        (SVs'Length = 2, "checking quantity of SVs after root-only call");
      Test_Assert.Assert
        (UVs'Length = 2, "checking quantity of UVs after root-only call");
   end;

   declare
      SV1, SV2, SV3, SV4 : Scenario_Variable;
      UV1, UV2, UV3, UV4 : Untyped_Variable;
   begin
      SV1 := Scenario_Variables (PT, "VAR_A1", Root_Only => False);
      SV2 := Scenario_Variables (PT, "VAR_A1", Root_Only => True);
      SV3 := Scenario_Variables (PT, "VAR_B1", Root_Only => False);
      SV4 := Scenario_Variables (PT, "VAR_B1", Root_Only => True);
      UV1 := Get_Untyped_Variable (PT, "VAR_A2", Root_Only => False);
      UV2 := Get_Untyped_Variable (PT, "VAR_A2", Root_Only => True);
      UV3 := Get_Untyped_Variable (PT, "VAR_B2", Root_Only => False);
      UV4 := Get_Untyped_Variable (PT, "VAR_B2", Root_Only => True);

      Test_Assert.Assert
        (SV1 /= No_Variable, "Root SV recursive check");
      Test_Assert.Assert
        (SV2 /= No_Variable, "Root SV non-recursive check");
      Test_Assert.Assert
        (SV3 /= No_Variable, "Non-root SV recursive ckeck");
      Test_Assert.Assert
        (SV4 = No_Variable, "Non-root SV non-recursive check");
      Test_Assert.Assert
        (UV1 /= No_Untyped_Variable, "Root UV recursive check");
      Test_Assert.Assert
        (UV2 /= No_Untyped_Variable, "Root UV non-recursive check");
      Test_Assert.Assert
        (UV3 /= No_Untyped_Variable, "Non-root UV recursive ckeck");
      Test_Assert.Assert
        (UV4 = No_Untyped_Variable, "Non-root UV non-recursive check");
   end;

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   Unload (PT);
   Free (Env);

   return Test_Assert.Report;
end Test;
