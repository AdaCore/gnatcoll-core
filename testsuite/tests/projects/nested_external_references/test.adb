with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Test_Assert;

with Ada.Containers.Indefinite_Ordered_Maps;

function Test return Integer is
   PT : Project_Tree;
   Env : Project_Environment_Access;

   package SV_Sets is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Scenario_Variable);
   use SV_Sets;

   SV_Map : SV_Sets.Map;
begin
   Initialize (Env);

   Env.Change_Environment ("A0", "a");
   Env.Change_Environment ("B0", "b");
   Env.Change_Environment ("C0", "c");

   Env.Change_Environment ("NO_DEF", "no_def");

   PT.Load (GNATCOLL.VFS.Create ("a.gpr"), Env);

   declare
      SVs : constant Scenario_Variable_Array := PT.Scenario_Variables;
      UVs : constant Untyped_Variable_Array  := PT.Untyped_Variables;
      SV  : Scenario_Variable;
   begin
      for SV of SVs loop
         Test_Assert.Assert
           (not SV_Map.Contains (External_Name (SV)),
            "Check no duplication of SV " & External_Name (SV));
         SV_Map.Include (External_Name (SV), SV);
      end loop;

      --  Check simple case nesting.
      Test_Assert.Assert
        (SV_Map.Contains ("A0"), "Check presense of A0");
      Test_Assert.Assert
        (SV_Map.Contains ("B0"), "Check presense of B0");
      Test_Assert.Assert
        (SV_Map.Contains ("C0"), "Check presense of C0");
      Test_Assert.Assert
        (SV_Map.Contains ("D0"), "Check presense of D0");

      SV := SV_Map.Element ("A0");
      Test_Assert.Assert
        (External_Default (SV), "b", "Check default of A0");
      Test_Assert.Assert (Value (SV), "a", "Check value of A0");

      SV := SV_Map.Element ("B0");
      Test_Assert.Assert
        (External_Default (SV), "c", "Check default of B0");
      Test_Assert.Assert (Value (SV), "b", "Check value of B0");

      SV := SV_Map.Element ("C0");
      Test_Assert.Assert
        (External_Default (SV), "d", "Check default of C0");
      Test_Assert.Assert (Value (SV), "c", "Check value of C0");

      SV := SV_Map.Element ("D0");
      Test_Assert.Assert
        (External_Default (SV), "d", "Check default of D0");
      Test_Assert.Assert (Value (SV), "d", "Check value of D0");

      --  Check that duplicating ones before or after do not break
      --  the unwinding of nested external references.
      Test_Assert.Assert
        (SV_Map.Contains ("A1"), "Check presense of A1");
      Test_Assert.Assert
        (SV_Map.Contains ("B1"), "Check presense of B1");
      Test_Assert.Assert
        (SV_Map.Contains ("C1"), "Check presense of C1");
      Test_Assert.Assert
        (SV_Map.Contains ("D1"), "Check presense of D1");
      Test_Assert.Assert
        (SV_Map.Contains ("A2"), "Check presense of A2");
      Test_Assert.Assert
        (SV_Map.Contains ("B2"), "Check presense of B2");
      Test_Assert.Assert
        (SV_Map.Contains ("C2"), "Check presense of C2");
      Test_Assert.Assert
        (SV_Map.Contains ("D2"), "Check presense of D2");

      --  Check transition of duplicating SV with non-matching types into a UV
      --  and that it does not prevent unwinding further.
      Test_Assert.Assert
        (not SV_Map.Contains ("B3"), "Check absence of B3");
      Test_Assert.Assert
        (SV_Map.Contains ("A3"), "Check presense of A3");
      Test_Assert.Assert
        (SV_Map.Contains ("C3"), "Check presense of C3");
      Test_Assert.Assert
        (SV_Map.Contains ("D3"), "Check presense of D3");
      Test_Assert.Assert
        (UVs'Length > 0 and then External_Name (UVs (UVs'First)) = "B3",
         "Check transition of B3 from SVs to UVs");

      --  Check that non-canonical nesting does not prevent collecting SVs
      --  declared prior to unexpected construct.
      Test_Assert.Assert
        (SV_Map.Contains ("A4"), "Check presense of A4");
      Test_Assert.Assert
        (SV_Map.Contains ("B4"), "Check presense of B4");

      SV_Map.Clear;
   end;

   PT.Unload;
   Free (Env);

   return Test_Assert.Report;
end Test;
