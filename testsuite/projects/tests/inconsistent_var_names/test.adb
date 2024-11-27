with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Test_Assert;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Ada.Containers.Indefinite_Ordered_Sets;

function Test return Integer is
   PT : Project_Tree;
   Env : Project_Environment_Access;

   procedure Errors (S : String);
   procedure Errors (S : String) is
   begin
      Test_Assert.Assert
        (Index (S, "VAR1") = 0
         and Index (S, "VAR2") = 0
         and Index (S, "VAR1_2") = 0,
         "check that SVs are not reported in ");
   end Errors;

   package UV_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);
   use UV_Sets;

   UV_Set : UV_Sets.Set;
begin
   Initialize (Env);

   PT.Load (GNATCOLL.VFS.Create ("p.gpr"), Env,
           Errors => Errors'Unrestricted_Access);

   declare
      UVs : constant Untyped_Variable_Array  := PT.Untyped_Variables;
   begin
      for UV of UVs loop
         Test_Assert.Assert
           (not UV_Set.Contains (External_Name (UV)),
            "check no duplication of UV " & External_Name (UV));
         UV_Set.Include (External_Name (UV));
      end loop;
   end;

   Test_Assert.Assert
     (UV_Set.Contains ("NESTED1"), "Check presense of NESTED1");
   Test_Assert.Assert
     (UV_Set.Contains ("NESTED2"), "Check presense of NESTED2");

   Test_Assert.Assert
     (not UV_Set.Contains ("VAR1"), "Check absence of VAR1");
   Test_Assert.Assert
     (not UV_Set.Contains ("VAR2"), "Check absence of VAR2");
   Test_Assert.Assert
     (not UV_Set.Contains ("VAR1_2"), "Check absence of VAR1_2");

   UV_Set.Clear;

   PT.Unload;
   Free (Env);

   return Test_Assert.Report;
end Test;
