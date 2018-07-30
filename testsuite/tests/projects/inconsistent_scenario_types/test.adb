with Gnatcoll.Projects; use GNATCOLL.Projects;
with Gnatcoll.VFS; use GNATCOLL.VFS;
with GNATCOLL.Projects.Aux;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Test_Assert;

function Test return Integer is
   Tree : Gnatcoll.Projects.Project_Tree;

   package String_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (String);
   use String_Lists;

   Lines : String_Lists.List;
   Cur   : String_Lists.Cursor;

   procedure Errors (S : String);
   procedure Errors (S : String) is
   begin
      Lines.Append (S);
   end Errors;

begin
   Load (Tree, Root_Project_Path => Create (+"p.gpr"),
         Errors => Errors'Unrestricted_Access);

   Cur := Lines.First;
   Test_Assert.Assert
     (Index
        (Element (Cur),
         "Ext2 have different sets of possible values") /= 0,
      "check first warning line");
   Next (Cur);
   Test_Assert.Assert
     (Index
        (Element (Cur),
         "Ext1 have different sets of possible values") /= 0,
      "check second warning line");
   Next (Cur);
   Test_Assert.Assert
     (Cur = No_Element, "check that there are no more warning lines");
   Lines.Clear;

   declare
      SVs : Scenario_Variable_Array := Tree.Scenario_Variables;
      UVs : Untyped_Variable_Array  := Tree.Untyped_Variables;
   begin
      Test_Assert.Assert
        (SVs'Length = 0, "check Scenario Variables ammount");
      Test_Assert.Assert
        (UVs'Length = 2, "check Untyped Variables ammount");
   end;

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (Tree.Root_Project);

   return Test_Assert.Report;

end Test;

