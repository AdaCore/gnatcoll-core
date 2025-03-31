with GNAT.Strings;
with GNATCOLL.Traces; use GNATCOLL.Traces;

with Test_Assert;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

function Test return Integer is
   Tree                  : Project_Tree;
   Builder_Switches_Impl : constant Attribute_Pkg_List :=
     Build ("Builder", "Switches");
   procedure Silent_Report (S : String) is null;
begin
   Parse_Config_File;
   Tree.Load
     (Create ("tree.gpr"), Errors => Silent_Report'Unrestricted_Access);

   declare
      Indexes : constant GNAT.Strings.String_List :=
        Attribute_Indexes (Tree.Root_Project, Builder_Switches_Impl);
   begin
      Test_Assert.Assert (Indexes'Length = 1);
      for S of Indexes loop
         Test_Assert.Assert (S.all = Others_Index_Name);
      end loop;
   end;

   return Test_Assert.Report;
end Test;
