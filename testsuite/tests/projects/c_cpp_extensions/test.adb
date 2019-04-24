with GNATCOLL.Projects;  use GNATCOLL.Projects;
with GNATCOLL.VFS;       use GNATCOLL.VFS;
with GNATCOLL.Projects.Aux;
with Test_Assert;

function Test return Integer is
   PT    : Project_Tree;
   Env   : Project_Environment_Access;
begin

   Initialize (Env);

   GNATCOLL.Projects.Load
     (PT,
      Root_Project_Path => Create ("p.gpr"),
      Env               => Env);

   declare
      Body_Ext : constant String := PT.Root_Project.Attribute_Value
        (Attribute    => Impl_Suffix_Attribute,
         Index        => "c");
      Spec_Ext : constant String := PT.Root_Project.Attribute_Value
        (Attribute    => Spec_Suffix_Attribute,
         Index        => "c");

   begin
      Test_Assert.Assert (Spec_Ext, ".h", "check C spec extension");
      Test_Assert.Assert (Body_Ext, ".c", "check C body extension");
   end;

   declare
      Body_Ext : constant String := PT.Root_Project.Attribute_Value
        (Attribute    => Impl_Suffix_Attribute,
         Index        => "c++");
      Spec_Ext : constant String := PT.Root_Project.Attribute_Value
        (Attribute    => Spec_Suffix_Attribute,
         Index        => "c++");

   begin
      Test_Assert.Assert (Spec_Ext, ".hh",  "check C++ spec extension");
      Test_Assert.Assert (Body_Ext, ".cpp", "check C++ body extension");
   end;

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   Unload (PT);
   Free (Env);

   return Test_Assert.Report;

end Test;
