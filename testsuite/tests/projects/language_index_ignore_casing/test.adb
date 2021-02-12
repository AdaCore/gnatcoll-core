with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects.Aux;

with GNAT.OS_Lib;       use GNAT.OS_Lib;

with Test_Assert;

function Test return Integer is
   PT     : Project_Tree;
   Env    : Project_Environment_Access;
   Attr   : constant Attribute_Pkg_String := Build ("foo", "switches");
   Attr_L : constant Attribute_Pkg_List   := Build ("foo", "list_switches");

   List_Res : String_List_Access;
begin
   declare
      Err : constant String :=
        Register_New_Attribute
          (Name                 => "switches",
           Pkg                  => "foo",
           Is_List              => False,
           Indexed              => True,
           Case_Sensitive_Index => True);
   begin
      Test_Assert.Assert (Err, "", "first attribute registration successful");
   end;

   declare
      Err : constant String :=
        Register_New_Attribute
          (Name                 => "list_switches",
           Pkg                  => "foo",
           Is_List              => True,
           Indexed              => True,
           Case_Sensitive_Index => True);
   begin
      Test_Assert.Assert (Err, "", "second attribute registration successful");
   end;

   Initialize (Env);
   PT.Load (Create ("p.gpr"), Env);

   --  String

   Test_Assert.Assert
     (PT.Root_Project.Has_Attribute (Attr, "ada"),
      "language lower-cased found (string)");
   Test_Assert.Assert
     (PT.Root_Project.Has_Attribute (Attr, "ADA"),
      "language upper-cased found (string)");
   Test_Assert.Assert
     (PT.Root_Project.Has_Attribute (Attr, "Ada"),
      "language mixed-cased found (string)");
   Test_Assert.Assert
     (PT.Root_Project.Has_Attribute (Attr, "aDA"),
      "language original-cased found (string)");

   Test_Assert.Assert
     (not PT.Root_Project.Has_Attribute (Attr, "asm"),
      "not a language lower-cased not found (string)");
   Test_Assert.Assert
     (not PT.Root_Project.Has_Attribute (Attr, "ASM"),
      "not a language upper-cased not found (string)");
   Test_Assert.Assert
     (not PT.Root_Project.Has_Attribute (Attr, "Asm"),
      "not a language mixed-cased not found (string)");
   Test_Assert.Assert
     (PT.Root_Project.Has_Attribute (Attr, "aSM"),
      "not a language original-cased found (string)");

   Test_Assert.Assert
     (PT.Root_Project.Attribute_Value (Attr, "ada"), "boo",
      "language lower-cased value correct (string)");
   Test_Assert.Assert
     (PT.Root_Project.Attribute_Value (Attr, "ADA"), "boo",
      "language upper-cased found value correct (string)");
   Test_Assert.Assert
     (PT.Root_Project.Attribute_Value (Attr, "Ada"), "boo",
      "language mixed-cased found calue correct (string)");
   Test_Assert.Assert
     (PT.Root_Project.Attribute_Value (Attr, "aDA"), "boo",
      "language original-cased found calue correct (string)");

   Test_Assert.Assert
     (PT.Root_Project.Attribute_Value (Attr, "asm"), "",
      "not a language lower-cased value correct (string)");
   Test_Assert.Assert
     (PT.Root_Project.Attribute_Value (Attr, "ASM"), "",
      "not a language upper-cased value correct (string)");
   Test_Assert.Assert
     (PT.Root_Project.Attribute_Value (Attr, "Asm"), "",
      "not a language mixed-cased value correct (string)");
   Test_Assert.Assert
     (PT.Root_Project.Attribute_Value (Attr, "aSM"), "foo",
      "not a language original-cased value correct (string)");

   --  List

   Test_Assert.Assert
     (PT.Root_Project.Has_Attribute (Attr_L, "ada"),
      "language lower-cased found (list)");
   Test_Assert.Assert
     (PT.Root_Project.Has_Attribute (Attr_L, "ADA"),
      "language upper-cased found (list)");
   Test_Assert.Assert
     (PT.Root_Project.Has_Attribute (Attr_L, "Ada"),
      "language mixed-cased found (list)");
   Test_Assert.Assert
     (PT.Root_Project.Has_Attribute (Attr_L, "aDA"),
      "language original-cased found (list)");

   Test_Assert.Assert
     (not PT.Root_Project.Has_Attribute (Attr_L, "asm"),
      "not a language lower-cased not found (list)");
   Test_Assert.Assert
     (not PT.Root_Project.Has_Attribute (Attr_L, "ASM"),
      "not a language upper-cased not found (list)");
   Test_Assert.Assert
     (not PT.Root_Project.Has_Attribute (Attr_L, "Asm"),
      "not a language mixed-cased not found (list)");
   Test_Assert.Assert
     (PT.Root_Project.Has_Attribute (Attr_L, "aSM"),
      "not a language original-cased found (list)");

   List_Res := PT.Root_Project.Attribute_Value (Attr_L, "ada");
   Test_Assert.Assert
     (List_Res /= null and then List_Res.all'Length = 1
      and then List_Res (1).all = "val1",
      "language lower-cased value correct (list)");
   Free (List_Res);

   List_Res := PT.Root_Project.Attribute_Value (Attr_L, "ADA");
   Test_Assert.Assert
     (List_Res /= null and then List_Res.all'Length = 1
      and then List_Res (1).all = "val1",
      "language upper-cased found value correct (list)");
   Free (List_Res);

   List_Res := PT.Root_Project.Attribute_Value (Attr_L, "Ada");
   Test_Assert.Assert
     (List_Res /= null and then List_Res.all'Length = 1
      and then List_Res (1).all = "val1",
      "language mixed-cased found calue correct");
   Free (List_Res);

   List_Res := PT.Root_Project.Attribute_Value (Attr_L, "aDA");
   Test_Assert.Assert
     (List_Res /= null and then List_Res.all'Length = 1
      and then List_Res (1).all = "val1",
      "language original-cased found calue correct (list)");
   Free (List_Res);

   List_Res := PT.Root_Project.Attribute_Value (Attr_L, "asm");
   Test_Assert.Assert
     (List_Res = null,
      "not a language lower-cased value correct (list)");
   Free (List_Res);

   List_Res := PT.Root_Project.Attribute_Value (Attr_L, "ASM");
   Test_Assert.Assert
     (List_Res = null,
      "not a language upper-cased found value correct (list)");
   Free (List_Res);

   List_Res := PT.Root_Project.Attribute_Value (Attr_L, "Asm");
   Test_Assert.Assert
     (List_Res = null,
      "not a language mixed-cased found calue correct (list)");
   Free (List_Res);

   List_Res := PT.Root_Project.Attribute_Value (Attr_L, "aSM");
   Test_Assert.Assert
     (List_Res /= null and then List_Res.all'Length = 1
      and then List_Res (1).all = "val2",
      "not a language original-cased found calue correct (list)");
   Free (List_Res);

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);

   PT.Unload;
   Free (Env);
   return Test_Assert.Report;
end Test;
