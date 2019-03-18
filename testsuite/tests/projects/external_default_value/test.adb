with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Test_Assert;

function Test return Integer is
   PT  : Project_Tree;
   Env : Project_Environment_Access;
begin
   Initialize (Env);

   GNATCOLL.Projects.Load
     (PT,
      Root_Project_Path => Create ("p.gpr"),
      Env => Env);

   declare
      UV_Array : constant Untyped_Variable_Array :=
        PT.Untyped_Variables;
      UV : Untyped_Variable;
      Expected_Value : constant String :=
        PT.Project_From_Name ("Q").Project_Path.Display_Dir_Name
        & "some_dir";
   begin
      Test_Assert.Assert
        (UV_Array'Length = 2, "Wrong number of Untyped Variables");
      if UV_Array'Length = 2 then
         UV := UV_Array (UV_Array'First);
         Test_Assert.Assert
           (External_Default (UV), Expected_Value,
            "Default external value is wrong for " & External_Name (UV));

         UV := UV_Array (UV_Array'Last);
         Test_Assert.Assert
           (External_Default (UV), Expected_Value,
            "Default external value is wrong for " & External_Name (UV));
      end if;
   end;

   PT.Unload;
   Free (Env);
   return Test_Assert.Report;
end Test;
