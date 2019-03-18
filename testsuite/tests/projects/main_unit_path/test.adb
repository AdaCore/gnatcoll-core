with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects.Aux;
with Test_Assert;

function Test return Integer is
   PT    : Project_Tree;
   Env   : Project_Environment_Access;
   Dummy : Boolean;

   VF_Full_Good : constant Virtual_File :=
     Get_Current_Dir / "src" / "foo.adb";
   VF_Full_Bad  : constant Virtual_File :=
     Get_Current_Dir / "not_src" / "foo.adb";
   VF_Relative  : constant Virtual_File :=
     Create ("boo") / "foo.adb";
begin
   Make_Dir (Create ("src"));
   Make_Dir (Create ("not_src"));
   Copy (Create ("foo.adb"), "src", Dummy);
   Copy (Create ("foo.adb"), "not_src", Dummy);
   Initialize (Env);

   GNATCOLL.Projects.Load
     (PT,
      Root_Project_Path => Create ("foo1.gpr"),
      Env => Env);

   Test_Assert.Assert
     (PT.Root_Project.Is_Main_File ("foo.adb"),
      "testing base name");
   Test_Assert.Assert
     (PT.Root_Project.Is_Main_File (VF_Full_Good.Full_Name),
      "testing full correct path");
   Test_Assert.Assert
     (not PT.Root_Project.Is_Main_File (VF_Full_Bad.Full_Name),
      "testing full incorrect path");
   Test_Assert.Assert
     (PT.Root_Project.Is_Main_File (VF_Relative.Full_Name),
      "testing relative path");

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);
   Unload (PT);
   Free (Env);

   return Test_Assert.Report;

end Test;
