with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects.Aux;

with Test_Assert;

function Test return Integer is
   PT     : Project_Tree;
   Env    : Project_Environment_Access;

   VF : Virtual_File;
begin
   Initialize (Env);
   PT.Load (Create ("prj.gpr"), Env);

   VF := PT.Create ("pkg_0-p_separate_common.adb");
   Test_Assert.Assert
     (PT.Other_File (VF).Display_Base_Name,
      "pkg_0.ads",
      "check other file for separate from unit with spec");

   VF := PT.Create ("main-separate_of_body.adb");
   Test_Assert.Assert
     (PT.Other_File (VF).Display_Base_Name,
      "main.adb",
      "check other file for separate from unit without spec");

   VF := PT.Create ("pkg_0-pack.adb");
   Test_Assert.Assert
     (PT.Other_File (VF).Display_Base_Name,
      "pkg_0.ads",
      "check other file for intermediate separate");

   VF := PT.Create ("pkg_0-pack-p_separate_of_separate.adb");
   Test_Assert.Assert
     (PT.Other_File (VF).Display_Base_Name,
      "pkg_0.ads",
      "check other file for separate of separate");

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);

   PT.Unload;
   Free (Env);
   return Test_Assert.Report;

end Test;
