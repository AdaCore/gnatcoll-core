with GNATCOLL.VFS; use GNATCOLL.VFS;
with Ada.Directories;
with Test_Assert;

function Test return Integer is
   package Dir renames Ada.Directories;
   package A renames Test_Assert;

begin
   Dir.Create_Directory ("test_subdir");
   Dir.Set_Directory ("./test_subdir");

   declare
      --  All the files refer to the same file using different paths
      --  (we assume the current directory is test_subdir).
      File1 : constant String := "main.adb";
      File2 : constant String := "./main.adb";
      File3 : constant String := "../test_subdir/main.adb";

      NormFile1 : constant Virtual_File := Create (+File1, Normalize => True);
      NormFile2 : constant Virtual_File := Create (+File2, Normalize => True);
      NormFile3 : constant Virtual_File := Create (+File3, Normalize => True);

      --  Create an absolute path. Create_From_Dir calls Create
      --  with an aboslute path, so this covers the special case
      --  in Create.
      NormFile4 : constant Virtual_File := Create_From_Dir
         (Create ("./", Normalize => True), "main.adb");

   begin
      --  Ensure that whatever version of the path was used to reference
      --  main.adb, then call to functions on that path (such as Get_Parent),
      --  will return the same value.
      A.Assert (+NormFile1.Get_Parent.Get_Parent.Full_Name =
                +NormFile2.Get_Parent.Get_Parent.Full_Name);
      A.Assert (+NormFile2.Get_Parent.Get_Parent.Full_Name =
                +NormFile3.Get_Parent.Get_Parent.Full_Name);
      A.Assert (+NormFile3.Get_Parent.Get_Parent.Full_Name =
                +NormFile4.Get_Parent.Get_Parent.Full_Name);
   end;
   return A.Report;
end Test;
