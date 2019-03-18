with GNAT.OS_Lib;  use GNAT.OS_Lib;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;

   DS : constant Character := GNAT.OS_Lib.Directory_Separator;

   Folder : Virtual_File;
   F : Virtual_File;
begin

   ------------
   -- Case 1 --
   ------------
   --  Using a directory that is not a full name (and where the .. cannot
   --  be resolved)

   Folder := Create (+".." & DS & ".." & DS & "foo" & DS);

   F := Create_From_Dir
      (Dir => Folder, Base_Name => +"file.ads");
   A.Assert (".." & DS & ".." & DS & "foo" & DS & "file.ads",
           F.Display_Full_Name,
           "With base name and relative directory");
   A.Assert (".." & DS & ".." & DS & "foo" & DS & "file.ads",
           F.Display_Full_Name (Normalize => True),
           "Normalized with base name and relative directory");

   F := Create_From_Dir
      (Dir => Folder, Base_Name => +"file.ads", Normalize => True);
   A.Assert (".." & DS & ".." & DS & "foo" & DS & "file.ads",
           F.Display_Full_Name,
           "With base name and relative directory");
   A.Assert (".." & DS & ".." & DS & "foo" & DS & "file.ads",
           F.Display_Full_Name (Normalize => True),
           "Normalized with base name and relative directory");

   ------------
   -- Case 2 --
   ------------

   Folder := Create (+"foo1" & DS & "foo2" & DS & "foo3" & DS & "");

   F := Create_From_Dir
      (Dir => Folder, Base_Name => +".." & DS & ".." & DS & "file.ads");
   A.Assert ("foo1" & DS & "foo2" & DS & "foo3" & DS
           & ".." & DS & ".." & DS & "file.ads",
           F.Display_Full_Name,
           "When file is relative to absolute directory");
   A.Assert ("foo1" & DS & "file.ads",
           F.Display_Full_Name (Normalize => True),
           "Normalized name when file is relative to absolute directory");

   F := Create_From_Dir (
      Dir => Folder, Base_Name => +".." & DS & ".." & DS & "file.ads",
      Normalize => True);
   A.Assert ("foo1" & DS & "foo2" & DS & "foo3" & DS & ".."
           & DS & ".." & DS & "file.ads",
           F.Display_Full_Name,
           "When file is relative to normalized absolute directory");
   A.Assert ("foo1" & DS & "file.ads",
           F.Display_Full_Name (Normalize => True),
           "Normalized name when file is relative to normalized absolute"
           & " directory");

   ------------
   -- Case 3 --
   ------------
   --  Using an absolute directory, where the .. can be resolved

   Folder := Create
      (+"" & DS & "foo1" & DS & "foo2" & DS & "foo3" & DS
       & ".." & DS & ".." & DS & "foo" & DS & "");

   F := Create_From_Dir
      (Dir => Folder, Base_Name => +"file.ads");
   A.Assert ("" & DS & "foo1" & DS & "foo2" & DS & "foo3" & DS & ".."
           & DS & ".." & DS & "foo" & DS & "file.ads",
           F.Display_Full_Name,
           "When file is relative to absolute directory with ..");
   A.Assert ("" & DS & "foo1" & DS & "foo" & DS & "file.ads",
           F.Display_Full_Name (Normalize => True),
           "Normalized name when file is relative to absolute directory"
           & " with ..");

   F := Create_From_Dir
      (Dir => Folder, Base_Name => +"file.ads", Normalize => True);
   A.Assert ("" & DS & "foo1" & DS & "foo" & DS & "file.ads",
           F.Display_Full_Name,
           "When file is relative to normalized absolute directory with ..");
   A.Assert ("" & DS & "foo1" & DS & "foo" & DS & "file.ads",
           F.Display_Full_Name (Normalize => True),
           "Normalized name when file is relative to normalized absolute"
           & " directory with ..");

   --  Check Create_From_Dir with a No_File entry
   declare
      Test_Success : Boolean := False;
   begin
      begin
         F := Create_From_Dir (No_File, +"file.ads");
      exception
         when VFS_Invalid_File_Error =>
            Test_Success := True;
      end;
      A.Assert (Test_Success, "Should raise Invalid_File_Error");
   end;

   return A.Report;
end Test;
