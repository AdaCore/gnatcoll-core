with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Test_Assert;       use Test_Assert;
with Ada.Directories;
with Ada.Direct_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

function Test return Integer is
   Tree      : Project_Tree;
   Proj_File : constant String := "p.gpr";
   Proj      : Project_Type;
   Dummy     : Boolean;

   function Read_File (File_Name : String) return String;
   --  Read a file and return its content as a String

   function Read_File (File_Name : String) return String
   is
      File_Size : constant Natural :=
                    Natural (Ada.Directories.Size (File_Name));

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      File     : File_String_IO.File_Type;
      Contents : File_String;

   begin
      File_String_IO.Open  (File, Mode => File_String_IO.In_File,
                                  Name => File_Name);
      File_String_IO.Read  (File, Item => Contents);
      File_String_IO.Close (File);

      return String (Contents);
   end Read_File;

begin
   Tree.Load (Create (Filesystem_String (Proj_File)));

   Proj := Tree.Root_Project;
   Proj.Rename_And_Move
      ("foobar_" & Proj.Name, Create
        (Filesystem_String
          (Ada.Directories.Containing_Directory
            (+Proj.Project_Path.Full_Name))));

   Tree.Recompute_View;
   Dummy := Proj.Save;

   declare
      Content : constant String := Read_File ("foobar_" & Proj_File);
   begin
      --  Check that the separator are not removed

      Assert (Index (Content, """src/"" & ""subsrc/""") > 0);

      --  Check that the project name has been updated

      Assert (Index (Content, "Foobar_P") > 0);
   end;

   return Report;
end Test;
