with GNATCOLL.OS.Temp; use GNATCOLL.OS.Temp;
with GNATCOLL.OS.FS;
with GNATCOLL.OS.Stat;
with Test_Assert;
with Ada.Directories;
with GNATCOLL.OS;
with Ada.Strings.UTF_Encoding;
with Ada.Environment_Variables;

function Test return Integer
is
   package Env renames Ada.Environment_Variables;
   package A renames Test_Assert;
   package FS renames GNATCOLL.OS.FS;
   package UTF8 renames Ada.Strings.UTF_Encoding;
   package Stat renames GNATCOLL.OS.Stat;

   type UTF_8_String_Access is access all UTF8.UTF_8_String;
begin
   --  Checks on invalid Temp_File_Handle
   declare
      Temp_File : Temp_File_Handle;
   begin
      --  A non initialized Temp_File_Handle should not return a Path
      A.Assert (Path (Temp_File)'Length > 0);
   exception
      when others =>
         A.Assert (True, "got expected exception");
   end;

   --  Perform random part generation to force update of the mersenne seed
   for Idx in  1 .. 1024 loop
      declare
         Part : constant UTF8.UTF_8_String := Random_Part;
      begin
         A.Assert (Part'Length = 13);
      end;
   end loop;

   begin
      declare
         Temp_File : UTF8.UTF_8_String :=
            Random_Path (Dir => ".", Prefix => "et/");
      begin
         A.Assert (False, "exception not raised");
      end;
   exception
      when GNATCOLL.OS.OS_Error =>
         A.Assert (True, "expect OS_Error if prefix contains a dir sep");
      when others =>
         A.Assert (False, "non expected exception");
   end;

   begin
      declare
         Temp_File : UTF8.UTF_8_String :=
            Random_Path (Dir => ".", Prefix => "et/tt");
      begin
         A.Assert (False, "exception not raised");
      end;
   exception
      when GNATCOLL.OS.OS_Error =>
         A.Assert (True, "expect OS_Error if suffix contains a dir sep");
      when others =>
         A.Assert (False, "non expected exception");
   end;

   declare
      Temp_File : UTF8.UTF_8_String :=
         Random_Path (Dir => ".", Prefix => "file-", Suffix => ".txt");
      Temp_File_Basename : constant UTF8.UTF_8_String :=
         Ada.Directories.Simple_Name (Temp_File);
   begin
      A.Assert
         (Temp_File_Basename'Length = 22,
          "check that basename as the right length");
   end;

   declare
      Temp_Dir_Path : UTF_8_String_Access;
   begin
      declare
         Temp_Dir : Temp_Dir_Handle := Create_Temp_Dir (Dir => ".");
         Fd       : FS.File_Descriptor;
      begin
         Temp_Dir_Path := new UTF8.UTF_8_String'(Path (Temp_Dir));
         A.Assert (Stat.Is_Directory (Stat.Stat (Temp_Dir_Path.all)),
                   "directory expected");
         Ada.Directories.Create_Directory (Temp_Dir_Path.all & "/my-temp-dir");

         Fd := FS.Open
            (Temp_Dir_Path.all & "/my-temp-dir/test.txt",
             Mode => FS.Write_Mode);
         FS.Write (Fd, "Hello");
         FS.Close (Fd);
      end;

      A.Assert (not Stat.Is_Directory (Stat.Stat (Temp_Dir_Path.all)),
                "directory should have been deleted");
   end;

   declare
      Temp_Dir_Path : UTF_8_String_Access;
   begin
      declare
         Temp_Dir : Temp_Dir_Handle := Create_Temp_Dir
            (Dir => ".", Auto_Delete => False);
      begin
         Temp_Dir_Path := new UTF8.UTF_8_String'(Path (Temp_Dir));
         A.Assert (Stat.Is_Directory (Stat.Stat (Temp_Dir_Path.all)),
                   "directory expected");
      end;

      A.Assert (Stat.Is_Directory (Stat.Stat (Temp_Dir_Path.all)),
                "directory should not have been deleted");
   end;

   begin
      declare
         Temp_Dir : Temp_Dir_Handle :=
            Create_Temp_Dir (Dir => "./non-existing-dir");
      begin
         A.Assert (Stat.Is_Directory (Stat.Stat (Path (Temp_Dir))),
                   "directory expected");
      end;
   exception
      when GNATCOLL.OS.OS_Error =>
         A.Assert (True, "expect OS_Error if directory cannot be created");
      when others =>
         A.Assert (False, "non expected exception");
   end;

   declare
      Temp_File : Temp_File_Handle := Create_Temp_File (Dir => ".");
   begin
      A.Assert (
         Ada.Directories.Simple_Name (Path (Temp_File))'Length = 13);
      FS.Write (File_Descriptor (Temp_File), "Hello");
   end;

   declare
      Temp_File_Path : UTF_8_String_Access;
   begin
      declare
         Temp_File : Temp_File_Handle := Create_Temp_File
            (Dir => ".", Auto_Delete => False);
      begin
         Temp_File_Path := new UTF8.UTF_8_String'(Path (Temp_File));
         A.Assert (Stat.Is_File (Stat.Stat (Temp_File_Path.all)),
                   "file expected");
      end;

      A.Assert (Stat.Is_File (Stat.Stat (Temp_File_Path.all)),
                "file should not have been deleted");
   end;

   declare
      Temp_File_Path : UTF_8_String_Access;
   begin
      declare
         Temp_File : Temp_File_Handle := Create_Temp_File (Dir => ".");
      begin
         Temp_File_Path := new UTF8.UTF_8_String'(Path (Temp_File));
         A.Assert (Stat.Is_File (Stat.Stat (Temp_File_Path.all)),
                   "file expected");
      end;

      A.Assert (not Stat.Is_File (Stat.Stat (Temp_File_Path.all)),
                "file should have been deleted");
   end;

   begin
      declare
         Temp_File : Temp_File_Handle :=
            Create_Temp_File (Dir => "./non-existing-dir");
      begin
         A.Assert (Stat.Is_File (Stat.Stat (Path (Temp_File))),
                   "file expected");
      end;
   exception
      when GNATCOLL.OS.OS_Error =>
         A.Assert (True, "expect OS_Error if file cannot be created");
      when others =>
         A.Assert (False, "non expected exception");
   end;

   declare
      Temp_Dir : constant UTF8.UTF_8_String := Create_Temp_Dir (Dir => ".");
   begin
      A.Assert (Stat.Is_Directory (Stat.Stat (Temp_Dir)));
   end;

   Ada.Directories.Create_Directory ("./my-temp-dir");
   Env.Set ("TMPDIR", "./my-temp-dir");
   declare
      Temp_File : Temp_File_Handle :=
         Create_Temp_File (Dir => System_Temp_Dir);
   begin
      A.Assert (
         Ada.Directories.Simple_Name (
            Ada.Directories.Containing_Directory (Path (Temp_File))),
         "my-temp-dir");
   end;

   Env.Set ("TMPDIR", "./my-temp-dir-new");
   declare
      Temp_File : Temp_File_Handle :=
         Create_Temp_File (Dir => System_Temp_Dir);
   begin
      A.Assert (
         Ada.Directories.Simple_Name (
            Ada.Directories.Containing_Directory (Path (Temp_File))),
         "my-temp-dir",
         "System_Temp_Dir should return a constant");
   end;

   return A.Report;
end Test;
