with Ada.Directories;
with GNATCOLL.OS.FSUtil;
with GNATCOLL.OS.Libc;
with GNATCOLL.OS.Libc.Stat;
with GNATCOLL.String_Builders;
with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;
   package Libc renames GNATCOLL.OS.Libc;
   package Stat renames GNATCOLL.OS.Libc.Stat;
   package SB renames GNATCOLL.String_Builders;

   use type Libc.File_Mode;

   Ada_Directory    : constant String := "ada_directory";
   FSUtil_Directory : constant String := "fsutil_directory";

   function Permissions (Path : String) return Libc.File_Mode;

   function Permissions (Path : String) return Libc.File_Mode is
      Name   : SB.String_Builder;
      Info   : Stat.Stat_Info;
      Status : Libc.Libc_Status;
   begin
      SB.Set (Name, Path);
      Status := Stat.Stat (SB.As_C_String (Name), Info);
      A.Assert (Status = Libc.Success, "stat " & Path);
      if Status /= Libc.Success then
         return 0;
      end if;

      --  Keep permission and special mode bits, excluding the file type.
      return Info.Mode and 8#7777#;
   end Permissions;

begin
   Ada.Directories.Create_Directory (Ada_Directory);
   A.Assert (GNATCOLL.OS.FSUtil.Create_Directory (FSUtil_Directory));

   A.Assert
     (Permissions (FSUtil_Directory) = Permissions (Ada_Directory),
      "FSUtil and Ada directory permissions should match");

   return A.Report;
end Test;
