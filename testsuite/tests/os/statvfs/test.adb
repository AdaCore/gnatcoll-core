with Test_Assert;
with GNAT.IO;
with GNATCOLL.OS.FS;
with GNATCOLL.OS.Libc.Stat;
with GNATCOLL.OS.Libc;
with GNATCOLL.String_Builders;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames GNAT.IO;
   package FS renames GNATCOLL.OS.FS;
   package Stat renames GNATCOLL.OS.Libc.Stat;
   package SB renames GNATCOLL.String_Builders;

   Result : Stat.Statvfs_Info;
   Status : GNATCOLL.OS.Libc.Libc_Status;
   Filename : SB.String_Builder;
   Fd : FS.File_Descriptor;
begin
   IO.Put_Line ("GNATCOLL.OS.Libc.Stat.Statvfs test");
   IO.Put_Line ("Call statvfs on current directory");
   SB.Set (Filename, ".");
   Status := Stat.Statvfs (SB.As_C_String (Filename), Result);
   A.Assert (Integer (Status), 0, "expect 0 status");
   IO.Put_Line ("block size:" & Result.Bsize'Img &
                ", fragment size:" & Result.Frsize'Img &
                ", blocks:" & Result.Blocks'Img &
                ", namemax:" & Result.Namemax'Img &
                ", pathmax:" & Result.Pathmax'Img);

   IO.Put_Line ("Statvfs should fail on non existing file");
   SB.Set (Filename, "non_dir/non_existing.txt");
   Status := Stat.Statvfs (SB.As_C_String (Filename), Result);
   A.Assert (Integer (Status), -1, "expect -1 status");

   IO.Put_Line ("Fstatvfs should work on /dev/null beside unclear semantic");
   Fd := FS.Open ("/dev/null");
   Status := Stat.Fstatvfs (Fd, Result);
   A.Assert (Integer (Status), 0, "expect 0 status");
   IO.Put_Line ("block size:" & Result.Bsize'Img &
                ", fragment size:" & Result.Frsize'Img &
                ", blocks:" & Result.Blocks'Img &
                ", namemax:" & Result.Namemax'Img &
                ", pathmax:" & Result.Pathmax'Img);
   FS.Close (Fd);

   IO.Put_Line ("Fstatvfs should fail on invalid file descriptor");
   Status := Stat.Fstatvfs (FS.Invalid_FD, Result);
   A.Assert (Integer (Status), -1, "expect -1 status");

   return A.Report;
end Test;
