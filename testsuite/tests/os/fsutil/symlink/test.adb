with GNATCOLL.OS.FSUtil;    use GNATCOLL.OS.FSUtil;
with GNATCOLL.OS.FS;        use GNATCOLL.OS.FS;
with GNATCOLL.OS.Stat;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Test_Assert;
with Ada.Text_IO;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames Ada.Text_IO;
   package Stat renames GNATCOLL.OS.Stat;

   function Check_File_Content
     (File_Name : String; Expected_Content : String) return Boolean;

   function Check_File_Content
     (File_Name : String; Expected_Content : String) return Boolean
   is
      FD : constant File_Descriptor := Open (File_Name, Mode => Read_Mode);
   begin
      declare
         Content : constant String := Read (FD);
      begin
         Close (FD);
         return Content = Expected_Content;
      end;
   end Check_File_Content;

   FD : File_Descriptor;
begin
   IO.Put_Line ("GNATCOLL.OS.FSUtil test");

   --  Test simple file symbolic link
   declare
      Target_Name : constant String := "file1";
      Link_Name   : constant String := "file1_link";
      FA          : Stat.File_Attributes;
   begin
      FD := Open (Target_Name, Mode => Write_Mode);
      Write (FD, "Input content");
      Close (FD);

      A.Assert (Check_File_Content (Target_Name, "Input content"));

      A.Assert (Create_Symbolic_Link (Link_Name, Target_Name));
      A.Assert (Check_File_Content (Link_Name, "Input content"));
      FA := Stat.Stat (Link_Name, Follow_Symlinks => False);
      A.Assert (Stat.Is_Symbolic_Link (FA));
      FA := Stat.Stat (Link_Name, Follow_Symlinks => True);
      A.Assert (Stat.Is_File (FA));

      A.Assert (not Create_Symbolic_Link (Link_Name, Target_Name));

      declare
         Target_Path : Unbounded_String;
      begin
         A.Assert (Read_Symbolic_Link (Link_Name, Target_Path));
         A.Assert (To_String (Target_Path) = Target_Name);
      end;

      A.Assert (Remove_File (Link_Name));
      FD := Open (Link_Name, Mode => Read_Mode);
      A.Assert (FD = Invalid_FD);

      FA := Stat.Stat (Link_Name);
      A.Assert (not Stat.Exists (FA));

      --  Ensure that we do not erase the target when removing the link
      A.Assert (Check_File_Content (Target_Name, "Input content"));
      A.Assert (Remove_File (Target_Name));

   end;

   --  Test directory symbolic link
   declare
      Target_Name : constant String := "dir1";
      Link_Name   : constant String := "dir1_link";
      FA          : Stat.File_Attributes;
   begin
      A.Assert (Create_Directory (Target_Name));

      A.Assert (Create_Symbolic_Link (Link_Name, Target_Name));

      FA := Stat.Stat (Link_Name, Follow_Symlinks => False);
      A.Assert (Stat.Is_Symbolic_Link (FA));
      FA := Stat.Stat (Link_Name, Follow_Symlinks => True);
      A.Assert (Stat.Is_Directory (FA));

      A.Assert (not Create_Symbolic_Link (Link_Name, Target_Name));

      declare
         Target_Path : Unbounded_String;
      begin
         A.Assert (Read_Symbolic_Link (Link_Name, Target_Path));
         A.Assert (To_String (Target_Path) = Target_Name);
      end;

      A.Assert (Remove_Directory (Link_Name));
      FD := Open (Link_Name, Mode => Read_Mode);
      A.Assert (FD = Invalid_FD);

      FA := Stat.Stat (Link_Name);
      A.Assert (not Stat.Exists (FA));

      --  Ensure that we do not erase the target when removing the link
      FD := Open (Target_Name, Mode => Read_Mode);
      A.Assert (FD /= Invalid_FD);
      Close (FD);
      A.Assert (Remove_Directory (Target_Name));
   end;

   --  Test relative symbolic link
   declare
      Target_Name : constant String := "../file1";
      Link_Name   : constant String := "dir1/file1_link";
      FA          : Stat.File_Attributes;
   begin
      A.Assert (Create_Directory ("dir1"));

      FD := Open ("file1", Mode => Write_Mode);
      Write (FD, "Input content");
      Close (FD);

      A.Assert (Check_File_Content ("file1", "Input content"));

      A.Assert (Create_Symbolic_Link (Link_Name, Target_Name));
      A.Assert (Check_File_Content (Link_Name, "Input content"));

      FA := Stat.Stat (Link_Name, Follow_Symlinks => False);
      A.Assert (Stat.Is_Symbolic_Link (FA));
      FA := Stat.Stat (Link_Name, Follow_Symlinks => True);
      A.Assert (Stat.Is_File (FA));

      A.Assert (not Create_Symbolic_Link (Link_Name, Target_Name));

      declare
         Target_Path : Unbounded_String;
      begin
         A.Assert (Read_Symbolic_Link (Link_Name, Target_Path));
         A.Assert (To_String (Target_Path) = Target_Name);
      end;

      A.Assert (Remove_File (Link_Name));
      FD := Open (Link_Name, Mode => Read_Mode);
      A.Assert (FD = Invalid_FD);

      FA := Stat.Stat (Link_Name);
      A.Assert (not Stat.Exists (FA));

      --  Ensure that we do not erase the target when removing the link
      A.Assert (Check_File_Content ("file1", "Input content"));
      A.Assert (Remove_File ("file1"));
      A.Assert (Remove_Directory ("dir1"));
   end;

   --  Test absolute symbolic link
   declare
      Target_Name : constant String := "/tmp/file1";
      Link_Name   : constant String := "file1_link";
      FA          : Stat.File_Attributes;
   begin

      FD := Open (Target_Name, Mode => Write_Mode);
      Write (FD, "Input content");
      Close (FD);

      A.Assert (Check_File_Content (Target_Name, "Input content"));

      A.Assert (Create_Symbolic_Link (Link_Name, Target_Name));
      A.Assert (Check_File_Content (Link_Name, "Input content"));

      FA := Stat.Stat (Link_Name, Follow_Symlinks => False);
      A.Assert (Stat.Is_Symbolic_Link (FA));
      FA := Stat.Stat (Link_Name, Follow_Symlinks => True);
      A.Assert (Stat.Is_File (FA));

      A.Assert (not Create_Symbolic_Link (Link_Name, Target_Name));

      declare
         Target_Path : Unbounded_String;
      begin
         A.Assert (Read_Symbolic_Link (Link_Name, Target_Path));
         A.Assert (To_String (Target_Path) = Target_Name);
      end;

      A.Assert (Remove_File (Link_Name));
      FD := Open (Link_Name, Mode => Read_Mode);
      A.Assert (FD = Invalid_FD);

      FA := Stat.Stat (Link_Name);
      A.Assert (not Stat.Exists (FA));

      --  Ensure that we do not erase the target when removing the link
      A.Assert (Check_File_Content (Target_Name, "Input content"));
      A.Assert (Remove_File (Target_Name));
   end;

   --  Test invalid target symbolic link
   declare
      Target_Name : constant String := "missing_file";
      Link_Name   : constant String := "missing_file_link";
      FA          : Stat.File_Attributes;
   begin
      A.Assert (Create_Symbolic_Link (Link_Name, Target_Name));

      FA := Stat.Stat (Link_Name, Follow_Symlinks => False);
      A.Assert (Stat.Is_Symbolic_Link (FA));
      FA := Stat.Stat (Link_Name, Follow_Symlinks => True);
      A.Assert (not Stat.Exists (FA));

      A.Assert (not Create_Symbolic_Link (Link_Name, Target_Name));

      declare
         Target_Path : Unbounded_String;
      begin
         A.Assert (Read_Symbolic_Link (Link_Name, Target_Path));
         A.Assert (To_String (Target_Path) = Target_Name);
      end;

      A.Assert (Remove_File (Link_Name));

      FA := Stat.Stat (Link_Name, Follow_Symlinks => False);
      A.Assert (not Stat.Exists (FA));
   end;

   return A.Report;

end Test;
