with GNATCOLL.OS.FSUtil;    use GNATCOLL.OS.FSUtil;
with GNATCOLL.OS.FS;        use GNATCOLL.OS.FS;
with GNATCOLL.OS.Dir;
with GNATCOLL.OS.Stat;
with GNATCOLL.OS.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Test_Assert;
with Ada.Text_IO;
with GNATCOLL.Strings_Impl;
with Ada.Characters.Handling;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames Ada.Text_IO;
   package Stat renames GNATCOLL.OS.Stat;
   package Dir renames GNATCOLL.OS.Dir;
   package C renames GNATCOLL.OS.Constants;

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

   --  Test Symbolic_Link_Is_Internal
   declare

      use Ada.Characters.Handling;
      package Strings is new GNATCOLL.Strings_Impl.Strings
        (GNATCOLL.Strings_Impl.Optimal_String_Size, Character, String);
      use Strings;

      function Basename (Path : String) return String;

      function Basename (Path : String) return String is
         X : Strings.XString;
      begin
         X.Set (Path);

         declare
            X_Arr : constant Strings.XString_Array :=
              X.Right_Split
                (Sep => C.Dir_Sep, Max_Split => 2, Omit_Empty => True);
            --  Extract symbolic link basename
         begin
            return X_Arr (1).To_String;
            --  Resolve symbolic links if needed, and normalize path
            --  to final link directory.
         end;

      end Basename;

      function Test_Symbolic_Link_Is_Internal
        (Target_Name : String; Should_Be_Internal : Boolean;
         Link_Name   : String := "link") return Boolean;

      function Test_Symbolic_Link_Is_Internal
        (Target_Name : String; Should_Be_Internal : Boolean;
         Link_Name   : String := "link") return Boolean
      is

         Top_Dir_Path : constant String := Dir.Path (Dir.Open ("."));
         FA           : Stat.File_Attributes;
      begin
         if not Create_Symbolic_Link (Link_Name, Target_Name) then
            IO.Put_Line ("Failed to create the symbolic link");
            return False;
         end if;

         FA := Stat.Stat (Link_Name, Follow_Symlinks => False);
         if not Stat.Is_Symbolic_Link (FA) then
            IO.Put_Line ("Symbolic link created is not a symbolic link");
            return False;
         end if;

         declare
            R : Boolean;
         begin
            if Should_Be_Internal then
               R := Symbolic_Link_Is_Internal (Top_Dir_Path, Link_Name);
            else
               R := not Symbolic_Link_Is_Internal (Top_Dir_Path, Link_Name);
            end if;

            if not Remove_File (Link_Name) then
               IO.Put_Line ("Failed to remove link");
               return False;
            end if;

            return R;
         end;
      end Test_Symbolic_Link_Is_Internal;
   begin

      A.Assert (Test_Symbolic_Link_Is_Internal ("file", True, "link"));

      A.Assert
        (Test_Symbolic_Link_Is_Internal
           ("." & C.Dir_Sep & "file", True, "link"));

      A.Assert
        (not Test_Symbolic_Link_Is_Internal ("" & C.Dir_Sep, True, "link"));

      A.Assert
        (not Test_Symbolic_Link_Is_Internal (C.Dir_Sep & "tmp", True, "link"));

      A.Assert
        (not Test_Symbolic_Link_Is_Internal
           (C.Dir_Sep &
            "veryloooooooooooooooooooooongpathasthefunctioncomparepathslength",
            True, "link"));

      A.Assert
        (not Test_Symbolic_Link_Is_Internal
           (".." & C.Dir_Sep & "file", True, "link"));

      A.Assert (Create_Directory ("dir1"));
      A.Assert
        (Test_Symbolic_Link_Is_Internal
           (".." & C.Dir_Sep & "file", True, "dir1" & C.Dir_Sep & "link"));

      A.Assert
        (Test_Symbolic_Link_Is_Internal
           (".." & C.Dir_Sep & "dir1" & C.Dir_Sep & ".." & C.Dir_Sep & "dir1" &
            C.Dir_Sep & ".." & C.Dir_Sep & "file",
            True, "dir1" & C.Dir_Sep & "link"));

      A.Assert (Remove_Directory ("dir1"));

      declare
         DH : Dir.Dir_Handle := Dir.Open (".");
      begin
         declare
            Test_Dir_Basename : constant String := Basename (Dir.Path (DH));
         begin
            A.Assert
              (not Test_Symbolic_Link_Is_Internal
                 (".." & C.Dir_Sep & Test_Dir_Basename & C.Dir_Sep & "file",
                  True, "link"));
         end;
         Dir.Close (DH);
      end;
   end;

   --  Test symbolic link copy
   declare
      Target_Name    : constant String := "file";
      Link_Name      : constant String := "file_link";
      Link_Copy_Name : constant String := "file_link_copy";
      FA             : Stat.File_Attributes;
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

      A.Assert (Copy_Symbolic_Link (Link_Name, Link_Copy_Name));
      A.Assert (Check_File_Content (Link_Copy_Name, "Input content"));
      FA := Stat.Stat (Link_Copy_Name, Follow_Symlinks => False);
      A.Assert (Stat.Is_Symbolic_Link (FA));
      FA := Stat.Stat (Link_Copy_Name, Follow_Symlinks => True);
      A.Assert (Stat.Is_File (FA));

      declare
         Target_Path      : Unbounded_String;
         Target_Path_Copy : Unbounded_String;
      begin
         A.Assert (Read_Symbolic_Link (Link_Name, Target_Path));
         A.Assert (Read_Symbolic_Link (Link_Copy_Name, Target_Path_Copy));
         A.Assert (To_String (Target_Path) = Target_Name);
         A.Assert (To_String (Target_Path_Copy) = Target_Name);
      end;

      A.Assert (Remove_File (Link_Name));
      A.Assert (Remove_File (Link_Copy_Name));

      --  Ensure that we do not erase the target when removing the link
      A.Assert (Check_File_Content (Target_Name, "Input content"));
      A.Assert (Remove_File (Target_Name));
   end;

   return A.Report;

end Test;
