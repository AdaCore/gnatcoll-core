------------------------------------------------------------------------------
--                                                                          --
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with GNATCOLL.VFS;       use GNATCOLL.VFS;
with GNAT.Strings;       use GNAT.Strings;
with GNATCOLL.OS.Constants; use GNATCOLL.OS, GNATCOLL.OS.Constants;

with Ada.Directories;
with Ada.Containers.Hashed_Maps;
with Ada.Environment_Variables;
with Interfaces.C.Strings;

with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;
   package AD renames Ada.Directories;

   procedure Test_F (Dir : Virtual_File);
   --  Perform tests on a specific file

   procedure Test_F (Dir : Virtual_File) is
      D, F        : Virtual_File;
      W           : Writable_File;
      Str         : String_Access;
      Success     : Boolean;
      Dirs, Files : File_Array_Access;
      Root        : Boolean;
   begin
      Root := Ada.Environment_Variables.Value ("USER") = "root";

      --  A file that does not exist yet
      F := Create_From_Dir
        (Dir => Dir, Base_Name => "foo.txt");

      A.Assert (+Base_Name (F), "foo.txt", "base name");
      A.Assert (+Full_Name (F), +Full_Name (Dir) & "foo.txt",
                "full name");
      A.Assert (+Dir_Name (F), +Full_Name (Dir), "dir name");
      A.Assert (+F.File_Extension, ".txt", "file extension");
      A.Assert (Is_Absolute_Path (F), "is absolute");
      A.Assert (not Is_Regular_File (F), "is regular file");
      A.Assert (+Relative_Path (F, Dir), "foo.txt", "relative path");
      A.Assert (Has_Suffix (F, "t"), "has suffix");
      A.Assert (not Is_Symbolic_Link (F), "is symlink");

      --  Now create the file

      W := Write_File (F);
      Write (W, "first word ");
      Close (W);
      W := Write_File (F, Append => True);
      Write (W, Interfaces.C.Strings.New_String ("second word"));
      Close (W);

      --  Check whether the file exists

      A.Assert (Is_Regular_File (F), "is regular file after creation");
      A.Assert (Is_Writable (F), "is writable after creation");

      --  Make the file unreadable

      Set_Readable (F, False);
      A.Assert (Is_Regular_File (F), "is regular file when unreadable");
      A.Assert (not Is_Readable (F) or else OS = Windows or else Root,
                "is readable");

      --  Try and read the file

      Str := Read_File (F);
      A.Assert (Str = null or Root, "can read unreadable file?");
      Free (Str);

      --  Make it readable again, and read again

      Set_Readable (F, True);
      Str := Read_File (F);
      A.Assert (Str.all, "first word second word", "contents when readable");
      A.Assert (Integer (Size (F)), Str.all'Length, "file size");
      Free (Str);

      --  Make the file read-only

      Set_Writable (F, False);
      A.Assert (not Is_Writable (F) or else OS = Windows or else Root,
                "is writable");

      --  Check directory operations

      A.Assert (Is_Directory (Dir), "is directory");

      D := Create_From_Dir (Dir, "sub/sub1");
      Make_Dir (D, Recursive => True);
      W := Write_File (Create_From_Dir (D, "foo"));
      Close (W);
      W := Write_File (Create_From_UTF8 (+Full_Name (D) & "/bar"));
      Close (W);
      Rename (D / "bar", Create_From_Dir (D, "bar.txt"), Success);
      A.Assert (Success, "rename");
      Copy (D, "sub/sub2", Success);
      A.Assert (Success, "copy");
      Remove_Dir (D, Recursive => False, Success => Success);
      A.Assert (not Success, "remove dir (non-recursive)");
      Dirs := Read_Dir_Recursive
         (Create_From_Dir (Dir, "sub"), Filter => Dirs_Only);
      A.Assert (Dirs.all'Length, 2, "dirs found");
      Files := Read_Files_From_Dirs (Dirs.all);
      A.Assert (Files.all'Length, 4, "files found");
      Unchecked_Free (Files);
      Unchecked_Free (Dirs);
      Remove_Dir (D, Recursive => True, Success => Success);
      A.Assert (Success, "remove dir (recursive)");
      Files := Read_Dir_Recursive (Dir, Extension => ".txt");
      A.Assert (Files.all'Length, 2, "txt files found");
      Unchecked_Free (Files);

      --  Delete the file

      Delete (F, Success);
      A.Assert (Success or else OS = Windows, "could delete");
      Delete (F, Success);
      A.Assert (not Success, "could delete again");
   end Test_F;

   Cur_Dir : constant Virtual_File := Get_Current_Dir;
   Cur_Dir_AD : constant String := AD.Current_Directory & Dir_Sep;
begin

   A.Assert (+Dir_Name (Cur_Dir), Cur_Dir_AD, "current directory");
   A.Assert (+Base_Name (Cur_Dir), "", "base name of dir");

   Test_F (Cur_Dir);

   --  Try manipulating No_File

   A.Assert (+Base_Name (No_File), "", "base name");
   A.Assert (+Full_Name (No_File), "", "full name");
   A.Assert (+Dir_Name (No_File), "", "dir name");
   A.Assert (+File_Extension (No_File), "", "file extension");
   A.Assert (not Is_Absolute_Path (No_File), "is absolute");
   A.Assert (not Is_Regular_File (No_File), "is regular file");

   --  Comparisons

   declare
      Default_Pref_Py : constant Virtual_File := Create_From_Dir
        (Dir => Get_Current_Dir, Base_Name => "default_pref.py");
      Default_Pref_Pyc : constant Virtual_File := Create_From_Dir
        (Dir => Get_Current_Dir, Base_Name => "default_pref.pyc");

      package Maps is new Ada.Containers.Hashed_Maps
         (Virtual_File, Integer, Full_Name_Hash, "=");
      use Maps;
      M : Maps.Map;
      C : Maps.Cursor;
      F1, F2 : Virtual_File;
   begin
      A.Assert (Default_Pref_Py < Default_Pref_Pyc,
                "default_pref.py < default_pref.pyc");
      A.Assert (not (Default_Pref_Pyc < Default_Pref_Py),
                "default_pref.pyc < default_pref.py");

      --  Check mapping functionality

      F1 := Create ("/a/b");
      M.Include (F1, 2);

      F2 := Create ("/a/b/c");
      F2 := F2.Get_Parent;    --  "/a/b/"
      C := M.Find (F2);

      A.Assert (Has_Element (C), "find parent in map");
   end;

   --  Normalizing

   declare
      F1 : constant String := Cur_Dir_AD & "obj//../obj/.///./main.o";
      F2 : constant String := +Create_From_Base (+F1).Full_Name;
      F3 : constant String := +Create (+F2, Normalize => True).Full_Name;
   begin
      A.Assert (F3, Cur_Dir_AD & "obj" & Dir_Sep & "main.o");
   end;

   return A.Report;

end Test;
