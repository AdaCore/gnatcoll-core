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
with GNATCOLL.OS.Constants;

with Ada.Directories;

with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;
   package AD renames Ada.Directories;
   package OSC renames GNATCOLL.OS.Constants;

   procedure Test_F (Dir : Virtual_File);
   --  Perform tests on a specific file

   procedure Test_F (Dir : Virtual_File) is
      D, F    : Virtual_File;
      W       : Writable_File;
      Str     : String_Access;
      Success : Boolean;
      Files   : File_Array_Access;
   begin
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

      --  Now create the file

      W := Write_File (F);
      Write (W, "first word ");
      Write (W, "second word");
      Close (W);

      --  Check whether the file exists

      A.Assert (Is_Regular_File (F), "is regular file after creation");
      A.Assert (Is_Writable (F), "is writable after creation");

      --  Make the file unreadable

      Set_Readable (F, False);
      A.Assert (Is_Regular_File (F), "is regular file when unreadable");

      --  Try and read the file

      Str := Read_File (F);
      if Str /= null then
         A.Assert (False, "can read unreadable file?");
      end if;
      Free (Str);

      --  Make it readable again, and read again

      Set_Readable (F, True);
      Str := Read_File (F);
      A.Assert (Str.all, "first word second word", "contents when readable");
      Free (Str);

      --  Check contents of directory

      A.Assert (Is_Directory (Dir), "is directory");

      D := Create_From_Dir (Dir, "sub/sub1");
      Make_Dir (D, Recursive => True);
      W := Write_File (Create_From_Dir (D, "foo"));
      Close (W);
      W := Write_File (Create_From_Dir (D, "bar.txt"));
      Close (W);
      Files := Read_Dir_Recursive (D, Extension => ".txt");
      A.Assert (Files.all'Length, 1, "files found");
      Unchecked_Free (Files);

      --  Delete the file

      Delete (F, Success);
      A.Assert (Success, "could delete");
      Delete (F, Success);
      A.Assert (not Success, "could delete again");
   end Test_F;

   Cur_Dir : constant Virtual_File := Get_Current_Dir;
   Cur_Dir_AD : constant String := AD.Current_Directory & OSC.Dir_Sep;
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
   begin
      A.Assert (Default_Pref_Py < Default_Pref_Pyc,
                "default_pref.py < default_pref.pyc");
      A.Assert (not (Default_Pref_Pyc < Default_Pref_Py),
                "default_pref.pyc < default_pref.py");
   end;

   return A.Report;

end Test;
