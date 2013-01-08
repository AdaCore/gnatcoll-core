------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2006-2013, AdaCore                     --
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

with GNAT.Expect;     use GNAT.Expect;
with GNAT.Regpat;     use GNAT.Regpat;
with GNATCOLL.Utils;

package body GNATCOLL.IO.Remote.Unix is

   procedure Free (Args : in out GNAT.OS_Lib.Argument_List);
   --  Free all strings in Args.

   ----------
   -- Free --
   ----------

   procedure Free (Args : in out GNAT.OS_Lib.Argument_List) is
   begin
      for J in Args'Range loop
         Free (Args (J));
      end loop;
   end Free;

   -----------------
   -- Current_Dir --
   -----------------

   function Current_Dir (Exec : access Server_Record'Class) return FS_String
   is
      Args : GNAT.OS_Lib.Argument_List :=
               (1 => new String'("pwd"));
      Output : String_Access;
      Status : Boolean;

   begin
      Exec.Execute_Remotely (Args, Output, Status);
      Free (Args);

      if Status then
         declare
            --  Don't try to translate the string into a directory, as this
            --  is all handled later at VFS level.
            Result : constant FS_String := FS_String (Output.all);
         begin
            Free (Output);
            return Result;
         end;

      else
         return "/";
      end if;
   end Current_Dir;

   --------------
   -- Home_Dir --
   --------------

   function Home_Dir (Exec : access Server_Record'Class) return FS_String
   is
      Args : GNAT.OS_Lib.Argument_List :=
               (new String'("echo"),
                new String'("$HOME"));
      Output : String_Access;
      Status : Boolean;

   begin
      Exec.Execute_Remotely (Args, Output, Status);
      Free (Args);

      if Status then
         declare
            --  Don't try to translate the string into a directory, as this
            --  is all handled later at VFS level.
            Result : constant FS_String := FS_String (Output.all);
         begin
            Free (Output);
            return Result;
         end;

      else
         return "/";
      end if;
   end Home_Dir;

   -------------
   -- Tmp_Dir --
   -------------

   function Tmp_Dir (Exec : access Server_Record'Class) return FS_String
   is
      Args : GNAT.OS_Lib.Argument_List :=
               (new String'("echo"),
                new String'("$TMP"));
      Output : String_Access;
      Status : Boolean;

   begin
      Exec.Execute_Remotely (Args, Output, Status);

      if not Status then
         GNAT.OS_Lib.Free (Args (2));
         Args (2) := new String'("$TMPDIR");
         Exec.Execute_Remotely (Args, Output, Status);
      end if;

      Free (Args);

      if Status then
         declare
            --  Don't try to translate the string into a directory, as this
            --  is all handled later at VFS level.
            Result : constant FS_String := FS_String (Output.all);
         begin
            Free (Output);
            return Result;
         end;

      else
         return "/tmp/";
      end if;
   end Tmp_Dir;

   ------------------------
   -- Get_Logical_Drives --
   ------------------------

   function Get_Logical_Drives
     (Exec : access Server_Record'Class) return String_List_Access
   is
      --  No drives on unix
      pragma Unreferenced (Exec);
   begin
      return null;
   end Get_Logical_Drives;

   --------------------
   -- Locate_On_Path --
   --------------------

   function Locate_On_Path
     (Exec : access Server_Record'Class;
      Base : FS_String) return FS_String
   is
      Args : GNAT.OS_Lib.Argument_List :=
               (new String'("which"),
                new String'(String (Base)));
      Output : String_Access;
      Status : Boolean;

   begin
      Exec.Execute_Remotely (Args, Output, Status);
      Free (Args);

      if Status then
         declare
            --  Don't try to translate the string into a directory, as this
            --  is all handled later at VFS level.
            Result : constant FS_String := FS_String (Output.all);
         begin
            Free (Output);
            return Result;
         end;

      else
         return "";
      end if;
   end Locate_On_Path;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File
     (Exec : access Server_Record'Class;
      File : FS_String)
      return Boolean
   is
      Args : GNAT.OS_Lib.Argument_List :=
                         (new String'("test"),
                          new String'("-r"),
                          new String'("""" & String (File) & """"));
      Status : Boolean;

   begin
      Exec.Execute_Remotely (Args, Status);
      Free (Args);
      return Status;
   end Is_Regular_File;

   ----------
   -- Size --
   ----------

   function Size
     (Exec : access Server_Record'Class;
      File : FS_String) return Long_Integer
   is
      Args : GNAT.OS_Lib.Argument_List :=
                         (new String'("stat"),
                          new String'("-s"),
                          new String'("""" & String (File) & """"));
      Status : Boolean;
      Regexp : constant Pattern_Matcher := Compile ("st_size=(\d+)");
      Output : String_Access;
      Matched : Match_Array (0 .. 1);
      Size    : Long_Integer := 0;

   begin
      Exec.Execute_Remotely (Args, Output, Status);
      Free (Args);

      if Status and then Output /= null then
         Match (Regexp, Output.all, Matched);
         if Matched (1) /= No_Match then
            Size := Long_Integer'Value
              (Output (Matched (1).First .. Matched (1).Last));
         end if;
      end if;

      Free (Output);
      return Size;
   end Size;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory
     (Exec : access Server_Record'Class;
      File : FS_String)
      return Boolean
   is
      Args : GNAT.OS_Lib.Argument_List :=
                         (new String'("test"),
                          new String'("-d"),
                          new String'("""" & String (File) & """"));
      Status : Boolean;

   begin
      Exec.Execute_Remotely (Args, Status);
      Free (Args);
      return Status;
   end Is_Directory;

   ----------------------
   -- Is_Symbolic_Link --
   ----------------------

   function Is_Symbolic_Link
     (Exec : access Server_Record'Class;
      File : FS_String)
      return Boolean
   is
      Args : GNAT.OS_Lib.Argument_List :=
                         (new String'("test"),
                          new String'("-L"),
                          new String'("""" & String (File) & """"));
      Status : Boolean;

   begin
      Exec.Execute_Remotely (Args, Status);
      Free (Args);
      return Status;
   end Is_Symbolic_Link;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp
     (Exec : access Server_Record'Class;
      File : FS_String)
      return Ada.Calendar.Time
   is
      Args : GNAT.OS_Lib.Argument_List :=
        (new String'("ls"),
         new String'("-l"),
         new String'("--time-style=full-iso"),
         new String'("""" & String (File) & """"),
         new String'("2>"),
         new String'("/dev/null"));
      Status  : Boolean;
      Regexp  : constant Pattern_Matcher
        := Compile ("(\d\d\d\d[-]\d\d[-]\d\d)\s+(\d\d:\d\d:\d\d[.]\d+)\s+");
      Matched : Match_Array (0 .. 2);
      Output  : String_Access;
      Year    : Natural;
      Month   : Natural;
      Day     : Natural;
      Hour    : Natural;
      Minute  : Natural;
      Second  : Ada.Calendar.Day_Duration;
      use type Ada.Calendar.Day_Duration;

   begin
      Exec.Execute_Remotely (Args, Output, Status);
      Free (Args);

      if Status and then Output /= null then
         Match (Regexp, Output.all, Matched);
         if Matched (0) = No_Match then
            Free (Output);
            return GNATCOLL.Utils.No_Time;
         end if;
         Year := Natural'Value
           (Output (Matched (1).First .. Matched (1).First + 3));
         Month := Natural'Value
           (Output (Matched (1).First + 5 .. Matched (1).First + 6));
         Day := Natural'Value
           (Output (Matched (1).First + 8 .. Matched (1).First + 9));
         Hour := Natural'Value
           (Output (Matched (2).First .. Matched (2).First + 1));
         Minute := Natural'Value
           (Output (Matched (2).First + 3 .. Matched (2).First + 4));
         Second := Ada.Calendar.Day_Duration'Value
           (Output (Matched (2).First + 6 .. Matched (2).Last));
         Second := Second
           + (60.0 * Ada.Calendar.Day_Duration (Minute))
              + (3600.0 * Ada.Calendar.Day_Duration (Hour));
         Free (Output);
         return Ada.Calendar.Time_Of (Year, Month, Day, Second);
      end if;

      Free (Output);
      return GNATCOLL.Utils.No_Time;
   end File_Time_Stamp;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable
     (Exec : access Server_Record'Class;
      File : FS_String)
      return Boolean
   is
      Args : GNAT.OS_Lib.Argument_List :=
        (new String'("test"),
         new String'("-w"),
         new String'("""" & String (File) & """"));
      Status : Boolean;

   begin
      Exec.Execute_Remotely (Args, Status);
      Free (Args);
      return Status;
   end Is_Writable;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable
     (Exec  : access Server_Record'Class;
      File  : FS_String;
      State : Boolean)
   is
      Args : GNAT.OS_Lib.Argument_List :=
        (1 => new String'("chmod"),
         2 => new String'("u+w"),
         3 => new String'("""" & String (File) & """"));
      Status : Boolean;
      pragma Unreferenced (Status);

   begin
      if not State then
         Args (2).all := "u-w";
      end if;

      Exec.Execute_Remotely (Args, Status);
      Free (Args);
   end Set_Writable;

   ------------------
   -- Set_Readable --
   ------------------

   procedure Set_Readable
     (Exec  : access Server_Record'Class;
      File  : FS_String;
      State : Boolean)
   is
      Args : GNAT.OS_Lib.Argument_List :=
        (1 => new String'("chmod"),
         2 => new String'("u+r"),
         3 => new String'("""" & String (File) & """"));
      Status : Boolean;
      pragma Unreferenced (Status);

   begin
      if not State then
         Args (2).all := "u-r";
      end if;

      Exec.Execute_Remotely (Args, Status);
      Free (Args);
   end Set_Readable;

   ------------
   -- Rename --
   ------------

   procedure Rename
     (Exec    : access Server_Record'Class;
      From    : FS_String;
      Dest    : FS_String;
      Success : out Boolean)
   is
      Args : GNAT.OS_Lib.Argument_List :=
        (new String'("mv"),
         new String'("""" & String (From) & """"),
         new String'("""" & String (Dest) & """"));

   begin
      Exec.Execute_Remotely (Args, Success);
      Free (Args);
   end Rename;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (Exec : access Server_Record'Class;
      From    : FS_String;
      Dest    : FS_String;
      Success : out Boolean)
   is
      Args : GNAT.OS_Lib.Argument_List :=
        (new String'("cp"),
         new String'("-f"),
         new String'("""" & String (From) & """"),
         new String'("""" & String (Dest) & """"));

   begin
      Exec.Execute_Remotely (Args, Success);
      Free (Args);
   end Copy;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Exec    : access Server_Record'Class;
      File    : FS_String;
      Success : out Boolean)
   is
      Args : GNAT.OS_Lib.Argument_List :=
        (new String'("rm"),
         new String'("-f"),
         new String'("""" & String (File) & """"));

   begin
      Exec.Execute_Remotely (Args, Success);
      Free (Args);
   end Delete;

   ---------------------
   -- Read_Whole_File --
   ---------------------

   function Read_Whole_File
     (Exec : access Server_Record'Class;
      File : FS_String)
      return GNAT.Strings.String_Access
   is
      Args : GNAT.OS_Lib.Argument_List :=
        (new String'("cat"),
         new String'("""" & String (File) & """"));
      Status : Boolean;
      Output : String_Access;

   begin
      Exec.Execute_Remotely (Args, Output, Status);
      Free (Args);

      return Output;
   end Read_Whole_File;

   ----------------
   -- Write_File --
   ----------------

   function Write_File
     (Exec    : access Server_Record'Class;
      File    : FS_String;
      Content : String)
      return Boolean
   is
      Pd : Process_Descriptor_Access;
      Args : GNAT.OS_Lib.Argument_List :=
        (1 => new String'("cat"),
         2 => new String'(">"),
         3 => new String'("""" & String (File) & """"),
         4 => new String'("<<"),
         5 => new String'("GPSEOF"));
      Regexp : constant Pattern_Matcher
        := Compile ("[>] ", Single_Line or Multiple_Lines);
      Res    : Expect_Match;

   begin
      Exec.Spawn_Remotely (Descriptor => Pd, Args => Args);

      Send (Pd.all, Content);
      Send (Pd.all, "GPSEOF");
      loop
         Expect (Pd.all, Res, Regexp, 5000);
         if Res = Expect_Timeout then
            Flush (Pd.all);
            Close (Pd.all);
            exit;
         end if;
      end loop;

      Free (Args);
      return True;

   exception
      when Process_Died =>
         Close (Pd.all);
         Free (Args);
         return False;
   end Write_File;

   ----------------
   -- Change_Dir --
   ----------------

   function Change_Dir
     (Exec : access Server_Record'Class;
      Dir  : FS_String)
      return Boolean
   is
      Args : GNAT.OS_Lib.Argument_List :=
        (new String'("cd"),
         new String'("""" & String (Dir) & """"));
      Status : Boolean;

   begin
      Exec.Execute_Remotely (Args, Status);
      Free (Args);

      return Status;
   end Change_Dir;

   --------------
   -- Read_Dir --
   --------------

   function Read_Dir
     (Exec       : access Server_Record'Class;
      Dir        : FS_String;
      Dirs_Only  : Boolean := False;
      Files_Only : Boolean := False)
      return GNAT.Strings.String_List
   is
      function Create_Args return GNAT.OS_Lib.Argument_List;
      --  Return dir arguments following the Dirs_Only and Files_Only arguments

      -----------------
      -- Create_Args --
      -----------------

      function Create_Args return GNAT.OS_Lib.Argument_List is
      begin
         --  Launch with sh to be able to redirect stderr to /dev/null, even
         --  when using (t)csh
         if Dirs_Only then
               return
                 (new String'("sh"),
                  new String'("-c"),
                  new String'("ls -AL1F '" &
                              String (Dir) &
                              "' 2> /dev/null | grep /$"));

         elsif Files_Only then
               return
                 (new String'("sh"),
                  new String'("-c"),
                  new String'("ls -AL1F '" &
                              String (Dir) &
                              "' 2> /dev/null | grep -v /$ | " &
                              "sed -e 's/[*=@\|]$//'"));

         else
            return
              (new String'("sh"),
               new String'("-c"),
               new String'("ls"),
               new String'("-A1"),
               new String'("'" & String (Dir) & "'"));
         end if;
      end Create_Args;

      Args     : GNAT.OS_Lib.Argument_List := Create_Args;
      Status   : Boolean;
      Output   : String_Access;
      Regexp   : constant Pattern_Matcher := Compile ("^(.+)$",
                                                      Multiple_Lines);
      Matched  : Match_Array (0 .. 1);
      Index    : Integer;
      Nb_Files : Natural;

   begin
      Exec.Execute_Remotely (Args, Output, Status);
      Free (Args);

      if Output /= null then
         Index    := Output'First;
         Nb_Files := 0;

         while Index <= Output'Last loop
            Match (Regexp, Output (Index .. Output'Last), Matched);
            exit when Matched (0) = No_Match;
            Index := Matched (1).Last + 1;

            if Output (Matched (1).First .. Matched (1).Last) /= "."
              and then Output (Matched (1).First .. Matched (1).Last) /= ".."
            then
               Nb_Files := Nb_Files + 1;
            end if;
         end loop;

         declare
            List : String_List (1 .. Nb_Files);
            File_Idx : Natural;
         begin
            Index    := Output'First;
            File_Idx := List'First;

            while Index /= Output'Last loop
               Match (Regexp, Output.all, Matched, Index);
               exit when Matched (0) = No_Match;
               Index := Matched (1).Last + 1;

               if Output (Matched (1).First .. Matched (1).Last) /= "."
                 and then Output (Matched (1).First .. Matched (1).Last)
                           /= ".."
               then
                  List (File_Idx) := new String'
                    (Output (Matched (1).First .. Matched (1).Last));
                  File_Idx := File_Idx + 1;
               end if;
            end loop;

            return List;
         end;
      end if;

      Free (Output);

      return (1 .. 0 => null);
   end Read_Dir;

   --------------
   -- Make_Dir --
   --------------

   function Make_Dir
     (Exec      : access Server_Record'Class;
      Dir       : FS_String;
      Recursive : Boolean)
      return Boolean
   is
      Status : Boolean;
      Args : GNAT.OS_Lib.Argument_List :=
        (1 => new String'("mkdir"),
         2 => new String'("-p"),
         3 => new String'("'" & String (Dir) & "'"));

   begin
      if Recursive then
         Exec.Execute_Remotely (Args, Status);
      else
         Exec.Execute_Remotely ((1 => Args (1), 2 => Args (3)), Status);
      end if;

      Free (Args);

      return Status;
   end Make_Dir;

   --------------
   -- Copy_Dir --
   --------------

   procedure Copy_Dir
     (Exec    : access Server_Record'Class;
      From    : FS_String;
      Dest    : FS_String;
      Success : out Boolean)
   is
      Args : GNAT.OS_Lib.Argument_List :=
        (new String'("cp"),
         new String'("-rf"),
         new String'("'" & String (From) & "'"),
         new String'("'" & String (Dest) & "'"));

   begin
      Exec.Execute_Remotely (Args, Success);
      Free (Args);
   end Copy_Dir;

   ----------------
   -- Delete_Dir --
   ----------------

   procedure Delete_Dir
     (Exec      : access Server_Record'Class;
      Dir       : FS_String;
      Recursive : Boolean;
      Success   : out Boolean)
   is
      Args : GNAT.OS_Lib.Argument_List :=
        (1 => new String'("rm"),
         2 => new String'("-r"),
         3 => new String'("'" & String (Dir) & "'"));

   begin
      if Recursive then
         Free (Args (2));
         Args (2) := new String'("-rf");
      end if;

      Exec.Execute_Remotely (Args, Success);
      Free (Args);
   end Delete_Dir;

end GNATCOLL.IO.Remote.Unix;
