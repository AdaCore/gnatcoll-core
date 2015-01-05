------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2006-2015, AdaCore                     --
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

with GNATCOLL.Utils;
with GNAT.Regpat;     use GNAT.Regpat;

package body GNATCOLL.IO.Remote.Windows is

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
      Args   : GNAT.OS_Lib.Argument_List :=
                 (1 => new String'("echo"),
                  2 => new String'("%CD%"));
      Output : GNAT.OS_Lib.String_Access;
      Status : Boolean;

   begin
      Exec.Execute_Remotely (Args, Output, Status);
      Free (Args);

      if Status then
         declare
            Result : constant FS_String := FS_String (Output.all);
         begin
            Free (Output);
            return Result;
         end;

      else
         return "C:\";
      end if;
   end Current_Dir;

   --------------
   -- Home_Dir --
   --------------

   function Home_Dir (Exec : access Server_Record'Class) return FS_String
   is
      Args   : GNAT.OS_Lib.Argument_List :=
                 (1 => new String'("echo"),
                  2 => new String'("%HOME%"));
      Output : GNAT.OS_Lib.String_Access;
      Status : Boolean;

   begin
      Exec.Execute_Remotely (Args, Output, Status);

      if not Status then
         GNAT.OS_Lib.Free (Args (2));
         Args (2) := new String'("%USERPROFILE%");
         Exec.Execute_Remotely (Args, Output, Status);
      end if;

      Free (Args);

      if Status then
         declare
            Result : constant FS_String := FS_String (Output.all);
         begin
            Free (Output);
            return Result;
         end;

      else
         return "C:\";
      end if;
   end Home_Dir;

   -------------
   -- Tmp_Dir --
   -------------

   function Tmp_Dir (Exec : access Server_Record'Class) return FS_String
   is
      Args   : GNAT.OS_Lib.Argument_List :=
                 (1 => new String'("echo"),
                  2 => new String'("%TMP%"));
      Output : GNAT.OS_Lib.String_Access;
      Status : Boolean;

   begin
      Exec.Execute_Remotely (Args, Output, Status);

      if not Status then
         GNAT.OS_Lib.Free (Args (2));
         Args (2) := new String'("%TMPDIR%");
         Exec.Execute_Remotely (Args, Output, Status);
      end if;

      Free (Args);

      if Status then
         declare
            Result : constant FS_String := FS_String (Output.all);
         begin
            Free (Output);
            return Result;
         end;

      else
         return "C:\tmp\";
      end if;
   end Tmp_Dir;

   ------------------------
   -- Get_Logical_Drives --
   ------------------------

   function Get_Logical_Drives
     (Exec : access Server_Record'Class) return String_List_Access
   is
      Status : Boolean;
      Args   : GNAT.OS_Lib.Argument_List :=
                 (1 => new String'("vol"),
                  2 => null,
                  3 => new String'("2>&1"));
      Ret    : String_List (1 .. 24);
      Idx    : Natural := Ret'First;

   begin
      for Drive in Character'('C') .. Character'('Z') loop
         Args (2) := new String'(Drive & ":");
         Exec.Execute_Remotely (Args, Status);
         Free (Args (2));

         if Status then
            Ret (Idx) := new String'(Drive & ":");
            Idx := Idx + 1;
         end if;
      end loop;

      Free (Args);

      return new String_List'(Ret (1 .. Idx - 1));
   end Get_Logical_Drives;

   --------------------
   -- Locate_On_Path --
   --------------------

   function Locate_On_Path
     (Exec : access Server_Record'Class;
      Base : FS_String) return FS_String
   is
      function Get_Base return String;

      function Get_Base return String is
      begin
         if Base'Length < 4
           or else Base (Base'Last - 3 .. Base'Last) /= ".exe"
         then
            return String (Base) & ".exe";
         else
            return String (Base);
         end if;
      end Get_Base;

      Args : GNAT.OS_Lib.Argument_List :=
               (new String'("for"),
                new String'("/f"),
                new String'("""usebackq"""),
                new String'("%i"),
                new String'("in"),
                new String'("('" & Get_Base & "')"),
                new String'("do"),
                new String'("@echo"),
                new String'("%~dp$PATH:i%i"));
      Output : String_Access;
      Status : Boolean;

   begin
      Exec.Execute_Remotely (Args, Output, Status);
      Free (Args);

      if Status then
         if Output.all /= Get_Base then
            declare
               --  Don't try to translate the string into a directory, as this
               --  is all handled later at VFS level.
               Result : constant FS_String := FS_String (Output.all);
            begin
               Free (Output);
               return Result;
            end;
         else
            Free (Output);
            return "";
         end if;
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
      --  Redirect stderr to stdout for synchronisation purpose
      --  (stderr is asynchronous on windows)
      Args   : GNAT.OS_Lib.Argument_List :=
                 (new String'("dir"),
                  new String'("/a-d"),
                  new String'("""" & String (File) & """"),
                  new String'("2>&1"));
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
               (new String'("dir"),
                new String'("/-C"),
                new String'("""" & String (File) & """"),
                new String'("2>&1"));
      Status : Boolean;
      Size   : Long_Integer := 0;
      Output : GNAT.Strings.String_Access;
      S : GNAT.Strings.String_List_Access;

   begin
      Exec.Execute_Remotely (Args, Output, Status);
      Free (Args);

      if Status and Output /= null then
         S := GNATCOLL.Utils.Split (Output.all, ' ');

         begin
            Size := Long_Integer'Value (S (S'First + 2).all);
         exception
            when Constraint_Error =>
               Size := 0;
         end;

         Free (S);
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
               (new String'("dir"),
                new String'("/ad"),
                new String'("""" & String (File) & """"),
                new String'("2>&1"));
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
      pragma Unreferenced (Exec, File);
   begin
      --  ??? There are now symbolic links on Windows (Vista, Server 2008).
      --  Should we handle them ?
      return False;
   end Is_Symbolic_Link;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp
     (Exec : access Server_Record'Class;
      File : FS_String)
      return Ada.Calendar.Time
   is
      Query_Args   : GNAT.OS_Lib.Argument_List :=
                       (new String'("reg"),
                        new String'("query"),
                        new String'("""HKCU\Control Panel\International"""),
                        new String'("/v"),
                        new String'("sShortDate"));
      Query_Regexp : constant Pattern_Matcher
        := Compile ("REG_SZ\s+([dMy]+)[^dMy]*([dMy]+)[^dMy]*([dMy]+)$");

      Regexp       : constant Pattern_Matcher
        := Compile ("(\d+)[^\d](\d+)[^\d](\d+)\s+(\d\d:\d\d)\s+");
      Args         : GNAT.OS_Lib.Argument_List :=
                       (new String'("dir"),
                        new String'("/tw"),
                        new String'("/4"),
                        new String'("""" & String (File) & """"),
                        new String'("2>"),
                        new String'("/dev/null"));
      Status       : Boolean;
      Matched      : Match_Array (0 .. 4);
      Output       : String_Access;
      Year         : Natural;
      Month        : Natural;
      Day          : Natural;
      Hour         : Natural;
      Minute       : Natural;
      Y_Pos        : Natural := 0;
      M_Pos        : Natural := 0;
      D_Pos        : Natural := 0;
      use Ada.Calendar;

   begin
      --  We need to first query the date format
      --  (dd/MM/ yyyy, yyyy-mm-dd, mm-dd-yyyy ?)
      Exec.Execute_Remotely (Query_Args, Output, Status);
      Free (Query_Args);

      if Status then
         Match (Query_Regexp, Output.all, Matched);

         if Matched (0) /= No_Match then
            for J in 1 .. 3 loop
               if Output (Matched (J).First) = 'd' then
                  D_Pos := J;
               elsif Output (Matched (J).First) = 'M' then
                  M_Pos := J;
               elsif Output (Matched (J).First) = 'y' then
                  Y_Pos := J;
               end if;
            end loop;
         end if;
      end if;

      Free (Output);

      if D_Pos = 0 or else M_Pos = 0 or else Y_Pos = 0 then
         return GNATCOLL.Utils.No_Time;
      end if;

      Exec.Execute_Remotely (Args, Output, Status);
      Free (Args);

      if Status then
         Match (Regexp, Output.all, Matched);

         if Matched (0) = No_Match then
            return GNATCOLL.Utils.No_Time;
         end if;
         Year := Natural'Value
           (Output (Matched (Y_Pos).First .. Matched (Y_Pos).Last));
         Month := Natural'Value
           (Output (Matched (M_Pos).First .. Matched (M_Pos).Last));
         Day := Natural'Value
           (Output (Matched (D_Pos).First .. Matched (D_Pos).Last));
         Hour := Natural'Value
           (Output (Matched (4).First .. Matched (4).First + 1));
         Minute := Natural'Value
           (Output (Matched (4).First + 3 .. Matched (4).First + 4));

         Free (Output);
         return Ada.Calendar.Time_Of
           (Year, Month, Day,
            Seconds => 60.0 * 60.0 * Day_Duration (Hour) +
              60.0 * Day_Duration (Minute));
      end if;

      Free (Output);
      return GNATCOLL.Utils.No_Time;

   exception
      when others =>
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
        (new String'("dir"),
         new String'("/a-r"),
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
     (Exec : access Server_Record'Class;
      File  : FS_String;
      State : Boolean)
   is
      Args   : GNAT.OS_Lib.Argument_List :=
                 (new String'("attrib"),
                  new String'("-r"),
                  new String'("""" & String (File) & """"),
                  new String'("2>&1"));
      Status : Boolean;
      pragma Unreferenced (Status);

   begin
      if not State then
         Args (2) (Args (2)'First) := '+';
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
      pragma Unreferenced (Exec, File, State);
   begin
      --  A file cannot be unreadable on Windows, unless you use ACL (which we
      --  don't here)
      return;
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
      Args  : GNAT.OS_Lib.Argument_List :=
                (new String'("ren"),
                 new String'("""" & String (From) & """"),
                 new String'("""" & String (Dest) & """"),
                 new String'("2>&1"));
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
      Args  : GNAT.OS_Lib.Argument_List :=
                (new String'("copy"),
                 new String'("""" & String (From) & """"),
                 new String'("""" & String (Dest) & """"),
                 new String'("2>&1"));

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
      Args  : GNAT.OS_Lib.Argument_List :=
                (new String'("erase"),
                 new String'("/f"),
                 new String'("""" & String (File) & """"),
                 new String'("2>&1"));

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
      Args   : GNAT.OS_Lib.Argument_List :=
                 (new String'("type"),
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
      Success : Boolean := True;
      procedure Internal_Write (S : String; First_Line : Boolean);
      --  Write a single line

      procedure Internal_Write (S : String; First_Line : Boolean) is
         Args  : GNAT.OS_Lib.Argument_List :=
                   (new String'("echo"),
                    new String'(S),
                    new String'(">>"),
                    new String'("""" & String (File) & """"));

      begin
         if First_Line then
            Args (3).all := "> ";
         end if;

         Exec.Execute_Remotely (Args, Success);
         Free (Args);
      end Internal_Write;

      Last : Natural := Content'First;
      Idx  : Natural := Content'First;
   begin
      while Idx <= Content'Last loop
         if Content (Idx) = ASCII.CR
           and then Content (Idx + 1) = ASCII.LF
         then
            Internal_Write (Content (Last .. Idx - 1), Last = Content'First);
            exit when not Success;
            Last := Idx + 2;
            Idx  := Idx + 2;

         elsif Content (Idx) = ASCII.LF then
            Internal_Write (Content (Last .. Idx - 1), Last = Content'First);
            exit when not Success;
            Last := Idx + 1;
            Idx  := Idx + 1;

         elsif Idx = Content'Last then
            Internal_Write (Content (Last .. Idx), Last = Content'First);
            exit when not Success;

         else
            Idx := Idx + 1;
         end if;
      end loop;

      return Success;
   end Write_File;

   ----------------
   -- Change_Dir --
   ----------------

   function Change_Dir
     (Exec : access Server_Record'Class;
      Dir  : FS_String)
      return Boolean
   is
      Args  : GNAT.OS_Lib.Argument_List :=
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
         if Dirs_Only then
            return (new String'("dir"),
                    new String'("/ad"),
                    new String'("/b"),
                    new String'(String (Dir)),
                    new String'("2>&1"));
         elsif Files_Only then
            return (new String'("dir"),
                    new String'("/a-d"),
                    new String'("/b"),
                    new String'(String (Dir)),
                    new String'("2>&1"));
         else
            return (new String'("dir"),
                    new String'("/b"),
                    new String'(String (Dir)),
                    new String'("2>&1"));
         end if;
      end Create_Args;

      Args     : GNAT.OS_Lib.Argument_List := Create_Args;
      Status   : Boolean;
      Output   : String_Access;
      Regexp   : constant Pattern_Matcher :=
                   Compile ("^(.+)$", Multiple_Lines);
      Matched  : Match_Array (0 .. 1);
      Index    : Natural;
      Nb_Files : Natural;

   begin
      Exec.Execute_Remotely (Args, Output, Status);
      Free (Args);

      if Status then
         Index    := Output'First;
         Nb_Files := 0;

         while Index <= Output'Last loop
            Match (Regexp, Output.all, Matched, Index);
            exit when Matched (0) = No_Match;
            Index := Matched (1).Last + 1;

            if Output (Matched (1).First .. Matched (1).Last) /= "."
              and then Output (Matched (1).First .. Matched (1).Last) /= ".."
            then
               Nb_Files := Nb_Files + 1;
            end if;
         end loop;

         declare
            List     : String_List (1 .. Nb_Files);
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
      Args   : GNAT.OS_Lib.Argument_List :=
                 (new String'("mkdir"),
                  new String'("""" & String (Dir) & """"),
                  new String'("2>&1"));
      Status : Boolean;
      pragma Unreferenced (Recursive);
      --  There is no non-recursive mkdir on Windows

   begin
      Exec.Execute_Remotely (Args, Status);
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
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
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
      Args   : GNAT.OS_Lib.Argument_List :=
                 (1 => new String'("rmdir"),
                  2 => new String'("/q"),
                  3 => new String'("""" & String (Dir) & """"),
                  4 => new String'("2>&1"));

   begin
      if Recursive then
         Free (Args (2));
         Args (2) := new String'("/q/s");
      end if;

      Exec.Execute_Remotely (Args, Success);
      Free (Args);
   end Delete_Dir;

end GNATCOLL.IO.Remote.Windows;
