------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2012, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;               use Ada.Calendar;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Text_IO;                use Ada.Text_IO;
with GNAT.Command_Line;          use GNAT.Command_Line;
with GNAT.Strings;               use GNAT.Strings;
with GNAT.OS_Lib;
with GNATCOLL.Xref;              use GNATCOLL.Xref;
with GNATCOLL.Arg_Lists;         use GNATCOLL.Arg_Lists;
with GNATCOLL.Paragraph_Filling;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Readline;          use GNATCOLL.Readline;
with GNATCOLL.SQL.Sqlite;        use GNATCOLL.SQL.Sqlite;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

procedure GNATInspect is
   Me : constant Trace_Handle := Create ("Inspect");
   use File_Sets;

   function Command_Line_Completion
     (Full_Line, Text : String; Start, Last : Integer)
      return Possible_Completions;
   --  Provides interactive command-line completion

   function Complete_Command (Text : String; State : Integer) return String;
   --  Find all commands starting with Text

   Invalid_Command : exception;
   procedure Process_Line (Line : String);
   procedure Process_File (File : String);
   --  Process a full line of commands.
   --  Raise Invalid_Command when the command is invalid.

   procedure On_Ctrl_C;
   pragma Convention (C, On_Ctrl_C);
   --  Handler for control-c, to make sure that the history is properly
   --  saved.

   type Ctrl_C_Handler is access procedure;
   pragma Convention (C, Ctrl_C_Handler);
   --  Any parameterless library level procedure can be used as a handler.
   --  Ctrl_C_Handler should not propagate exceptions.

   procedure Install_Ctrl_C_Handler (Handler : Ctrl_C_Handler);
   pragma Import (C, Install_Ctrl_C_Handler, "__gnat_install_int_handler");
   --  Set up Handler to be called if the operator hits Ctrl-C

   function Get_Entity (Arg : String) return Entity_Information;
   --  Return the entity matching the "name:file:line:column" argument

   procedure Process_Body (Args : Arg_List);
   procedure Process_Calls (Args : Arg_List);
   procedure Process_Callers (Args : Arg_List);
   procedure Process_Child_Types (Args : Arg_List);
   procedure Process_Decl (Args : Arg_List);
   procedure Process_Depends_On (Args : Arg_List);
   procedure Process_Help (Args : Arg_List);
   procedure Process_Importing (Args : Arg_List);
   procedure Process_Imports (Args : Arg_List);
   procedure Process_Methods (Args : Arg_List);
   procedure Process_Name (Args : Arg_List);
   procedure Process_Params (Args : Arg_List);
   procedure Process_Parent_Types (Args : Arg_List);
   procedure Process_Project (Args : Arg_List);
   procedure Process_Refresh (Args : Arg_List);
   procedure Process_Refs (Args : Arg_List);
   procedure Process_Shell (Args : Arg_List);
   --  Process the various commands.
   --  Args is the command line entered by the user, so Get_Command (Args) for
   --  instance is the command being executed.

   procedure Set_Variable (Name, Value : String);
   --  Change the value of a variable

   function Image (File : Virtual_File) return String;
   function Image (Self : Entity_Information) return String;
   --  Return a display version of the argument

   procedure Dump (Curs : in out Files_Cursor);
   procedure Dump (Curs : in out Entities_Cursor);
   procedure Dump (Refs : in out References_Cursor; Name : String);
   procedure Dump (Curs : File_Sets.Set);
   --  Display the list of files

   procedure Output_Prefix (Count : in out Natural);
   --  Print the prefix for each output line

   type Command_Descr is record
      Name    : GNAT.Strings.String_Access;
      Args    : GNAT.Strings.String_Access;
      Help    : GNAT.Strings.String_Access;
      Handler : access procedure (Args : Arg_List);
   end record;

   Commands : constant array (Natural range <>) of Command_Descr :=
     ((new String'("importing"),
       new String'("filename"),
       new String'("List the files that import the file (via with statements"
         & " in Ada or #include in C for instance)"),
       Process_Importing'Access),

      (new String'("imports"),
       new String'("filename"),
       new String'("List the files that the file imports (via with statements"
         & " in Ada or #include in C for instance). See also 'depends_on'"),
       Process_Imports'Access),

      (new String'("child_types"),
       new String'("name:file:line:column"),
       new String'("The list of child types of the entity (for instance"
         & " classes that inherit from the entity). See also 'parent_types'"),
       Process_Child_Types'Access),

      (new String'("parent_types"),
       new String'("name:file:line:column"),
       new String'("The parent types of the entity (for instance the classes"
           & " or interfaces from which it derives). See also 'child_types'"),
       Process_Parent_Types'Access),

      (new String'("methods"),
       new String'("name:file:line:column"),
       new String'("Returns the list of methods (or primitive operations) for"
           & " the entity"),
       Process_Methods'Access),

      (new String'("depends"),
       new String'("filename"),
       new String'("List the files that the file depends on (recursively"
           & " calling 'imports'"),
       Process_Depends_On'Access),

      (new String'("calls"),
       new String'("name:file:line:column"),
       new String'("List all entities called by the entity"),
       Process_Calls'Access),

      (new String'("callers"),
       new String'("name:file:line:column"),
       new String'("List all entities that call the entity. This information"
         & " also available from a call to 'refs', but 'callers' return the"
         & " callers directly, instead of references to the original entity"),
       Process_Callers'Access),

      (new String'("decl"),
       new String'("name:file:line:column"),
       new String'("Print the location of the declaration for the entity"
           & " referenced at the given location"),
       Process_Decl'Access),

      (new String'("body"),
       new String'("name:file:line:column"),
       new String'("Print the location of the body for the entity"
           & " referenced at the given location"),
       Process_Body'Access),

      (new String'("help"),
       new String'("[command or variable name]"),
       new String'("Display the list of commands and their syntax."),
       Process_Help'Access),

      (new String'("qname"),
       new String'("name:file:line:column"),
       new String'("Return the fully qualified name for the entity"),
       Process_Name'Access),

      (new String'("params"),
       new String'("name:file:line:column"),
       new String'("Return the list of parameters for the subprogram"),
       Process_Params'Access),

      (new String'("refresh"),
       null,
       new String'("Refresh the contents of the xref database."),
       Process_Refresh'Access),

      (new String'("refs"),
       new String'("name:file:line:column"),
       new String'("Display all known references to the entity."),
       Process_Refs'Access),

      (new String'("shell"),
       null,
       new String'("Execute a shell command (an alternative is to use '!'"
           & " as the command."),
       Process_Shell'Access),

      (new String'("time"),
       new String'("command arguments"),
       new String'("Execute the command as usual, and report the time it took"
         & " to execute it"),
       null));

   type Variable_Descr is record
      Name : GNAT.Strings.String_Access;
      Help : GNAT.Strings.String_Access;
   end record;

   Variables : constant array (Natural range <>) of Variable_Descr :=
     (1 => (new String'("absolute_paths"),
            new String'("If True, display absolute file names, otherwise"
              & " display base names only")),
      2 => (new String'("runtime"),
            new String'("Whether to include runtime files in the database")));

   History_File : GNAT.Strings.String_Access;

   Complete_Command_List_Index : Integer;
   --  Global variable used by Complete_Command

   Xref    : Xref_Database;
   --  The xref database

   Project_Is_Default : Boolean := True;  --  Whether we have the default prj
   Env     : Project_Environment_Access;
   Tree    : Project_Tree;
   --  The currently loaded project tree

   Cmdline               : Command_Line_Configuration;
   Commands_From_Switch  : aliased GNAT.Strings.String_Access;
   Commands_From_File    : aliased GNAT.Strings.String_Access;
   DB_Name               : aliased GNAT.Strings.String_Access :=
     new String'("gnatinspect.db");
   Nightly_DB_Name       : aliased GNAT.Strings.String_Access;
   Include_Runtime_Files : aliased Boolean;
   Display_Full_Paths    : aliased Boolean;
   Verbose               : aliased Boolean;
   Project_Name          : aliased GNAT.Strings.String_Access;
   --  The options from the command line

   ----------------------
   -- Complete_Command --
   ----------------------

   function Complete_Command (Text : String; State : Integer) return String is
      C : Integer;
      Tx : constant String := To_Lower (Text);

   begin
      if State = 0 then
         Complete_Command_List_Index := Commands'First;
      end if;

      while Complete_Command_List_Index <= Commands'Last loop
         C := Complete_Command_List_Index;
         Complete_Command_List_Index := Complete_Command_List_Index + 1;

         if Starts_With (Commands (C).Name.all, Tx) then
            return Commands (C).Name.all;
         end if;
      end loop;

      loop
         C :=
           Variables'First + Complete_Command_List_Index - Commands'Last - 1;
         exit when C > Variables'Last;
         Complete_Command_List_Index := Complete_Command_List_Index + 1;

         if Starts_With (Variables (C).Name.all, Tx) then
            return Variables (C).Name.all & ":=";
         end if;
      end loop;

      return "";
   end Complete_Command;

   -----------------------------
   -- Command_Line_Completion --
   -----------------------------

   function Command_Line_Completion
     (Full_Line, Text : String; Start, Last : Integer)
      return Possible_Completions
   is
      pragma Unreferenced (Last);
   begin
      if Start = 0 then
         return Completion_Matches
           (Text, Complete_Command'Unrestricted_Access);

      elsif Ada.Strings.Fixed.Trim
        (Full_Line (Full_Line'First .. Start - 1 + Full_Line'First),
         Ada.Strings.Both) = "help"
      then
         return Completion_Matches
           (Text, Complete_Command'Unrestricted_Access);
      else
         return null;  --  default completion from readline.
      end if;
   end Command_Line_Completion;

   -----------
   -- Image --
   -----------

   function Image (File : Virtual_File) return String is
   begin
      if Display_Full_Paths then
         return File.Display_Full_Name;
      else
         return +File.Base_Name;
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Self : Entity_Information) return String is
      Decl   : Entity_Declaration;
   begin
      Decl := Xref.Declaration (Self);
      return To_String (Decl.Name) & ":"
        & Image (Decl.Location.File) & ":"
        & Image (Decl.Location.Line, Min_Width => 0)
        & ':'
        & Image (Decl.Location.Column, Min_Width => 0);
   end Image;

   ------------------
   -- Process_Help --
   ------------------

   procedure Process_Help (Args : Arg_List) is
      Display_Section : Boolean := False;
   begin
      for C in Commands'Range loop
         if Args_Length (Args) = 0
           or else Nth_Arg (Args, 1) = Commands (C).Name.all
         then
            Put ("  " & Commands (C).Name.all);
            if Commands (C).Args = null then
               New_Line;
            else
               Put_Line (" " & Commands (C).Args.all);
            end if;

            Put
              (Ada.Strings.Unbounded.To_String
                 (GNATCOLL.Paragraph_Filling.Knuth_Fill
                    (Commands (C).Help.all,
                     Max_Line_Length => 70,
                     Line_Prefix     => "      ")));
         end if;
      end loop;

      for C in Variables'Range loop
         if Args_Length (Args) = 0
           or else Nth_Arg (Args, 1) = Commands (C).Name.all
         then
            if not Display_Section then
               New_Line;
               Put_Line ("  == Variable ==");
               Display_Section := True;
            end if;

            Put_Line ("  " & Variables (C).Name.all);
            Put
              (Ada.Strings.Unbounded.To_String
                 (GNATCOLL.Paragraph_Filling.Knuth_Fill
                    (Variables (C).Help.all,
                     Max_Line_Length => 70,
                     Line_Prefix     => "      ")));
         end if;
      end loop;
   end Process_Help;

   ---------------------
   -- Process_Project --
   ---------------------

   procedure Process_Project (Args : Arg_List) is
      GNAT_Version : GNAT.Strings.String_Access;
   begin
      Initialize (Env);
      Env.Set_Path_From_Gnatls
        (Gnatls       => "gnatls",
         GNAT_Version => GNAT_Version,
         Errors       => Put_Line'Access);
      Free (GNAT_Version);

      Env.Register_Default_Language_Extension
        (Language_Name       => "C",
         Default_Spec_Suffix => ".h",
         Default_Body_Suffix => ".c");
      Free (GNAT_Version);

      if Args_Length (Args) < 1 then
         Trace (Me, "processing 'PROJECT' empty");
         Project_Is_Default := True;
         Tree.Load_Empty_Project
           (Env               => Env,
            Name              => "default",
           Recompute_View     => False);
         Tree.Root_Project.Set_Attribute
           (Source_Dirs_Attribute,
            Values => (1 => new String'(".")));
         Tree.Root_Project.Set_Attribute
           (Languages_Attribute, (1 => new String'("Ada")));
         Tree.Recompute_View (Errors => Ada.Text_IO.Put_Line'Access);
      else
         Trace (Me, "processing 'PROJECT' '" & Nth_Arg (Args, 1) & "'");
         Project_Is_Default := False;
         Tree.Load
           (Root_Project_Path => Create (+Nth_Arg (Args, 1)),
            Env               => Env,
            Errors            => Put_Line'Access);
      end if;

   exception
      when GNATCOLL.Projects.Invalid_Project =>
         Put_Line ("Error: invalid project file: '"
                   & Nth_Arg (Args, 1) & "'");
   end Process_Project;

   ---------------------
   -- Process_Refresh --
   ---------------------

   procedure Process_Refresh (Args : Arg_List) is
      pragma Unreferenced (Args);
   begin
      if Env /= null then
         Xref.Parse_All_LI_Files
           (Tree                => Tree,
            Project             => Tree.Root_Project,
            Parse_Runtime_Files => not Project_Is_Default
              and then Include_Runtime_Files,
            From_DB_Name        => Nightly_DB_Name.all,
            To_DB_Name          => DB_Name.all);
      end if;
   end Process_Refresh;

   -------------------
   -- Process_Shell --
   -------------------

   procedure Process_Shell (Args : Arg_List) is
      Cmd     : constant String := Nth_Arg (Args, 1);
      Command : GNAT.Strings.String_Access;
   begin
      Command := GNAT.OS_Lib.Locate_Exec_On_Path (Cmd);
      if Command = null then
         Put_Line ("Cannot locate '" & Cmd & "' on PATH");
         return;
      end if;

      declare
         Arguments : constant String_List :=
           To_List (Args, Include_Command => False);
         Success : Boolean;
      begin
         GNAT.OS_Lib.Spawn
           (Command.all, Arguments (Arguments'First + 1 .. Arguments'Last),
            Success);
         Free (Command);

         if not Success then
            Put_Line
              ("Error: failed to execute '"
               & To_Display_String (Args, Include_Command => False) & "'");
         end if;
      end;
   end Process_Shell;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity (Arg : String) return Entity_Information is
      Words  : String_List_Access := Split (Arg, On => ':');
      Entity : Entity_Information := No_Entity;
   begin
      if Words'Length = 4 then
         Entity := Xref.Get_Entity
           (Name   => Words (Words'First).all,
            File   => Words (Words'First + 1).all,
            Line   => Integer'Value (Words (Words'First + 2).all),
            Column => Integer'Value (Words (Words'First + 3).all));
      elsif Words'Length = 3 then
         Entity := Xref.Get_Entity
           (Name   => Words (Words'First).all,
            File   => Words (Words'First + 1).all,
            Line   => Integer'Value (Words (Words'First + 2).all));
      elsif Words'Length = 2 then
         Entity := Xref.Get_Entity
           (Name   => Words (Words'First).all,
            File   => Words (Words'First + 1).all);
      else
         Put_Line ("Invalid parameter, expecting name:file:line:column => '"
                   & Arg & "'");
         Free (Words);
         return No_Entity;
      end if;

      Free (Words);

      if Entity = No_Entity then
         Put_Line ("Error: entity not found '" & Arg & "'");
      end if;

      if Is_Fuzzy_Match (Entity) then
         Put_Line ("   fuzzy match for the entity");
      end if;

      return Entity;
   end Get_Entity;

   -------------------
   -- Output_Prefix --
   -------------------

   procedure Output_Prefix (Count : in out Natural) is
   begin
      if Verbose then
         Put (" ");
         Put (Image (Count, Min_Width => 3, Padding => ' '));
         Put ("> ");
         Count := Count + 1;
      end if;
   end Output_Prefix;

   ----------
   -- Dump --
   ----------

   procedure Dump (Refs : in out References_Cursor; Name : String) is
      Ref    : Entity_Reference;
      Count  : Natural := 1;
   begin
      while Has_Element (Refs) loop
         Ref := Refs.Element;
         Output_Prefix (Count);
         Put (Name & ':' & Image (Ref.File) & ":"
              & Image (Ref.Line, Min_Width => 0)
              & ':'
              & Image (Ref.Column, Min_Width => 0)
              & " (" & To_String (Ref.Kind) & ")");

         if Ref.Scope /= No_Entity then
            Put_Line (" scope=" & Image (Ref.Scope));
         else
            New_Line;
         end if;

         Next (Refs);
      end loop;
   end Dump;

   ------------------
   -- Process_Refs --
   ------------------

   procedure Process_Refs (Args : Arg_List) is
      Entity : Entity_Information;
      Refs   : References_Cursor;
   begin
      if Args_Length (Args) /= 1 then
         Put_Line ("Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));
      Refs := Xref.References (Entity);
      Dump (Refs, To_String (Xref.Declaration (Entity).Name));
   end Process_Refs;

   --------------------
   -- Process_Params --
   --------------------

   procedure Process_Params (Args : Arg_List) is
      Entity : Entity_Information;
      Ents   : Parameters_Cursor;
      Param  : Parameter_Information;
      Count  : Natural := 1;
   begin
      if Args_Length (Args) /= 1 then
         Put_Line ("Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));

      Ents := Xref.Parameters (Entity);
      while Has_Element (Ents) loop
         Param  := Ents.Element;
         Output_Prefix (Count);
         Put_Line (Image (Param.Parameter)
                   & " (" & Param.Kind'Img & ")");
         Next (Ents);
      end loop;
   end Process_Params;

   ------------------
   -- Set_Variable --
   ------------------

   procedure Set_Variable (Name, Value : String) is
      N : constant String :=
        To_Lower (Ada.Strings.Fixed.Trim (Name, Ada.Strings.Both));
      V : constant String := Ada.Strings.Fixed.Trim (Value, Ada.Strings.Both);
      B : Boolean;

      Invalid_Value : exception;

      function To_Boolean (V : String) return Boolean;
      function To_Boolean (V : String) return Boolean is
      begin
         return Boolean'Value (V);
      exception
         when Constraint_Error =>
            Put_Line ("Error: Expected boolean, got '" & V & "'");
            raise Invalid_Value;
      end To_Boolean;

   begin
      if N = "absolute_paths" then
         Display_Full_Paths := To_Boolean (V);
      elsif N = "runtime" then
         B := To_Boolean (V);
         if B /= Include_Runtime_Files then
            Include_Runtime_Files := B;
            Process_Refresh (Empty_Command_Line);
         end if;
      else
         Put_Line ("Error: Unknown variable '" & N & "'");
      end if;
   exception
      when Invalid_Value =>
         null;
   end Set_Variable;

   ------------------
   -- Process_Line --
   ------------------

   procedure Process_Line (Line : String) is
      Expr  : String_List_Access;
      Colon : Integer;
   begin
      if Ada.Strings.Fixed.Trim (Line, Ada.Strings.Both) = "" then
         return;
      end if;

      Expr := Split (Line, On => ';');

      for C in Expr'Range loop
         if Ada.Strings.Fixed.Trim (Expr (C).all, Ada.Strings.Both) = "" then
            null;

         elsif Expr (C) (Expr (C)'First) = '!' then
            Process_Line ("shell "
                          & Expr (C) (Expr (C)'First + 1 .. Expr (C)'Last));

         else
            Colon := Ada.Strings.Fixed.Index (Expr (C).all, ":=");
            if Colon >= Expr (C)'First then
               if Verbose then
                  Put_Line (Expr (C).all);
               end if;
               Set_Variable (Expr (C) (Expr (C)'First .. Colon - 1),
                             Expr (C) (Colon + 2 .. Expr (C)'Last));
            else
               declare
                  List : constant Arg_List :=
                    Parse_String (Expr (C).all, Mode => Separate_Args);
                  Cmd  : constant String := To_Lower (Get_Command (List));
                  Found : Boolean := False;
                  Start : Time;
               begin
                  if Cmd = "time" then
                     Start := Clock;
                     Process_Line (Expr (C)
                                   (Expr (C)'First + 5 .. Expr (C)'Last));
                     Put_Line (Duration'Image (Clock - Start) & " s");

                  else
                     for Co in Commands'Range loop
                        if Commands (Co).Name.all = Cmd then
                           if Verbose then
                              Put_Line (Expr (C).all);
                           end if;
                           Commands (Co).Handler (List);
                           Found := True;
                           exit;
                        end if;
                     end loop;

                     if not Found then
                        Put_Line ("Invalid command: '" & Cmd & "'");
                        raise Invalid_Command;
                     end if;
                  end if;
               end;
            end if;
         end if;
      end loop;

      Free (Expr);
   end Process_Line;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File (File : String) is
      Str : GNAT.Strings.String_Access;
      Lines : String_List_Access;
   begin
      Str := Create (+File).Read_File;
      Lines := Split (Str.all, ASCII.LF);
      for L in Lines'Range loop
         declare
            Line : constant String :=
              Ada.Strings.Fixed.Trim (Lines (L).all, Ada.Strings.Both);
         begin
            if Starts_With (Line, "--") then
               if Verbose then
                  Put_Line (Line);
               end if;
            else
               Process_Line (Lines (L).all);
            end if;
         end;
      end loop;

      Free (Lines);
      Free (Str);

   exception
      when others =>
         Free (Str);
   end Process_File;

   ----------
   -- Dump --
   ----------

   procedure Dump (Curs : in out Files_Cursor) is
      F     : Virtual_File;
      Count : Natural := 1;
   begin
      while Curs.Has_Element loop
         F := Curs.Element;
         Output_Prefix (Count);
         Put_Line (Image (F));
         Curs.Next;
      end loop;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Curs : in out Entities_Cursor) is
      E : Entity_Information;
      Count : Natural := 1;
   begin
      while Curs.Has_Element loop
         E := Curs.Element;
         Output_Prefix (Count);
         Put_Line (Image (E));
         Curs.Next;
      end loop;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Curs : File_Sets.Set) is
      C : File_Sets.Cursor := Curs.First;
      Count : Natural := 1;
   begin
      while Has_Element (C) loop
         Output_Prefix (Count);
         Put_Line (Image (Element (C)));
         Next (C);
      end loop;
   end Dump;

   -----------------------
   -- Process_Importing --
   -----------------------

   procedure Process_Importing (Args : Arg_List) is
      Curs  : Files_Cursor;
   begin
      Curs := Xref.Importing (Tree.Create (+Nth_Arg (Args, 1)));
      Dump (Curs);
   end Process_Importing;

   ---------------------
   -- Process_Imports --
   ---------------------

   procedure Process_Imports (Args : Arg_List) is
      Curs  : Files_Cursor;
   begin
      Curs := Xref.Imports (Tree.Create (+Nth_Arg (Args, 1)));
      Dump (Curs);
   end Process_Imports;

   ------------------------
   -- Process_Depends_On --
   ------------------------

   procedure Process_Depends_On (Args : Arg_List) is
      Deps : constant File_Sets.Set :=
        Xref.Depends_On (Tree.Create (+Nth_Arg (Args, 1)));
   begin
      Dump (Deps);
   end Process_Depends_On;

   ------------------
   -- Process_Name --
   ------------------

   procedure Process_Name (Args : Arg_List) is
      Entity : Entity_Information;
      Count  : Natural := 1;
   begin
      if Args_Length (Args) /= 1 then
         Put_Line ("Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));
      Output_Prefix (Count);
      Put_Line (Xref.Qualified_Name (Entity));
   end Process_Name;

   -------------------
   -- Process_Calls --
   -------------------

   procedure Process_Calls (Args : Arg_List) is
      Entity  : Entity_Information;
      Callees : Entities_Cursor;
   begin
      if Args_Length (Args) /= 1 then
         Put_Line ("Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));
      Callees := Xref.Calls (Entity);
      Dump (Callees);
   end Process_Calls;

   ---------------------
   -- Process_Callers --
   ---------------------

   procedure Process_Callers (Args : Arg_List) is
      Entity  : Entity_Information;
      Callers : Entities_Cursor;
   begin
      if Args_Length (Args) /= 1 then
         Put_Line ("Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));
      Callers := Xref.Callers (Entity);
      Dump (Callers);
   end Process_Callers;

   -------------------------
   -- Process_Child_Types --
   -------------------------

   procedure Process_Child_Types (Args : Arg_List) is
      Entity   : Entity_Information;
      Children : Entities_Cursor;
   begin
      if Args_Length (Args) /= 1 then
         Put_Line ("Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));
      Children := Xref.Child_Types (Entity);
      Dump (Children);
   end Process_Child_Types;

   --------------------------
   -- Process_Parent_Types --
   --------------------------

   procedure Process_Parent_Types (Args : Arg_List) is
      Entity   : Entity_Information;
      Children : Entities_Cursor;
   begin
      if Args_Length (Args) /= 1 then
         Put_Line ("Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));
      Children := Xref.Parent_Types (Entity);
      Dump (Children);
   end Process_Parent_Types;

   ---------------------
   -- Process_Methods --
   ---------------------

   procedure Process_Methods (Args : Arg_List) is
      Entity   : Entity_Information;
      Children : Entities_Cursor;
   begin
      if Args_Length (Args) /= 1 then
         Put_Line ("Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));
      Children := Xref.Methods (Entity);
      Dump (Children);
   end Process_Methods;

   ------------------
   -- Process_Decl --
   ------------------

   procedure Process_Decl (Args : Arg_List) is
      Entity  : Entity_Information;
      Decl    : Entity_Declaration;
      Count   : Natural := 1;
   begin
      if Args_Length (Args) /= 1 then
         Put_Line ("Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));

      if Entity /= No_Entity then
         Decl := Xref.Declaration (Entity);
         Output_Prefix (Count);
         Put_Line (To_String (Decl.Name)
                   & ":" & Image (Decl.Location.File)
                   & ":" & Image (Decl.Location.Line, Min_Width => 0)
                   & ":" & Image (Decl.Location.Column, Min_Width => 0));
      end if;
   end Process_Decl;

   ------------------
   -- Process_Body --
   ------------------

   procedure Process_Body (Args : Arg_List) is
      Entity  : Entity_Information;
      Refs    : References_Cursor;
   begin
      if Args_Length (Args) /= 1 then
         Put_Line ("Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));
      Refs := Xref.Bodies (Entity);
      Dump (Refs, To_String (Xref.Declaration (Entity).Name));
   end Process_Body;

   ---------------
   -- On_Ctrl_C --
   ---------------

   procedure On_Ctrl_C is
   begin
      GNATCOLL.Readline.Finalize (History_File => History_File.all);
      Free (Xref);
      Free (History_File);
      GNAT.OS_Lib.OS_Exit (0);
   end On_Ctrl_C;

begin
   GNATCOLL.Traces.Parse_Config_File;

   Set_Usage
     (Cmdline,
      Help => "Query cross-references on source code");
   Define_Switch
     (Cmdline,
      Output      => DB_Name'Access,
      Long_Switch => "--db=",
      Help        => "Specifies the name of the database (or ':memory:')");
   Define_Switch
     (Cmdline,
      Output      => Nightly_DB_Name'Access,
      Long_Switch => "--nightlydb=",
      Help        => "Specifies the name of a prebuilt database");
   Define_Switch
     (Cmdline,
      Output      => Include_Runtime_Files'Access,
      Long_Switch => "--runtime",
      Help        =>
        "Also parse LI files not from the project (run time for instance)");
   Define_Switch
     (Cmdline,
      Output      => Commands_From_Switch'Access,
      Switch      => "-c:",
      Long_Switch => "--command=",
      Help        => "Execute the commands from ARG, and exit");
   Define_Switch
     (Cmdline,
      Output      => Commands_From_File'Access,
      Switch      => "-f:",
      Long_Switch => "--file=",
      Help        => "Execute the commands from the file ARG, and exit");
   Define_Switch
     (Cmdline,
      Output      => Display_Full_Paths'Access,
      Long_Switch => "--basenames",
      Value       => False,
      Help        => "Only display file names, instead of full path");
   Define_Switch
     (Cmdline,
      Output      => Verbose'Access,
      Switch      => "-v",
      Long_Switch => "--verbose",
      Help        => "Print commands before executing them");
   Define_Switch
     (Cmdline,
      Output      => Project_Name'Access,
      Switch      => "-P:",
      Long_Switch => "--project=",
      Help        => "Load the given project. If unspecified, use sources and"
        & " ALI files from current directory.");

   Getopt (Cmdline);

   if Project_Name.all = "" then
      Process_Project (Empty_Command_Line);   --  Load files from current dir
   else
      declare
         List : Arg_List;
      begin
         Set_Nth_Arg (List, 1, Project_Name.all);
         Process_Project (List);
      end;
   end if;

   if DB_Name.all /= ":memory:" then
      declare
         N : constant String := DB_Name.all;
      begin
         Free (DB_Name);
         DB_Name := new String'
           (Create_From_Dir
              (Dir       => Tree.Root_Project.Object_Dir,
               Base_Name => +N).Display_Full_Name);
      end;
   end if;

   Xref.Setup_DB
     (GNATCOLL.SQL.Sqlite.Setup (Database => DB_Name.all));

   --  Initial loading of the database

   Process_Refresh (Empty_Command_Line);

   if Commands_From_Switch.all /= "" then
      Process_Line (Commands_From_Switch.all);
      return;
   elsif Commands_From_File.all /= "" then
      Process_File (Commands_From_File.all);
      return;
   end if;

   History_File := new String'
     (Create_From_Dir
        (Dir       => Tree.Root_Project.Object_Dir,
         Base_Name => +".gnatinspect_hist").Display_Full_Name);

   Install_Ctrl_C_Handler (On_Ctrl_C'Access);

   GNATCOLL.Readline.Initialize
     (Appname      => "gnatcollxref",
      History_File => History_File.all,
      Completer    => Command_Line_Completion'Unrestricted_Access);

   Put_Line ("Type 'help' for more information");
   loop
      declare
         Input : constant String := GNATCOLL.Readline.Get_Line (">>> ");
      begin
         exit when Input = "exit";
         Process_Line (Input);
      exception
         when Invalid_Command =>
            null;
      end;
   end loop;

   On_Ctrl_C;

exception
   when GNAT.Command_Line.Exit_From_Command_Line
      | Ada.Text_IO.End_Error =>
      null;
   when Invalid_Command =>
      null;
end GNATInspect;
