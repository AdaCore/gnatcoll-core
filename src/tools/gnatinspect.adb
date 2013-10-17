------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Text_IO;                use Ada.Text_IO;
with GNAT.Command_Line;          use GNAT.Command_Line;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
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

   Output_Lead : aliased GNAT.Strings.String_Access := new String'(" ");
   --  Prefix written at the beginning of each response line.

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

   function Get_Entity (Arg : String) return Entity_Information;
   --  Return the entity matching the "name:file:line:column" argument

   procedure Parse_Command_Line (Switch, Parameter, Section : String);
   --  Handles some switches from the command line. Other switches are handled
   --  directly by Getopt and will set the corresponding local variables.

   type My_Xref_Database is new Xref_Database with null record;

   overriding function Image
      (Self : My_Xref_Database; File : Virtual_File) return String;
   function Image (Self : Entity_Information) return String;
   --  Return a display version of the argument

   procedure Dump (Curs : in out Files_Cursor);
   procedure Dump (Curs : in out Entities_Cursor'Class);
   procedure Dump (Refs : in out References_Cursor'Class);
   procedure Dump (Curs : File_Sets.Set);
   --  Display the list of files

   procedure Output_Prefix (Count : in out Natural);
   --  Print the prefix for each output line

   Xref    : aliased My_Xref_Database;
   --  The xref database

   generic
      with function Compute
        (Self : Xref_Database'Class;
         Entity : Entity_Information) return Entity_Information;
   procedure Process_Command_With_Single (Args : Arg_List);

   generic
      with procedure Compute
        (Self   : Xref_Database'Class;
         Entity : Entity_Information;
         Cursor : out Entities_Cursor'Class);
      Recurse : Boolean := False;
   procedure Process_Command_Entities (Args : Arg_List);

   ---------------------------------
   -- Process_Command_With_Single --
   ---------------------------------

   procedure Process_Command_With_Single (Args : Arg_List) is
      Entity  : Entity_Information;
      Comp    : Entity_Information;
      Count   : Natural := 1;
   begin
      if Args_Length (Args) /= 1 then
         Put_Line ("Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));
      Comp := Compute (Xref, Entity);
      if Comp /= No_Entity then
         Output_Prefix (Count);
         Put_Line (Image (Comp));
      end if;
   end Process_Command_With_Single;

   ------------------------------
   -- Process_Command_Entities --
   ------------------------------

   procedure Process_Command_Entities (Args : Arg_List) is
      Entity   : Entity_Information;
      Children : Entities_Cursor;
      Rec_Children : Recursive_Entities_Cursor;

      procedure Do_Compute
        (Self   : Xref_Database'Class;
         Entity : Entity_Information;
         Cursor : out Entities_Cursor'Class);
      --  Proxy for Compute, so that we can call Recursive.

      procedure Do_Compute
        (Self   : Xref_Database'Class;
         Entity : Entity_Information;
         Cursor : out Entities_Cursor'Class)
      is
      begin
         Compute (Self, Entity, Cursor);
      end Do_Compute;

   begin
      if Args_Length (Args) /= 1 then
         Put_Line ("Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));

      if Recurse then
         Recursive
           (Self    => Xref'Unchecked_Access,
            Entity  => Entity,
            Compute => Do_Compute'Unrestricted_Access,
            Cursor  => Rec_Children);
         Dump (Rec_Children);
      else
         Compute (Xref, Entity, Cursor => Children);
         Dump (Children);
      end if;
   end Process_Command_Entities;

   procedure Process_Body (Args : Arg_List);
   procedure Process_Calls
     is new Process_Command_Entities (Calls);
   procedure Process_Callers
     is new Process_Command_Entities (Callers);
   procedure Process_Child_Types
     is new Process_Command_Entities (Child_Types);
   procedure Process_Child_Types_Recursive (Args : Arg_List);
   procedure Process_Component
     is new Process_Command_With_Single (Component_Type);
   procedure Process_Decl (Args : Arg_List);
   procedure Process_Doc (Args : Arg_List);
   procedure Process_Depends_On (Args : Arg_List);
   procedure Process_Entities (Args : Arg_List);
   procedure Process_Help (Args : Arg_List);
   procedure Process_Importing (Args : Arg_List);
   procedure Process_Imports (Args : Arg_List);
   procedure Process_Method_Of
     is new Process_Command_With_Single (Method_Of);
   procedure Process_Fields
     is new Process_Command_Entities (Fields);
   procedure Process_Methods
     is new Process_Command_Entities (Methods);
   procedure Process_Literals
     is new Process_Command_Entities (Literals);
   procedure Process_Name (Args : Arg_List);
   procedure Process_Overrides
     is new Process_Command_With_Single (Overrides);
   procedure Process_Overridden
     is new Process_Command_Entities (Overridden_By, Recurse => False);
   procedure Process_Overridden_Recursive
     is new Process_Command_Entities (Overridden_By, Recurse => True);
   procedure Process_Params (Args : Arg_List);
   procedure Process_Parent_Types
     is new Process_Command_Entities (Parent_Types);
   procedure Process_Pointed_Type
     is new Process_Command_With_Single (Pointed_Type);
   procedure Process_Refresh (Args : Arg_List);
   procedure Process_Refs (Args : Arg_List);
   procedure Process_Refs_Overriding (Args : Arg_List);
   procedure Process_Scenario (Args : Arg_List);
   procedure Process_Shell (Args : Arg_List);
   procedure Process_Type
     is new Process_Command_With_Single (Type_Of);
   --  Process the various commands.
   --  Args is the command line entered by the user, so Get_Command (Args) for
   --  instance is the command being executed.

   procedure Load_Project (Path : Virtual_File);
   --  Load the given project (but does not compute its view)

   procedure Set_Variable (Name, Value : String);
   --  Change the value of a variable

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

      (new String'("child_types_recursive"),
       new String'("name:file:line:column"),
       new String'("The list of child types of the entity (for instance"
         & " classes that inherit from the entity). This also includes"
         & " grand-children and down."),
       Process_Child_Types_Recursive'Access),

      (new String'("parent_types"),
       new String'("name:file:line:column"),
       new String'("The parent types of the entity (for instance the classes"
           & " or interfaces from which it derives). See also 'child_types'"),
       Process_Parent_Types'Access),

      (new String'("fields"),
       new String'("name:file:line:column"),
       new String'("Returns the list of fields for the entity"),
       Process_Fields'Access),

      (new String'("methods"),
       new String'("name:file:line:column"),
       new String'("Returns the list of methods (or primitive operations) for"
           & " the entity"),
       Process_Methods'Access),

      (new String'("method_of"),
       new String'("name:file:line:column"),
       new String'("Returns the class or tagged type for which the entity is"
           & " a method or a primitive operation"),
       Process_Method_Of'Access),

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

      (new String'("overrides"),
       new String'("name:file:line:column"),
       new String'("The entity that is overridden by the parameter (generally"
         & " a method from a parent class"),
       Process_Overrides'Access),

      (new String'("overridden"),
       new String'("name:file:line:column"),
       new String'("The list of entities that override the parameter"
         & "(generally methods from children classes"),
       Process_Overridden'Access),

      (new String'("overridden_recursive"),
       new String'("name:file:line:column"),
       new String'("The list of entities that override the parameter"
         & "(generally methods from children classes. This is recursive)."),
       Process_Overridden_Recursive'Access),

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

      (new String'("doc"),
       new String'("name:file:line:column"),
       new String'("Display the documentation for the entity"),
       Process_Doc'Access),

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

      (new String'("refs_overriding"),
       new String'("name:file:line:column"),
       new String'("Display all known references to the entity or one of its"
           & " overriding entities"),
       Process_Refs_Overriding'Access),

      (new String'("entities"),
       new String'("file"),
       new String'("List all entities referenced or declared in the file"),
       Process_Entities'Access),

      (new String'("type"),
       new String'("name:file:line:column"),
       new String'("Return the type of the entity (variable or constant)."
           & " For an enumeration literal, this returns the corresponding"
           & " enumeration"),
       Process_Type'Access),

      (new String'("component"),
       new String'("name:file:line:column"),
       new String'("Return the component type of the entity (for arrays"
          & " for instance"),
       Process_Component'Access),

      (new String'("literals"),
       new String'("name:file:line:column"),
       new String'("Return the valid literal values for an enumeration"),
       Process_Literals'Access),

      (new String'("pointed"),
       new String'("name:file:line:column"),
       new String'("Return the type pointed to by the entity"),
       Process_Pointed_Type'Access),

      (new String'("shell"),
       null,
       new String'("Execute a shell command (an alternative is to use '!'"
           & " as the command."),
       Process_Shell'Access),

      (new String'("scenario"),
       new String'("VARIABLE VALUE"),
       new String'("Change the value of a scenario variable, and reparse the"
         & " project"),
       Process_Scenario'Access),

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
            new String'("Whether to include runtime files in the database")),
      3 => (new String'("leading_doc"),
            new String'("Whether the documentation appears before entities"
              & " in general")));

   History_File : GNAT.Strings.String_Access;

   Complete_Command_List_Index : Integer;
   --  Global variable used by Complete_Command

   Project_Is_Default : Boolean := True;  --  Whether we have the default prj
   Env     : Project_Environment_Access;
   Tree    : Project_Tree;
   --  The currently loaded project tree

   procedure Display_Progress (Current, Total : Integer);
   --  Display the current progress of the parsing

   Previous_Progress : Natural := 0;
   Progress_Reporter : access procedure (Current, Total : Integer) := null;

   Cmdline               : Command_Line_Configuration;
   Commands_From_Switch  : aliased GNAT.Strings.String_Access;
   Commands_From_File    : aliased GNAT.Strings.String_Access;
   DB_Name               : aliased GNAT.Strings.String_Access :=
     new String'("gnatinspect.db");
   Nightly_DB_Name       : aliased GNAT.Strings.String_Access;
   Include_Runtime_Files : aliased Boolean;
   Display_Full_Paths    : aliased Boolean;
   Exit_After_Refresh    : aliased Boolean;
   Verbose               : aliased Boolean;
   Support_Symlinks      : aliased Boolean;
   Show_Progress         : aliased Boolean;
   Project_Name          : aliased GNAT.Strings.String_Access;
   Subdirs               : aliased GNAT.Strings.String_Access;
   Traces_File_Name      : aliased GNAT.Strings.String_Access;
   Config_File           : aliased GNAT.Strings.String_Access;
   Autoconf              : aliased Boolean;
   ALI_Encoding          : aliased GNAT.Strings.String_Access :=
     new String'("");
   Force_Refresh         : aliased Boolean := False;
   Look_Before_First_For_Doc : Boolean := True;
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

   overriding function Image
     (Self : My_Xref_Database; File : Virtual_File) return String
   is
      pragma Unreferenced (Self);
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
      if Self = No_Entity then
         return "Unknown entity";
      else
         Decl := Xref.Declaration (Self);

         if Is_Predefined_Entity (Decl) then
            return "predefined entity: " & To_String (Decl.Name);
         else
            return To_String (Decl.Name) & ":" & Xref.Image (Decl.Location);
         end if;
      end if;
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

   procedure Load_Project (Path : Virtual_File) is
      GNAT_Version : GNAT.Strings.String_Access;
   begin
      Env.Set_Path_From_Gnatls
        (Gnatls       => "gnatls",
         GNAT_Version => GNAT_Version,
         Errors       => Put_Line'Access);
      Free (GNAT_Version);

      --  The default extensions must match those defined in the gprconfig
      --  knowledge base.
      Env.Register_Default_Language_Extension
        (Language_Name       => "C",
         Default_Spec_Suffix => ".h",
         Default_Body_Suffix => ".c");
      Env.Register_Default_Language_Extension
        (Language_Name       => "C++",
         Default_Spec_Suffix => ".hh",
         Default_Body_Suffix => ".cpp");

      if Path = No_File then
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
         Trace (Me, "processing 'PROJECT' '" & Path.Display_Full_Name & "'");
         Project_Is_Default := False;
         Tree.Load
           (Root_Project_Path => Path,
            Env               => Env,
            Errors            => Put_Line'Access);
      end if;

   exception
      when GNATCOLL.Projects.Invalid_Project =>
         Put_Line ("Error: invalid project file: '"
                   & Path.Display_Full_Name & "'");
   end Load_Project;

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
            Show_Progress       => Progress_Reporter,
            ALI_Encoding        => ALI_Encoding.all,
            From_DB_Name        => Nightly_DB_Name.all,
            To_DB_Name          => DB_Name.all,
            Force_Refresh       => Force_Refresh);
      end if;
   end Process_Refresh;

   ----------------------
   -- Process_Scenario --
   ----------------------

   procedure Process_Scenario (Args : Arg_List) is
      Name  : constant String := Nth_Arg (Args, 1);
      Value : constant String := Nth_Arg (Args, 2);
   begin
      Env.Change_Environment (Name, Value);
      Tree.Recompute_View (Errors => Ada.Text_IO.Put_Line'Access);
   end Process_Scenario;

   -------------------
   -- Process_Shell --
   -------------------

   procedure Process_Shell (Args : Arg_List) is
      Cmd     : constant String := Nth_Arg (Args, 1);
      Command : GNAT.Strings.String_Access;
   begin
      Command := GNAT.OS_Lib.Locate_Exec_On_Path (Cmd);
      if Command = null then
         Put_Line (Output_Lead.all
                   & "Cannot locate '" & Cmd & "' on PATH");
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
              (Output_Lead.all & "Error: failed to execute '"
               & To_Display_String (Args, Include_Command => False) & "'");
         end if;
      end;
   end Process_Shell;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity (Arg : String) return Entity_Information is
      Words  : String_List_Access := Split (Arg, On => ':');
      Ref    : Entity_Reference;
   begin
      if Words'Length = 4 then
         Ref := Xref.Get_Entity
           (Name   => Words (Words'First).all,
            File   => Format_Pathname
              (Style => UNIX,
               Path  => Words (Words'First + 1).all),
            Line   => Integer'Value (Words (Words'First + 2).all),
            Column => Visible_Column
              (Integer'Value (Words (Words'First + 3).all)));
      elsif Words'Length = 3 then
         Ref := Xref.Get_Entity
           (Name   => Words (Words'First).all,
            File   => Format_Pathname
              (Style => UNIX,
               Path  => Words (Words'First + 1).all),
            Line   => Integer'Value (Words (Words'First + 2).all));
      elsif Words'Length = 2 then
         Ref := Xref.Get_Entity
           (Name   => Words (Words'First).all,
            File   => Format_Pathname
              (Style => UNIX,
               Path  => Words (Words'First + 1).all));
      else
         Put_Line (Output_Lead.all
                   & "Invalid parameter, expecting name:file:line:column => '"
                   & Arg & "'");
         Free (Words);
         return No_Entity;
      end if;

      Free (Words);

      if Ref.Entity = No_Entity then
         Put_Line (Output_Lead.all
                   & "Error: entity not found '" & Arg & "'");
      elsif Is_Fuzzy_Match (Ref.Entity) then
         Put_Line (Output_Lead.all
                   & "fuzzy match for the entity");
      end if;

      return Ref.Entity;
   end Get_Entity;

   -------------------
   -- Output_Prefix --
   -------------------

   procedure Output_Prefix (Count : in out Natural) is
   begin
      if Verbose then
         Put (Output_Lead.all);
         Put (Image (Count, Min_Width => 3, Padding => ' '));
         Put ("> ");
         Count := Count + 1;
      end if;
   end Output_Prefix;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Refs : in out References_Cursor'Class)
   is
      Ref    : Entity_Reference;
      Count  : Natural := 1;
   begin
      while Has_Element (Refs) loop
         Ref := Refs.Element;
         Output_Prefix (Count);

         declare
            Name : constant String :=
              To_String (Xref.Declaration (Ref.Entity).Name);
         begin
            Put
              (Name & ':' & Xref.Image (Ref)
               & " (" & To_String (Ref.Kind) & ")");

            if Ref.Scope /= No_Entity then
               Put_Line (" scope=" & Image (Ref.Scope));
            else
               New_Line;
            end if;
         end;

         Next (Refs);
      end loop;
   end Dump;

   ------------------
   -- Process_Refs --
   ------------------

   procedure Process_Refs (Args : Arg_List) is
      Entity  : Entity_Information;
      Refs    : References_Cursor;
      Renamed : Entity_Information;
   begin
      if Args_Length (Args) /= 1 then
         Put_Line (Output_Lead.all & "Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));

      Renamed := Xref.Renaming_Of (Entity);
      if Renamed /= No_Entity then
         Put_Line (Output_Lead.all & "Renaming of " & Image (Renamed));
      end if;

      Xref.References (Entity, Cursor => Refs);
      Dump (Refs);
   end Process_Refs;

   -----------------------------
   -- Process_Refs_Overriding --
   -----------------------------

   procedure Process_Refs_Overriding (Args : Arg_List) is
      Refs   : Recursive_References_Cursor;
      Entity : Entity_Information;
   begin
      if Args_Length (Args) /= 1 then
         Put_Line (Output_Lead.all & "Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));
      Recursive
        (Self    => Xref'Unchecked_Access,
         Entity  => Entity,
         Compute => References'Access,
         Cursor  => Refs);
      Dump (Refs);
   end Process_Refs_Overriding;

   -----------------------------------
   -- Process_Child_Types_Recursive --
   -----------------------------------

   procedure Process_Child_Types_Recursive (Args : Arg_List) is
      Entity : Entity_Information;
      Children : Recursive_Entities_Cursor;
   begin
      if Args_Length (Args) /= 1 then
         Put_Line (Output_Lead.all & "Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));

      Recursive
        (Self    => Xref'Unchecked_Access,
         Entity  => Entity,
         Compute => Child_Types'Access,
         Cursor  => Children);
      Dump (Children);
   end Process_Child_Types_Recursive;

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
         Put_Line (Output_Lead.all & "Invalid number of arguments");
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
            Put_Line
              (Output_Lead.all & "Error: Expected boolean, got '" & V & "'");
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
      elsif N = "leading_doc" then
         Look_Before_First_For_Doc := To_Boolean (V);
      else
         Put_Line (Output_Lead.all & "Error: Unknown variable '" & N & "'");
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
                     Put_Line
                       (Output_Lead.all
                        & Duration'Image (Clock - Start) & " s");

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
                        Put_Line
                          (Output_Lead.all & "Invalid command: '" & Cmd & "'");
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
      Lines := Split (Str.all, ASCII.LF, Omit_Empty_Lines => False);
      for L in Lines'Range loop
         declare
            Line : constant String := Lines (L).all;
         begin
            if Line = "" then
               if Verbose then
                  Put_Line (Line);
               end if;

            elsif Starts_With (Line, Output_Lead.all) then
               --  Ignore these lines, they are not even output. In particular,
               --  we use it for the testsuite to mix commands and expected
               --  output.
               null;

            elsif Line (Line'First) = '[' then
               --  Ignore lines starting with [. In the testsuite, these are
               --  the output of GNATCOLL.Traces associated with the test
               null;

            elsif Starts_With (Line, "--") then
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
         raise;
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
         Put_Line (Xref.Image (F));
         Curs.Next;
      end loop;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump (Curs : in out Entities_Cursor'Class) is
      E : Entity_Information;
      Count : Natural := 1;
   begin
      while Curs.Has_Element loop
         E := Curs.Element;
         if E /= No_Entity then
            Output_Prefix (Count);
            Put_Line (Image (E));
         end if;
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
         Put_Line (Xref.Image (Element (C)));
         Next (C);
      end loop;
   end Dump;

   -----------------------
   -- Process_Importing --
   -----------------------

   procedure Process_Importing (Args : Arg_List) is
      Curs  : Files_Cursor;
   begin
      Curs := Xref.Imported_By (Tree.Create (+Nth_Arg (Args, 1)));
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
         Put_Line (Output_Lead.all & "Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));
      Output_Prefix (Count);
      Put_Line (Xref.Qualified_Name (Entity));
   end Process_Name;

   -----------------
   -- Process_Doc --
   -----------------

   procedure Process_Doc (Args : Arg_List) is
      Info   : GNATCOLL.Projects.File_Info;
      Syntax : Language_Syntax;
      Entity : Entity_Information;
      Decl   : Entity_Declaration;
   begin
      if Args_Length (Args) /= 1 then
         Put_Line (Output_Lead.all & "Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));
      Decl := Xref.Declaration (Entity);
      Info := Tree.Info (Decl.Location.File);

      if Info.Language = "ada" then
         Syntax := Ada_Syntax;
      elsif Info.Language = "c" then
         Syntax := C_Syntax;
      elsif Info.Language = "c++" then
         Syntax := Cpp_Syntax;
      else
         Put_Line
           (Output_Lead.all
            & "Unknown language for " & Decl.Location.File.Display_Base_Name);
         return;
      end if;

      Put_Line
        (Output_Lead.all & Xref.Overview (Entity => Entity, Format => Text));
      Put_Line (Output_Lead.all);

      declare
         Doc : constant String :=
           Xref.Documentation
             (Entity            => Entity,
              Language          => Syntax,
              Look_Before_First => Look_Before_First_For_Doc);
         Index, Eol : Natural;
      begin
         if Doc /= "" then
            Index := Doc'First;
            while Index <= Doc'Last loop
               Put (Output_Lead.all);
               Eol := GNATCOLL.Utils.EOL (Doc (Index .. Doc'Last));
               Put_Line (Doc (Index .. Eol - 1));
               Index := Eol + 1;
            end loop;

            Put_Line (Output_Lead.all & "");
         end if;
      end;
   end Process_Doc;

   ----------------------
   -- Process_Entities --
   ----------------------

   procedure Process_Entities (Args : Arg_List) is
      Entities : Entities_Cursor;
   begin
      Xref.Referenced_In
        (File   => Tree.Create (+Nth_Arg (Args, 1)),
         Cursor => Entities);
      Dump (Entities);
   end Process_Entities;

   ------------------
   -- Process_Decl --
   ------------------

   procedure Process_Decl (Args : Arg_List) is
      Entity  : Entity_Information;
      Decl    : Entity_Declaration;
      Count   : Natural := 1;
   begin
      if Args_Length (Args) /= 1 then
         Put_Line (Output_Lead.all & "Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));

      if Entity /= No_Entity then
         Decl := Xref.Declaration (Entity);
         Output_Prefix (Count);
         Put_Line (To_String (Decl.Name) & ":" & Xref.Image (Decl.Location));
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
         Put_Line (Output_Lead.all & "Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));
      Xref.Bodies (Entity, Cursor => Refs);
      Dump (Refs);
   end Process_Body;

   ---------------
   -- On_Ctrl_C --
   ---------------

   procedure On_Ctrl_C is
   begin
      if History_File /= null then
         GNATCOLL.Readline.Finalize (History_File => History_File.all);
      end if;
      Free (Xref);
      Free (History_File);
      GNAT.OS_Lib.OS_Exit (0);
   end On_Ctrl_C;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line (Switch, Parameter, Section : String) is
      pragma Unreferenced (Section);
      Equal : Natural;
   begin
      if Switch = "-X" then
         Equal := Ada.Strings.Fixed.Index (Parameter, "=");
         Env.Change_Environment
           (Name  => Parameter (Parameter'First .. Equal - 1),
            Value => Parameter (Equal + 1 .. Parameter'Last));

      elsif Switch = "--configdb" then
         Add_Config_Dir
           (Env.all,
            Create_From_Base (+Parameter, Get_Current_Dir.Full_Name.all));
      end if;
   end Parse_Command_Line;

   ----------------------
   -- Display_Progress --
   ----------------------

   procedure Display_Progress (Current, Total : Integer) is
      Now : constant Integer := Integer (Float'Floor
        (Float (Current) / Float (Total) * 100.0));
   begin
      if Now /= Previous_Progress then
         Put_Line ("completed" & Current'Img
                   & " out of" & Total'Img
                   & " (" & Image (Now, Min_Width => 0) & "%)...");
         Previous_Progress := Now;
      end if;
   end Display_Progress;

begin
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
      Help        => "Execute the commands from the file ARG, and exit."
      & " All lines are read, omitting comment lines and lines starting with"
      & " the lead specified by --lead.");
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
      Output      => Exit_After_Refresh'Access,
      Long_Switch => "--exit",
      Help        => "Refresh the database, and exit (no interactive mode)");
   Define_Switch
     (Cmdline,
      Output      => Project_Name'Access,
      Switch      => "-P:",
      Long_Switch => "--project=",
      Help        => "Load the given project (mandatory)");
   Define_Switch
     (Cmdline,
      Switch      => "-X:",
      Help        => "Specify an external reference in the project");
   Define_Switch
     (Cmdline,
      Output      => Support_Symlinks'Access,
      Long_Switch => "--symlinks",
      Help        => "Take additional time to resolve symbolic links");
   Define_Switch
     (Cmdline,
      Output      => Subdirs'Access,
      Long_Switch => "--subdirs=",
      Help        => "Object files will be found in a subdirectory of obj");
   Define_Switch
     (Cmdline,
      Output      => Traces_File_Name'Access,
      Long_Switch => "--tracefile=",
      Help        => "Specify an alternative traces configuration file");
   Define_Switch
     (Cmdline,
      Output      => Show_Progress'Access,
      Switch      => "-d",
      Help        => "Show progress as LI files are parsed");
   Define_Switch
     (Cmdline,
      Output      => Output_Lead'Access,
      Long_Switch => "--lead=",
      Help        => "Set the prefix to display at the beginning of each"
      & " line of output. This can be used to more easily process the output"
      & " of gnatinspect if you need to.");
   Define_Switch
     (Cmdline,
      Output      => ALI_Encoding'Access,
      Long_Switch => "--encoding=",
      Switch      => "-e=",
      Help        => "The character encoding used for source and ALI files");
   Define_Switch
     (Cmdline,
      Output      => Config_File'Access,
      Long_Switch => "--config=",
      Help        => "Specify the configuration file (.cgpr) to load before"
      & " loading the project");
   Define_Switch
     (Cmdline,
      Output      => Autoconf'Access,
      Long_Switch => "--autoconf",
      Help        => "Run gprconfig to generate the config file if it doesn't"
      & " exist");
   Define_Switch
     (Cmdline,
      Long_Switch => "--configdb=",
      Help        => "An extra directory to be parsed by gprconfig to generate"
        & " the configuration file");
   Define_Switch
     (Cmdline,
      Output      => Force_Refresh'Access,
      Long_Switch => "--force",
      Help        => "Force reloading of all ALI files");

   Initialize (Env);

   Getopt (Cmdline, Parse_Command_Line'Unrestricted_Access);

   if Project_Name.all = "" then
      Free (Project_Name);
      Project_Name := new String'(GNAT.Command_Line.Get_Argument);

      if Project_Name.all = "" then
         Put_Line ("No project file specified");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;
   end if;

   GNATCOLL.VFS.Symbolic_Links_Support (Support_Symlinks);

   --  If the user has specified a trace file that doesn't exist, we do not
   --  want to fallback on the default, so we explicitly test first.
   if Traces_File_Name.all = ""
     or else Is_Regular_File (GNATCOLL.VFS.Create (+Traces_File_Name.all))
   then
      GNATCOLL.Traces.Parse_Config_File
        (Traces_File_Name.all, Force_Activation => False);
   end if;

   if Show_Progress then
      Progress_Reporter := Display_Progress'Unrestricted_Access;
   end if;

   if Subdirs.all /= "" then
      Env.Set_Object_Subdir (+Subdirs.all);
   end if;

   declare
      Path : Virtual_File := Create (+Project_Name.all);
   begin
      if not Path.Is_Regular_File then
         Path := Create (+Project_Name.all & ".gpr");

         if not Path.Is_Regular_File then
            Put_Line ("No such file: " & Project_Name.all);
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            return;
         end if;
      end if;

      if Config_File /= null and then Config_File.all /= "" then
         Env.Set_Config_File
           (Create_From_Base
              (+Config_File.all, Get_Current_Dir.Full_Name.all));
      end if;

      if Autoconf then
         Env.Set_Automatic_Config_File (Autoconf);
         if Config_File = null or else Config_File.all = "" then
            Env.Set_Config_File
              (Create_From_Base
                 ("auto.cgpr", Get_Current_Dir.Full_Name.all));
         end if;
      end if;
      Load_Project (Path);
   end;

   if DB_Name.all /= ":memory:" then
      declare
         N : constant String := DB_Name.all;
         Dir : Virtual_File := Tree.Root_Project.Object_Dir;
         Dir2 : Virtual_File;
      begin
         Free (DB_Name);

         --  If the project does not have an object directory, create
         --  the database in the directory containing the project file.
         if Dir = No_File then
            Dir := Tree.Root_Project.Project_Path.Dir;
            Trace (Me, "Root project does not have an object dir:" & ASCII.LF
                   & "creating database in " & (+Dir.Full_Name.all));
         end if;

         Dir := Create_From_Base
           (Base_Dir => Dir.Full_Name.all, Base_Name => +N);
         Dir2 := Create (Dir.Dir_Name);

         if not Dir2.Is_Directory then
            Dir2.Make_Dir (Recursive => True);
         end if;

         DB_Name := new String'(Dir.Display_Full_Name);
      end;
   end if;

   Install_Ctrl_C_Handler (On_Ctrl_C'Unrestricted_Access);

   Xref.Setup_DB
     (GNATCOLL.SQL.Sqlite.Setup (Database => DB_Name.all));

   --  Initial loading of the database

   Process_Refresh (Empty_Command_Line);

   if Commands_From_Switch.all /= "" then
      Process_Line (Commands_From_Switch.all);
      Exit_After_Refresh := True;
   elsif Commands_From_File.all /= "" then
      Process_File (Commands_From_File.all);
      Exit_After_Refresh := True;
   end if;

   if Exit_After_Refresh then
      On_Ctrl_C;
      return;
   end if;

   History_File := new String'
     (Create_From_Dir
        (Dir       => GNATCOLL.VFS.Get_Home_Directory,
         Base_Name => +".gnatinspect_hist").Display_Full_Name);

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
      On_Ctrl_C;
   when Invalid_Command =>
      On_Ctrl_C;
   when E : others =>
      On_Ctrl_C;
      Put_Line ("Unexpected exception");
      Put_Line (Exception_Information (E));
end GNATInspect;
