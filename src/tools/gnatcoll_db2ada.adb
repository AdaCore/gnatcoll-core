------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with GNAT.Command_Line;          use GNAT.Command_Line;
with GNAT.Expect;                use GNAT.Expect;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Regexp;                use GNAT.Regexp;
with GNAT.Strings;
with GNATCOLL.Arg_Lists;         use GNATCOLL.Arg_Lists;
with GNATCOLL.SQL.Exec;          use GNATCOLL.SQL, GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Inspect;       use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Postgres;
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.Strings;           use GNATCOLL.Strings;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

procedure GNATCOLL_Db2Ada is
   Me : constant Trace_Handle := Create ("DB2ADA");

   Generated     : GNAT.Strings.String_Access := new String'("Database");
   Generated_Orm : GNAT.Strings.String_Access := new String'("ORM_Queries");
   Load_File     : Virtual_File := GNATCOLL.VFS.No_File;  --  File to load
   Output_Dir    : Virtual_File := Get_Current_Dir;

   DB_Model   : GNAT.OS_Lib.String_Access := null;
   Orm_Tables : GNAT.OS_Lib.String_Access := null;

   type Output_Kind is
     (Output_Ada_Specs,
      Output_Ada_Enums,
      Output_Ada_Enums_Image,
      Output_Text,
      Output_Orm,
      Output_Dot,
      Output_Load,
      Output_Adacreate,
      Output_Createdb);
   Output : array (Output_Kind) of Boolean := (others => False);
   --  The type of output for this utility

   Need_Schema_For_Output : constant array (Output_Kind) of Boolean :=
     (Output_Ada_Enums => False, Output_Ada_Enums_Image => False,
      others => True);
   --  Whether the various outputs require a database schema.

   Schema  : DB_Schema;
   DB_IO   : DB_Schema_IO;
   File_IO : File_Schema_IO;

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (String);
   use String_Lists;

   type Dumped_Enums is record
      Table     : Unbounded_String;
      Id        : Unbounded_String;
      Base_Type : Unbounded_String;
      Type_Name : Unbounded_String;
      Prefix    : Unbounded_String;
      Names     : String_Lists.List;
      Values    : String_Lists.List;
   end record;
   --  Describes a table to be dumped. All values from this table will have an
   --  Ada constant with the same value generated for them. This applies for
   --  tables that correspond to enumeration types and contain special values
   --  that are useful for the logic of the code.
   --  Generated code looks like:
   --       subtype <Type_Name> is <Base_Type>;
   --  For each value in the table, the following is dumped:
   --      <Prefix>_<name> : constant <Type_Name> := value;

   package Enumeration_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Dumped_Enums);
   use Enumeration_Lists;
   Enumerations : Enumeration_Lists.List;

   procedure Add_Enumeration
     (DB : access Database_Connection_Record'Class;
      Table, Id, Name, Prefix, Base_Type : String);
   --  Register a table that should be dumped

   type Dumped_Vars is record
      Name    : Unbounded_String;
      Value   : Unbounded_String;
      Comment : Unbounded_String;
   end record;

   function Ada_Quote (Str : String) return String;
   --  If Str contains quotes, duplicate them so that Str can be inserted in
   --  generated code

   package Variables_List is new Ada.Containers.Doubly_Linked_Lists
     (Dumped_Vars);
   use Variables_List;
   Variables : Variables_List.List;

   procedure Add_Variable
     (DB : access Database_Connection_Record'Class;
      Name, Table, Field, Where, Comment : String);
   --  Register a new variable to be dumped

   procedure Print_Help;
   --  Print the help and exit the application

   procedure Main;
   --  Get the list of parameters to use to connect to the postgres database

   procedure Dump_Tables
     (Connection : access Database_Connection_Record'Class;
      Enums      : String_Lists.List;
      Vars       : String_Lists.List);
   --  Dump the contents of some tables into Trans. We unfortunately need some
   --  hard-coded strings for some tables, and it is better to create Ada
   --  constants for those rather than hard-code them every where. At least
   --  when they are renamed we will be forced to change the Ada code.

   procedure Generate
     (Generated : String; Include_Database_Create : Boolean);
   procedure Generate
     (Generated : String; Include_Database_Create : Boolean) is separate;
   --  Generate the actual output. This can be implemented either through
   --  Ada.Text_IO or using the templates parser
   --  If Include_Database_Create is true, an additional subprogram is
   --  generated to regenerate the database and load initial data without
   --  requiring external files.

   procedure Generate_Orm;
   --  Generate the ORM API via a python script

   procedure Generate_Dot;
   --  Generate a dot graph for the database schema

   procedure Spawn_Dborm (Command : String; Extra_Args : Argument_List);
   --  Spawn "python dborm.py COMMAND dbschema.txt EXTRA_ARGS".
   --  Do not free Extra_Args, it is already freed in the context of this
   --  procedure

   ---------------
   -- Ada_Quote --
   ---------------

   function Ada_Quote (Str : String) return String is
   begin
      if Str (Str'First) = '"' then
         return '"' & Str & '"';
      else
         return Str;
      end if;
   end Ada_Quote;

   -----------------
   -- Spawn_Dborm --
   -----------------

   procedure Spawn_Dborm (Command : String; Extra_Args : Argument_List) is
      DbOrm1 : constant String :=   --  official setup
        Executable_Location & "share/gnatcoll/dborm.py";
      DbOrm2 : constant String :=   --  development setup
        Executable_Location & "../dborm.py";
      Status  : aliased Integer;
      Args    : Argument_List (1 .. 3 + Extra_Args'Length);
   begin
      if DB_Model = null then
         Put_Line ("FAILED: -orm currently requires a textual description of");
         Put_Line ("   the schema, and does not read it from a live database"
                   & " (use -dbmodel=FILE)");
         return;
      end if;

      if Is_Regular_File (DbOrm1) then
         Args (1) := new String'(DbOrm1);
      elsif Is_Regular_File (DbOrm2) then
         Args (1) := new String'(DbOrm2);
      else
         Put_Line ("FAILED: could not find " & DbOrm1);
         return;
      end if;

      Args (2) := new String'(Command);
      Args (3) := new String'(DB_Model.all);
      Args (4 .. Args'Length) := Extra_Args;

      declare
         Output : constant String := Get_Command_Output
           (Command     => "python",
            Arguments   => Args,
            Input       => "",
            Status      => Status'Access,
            Err_To_Out  => True);
      begin
         if Output /= "" then
            Put_Line (Output);
         end if;

         if Status /= 0 then
            Put_Line ("FAILED to execute command: "
                      & Argument_List_To_String (Args));
         end if;
      exception
         when Invalid_Process =>
            Put_Line ("FAILED to spawn python");
      end;

      Free (Args);
   end Spawn_Dborm;

   ------------------
   -- Generate_Dot --
   ------------------

   procedure Generate_Dot is
      Args : Argument_List (1 .. 200);
      A    : Integer := Args'First;
   begin
      loop
         declare
            Cluster : constant String := GNAT.Command_Line.Get_Argument;
         begin
            exit when Cluster = "";
            Args (A) := new String'(Cluster);
            A := A + 1;
         end;
      end loop;

      Spawn_Dborm ("-graph", Args (Args'First .. A - 1));
   end Generate_Dot;

   ------------------
   -- Generate_Orm --
   ------------------

   procedure Generate_Orm is
      Args    : Argument_List (1 .. 4);
   begin
      Args (1) := new String'(Generated_Orm.all);
      Args (2) := new String'(Generated.all);
      Args (3) := new String'(Output_Dir.Display_Full_Name);

      if Orm_Tables /= null then
         Args (4) := new String'(Orm_Tables.all);
      else
         Args (4) := new String'("");
      end if;
      Spawn_Dborm ("-ada", Args);
   end Generate_Orm;

   ----------------
   -- Print_Help --
   ----------------

   procedure Print_Help is
   begin
      Put_Line ("==== Specifying the database");
      Put_Line ("-dbmodel <file>: textual description of the database schema");
      Put_Line ("                 Not compatible with -enum and -var");
      Put_Line ("-dbhost <host>: host on which the database runs");
      Put_Line ("-dbname <name>: name of the database");
      Put_Line ("-dbuser <user>: user name to log in the database");
      Put_Line ("-dbpasswd <passwd>: password for the database");
      Put_Line ("-dbport <port>: port for the database");
      Put_Line ("-dbtype <type>: database backend to use"
                & " (default is postgreSQL)");
      Put_Line ("-dbfilter REGEXP: regular expression filtering tables used");
      Put_Line ("    for -text and -api output generation from database.");
      Put_Line ("    The default is to use all tables.");
      New_Line;
      Put_Line ("==== Specifying output");
      Put_Line ("The default output is a set of Ada files that represent the");
      Put_Line ("database schema.");
      Put_Line ("-enum table,id,name,prefix,base");
      Put_Line ("    Name of a table to dump. Used for for enumeration-like");
      Put_Line ("    tables, which might contain special values. This will");
      Put_Line ("    generate Ada code like");
      Put_Line ("        subtype <id>_id is <base>;");
      Put_Line ("        <prefix>_... : constant <id>_id := ...;");
      Put_Line ("-enum-image");
      Put_Line ("    Generate image function for integer enums");
      Put_Line ("-var name,table,field,criteria,comment");
      Put_Line ("    Similar to -enum, but dumps one specific value");
      Put_Line ("    from a table, selected with criteria.");
      Put_Line ("-text: generate a textual description of the database,");
      Put_Line ("    instead of usual output. Disables -api.");
      Put_Line ("-omit-schema: Schema name have to be omitted in text output");
      Put_Line ("-createdb: Creates the database given by -dbname");
      Put_Line ("-adacreate: Generates an Ada function to create the schema");
      Put_Line ("    and load the initial data (embedded files from -dbmodel");
      Put_Line ("    and -load, if specified). This requires -api");
      Put_Line ("-api PKG: generate an Ada package describing the schema");
      Put_Line ("    This is the default output, with PKG='database'");
      Put_Line
        ("-api-enums PKG: generates an Ada package that extracts values");
      Put_Line
        ("    from a database (see -enum and -var). Similar to -api, but");
      Put_Line ("    does not dump the tables schema");
      Put_Line ("-orm PKG: generate a high-level Ada package to manipulate");
      Put_Line ("    Ada objects rather than SQL queries. This package");
      Put_Line ("    depends on the one generated by -api.");
      Put_Line ("-ormtables LIST: a comma-separated list of tables for which");
      Put_Line ("    an ORM binding should be generated. The default is");
      Put_Line ("    to bind all tables");
      Put_Line ("-dot: generate in the current directory a file schema.dot");
      Put_Line ("    representing the database schema. If possible, this is");
      Put_Line ("    converted to Postscript via the graphviz utility 'dot'");
      Put_Line ("-load FILE: load the file contents into the database.");
      Put_Line ("    You should also use -dbmodel to specify the schema.");
      Put_Line ("-output DIR: directory in which created files should go");
      Put_Line ("    Applies to -api, -orm and -api-enums");

      GNAT.OS_Lib.OS_Exit (0);
   end Print_Help;

   ----------
   -- Main --
   ----------

   procedure Main is
      DB_Name    : GNAT.OS_Lib.String_Access := new String'("");
      DB_Host    : GNAT.OS_Lib.String_Access := new String'("");
      DB_User    : GNAT.OS_Lib.String_Access := new String'("");
      DB_Passwd  : GNAT.OS_Lib.String_Access := new String'("");
      DB_Port    : Integer := -1;
      DB_Type    : GNAT.OS_Lib.String_Access := new String'("postgresql");
      DB_Filter  : Regexp := Compile (".*");

      Enums, Vars : String_Lists.List;
      --  The internal index corresponding to each table. This is used to
      --  create the adjacency matrix, that indicates whether there is a known
      --  relationship between two tables.

      Descr       : Database_Description;
      Connection  : Database_Connection;
      Need_Schema : Boolean;
   begin
      loop
         case Getopt ("dbhost= h -help dbname= dbuser= dbpasswd= enum= var="
                      & " dbtype= dbmodel= dot text orm= createdb api="
                      & " adacreate dbport= enum-image dbfilter= omit-schema="
                      & " ormtables= api-enums= load= output=")
         is
            when 'h' | '-' =>
               Print_Help;

            when 'a' =>
               if Full_Switch = "api" then
                  if Parameter /= "" then
                     Free (Generated);
                     Generated := new String'(Parameter);
                  end if;
                  Output (Output_Ada_Specs) := True;
                  Output (Output_Ada_Enums) := True;

               elsif Full_Switch = "api-enums" then
                  if Parameter /= "" then
                     Free (Generated);
                     Generated := new String'(Parameter);
                  end if;
                  Output (Output_Ada_Enums) := True;

               elsif Full_Switch = "adacreate" then
                  Output (Output_Adacreate) := True;
               end if;

            when 'd' =>
               if Full_Switch = "dot" then
                  Output (Output_Dot) := True;
               elsif Full_Switch = "dbhost" then
                  Free (DB_Host);
                  DB_Host := new String'(Parameter);
               elsif Full_Switch = "dbname" then
                  Free (DB_Name);
                  DB_Name := new String'(Parameter);
               elsif Full_Switch = "dbuser" then
                  Free (DB_User);
                  DB_User := new String'(Parameter);
               elsif Full_Switch = "dbpasswd" then
                  Free (DB_Passwd);
                  DB_Passwd := new String'(Parameter);
               elsif Full_Switch = "dbtype" then
                  Free (DB_Type);
                  DB_Type := new String'(Parameter);
               elsif Full_Switch = "dbport" then
                  begin
                     DB_Port := Integer'Value (Parameter);
                  exception
                     when Constraint_Error =>
                        DB_Port := -1;
                  end;
               elsif Full_Switch = "dbmodel" then
                  Free (DB_Model);
                  DB_Model := new String'(Parameter);
               elsif Full_Switch = "dbfilter" then
                  DB_Filter := GNAT.Regexp.Compile (Parameter);
               end if;

            when 'c' =>
               Output (Output_Createdb) := True;

            when 'e' =>
               if Full_Switch = "enum-image" then
                  Output (Output_Ada_Enums_Image) := True;
               else
                  Append (Enums, Parameter);
               end if;

            when 'v' =>
               Append (Vars, Parameter);

            when 't' =>
               Output (Output_Text) := True;

            when 'l' =>
               Output (Output_Load) := True;
               Load_File := Create (+Parameter);

            when 'o' =>
               if Full_Switch = "ormtables" then
                  Free (Orm_Tables);
                  Orm_Tables := new String'(Parameter);

               elsif Full_Switch = "orm" then
                  if Parameter /= "" then
                     Free (Generated_Orm);
                     Generated_Orm := new String'(Parameter);
                  end if;

                  Output (Output_Orm) := True;

               elsif Full_Switch = "output" then
                  Output_Dir := Create (+Parameter);
               elsif Full_Switch = "omit-schema" then
                  File_IO.Omit_Schema.Include (Parameter);
               end if;

            when others =>
               exit;
         end case;
      end loop;

      if Output = (Output_Kind => False) then
         Output := (Output_Ada_Specs => True,
                    Output_Ada_Enums => True,
                    others           => False);
      end if;

      Need_Schema := False;
      for J in Need_Schema_For_Output'Range loop
         if Output (J) and then Need_Schema_For_Output (J) then
            Need_Schema := True;
            exit;
         end if;
      end loop;

      if DB_Name.all /= "" then
         --  If the user specified the name of a database, we connect to it.
         --  This might be to read the schema, or to create the database

         if DB_Type.all = "postgresql" then
            Descr := GNATCOLL.SQL.Postgres.Setup
              (Database      => DB_Name.all,
               User          => DB_User.all,
               Host          => DB_Host.all,
               Password      => DB_Passwd.all,
               Port          => DB_Port,
               Cache_Support => False);
         elsif DB_Type.all = "sqlite" then
            Descr := GNATCOLL.SQL.Sqlite.Setup
              (Database      => DB_Name.all,
               Cache_Support => False);
         else
            Ada.Text_IO.Put_Line ("Unknown dbtype: " & DB_Type.all);
            Set_Exit_Status (Failure);
            return;
         end if;

         if Descr = null then
            Ada.Text_IO.Put_Line ("Database not supported: " & DB_Type.all);
            Set_Exit_Status (Failure);
            return;
         end if;

         Connection := Descr.Build_Connection;
         DB_IO.DB := Connection;
         DB_IO.Filter := DB_Filter;

         --  If we should read the model from the database

         if DB_Model = null then
            if Need_Schema then
               Schema := DB_IO.Read_Schema;
            end if;
         end if;
      end if;

      if DB_Model /= null then
         File_IO.File := GNATCOLL.VFS.Create (+DB_Model.all);

         if Need_Schema then
            Schema := File_IO.Read_Schema;
         end if;
      end if;

      --  Output will always be to stdout

      File_IO.File := No_File;

      Free (DB_Name);
      Free (DB_Host);
      Free (DB_User);
      Free (DB_Passwd);
      Free (DB_Type);

      if Need_Schema and then Schema = No_Schema then
         Put_Line ("Could not parse the database schema, exiting...");
         Set_Exit_Status (Failure);
         return;
      end if;

      --  The order below is significant, in case multiple switches are
      --  specified on the command line

      if Output (Output_Createdb) then
         DB_IO.Write_Schema (Schema);
      end if;

      if Output (Output_Load) then
         Load_Data
           (DB     => DB_IO.DB,
            File   => Load_File,
            Schema => Schema);
         DB_IO.DB.Commit;
      end if;

      if Output (Output_Ada_Specs)
        or else Output (Output_Ada_Enums)
        or else Output (Output_Adacreate)
      then
         Dump_Tables (Connection, Enums, Vars);
         Generate (Generated.all,
                   Include_Database_Create => Output (Output_Adacreate));
      end if;

      if Output (Output_Text) then
         File_IO.Write_Schema (Schema);
      end if;

      if Output (Output_Orm) then
         Generate_Orm;
      end if;

      if Output (Output_Dot) then
         Generate_Dot;
      end if;

      Free (DB_Model);
      Free (Generated);
      Free (Generated_Orm);

   exception
      when GNAT.Command_Line.Invalid_Switch
         | GNAT.Command_Line.Invalid_Parameter =>
         Put_Line ("gnatcoll_db2ada: unrecognized option '-"
                   & Full_Switch & "'");
         Put_Line ("Try `gnatcoll_db2ada --help` for more information.");
         Set_Exit_Status (Failure);
   end Main;

   ---------------------
   -- Add_Enumeration --
   ---------------------

   procedure Add_Enumeration
     (DB : access Database_Connection_Record'Class;
      Table, Id, Name, Prefix, Base_Type : String)
   is
      function Quote (Str : String) return String;
      --  Replace characters that are not acceptable in an Ada
      --  identifier

      -----------
      -- Quote --
      -----------

      function Quote (Str : String) return String is
         S : Unbounded_String;
      begin
         for C in Str'Range loop
            if not Is_Alphanumeric (Str (C)) then
               --  Some special cases to try and keep meaningful
               --  identifiers.

               if Str (C) = '+' then
                  Append (S, "plus");
               elsif Str (C) = '?' then
                  Append (S, "question");
               else
                  Append (S, '_');
               end if;
            else
               Append (S, Str (C));
            end if;
         end loop;

         declare
            S2 : constant String := To_String (S);
         begin
            for C in reverse S2'Range loop
               if S2 (C) /= '_' then
                  return S2 (S2'First .. C);
               end if;
            end loop;
            return "";
         end;
      end Quote;

      Enum : Dumped_Enums;
      R    : GNATCOLL.SQL.Exec.Forward_Cursor;

   begin
      Enum.Table  := To_Unbounded_String (Table);
      Enum.Id     := To_Unbounded_String (Id);
      Enum.Prefix := To_Unbounded_String (Prefix);

      if Base_Type = "" then
         Enum.Base_Type := To_Unbounded_String ("Integer");
      else
         Enum.Base_Type := To_Unbounded_String (Base_Type);
      end if;

      Enum.Type_Name := To_Unbounded_String (Prefix & "_Id");

      if Name /=  "" then
         R.Fetch
           (DB,
            "SELECT """ & Id
            & """, """ & Name & """ FROM """
            & Table & """ ORDER BY lower(" & Name & ")");
         while Has_Row (R) loop
            Append (Enum.Values, Value (R, 0));
            Append (Enum.Names,  Quote (Value (R, 1)));
            Next (R);
         end loop;
      end if;

      Append (Enumerations, Enum);
   end Add_Enumeration;

   ------------------
   -- Add_Variable --
   ------------------

   procedure Add_Variable
     (DB : access Database_Connection_Record'Class;
      Name, Table, Field, Where, Comment : String)
   is
      R   : GNATCOLL.SQL.Exec.Forward_Cursor;
      Var : Dumped_Vars;
   begin
      if Where /= "" then
         R.Fetch (DB, "SELECT " & Field
                  & " FROM """ & Table & """ WHERE " & Where);
      else
         R.Fetch (DB, "SELECT " & Field & " FROM """ & Table & '"');
      end if;

      Var.Name    := To_Unbounded_String (Name);
      Var.Value   := To_Unbounded_String (Value (R, 0));
      Var.Comment := To_Unbounded_String (Comment);
      Append (Variables, Var);
   end Add_Variable;

   -----------------
   -- Dump_Tables --
   -----------------

   procedure Dump_Tables
     (Connection : access Database_Connection_Record'Class;
      Enums      : String_Lists.List;
      Vars       : String_Lists.List)
   is
      C : String_Lists.Cursor;
      Comma1, Comma2, Comma3, Comma4 : Integer;
   begin
      C := First (Enums);
      while Has_Element (C) loop
         declare
            Str : constant String := Element (C);
         begin
            Comma1 := Index (Str, ",");
            Comma2 := Index (Str (Comma1 + 1 .. Str'Last), ",");
            Comma3 := Index (Str (Comma2 + 1 .. Str'Last), ",");
            Comma4 := Index (Str (Comma3 + 1 .. Str'Last), ",");
            if Comma4 < Str'First
              or Comma3 < Str'First
              or Comma2 < Str'First
            then
               Ada.Text_IO.Put_Line ("Missing arguments for -enum " & Str);
               return;
            end if;

            Add_Enumeration
              (Connection,
               Table     => Str (Str'First .. Comma1 - 1),
               Id        => Str (Comma1 + 1 .. Comma2 - 1),
               Name      => Str (Comma2 + 1 .. Comma3 - 1),
               Prefix    => Str (Comma3 + 1 .. Comma4 - 1),
               Base_Type => Str (Comma4 + 1 .. Str'Last));
         end;
         Next (C);
      end loop;

      C := First (Vars);
      while Has_Element (C) loop
         declare
            Str : constant String := Element (C);
         begin
            Comma1 := Index (Str, ",");
            Comma2 := Index (Str (Comma1 + 1 .. Str'Last), ",");
            Comma3 := Index (Str (Comma2 + 1 .. Str'Last), ",");
            Comma4 := Index (Str (Comma3 + 1 .. Str'Last), ",");
            if Comma4 < Str'First then
               Ada.Text_IO.Put_Line ("Missing arguments for -var " & Str);
               return;
            end if;

            Add_Variable
              (Connection,
               Name      => Str (Str'First .. Comma1 - 1),
               Table     => Str (Comma1 + 1 .. Comma2 - 1),
               Field     => Str (Comma2 + 1 .. Comma3 - 1),
               Where     => Str (Comma3 + 1 .. Comma4 - 1),
               Comment   => Str (Comma4 + 1 .. Str'Last));
         end;
         Next (C);
      end loop;
   end Dump_Tables;

begin
   GNATCOLL.Traces.Parse_Config_File;
   Main;
   GNATCOLL.Traces.Finalize;

exception
   when E : Invalid_Type =>
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, Exception_Message (E));
      Set_Exit_Status (Failure);

   when E : others =>
      Trace (Me, E);
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "A database error occurred, please try again...");
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, Exception_Information (E));
      Set_Exit_Status (Failure);
end GNATCOLL_Db2Ada;
