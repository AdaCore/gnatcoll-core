-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2005-2011, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- As a special exception, if other files instantiate generics  from --
-- this unit, or you link this  unit with other files to produce  an --
-- executable, this unit does not by itself cause the resulting exe- --
-- cutable  to be covered by  the  GNU General  Public License. This --
-- exception does not however  invalidate any other reasons why  the --
-- executable  file  might  be  covered  by  the  GNU General Public --
-- License.                                                          --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with GNAT.Command_Line;          use GNAT.Command_Line;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Strings;
with GNATCOLL.SQL.Exec;          use GNATCOLL.SQL, GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Inspect;       use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Postgres;
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

procedure GNATCOLL_Db2Ada is

   Generated : GNAT.Strings.String_Access := new String'("Database");

   type Output_Kind is (Output_Ada_Specs, Output_Text, Output_Createdb);
   Output : Output_Kind := Output_Ada_Specs;
   --  The type of output for this utility

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
   --      <prefix>_<name> : constant <Type_Name> := value;

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

   procedure Get_Database_Connection;
   --  Get the list of parameters to use to connect to the postgres database

   procedure Dump_Tables
     (Connection : access Database_Connection_Record'Class;
      Enums      : String_Lists.List;
      Vars       : String_Lists.List);
   --  Dump the contents of some tables into Trans. We unfortunately need some
   --  hard-coded strings for some tables, and it is better to create Ada
   --  constants for those rather than hard-code them every where. At least
   --  when they are renamed we will be forced to change the Ada code.

   procedure Generate (Generated : String);
   procedure Generate (Generated : String) is separate;
   --  Generate the actual output. This can be implemented either through
   --  Ada.Text_IO or using the templates parser

   ----------------
   -- Print_Help --
   ----------------

   procedure Print_Help is
      use Ada.Text_IO;
   begin
      Put_Line ("==== Specifying the database");
      Put_Line ("-dbmodel <file>: textual description of the database schema");
      Put_Line ("                 Not compatible with -enum and -var");
      Put_Line ("-dbhost <host>: host on which the database runs");
      Put_Line ("-dbname <name>: name of the database");
      Put_Line ("-dbuser <user>: user name to log in the database");
      Put_Line ("-dbpasswd <passwd>: password for the database");
      Put_Line ("-dbtype <type>: database backend to use"
                & " (default is postgreSQL)");
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
      Put_Line ("-var name,table,field,criteria,comment");
      Put_Line ("    Similar to -enum, but dumps one specific value");
      Put_Line ("    from a table, selected with criteria.");
      Put_Line ("-text: generate a textual description of the database,");
      Put_Line ("    instead of usual output");
      Put_Line ("-createdb: return the SQL commands to create the database");
      Put_Line ("-api PKG: generate an Ada package describing the schema");
      Put_Line ("    This is the default output, with PKG='database'");

      GNAT.OS_Lib.OS_Exit (0);
   end Print_Help;

   -----------------------------
   -- Get_Database_Connection --
   -----------------------------

   procedure Get_Database_Connection is
      DB_Name   : GNAT.OS_Lib.String_Access := new String'("");
      DB_Host   : GNAT.OS_Lib.String_Access := new String'("");
      DB_User   : GNAT.OS_Lib.String_Access := new String'("");
      DB_Passwd : GNAT.OS_Lib.String_Access := new String'("");
      DB_Model  : GNAT.OS_Lib.String_Access := null;
      DB_Type   : GNAT.OS_Lib.String_Access := new String'("postgresql");

      Enums, Vars : String_Lists.List;
      --  The internal index corresponding to each table. This is used to
      --  create the adjacency matrix, that indicates whether there is a known
      --  relationship between two tables.

      Descr      : Database_Description;
      Connection : Database_Connection;
   begin
      loop
         case Getopt ("dbhost= h -help dbname= dbuser= dbpasswd= enum= var="
                      & " dbtype= dbmodel= text createdb api=") is
            when 'h' | '-' =>
               Print_Help;

            when 'a' =>
               if Parameter /= "" then
                  Free (Generated);
                  Generated := new String'(Parameter);
               end if;

               Output := Output_Ada_Specs;

            when 'd' =>
               if Full_Switch = "dbhost" then
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
               elsif Full_Switch = "dbmodel" then
                  Free (DB_Model);
                  DB_Model := new String'(Parameter);
               end if;

            when 'c' =>
               Output := Output_Createdb;

            when 'e' =>
               Append (Enums, Parameter);

            when 'v' =>
               Append (Vars, Parameter);

            when 't' =>
               Output := Output_Text;

            when others =>
               exit;
         end case;
      end loop;

      if DB_Model = null then
         if DB_Type.all = "postgresql" then
            Descr := GNATCOLL.SQL.Postgres.Setup
              (Database      => DB_Name.all,
               User          => DB_User.all,
               Host          => DB_Host.all,
               Password      => DB_Passwd.all,
               Cache_Support => False);
         elsif DB_Type.all = "sqlite" then
            Descr := GNATCOLL.SQL.Sqlite.Setup
              (Database      => DB_Name.all,
               Cache_Support => False);
         else
            Ada.Text_IO.Put_Line ("Unknown dbtype: " & DB_Type.all);
            return;
         end if;

         Connection := Descr.Build_Connection;
         Reset_Connection (Descr, Connection);
         Dump_Tables (Connection, Enums, Vars);
         DB_IO.DB := Connection;
         DB_IO.Read_Schema (Schema);

      else
         File_IO.Filename := To_Unbounded_String (DB_Model.all);
         File_IO.Read_Schema (Schema);
      end if;

      --  Output will always be to stdout

      File_IO.Filename := To_Unbounded_String ("-");
      DB_IO.DB         := null;

      Free (DB_Name);
      Free (DB_Host);
      Free (DB_User);
      Free (DB_Passwd);
      Free (DB_Model);
      Free (DB_Type);
   end Get_Database_Connection;

   ---------------------
   -- Add_Enumeration --
   ---------------------

   procedure Add_Enumeration
     (DB : access Database_Connection_Record'Class;
      Table, Id, Name, Prefix, Base_Type : String)
   is
      Enum : Dumped_Enums;
      R    : GNATCOLL.SQL.Exec.Forward_Cursor;
   begin
      Enum.Table := To_Unbounded_String (Table);
      Enum.Id    := To_Unbounded_String (Id);

      if Base_Type = "" then
         Enum.Base_Type := To_Unbounded_String ("Integer");
      else
         Enum.Base_Type := To_Unbounded_String (Base_Type);
      end if;

      Enum.Type_Name := To_Unbounded_String (Prefix & "_Id");

      if Name /=  "" then
         R.Fetch
           (DB,
            "SELECT " & Id & ", " & Name & " FROM " & Table
            & " ORDER BY " & Name);
         while Has_Row (R) loop
            Append (Enum.Values, Value (R, 0));
            Append (Enum.Names,  Prefix & "_" & Value (R, 1));
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
      R.Fetch (DB, "SELECT " & Field & " FROM " & Table & " WHERE " & Where);

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
   Get_Database_Connection;

   --  Create the package Database_Typed_Entities

   case Output is
      when Output_Ada_Specs =>
         Generate (Generated.all);

      when Output_Text =>
         File_IO.Write_Schema (Schema);

      when Output_Createdb =>
         DB_IO.Write_Schema (Schema);
   end case;

exception
   when Invalid_Type =>
      Set_Exit_Status (Failure);

   when E : others =>
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "A database error occurred, please try again...");
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, Exception_Information (E));
      Set_Exit_Status (Failure);
end GNATCOLL_Db2Ada;
