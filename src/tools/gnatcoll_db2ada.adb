-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2005-2010, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
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
with Ada.Containers.Indefinite_Ordered_Maps;  use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Maps;           use Ada.Strings.Maps;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with GNAT.Command_Line;          use GNAT.Command_Line;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Strings;
with GNATCOLL.Mmap;              use GNATCOLL.Mmap;
with GNATCOLL.SQL.Exec;          use GNATCOLL.SQL, GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Postgres;      use GNATCOLL.SQL.Postgres;
with GNATCOLL.SQL.Sqlite;        use GNATCOLL.SQL.Sqlite;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

procedure GNATCOLL_Db2Ada is

   Generated : constant String := "Database";

   type Output_Kind is (Output_Ada_Specs, Output_Text, Output_Createdb);
   Output : Output_Kind := Output_Ada_Specs;
   --  The type of output for this utility

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

   type Attribute_Description is record
      Name        : Ada.Strings.Unbounded.Unbounded_String;
      Field_Type  : Ada.Strings.Unbounded.Unbounded_String;
      Ada_Type    : Ada.Strings.Unbounded.Unbounded_String;
      Value_Func  : Ada.Strings.Unbounded.Unbounded_String;
      Index       : Integer;  --  internal index in database
      Description : Ada.Strings.Unbounded.Unbounded_String;
      Default     : Ada.Strings.Unbounded.Unbounded_String;
      PK          : Boolean;  --  Part of the primary key ?
      Not_Null    : Boolean;
   end record;

   package Attribute_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Attribute_Description);
   use Attribute_Lists;

   type Foreign_Key_Description is record
      To_Table        : Ada.Strings.Unbounded.Unbounded_String;
      From_Attributes : String_Lists.List;
      To_Attributes   : String_Lists.List;
      Ambiguous       : Boolean;
   end record;
   --  A foreign key from one table to another
   --      From_Table (From_Attributes) REFERENCES To_Table (To_Attributes)
   --  Ambiguous is set to True when a To_Table (To_Attribute) tuple appear in
   --  several foreign keys e.g.
   --     (who_contact) REFERENCES contact(id)
   --     (contact)     REFERENCES contact(id)

   package Foreign_Keys is new Ada.Containers.Doubly_Linked_Lists
     (Foreign_Key_Description);
   use Foreign_Keys;

   type Table_Description is record
      Kind        : Relation_Kind;
      Index       : Integer;
      Description : Ada.Strings.Unbounded.Unbounded_String;
      Attributes  : Attribute_Lists.List;
      Foreign     : Foreign_Keys.List;
   end record;

   package Tables_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Table_Description, "<", "=");
   use Tables_Maps;

   procedure To_Ada_Type
     (SQL_Type    : String;
      Table, Attr : String;
      Descr       : in out Attribute_Description);
   --  Return the Ada type matching a SQL type.
   --  Ada_Type could be a specific name based on a --enum if a matching one
   --  was provided

   procedure Get_Foreign_Keys
     (Connection : access Database_Connection_Record'Class);
   --  Compute all the foreign keys between all tables in the database

   Tables            : Tables_Maps.Map;

   procedure Print_Help;
   --  Print the help and exit the application

   function Attribute_Name
     (Descr     : Table_Description;
      Index     : Natural) return String;
   --  Return the attribute name given its index in the table. Information
   --  is extracted from All_Attrs

   procedure Get_Database_Connection;
   --  Get the list of parameters to use to connect to the postgres database

   procedure Mark_FK_As_Ambiguous
     (Table     : in out Table_Description;
      Foreign   : String;
      Ambiguous : out Boolean);
   --  Mark all foreign keys from Table to Foreign as ambiguous (ie there are
   --  multiple references to the same foreign table, so we need special care
   --  in code generation). Ambiguous is set to False if there was no such
   --  FK yet.

   procedure Dump_Tables
     (Connection : access Database_Connection_Record'Class;
      Enums      : String_Lists.List;
      Vars       : String_Lists.List);
   --  Dump the contents of some tables into Trans. We unfortunately need some
   --  hard-coded strings for some tables, and it is better to create Ada
   --  constants for those rather than hard-code them every where. At least
   --  when they are renamed we will be forced to change the Ada code.

   procedure Parse_Table
     (Connection  : access Database_Connection_Record'Class;
      Table       : String;
      Attributes  : in out Attribute_Lists.List);
   --  Get the attributes of the specified table

   procedure Get_Tables
     (Connection : access Database_Connection_Record'Class);
   procedure Get_Tables_From_Txt (File : String);
   --  Get the list of tables in the database. The first version gets it from
   --  an existing database, the second from a text file description.

   procedure Generate_Text;
   --  Generate a textual description of the database

   procedure Generate (Generated : String);
   procedure Generate (Generated : String) is separate;
   --  Generate the actual output. This can be implemented either through
   --  Ada.Text_IO or using the templates parser

   procedure Generate_Createdb;
   --  Generate the SQL commands needed to recreate the tables. This does not
   --  include the creation of the database itself.

   -----------------
   -- To_Ada_Type --
   -----------------

   procedure To_Ada_Type
     (SQL_Type    : String;
      Table, Attr : String;
      Descr       : in out Attribute_Description)
   is
      Typ    : constant String := To_Lower (SQL_Type);
      C      : Enumeration_Lists.Cursor := First (Enumerations);
      Enum   : Dumped_Enums;
   begin
      if Typ = "boolean" then
         Descr.Field_Type := To_Unbounded_String ("Boolean");
         Descr.Ada_Type   := To_Unbounded_String ("Boolean");
         Descr.Value_Func := To_Unbounded_String ("Boolean_Value");

      elsif Typ = "text"
        or else (Typ'Length >= 9
                 and then Typ (Typ'First .. Typ'First + 8) = "character")
      then
         Descr.Field_Type := To_Unbounded_String ("Text");
         Descr.Ada_Type   := To_Unbounded_String ("String");
         Descr.Value_Func := To_Unbounded_String ("Value");

      elsif Typ = "integer"
        or else Typ = "smallint"
        or else Typ = "oid"
        or else (Typ'Length >= 7
                 and then Typ (Typ'First .. Typ'First + 6) = "numeric")
      then
         Descr.Field_Type := To_Unbounded_String ("Integer");
         Descr.Ada_Type   := To_Unbounded_String ("Integer");
         Descr.Value_Func := To_Unbounded_String ("Integer_Value");

      elsif Typ = "date" then
         Descr.Field_Type := To_Unbounded_String ("Date");
         Descr.Ada_Type   := To_Unbounded_String ("Ada.Calendar.Time");
         Descr.Value_Func := To_Unbounded_String ("Time_Value");

      elsif Typ = "timestamp without time zone"
        or else Typ = "timestamp with time zone"
        or else Typ = "timestamp"
      then
         Descr.Field_Type := To_Unbounded_String ("Time");
         Descr.Ada_Type   := To_Unbounded_String ("Ada.Calendar.Time");
         Descr.Value_Func := To_Unbounded_String ("Time_Value");

      elsif Typ = "double precision" then
         Descr.Field_Type := To_Unbounded_String ("Float");
         Descr.Ada_Type   := To_Unbounded_String ("Float");
         Descr.Value_Func := To_Unbounded_String ("Float_Value");

      else
         Put_Line (Standard_Error, "Don't know how to convert type " & Typ);
         Descr.Field_Type := To_Unbounded_String ("");
         Descr.Ada_Type   := To_Unbounded_String ("");
         Descr.Value_Func := To_Unbounded_String ("");
      end if;

      --  ??? Not efficient, since we are traversing the list for each field
      --  However, we have a small number of tables in general anyway

      while Has_Element (C) loop
         Enum := Element (C);
         if Enum.Table = Table and then Enum.Id = Attr then
            Descr.Ada_Type := To_Unbounded_String
              (Generated & '.' & Capitalize (To_String (Enum.Type_Name)));
            Descr.Value_Func := Descr.Ada_Type & " (" & Descr.Value_Func;
            exit;
         end if;

         Next (C);
      end loop;
   end To_Ada_Type;

   --------------------
   -- Attribute_Name --
   --------------------

   function Attribute_Name
     (Descr     : Table_Description;
      Index     : Natural) return String
   is
      A     : Attribute_Lists.Cursor := First (Descr.Attributes);
   begin
      while Has_Element (A) loop
         if Element (A).Index = Index then
            return To_String (Element (A).Name);
         end if;
         Next (A);
      end loop;
      return "";
   end Attribute_Name;

   ----------------------
   -- Get_Foreign_Keys --
   ----------------------

   procedure Get_Foreign_Keys
     (Connection : access Database_Connection_Record'Class)
   is
      procedure Compute_Foreign_Keys
        (Name  : String;
         Table : in out Table_Description);
      --  Compute the foreign keys for a specific table

      --------------------------
      -- Compute_Foreign_Keys --
      --------------------------

      procedure Compute_Foreign_Keys
        (Name  : String;
         Table : in out Table_Description)
      is
         Prev_Index : Integer := -1;
         Descr      : Foreign_Key_Description;

         procedure On_Key
           (Index             : Positive;
            Local_Attribute   : Integer;
            Foreign_Table     : String;
            Foreign_Attribute : Integer);
         --  Called for each foreign key in the table

         procedure On_Key
           (Index             : Positive;
            Local_Attribute   : Integer;
            Foreign_Table     : String;
            Foreign_Attribute : Integer)
         is
         begin
            if Prev_Index /= Index then
               --  A new foreign key, as opposed to a new attribute in the same
               --  key

               if Prev_Index /= -1 then
                  Append (Table.Foreign, Descr);
               end if;

               Prev_Index := Index;
               Descr :=
                 (To_Table        => To_Unbounded_String (Foreign_Table),
                  From_Attributes => String_Lists.Empty_List,
                  To_Attributes   => String_Lists.Empty_List,
                  Ambiguous       => False);

               Mark_FK_As_Ambiguous (Table, Foreign_Table, Descr.Ambiguous);
            end if;

            Append
              (Descr.From_Attributes, Attribute_Name (Table, Local_Attribute));
            Append
              (Descr.To_Attributes,
               Attribute_Name
                 (Element (Find (Tables, Foreign_Table)), Foreign_Attribute));
         end On_Key;

      begin
         Foreach_Foreign_Key
           (Connection,
            Table_Name => Name,
            Callback   => On_Key'Access);

         if Prev_Index /= -1 then
            Append (Table.Foreign, Descr);
         end if;
      end Compute_Foreign_Keys;

      T : Tables_Maps.Cursor := First (Tables);
   begin
      while Has_Element (T) loop
         Update_Element (Tables, T, Compute_Foreign_Keys'Access);
         Next (T);
      end loop;
   end Get_Foreign_Keys;

   --------------------------
   -- Mark_FK_As_Ambiguous --
   --------------------------

   procedure Mark_FK_As_Ambiguous
     (Table     : in out Table_Description;
      Foreign   : String;
      Ambiguous : out Boolean)
   is
      C     : Foreign_Keys.Cursor := First (Table.Foreign);
      Descr : Foreign_Key_Description;
   begin
      Ambiguous := False;

      while Has_Element (C) loop
         Descr := Element (C);
         if Descr.To_Table = Foreign then
            if not Descr.Ambiguous then
               Descr.Ambiguous := True;
               Replace_Element (Table.Foreign, C, Descr);
            end if;

            Ambiguous := True;
            return;
         end if;
         Next (C);
      end loop;
   end Mark_FK_As_Ambiguous;

   -----------------
   -- Parse_Table --
   -----------------

   procedure Parse_Table
     (Connection  : access Database_Connection_Record'Class;
      Table       : String;
      Attributes  : in out Attribute_Lists.List)
   is
      procedure On_Field
        (Name           : String;
         Typ            : String;
         Index          : Natural;
         Description    : String;
         Default_Value  : String;
         Is_Primary_Key : Boolean;
         Not_Null       : Boolean);
      --  Called when a new field is discovered

      procedure On_Field
        (Name           : String;
         Typ            : String;
         Index          : Natural;
         Description    : String;
         Default_Value  : String;
         Is_Primary_Key : Boolean;
         Not_Null       : Boolean)
      is
         Descr : Attribute_Description;
      begin
         Descr.Name     := To_Unbounded_String (Name);
         To_Ada_Type (Typ, Table, Name, Descr);
         Descr.Index    := Index;
         Descr.Description := To_Unbounded_String (Description);
         Descr.PK       := Is_Primary_Key;
         Descr.Not_Null := Not_Null;
         Descr.Default  := To_Unbounded_String (Default_Value);
         Append (Attributes, Descr);
      end On_Field;

   begin
      Foreach_Field
        (Connection,
         Table_Name => Table,
         Callback   => On_Field'Access);
   end Parse_Table;

   ----------------
   -- Get_Tables --
   ----------------

   procedure Get_Tables
     (Connection : access Database_Connection_Record'Class)
   is
      T : Natural := 0;

      procedure On_Table (Name, Description : String; Kind : Relation_Kind);
      --  Called when a new table is discovered

      procedure On_Table (Name, Description : String; Kind : Relation_Kind) is
         Descr : Table_Description;
      begin
         T := T + 1;
         Descr.Kind        := Kind;
         Descr.Index       := T;
         Descr.Description := To_Unbounded_String (Description);
         Parse_Table (Connection, Name, Descr.Attributes);
         Insert (Tables, Name, Descr);
      end On_Table;

   begin
      Foreach_Table (Connection, On_Table'Access);
   end Get_Tables;

   -------------------------
   -- Get_Tables_From_Txt --
   -------------------------

   procedure Get_Tables_From_Txt (File : String) is
      Str : GNAT.Strings.String_Access;
      T   : Natural := 0;
      First, Last : Natural;

      function EOL return Natural;
      --  Return the position of the next End-Of-Line character after First

      function EOW return Natural;
      --  Return the position of end-of-word starting at First

      procedure Parse_Table (Name : String);
      --  Parse a table description

      procedure Parse_FK (Name : String);
      --  Parse all foreign keys for table Name

      function EOL return Natural is
         Last : Natural := First;
      begin
         while Last <= Str'Last and then Str (Last) /= ASCII.LF loop
            Last := Last + 1;
         end loop;
         return Last;
      end EOL;

      function EOW return Natural is
         Last : Natural := First;
      begin
         while Last <= Str'Last
           and then Str (Last) /= ASCII.HT
         loop
            Last := Last + 1;
         end loop;
         return Last;
      end EOW;

      procedure Parse_Table (Name : String) is
         Descr : Table_Description;
         Attr  : Attribute_Description;
      begin
         T := T + 1;
         Descr.Kind        := Kind_Table;
         Descr.Index       := T;
         Descr.Description := Null_Unbounded_String;

         Attr.Index := -1;

         First := EOL + 1;
         while First <= Str'Last
           and then Str (First) = ASCII.HT
         loop
            First := First + 1;
            Last := EOW;

            if Str (First .. Last - 1) = "FK:" then
               --  Skip foreign keys for now, we'll do a second pass once we
               --  know all tables and fields
               First := EOL + 1;

            elsif Str (First .. Last - 1) = "DOC:" then
               --  Applies to the previous attribute, still in Attr

               First := EOL + 1;
               Attr.Description := To_Unbounded_String
                 (Str (Last + 1 .. First - 2));
               Replace_Element
                 (Descr.Attributes, Descr.Attributes.Last, Attr);

            else
               Attr.Name  := To_Unbounded_String (Str (First .. Last - 1));

               First := Last + 1;
               Last  := EOW;
               To_Ada_Type
                 (SQL_Type => Str (First .. Last - 1),
                  Attr     => To_String (Attr.Name),
                  Table    => Name,
                  Descr    => Attr);

               Attr.Index := Attr.Index + 1;
               Attr.Description := Null_Unbounded_String;

               First := Last + 1;
               Attr.PK    := Str (First .. First + 1) = "PK";
               First := EOW + 1;

               Attr.Not_Null := First + 7 <= Str'Last
                 and then Str (First .. First + 7) = "NOT NULL";
               First := EOW + 1;

               Last := EOL;
               Attr.Default  := To_Unbounded_String (Str (First .. Last - 1));

               Append (Descr.Attributes, Attr);

               First := Last + 1;
            end if;
         end loop;

         Insert (Tables, Name, Descr);
      end Parse_Table;

      procedure Parse_FK (Name : String) is
         Descr   : Table_Description := Element (Tables.Find (Name));
         FK    : Foreign_Key_Description;
         Last_In_Line : Natural;
      begin
         First := EOL + 1;
         while First <= Str'Last
           and then Str (First) = ASCII.HT
         loop
            First := First + 1;
            Last := EOW;

            if Str (First .. Last - 1) = "FK:" then
               First := Last + 1;
               Last  := EOW;
               FK :=
                 (To_Table  => To_Unbounded_String (Str (First .. Last - 1)),
                  Ambiguous => False,
                  From_Attributes => String_Lists.Empty_List,
                  To_Attributes   => String_Lists.Empty_List);

               Mark_FK_As_Ambiguous
                 (Descr, To_String (FK.To_Table), FK.Ambiguous);

               First := Last + 1;
               Last_In_Line := EOL;

               while First < Last_In_Line
                 and then Str (First) /= '-'
               loop
                  Last := First;
                  while Last < Last_In_Line
                    and then Str (Last) /= ' '
                    and then Str (Last) /= '-'
                  loop
                     Last := Last + 1;
                  end loop;

                  Append (FK.From_Attributes, Str (First .. Last - 1));

                  First := Last;
                  while Str (First) = ' ' loop
                     First := First + 1;
                  end loop;
               end loop;

               First := First + 2; --  skip '->'
               while Str (First) = ' ' loop
                  First := First + 1;
               end loop;

               while First < Last_In_Line loop
                  Last := EOW;
                  Append (FK.To_Attributes, Str (First .. Last - 1));
                  First := Last + 1;
               end loop;

               First := Last_In_Line + 1;
            else
               First := EOL + 1; --  Already done
            end if;
         end loop;
      end Parse_FK;

   begin
      Str := Read_Whole_File (File);
      First := Str'First;

      while First <= Str'Last loop
         Last := EOL;

         if First + 5 <= Last
           and then Str (First .. First + 5) = "table "
         then
            Parse_Table (Name => Str (First + 6 .. Last - 1));
         else
            First := Last + 1;  --  Skip line
         end if;
      end loop;

      --  Now a second pass to get all foreign keys

      First := Str'First;
      while First <= Str'Last loop
         Last := EOL;

         if First + 5 <= Last
           and then Str (First .. First + 5) = "table "
         then
            Parse_FK (Name => Str (First + 6 .. Last - 1));
         else
            First := Last + 1;
         end if;
      end loop;

      Free (Str);

   exception
      when Name_Error =>
         Put_Line ("Could not open " & File);
   end Get_Tables_From_Txt;

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
      Put_Line ("-dbtype <type>: database backend to use"
                & " (default is " & DBMS_Postgresql & ")");
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
      Put_Line ("       instead of usual output");
      Put_Line ("-createdb: return the SQL commands to create the database");

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
      DB_Type   : GNAT.OS_Lib.String_Access := new String'(DBMS_Postgresql);

      Enums, Vars : String_Lists.List;
      --  The internal index corresponding to each table. This is used to
      --  create the adjacency matrix, that indicates whether there is a known
      --  relationship between two tables.

      Connection : Database_Connection;
      Descr      : Database_Description;
   begin
      loop
         case Getopt ("dbhost= h -help dbname= dbuser= dbpasswd= enum= var="
                      & " dbtype= dbmodel= text createdb") is
            when 'h' | '-' =>
               Print_Help;

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
         Setup_Database
           (Descr,
            Database      => DB_Name.all,
            User          => DB_User.all,
            Host          => DB_Host.all,
            Password      => DB_Passwd.all,
            DBMS          => DB_Type.all,
            Cache_Support => False);

         if Get_DBMS (Descr) = DBMS_Postgresql then
            Connection := Build_Postgres_Connection (Descr);
         elsif Get_DBMS (Descr) = DBMS_Sqlite then
            Connection := Build_Sqlite_Connection (Descr);
         else
            Put_Line ("Unknown dbtype: " & Get_DBMS (Descr));
            return;
         end if;

         Reset_Connection (Descr, Connection);

         Dump_Tables (Connection, Enums, Vars);
         Get_Tables  (Connection);

         --  Separate pass to get the foreign keys, since we first need the
         --  list of all tables and their attributes to resolve the names

         Get_Foreign_Keys (Connection);

      else
         Get_Tables_From_Txt (DB_Model.all);
      end if;

      Free (DB_Name);
      Free (DB_Host);
      Free (DB_User);
      Free (DB_Passwd);
      Free (DB_Model);
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
               Put_Line ("Missing arguments for -enum " & Str);
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
               Put_Line ("Missing arguments for -var " & Str);
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

   -------------------
   -- Generate_Text --
   -------------------

   procedure Generate_Text is
      C : Tables_Maps.Cursor := First (Tables);
      T_Descr : Table_Description;
      A  : Attribute_Lists.Cursor;
      K  : Foreign_Keys.Cursor;
      FK : Foreign_Key_Description;
      S  : String_Lists.Cursor;
   begin
      --  All tables and their attributes

      while Has_Element (C) loop
         T_Descr := Element (C);
         case T_Descr.Kind is
            when Kind_Table => Put_Line ("table " & Key (C));
            when Kind_View  => Put_Line ("view " & Key (C));
         end case;

         A := First (T_Descr.Attributes);
         while Has_Element (A) loop
            Put (ASCII.HT & To_String (Element (A).Name)
                 & ASCII.HT & To_String (Element (A).Field_Type));

            if Element (A).PK then
               Put (ASCII.HT & "PK");
            else
               Put (ASCII.HT);
            end if;

            if Element (A).Not_Null then
               Put (ASCII.HT & "NOT NULL");
            else
               Put (ASCII.HT & "NULL");
            end if;

            Put_Line (ASCII.HT & To_String (Element (A).Default));

            if Element (A).Description /= "" then
               Put_Line
                 (ASCII.HT & "DOC:" & ASCII.HT
                  & To_String
                    (Translate (Element (A).Description,
                     Mapping => To_Mapping ("" & ASCII.LF, " "))));
            end if;

            Next (A);
         end loop;

         K := First (T_Descr.Foreign);
         while Has_Element (K) loop
            FK := Element (K);

            Put (ASCII.HT & "FK:"
                 & ASCII.HT & To_String (FK.To_Table)
                 & ASCII.HT);

            S  := First (FK.From_Attributes);
            while Has_Element (S) loop
               Put (Element (S) & " ");
               Next (S);
            end loop;

            Put ("-> ");

            S  := First (FK.To_Attributes);
            while Has_Element (S) loop
               Put (Element (S) & " ");
               Next (S);
            end loop;

            New_Line;
            Next (K);
         end loop;

         New_Line;
         Next (C);
      end loop;
   end Generate_Text;

   -----------------------
   -- Generate_Createdb --
   -----------------------

   procedure Generate_Createdb is
      C : Tables_Maps.Cursor := First (Tables);
      T_Descr : Table_Description;
      A  : Attribute_Lists.Cursor;
      K  : Foreign_Keys.Cursor;
      FK : Foreign_Key_Description;
      S  : String_Lists.Cursor;
      First_Line : Boolean;
   begin
      --  All tables and their attributes

      while Has_Element (C) loop
         T_Descr := Element (C);

         case T_Descr.Kind is
            when Kind_Table =>
               Put_Line ("CREATE TABLE " & Key (C) & " (");
               First_Line := True;

               A := First (T_Descr.Attributes);
               while Has_Element (A) loop
                  if not First_Line then
                     Put_Line (",");
                  end if;
                  First_Line := False;

                  Put ("   " & To_String (Element (A).Name)
                       & " " & To_String (Element (A).Field_Type));

                  if Element (A).Not_Null then
                     Put (" NOT NULL");
                  end if;

                  if Element (A).Default /= "" then
                     Put (" DEFAULT " & To_String (Element (A).Default));
                  end if;

                  Next (A);
               end loop;

               A := First (T_Descr.Attributes);
               while Has_Element (A) loop
                  if Element (A).PK then
                     Put_Line (",");
                     Put ("   PRIMARY KEY ("
                          & To_String (Element (A).Name)
                          & ")");
                  end if;
                  Next (A);
               end loop;

               K := First (T_Descr.Foreign);
               while Has_Element (K) loop
                  FK := Element (K);
                  Put_Line (",");
                  Put ("   FOREIGN KEY (");

                  S  := First (FK.From_Attributes);
                  while Has_Element (S) loop
                     if S /= First (FK.From_Attributes) then
                        Put (",");
                     end if;
                     Put (Element (S));
                     Next (S);
                  end loop;

                  Put (") REFERENCES ");
                  Put (To_String (FK.To_Table));
                  Put (" (");

                  S  := First (FK.To_Attributes);
                  while Has_Element (S) loop
                     if S /= First (FK.To_Attributes) then
                        Put (",");
                     end if;
                     Put (Element (S));
                     Next (S);
                  end loop;

                  Put (")");
                  Next (K);
               end loop;

               Put_Line (");");

            when Kind_View  =>
               Put_Line ("CREATE VIEW " & Key (C) & " AS <unsupported>;");
         end case;

         Next (C);
      end loop;
   end Generate_Createdb;

begin
   Get_Database_Connection;

   --  Create the package Database_Typed_Entities

   case Output is
      when Output_Ada_Specs =>
         Generate (Generated);
      when Output_Text =>
         Generate_Text;
      when Output_Createdb =>
         Generate_Createdb;
   end case;

exception
   when E : others =>
      Put_Line (Standard_Error,
                "A database error occurred, please try again...");
      Put_Line (Standard_Error, Exception_Information (E));
      Set_Exit_Status (Failure);
end GNATCOLL_Db2Ada;
