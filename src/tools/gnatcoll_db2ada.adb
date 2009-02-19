-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2005-2009, AdaCore                  --
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
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with GNAT.Command_Line;          use GNAT.Command_Line;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNATCOLL.SQL.Exec;          use GNATCOLL.SQL, GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Postgres;      use GNATCOLL.SQL.Postgres;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

procedure GNATCOLL_Db2Ada is

   Generated : constant String := "Database";

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
   end record;

   package Attribute_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Attribute_Description);
   use Attribute_Lists;

   type Query_Description is record
      Table       : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Query_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Query_Description);
   use Query_Lists;
   Queries : Query_Lists.List;

   type Foreign_Key_Description is record
      To_Table : Ada.Strings.Unbounded.Unbounded_String;
      From_Attributes, To_Attributes : String_Lists.List;
   end record;
   --  A foreign key from one table to another
   --      From_Table (From_Attributes) REFERENCES To_Table (To_Attributes)

   package Foreign_Keys is new Ada.Containers.Doubly_Linked_Lists
     (Foreign_Key_Description);
   use Foreign_Keys;

   type Table_Description is record
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

   function Attribute_Name
     (Descr     : Table_Description;
      Index     : Natural) return String;
   --  Return the attribute name given its index in the table. Information
   --  is extracted from All_Attrs

   procedure Get_Database_Connection
     (Descr : in out Database_Description;
      Enums : out String_Lists.List;
      Vars  : out String_Lists.List);
   --  Get the list of parameters to use to connect to the postgres database

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
   --  Get the list of tables in the database

   procedure Generate (Generated : String);
   procedure Generate (Generated : String) is separate;
   --  Generate the actual output. This can be implemented either through
   --  Ada.Text_IO or using the templates parser

   -----------------
   -- To_Ada_Type --
   -----------------

   procedure To_Ada_Type
     (SQL_Type    : String;
      Table, Attr : String;
      Descr       : in out Attribute_Description)
   is
      C      : Enumeration_Lists.Cursor := First (Enumerations);
      Enum   : Dumped_Enums;
   begin
      if SQL_Type = "boolean" then
         Descr.Field_Type := To_Unbounded_String ("Boolean");
         Descr.Ada_Type   := To_Unbounded_String ("Boolean");
         Descr.Value_Func := To_Unbounded_String ("Boolean_Value");

      elsif SQL_Type = "text"
        or else (SQL_Type'Length >= 9
                 and then SQL_Type (SQL_Type'First .. SQL_Type'First + 8) =
                   "character")
      then
         Descr.Field_Type := To_Unbounded_String ("Text");
         Descr.Ada_Type   := To_Unbounded_String ("String");
         Descr.Value_Func := To_Unbounded_String ("Value");

      elsif SQL_Type = "integer"
        or else SQL_Type = "smallint"
        or else SQL_Type = "oid"
        or else (SQL_Type'Length >= 7
                 and then SQL_Type (SQL_Type'First .. SQL_Type'First + 6) =
                   "numeric")
      then
         Descr.Field_Type := To_Unbounded_String ("Integer");
         Descr.Ada_Type   := To_Unbounded_String ("Integer");
         Descr.Value_Func := To_Unbounded_String ("Integer_Value");

      elsif SQL_Type = "date"
        or else SQL_Type = "timestamp without time zone"
        or else SQL_Type = "timestamp with time zone"
      then
         Descr.Field_Type := To_Unbounded_String ("Time");
         Descr.Ada_Type   := To_Unbounded_String ("Ada.Calendar.Time");
         Descr.Value_Func := To_Unbounded_String ("Time_Value");

      elsif SQL_Type = "double precision" then
         Descr.Field_Type := To_Unbounded_String ("Float");
         Descr.Ada_Type   := To_Unbounded_String ("Float");
         Descr.Value_Func := To_Unbounded_String ("Float_Value");

      else
         Put_Line (Standard_Error,
                   "Don't know how to convert type " & SQL_Type);
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
               --  a new foreign key, as opposed to a new attribute in the same
               --  key

               if Prev_Index /= -1 then
                  Append (Table.Foreign, Descr);
               end if;

               Prev_Index  := Index;
               Descr       :=
                 (To_Table        => To_Unbounded_String (Foreign_Table),
                  From_Attributes => String_Lists.Empty_List,
                  To_Attributes   => String_Lists.Empty_List);
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

   -----------------
   -- Parse_Table --
   -----------------

   procedure Parse_Table
     (Connection  : access Database_Connection_Record'Class;
      Table       : String;
      Attributes  : in out Attribute_Lists.List)
   is
      procedure On_Field
        (Name        : String;
         Typ         : String;
         Index       : Natural;
         Description : String);
      --  Called when a new field is discovered

      procedure On_Field
        (Name        : String;
         Typ         : String;
         Index       : Natural;
         Description : String)
      is
         Descr : Attribute_Description;
      begin
         Descr.Name     := To_Unbounded_String (Name);
         To_Ada_Type (Typ, Table, Name, Descr);
         Descr.Index    := Index;
         Descr.Description := To_Unbounded_String (Description);
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

      procedure On_Table (Name, Description : String);
      --  Called when a new table is discovered

      procedure On_Table (Name, Description : String) is
         Descr : Table_Description;
      begin
         T := T + 1;
         Descr.Index       := T;
         Descr.Description := To_Unbounded_String (Description);
         Parse_Table (Connection, Name, Descr.Attributes);
         Insert (Tables, Name, Descr);
      end On_Table;

   begin
      Foreach_Table (Connection, On_Table'Access);
   end Get_Tables;

   -----------------------------
   -- Get_Database_Connection --
   -----------------------------

   procedure Get_Database_Connection
     (Descr : in out Database_Description;
      Enums : out String_Lists.List;
      Vars  : out String_Lists.List)
   is
      DB_Name   : GNAT.OS_Lib.String_Access := new String'("");
      DB_Host   : GNAT.OS_Lib.String_Access := new String'("");
      DB_User   : GNAT.OS_Lib.String_Access := new String'("");
      DB_Passwd : GNAT.OS_Lib.String_Access := new String'("");
      DB_Type   : GNAT.OS_Lib.String_Access := new String'(DBMS_Postgresql);
   begin
      loop
         case Getopt ("dbhost: h dbname: dbuser: dbpasswd: enum: var:"
                      & " dbtype: query:") is
            when 'h' =>
               Put_Line
                 ("-dbhost <host>: host on which the database runs");
               Put_Line ("-dbname <name>: name of the database");
               Put_Line
                 ("-dbuser <user>: user name to log in the database");
               Put_Line ("-dbpasswd <passwd>: password for the database");
               Put_Line ("-dbtype <type>: database backend to use"
                         & " (current is " & DB_Type.all & ")");
               New_Line;
               Put_Line ("-enum table,id,name,prefix,base");
               Put_Line
                 ("    Name of a table to dump. This is generally used");
               Put_Line
                 ("    for enumeration-like tables, which might contain");
               Put_Line
                 ("    special values. This will generate Ada code like");
               Put_Line
                 ("        subtype <id>_id is <base>;");
               Put_Line
                 ("        <prefix>_... : constant <id>_id := ...;");
               Put_Line ("-var name,table,field,criteria,comment");
               Put_Line
                 ("    Similar to -enum, but dumps one specific value");
               Put_Line
                 ("    from a table, selected with criteria.");
               Put_Line ("-query table");
               Put_Line
                 ("    Generate an entry for table in the queries package");
               GNAT.OS_Lib.OS_Exit (0);
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
               end if;
            when 'q' =>
               Query_Lists.Append
                 (Queries, (Table => To_Unbounded_String (Parameter)));
            when 'e' =>
               Append (Enums, Parameter);
            when 'v' =>
               Append (Vars, Parameter);
            when others =>
               exit;
         end case;
      end loop;

      Setup_Database
        (Descr,
         Database      => DB_Name.all,
         User          => DB_User.all,
         Host          => DB_Host.all,
         Password      => DB_Passwd.all,
         DBMS          => DB_Type.all,
         Cache_Support => False);
      Free (DB_Name);
      Free (DB_Host);
      Free (DB_User);
      Free (DB_Passwd);
   end Get_Database_Connection;

   ---------------------
   -- Add_Enumeration --
   ---------------------

   procedure Add_Enumeration
     (DB : access Database_Connection_Record'Class;
      Table, Id, Name, Prefix, Base_Type : String)
   is
      Enum : Dumped_Enums;
      R    : Query_Result;
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
         Execute
           (DB, R,
            "SELECT " & Id & ", " & Name & " FROM " & Table
            & " ORDER BY " & Name);
         for T in 0 .. Tuple_Count (R) - 1 loop
            Append (Enum.Values, Value (R, T, 0));
            Append (Enum.Names,  Prefix & "_" & Value (R, T, 1));
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
      R   : Query_Result;
      Var : Dumped_Vars;
   begin
      Execute
        (DB, R,
         "SELECT " & Field & " FROM " & Table & " WHERE " & Where);

      Var.Name    := To_Unbounded_String (Name);
      Var.Value   := To_Unbounded_String (Value (R, 0, 0));
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

   ----------------------
   -- Generate_Queries --
   ----------------------

   procedure Generate_Queries is
      function Is_Enum (Table, Id, Ada_Type : String) return String;
      --  If Table.Id is a registered enumeration type, return the name of the
      --  Ada type. If not, return Ada_Type

      function Is_Enum (Table, Id, Ada_Type : String) return String is
         C      : Enumeration_Lists.Cursor := First (Enumerations);
         Enum   : Dumped_Enums;
      begin
         --  ??? Not efficient, since we are traversing the list for each field
         --  However, we have a small number of tables in general anyway

         while Has_Element (C) loop
            Enum := Element (C);
            if Enum.Table = Table and then Enum.Id = Id then
               return To_String (Enum.Type_Name);
            end if;

            Next (C);
         end loop;

         return Ada_Type;
      end Is_Enum;

      Spec_File, Body_File : File_Type;
      Q       : Query_Lists.Cursor;
      T       : Tables_Maps.Cursor;
      T_Descr : Table_Description;
      A       : Attribute_Lists.Cursor;
      Count   : Natural;

   begin
      Create (Spec_File, Name => To_Lower (Generated) & "-queries.ads");
      Create (Body_File, Name => To_Lower (Generated) & "-queries.adb");

      Put_Line (Spec_File, "with GNATCOLL.SQL; use GNATCOLL.SQL;");
      Put_Line (Spec_File, "with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;");
      Put_Line (Spec_File, "package " & Generated & ".Queries is");
      Put_Line (Spec_File, "   pragma Warnings (Off);");
      New_Line (Spec_File);
      Put_Line (Spec_File, "   type Base_Info is abstract tagged private;");

      Put_Line (Body_File, "package body " & Generated & ".Queries is");
      Put_Line (Body_File, "   pragma Warnings (Off);");

      Q := First (Queries);
      while Has_Element (Q) loop
         T := Find (Tables, To_String (Element (Q).Table));
         T_Descr := Element (T);

         declare
            Info : constant String :=
              Capitalize (To_String (Element (Q).Table)) & "_Info";
         begin
            New_Line (Spec_File);
            Put_Line (Spec_File, "   type " & Info & " is new Base_Info"
                      & " with private;");

            Count := 0;

            A := First (T_Descr.Attributes);
            while Has_Element (A) loop
               Put_Line
                 (Spec_File, "   function "
                  & Capitalize (To_String (Element (A).Name))
                  & " (Self : " & Info & ") return "
                  & To_String (Element (A).Ada_Type) & ";");

               New_Line (Body_File);
               Put_Line
                 (Body_File, "   function "
                  & Capitalize (To_String (Element (A).Name))
                  & " (Self : " & Info & ") return "
                  & To_String (Element (A).Ada_Type) & " is");
               Put_Line (Body_File, "   begin");
               Put_Line (Body_File, "      return "
                         & To_String (Element (A).Value_Func)
                         & " (Self.Res,"
                         & " Self.Idx, Self.Base +"
                         & Count'Img & ");");
               Put_Line
                 (Body_File, "   end "
                  & Capitalize (To_String (Element (A).Name)) & ";");

               Count := Count + 1;
               Next (A);
            end loop;
         end;

         Next (Q);
      end loop;

      New_Line (Spec_File);
      Put_Line (Spec_File, "private");
      Put_Line (Spec_File, "   type Base_Info is abstract tagged record");
      Put_Line (Spec_File, "      Res  : GNATCOLL.SQL.Exec.Query_Result;");
      Put_Line (Spec_File, "      Idx  : GNATCOLL.SQL.Exec.Tuple_Index;");
      Put_Line (Spec_File, "      Base : GNATCOLL.SQL.Exec.Field_Index;");
      Put_Line (Spec_File, "   end record;");

      Q := First (Queries);
      while Has_Element (Q) loop
         declare
            Info : constant String :=
              Capitalize (To_String (Element (Q).Table)) & "_Info";
         begin
            New_Line (Spec_File);
            Put_Line (Spec_File, "   type " & Info & " is new Base_Info"
                      & " with null record;");
         end;

         Next (Q);
      end loop;

      Put_Line (Spec_File, "end " & Generated & ".Queries;");
      Put_Line (Body_File, "end " & Generated & ".Queries;");

      Close (Spec_File);
      Close (Body_File);
   end Generate_Queries;

   DB_Descr          : Database_Description;
   Spec_File         : File_Type;
   Body_File         : File_Type;
   Connection        : Database_Connection;
   Enums, Vars       : String_Lists.List;
   --  The internal index corresponding to each table. This is used to create
   --  the adjacency matrix, that indicates whether there is a known
   --  relationship between two tables.

begin
   Get_Database_Connection (DB_Descr, Enums, Vars);
   Connection := Build_Postgres_Connection;
   Reset_Connection (DB_Descr, Connection);

   Dump_Tables (Connection, Enums, Vars);
   Get_Tables  (Connection);

   --  Separate pass to get the foreign keys, since we first need the list of
   --  all tables and their attributes to resolve the names

   Get_Foreign_Keys (Connection);

   --  Create the package Database_Typed_Entities

   Generate (Generated);

   if Length (Queries) /= 0 then
      Generate_Queries;
   end if;

exception
   when E : others =>
      Put_Line (Standard_Error,
                "A database error occurred, please try again...");
      Put_Line (Standard_Error, Exception_Information (E));
      Set_Exit_Status (Failure);
end GNATCOLL_Db2Ada;
