-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                    Copyright (C) 2011, AdaCore                    --
--                                                                   --
-- This is free software;  you can redistribute it and/or modify  it --
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

--  This package provides introspection capabilities in databases.
--  In particular, they deal with querying the database schema either from an
--  existing (and running) database, or via files. They can also be used to
--  load initial data in a database.
--
--  See also GNATCOLL.SQL.Exec.Foreach_Table for lower-level iterators.
--
--  Most types in this package are smart pointers, that will automatically be
--  deallocated when needed, so you do not need to worry about memory
--  management in this package.

private with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
private with GNATCOLL.Refcount.Weakref;
with GNATCOLL.SQL.Exec;           use GNATCOLL.SQL.Exec;
with GNATCOLL.VFS;
private with GNAT.Strings;

package GNATCOLL.SQL.Inspect is

   type Table_Description is tagged private;
   type Field is tagged private;
   type Field_List is tagged private;

   type Field_Type is
     (Field_Text,
      Field_Integer,
      Field_Date,
      Field_Time,
      Field_Timestamp,
      Field_Float,
      Field_Boolean,
      Field_Autoincrement);

   Invalid_Type : exception;
   --  Raise by Read_Schema when some unknown type is used.

   function From_SQL
     (SQL_Type : String) return Field_Type;
   --  Convert a SQL type to a field type, or raise Invalid_Type

   function To_SQL
     (Typ          : Field_Type;
      For_Database : Boolean := True) return String;
   --  Return the Ada type to use for Typ.
   --  If For_Database is True, the returned value can be used in a
   --  "CREATE TABLE" statement. Otherwise, it is prefixed with "SQL_Field_"
   --  to represent the corresponding GNATCOLL.SQL type

   ------------
   -- Fields --
   ------------
   --  The fields in a table

   No_Field : constant Field;

   function Id (Self : Field) return Positive;
   --  A unique Id for this field, which is not guaranteed to be the same the
   --  next time you query the schema from a running database.

   function Name (Self : Field) return String;
   --  The name of the field (normalized)

   function Description (Self : Field) return String;
   --  Any comment associated with the field

   function Get_Table (Self : Field) return Table_Description'Class;
   --  The table to which the field belongs

   function Get_Type (Self : Field) return Field_Type;
   --  The type of the field.
   --  If the field is a foreign key, this returns the type of the field it
   --  points to, unless a specific type was set.

   procedure Set_Active (Self : in out Field; Active : Boolean);
   function Is_Active (Self : Field) return Boolean;
   --  A special marker that indicates whether special treatement should be
   --  done on a field.
   --  By default, all fields are marked as active, but you could use this
   --  marker to filter out some fields. See Is_Active for tables.

   function Can_Be_Null (Self : Field) return Boolean;
   --  Whether the field can be null

   function Default (Self : Field) return String;
   --  The default value for this field.
   --  This is untyped data, so needs to be analyzed using the field type as
   --  appropriate.

   function Is_PK (Self : Field) return Boolean;
   --  Whether this field is part of the primary key for the table

   function Is_FK (Self : Field) return Field;
   --  If Self is a foreign key, returns the field it points to.
   --  Otherwise, returns No_Field.

   ------------
   -- Tables --
   ------------
   --  The following type represents a table in a database. It provides
   --  introspection capabilities (list of fields,...).

   No_Table : constant Table_Description;

   function Id (Self : Table_Description) return Positive;
   --  A unique Id for the table.
   --  This id is only valid for this session, ie the same table might have a
   --  different id the next time you start your application or retrieve the
   --  schema from a database.

   function Name (Self : Table_Description) return String;
   --  The name of the table (normalized).
   --  We recommand that this name be a plural (like "Objects"), and reserve
   --  the use of singular names for rows.

   function Row_Name (Self : Table_Description) return String;
   --  The name for an object read from the table.
   --  By default, this will be the same as the table's name, but it is often
   --  useful to use singular names instead, in particular when Ada code is
   --  automatically generated from the database schema.

   function Description (Self : Table_Description) return String;
   --  Any comment set for this table.

   function Get_Kind (Self : Table_Description) return Relation_Kind;
   --  Whether Self is an actual table or a view

   function Is_Abstract (Self : Table_Description) return Boolean;
   --  Whether the table is "abstract".
   --  This never occurs when a schema is read from an existing database, since
   --  all tables there are concrete. However, when a schema is defined through
   --  a text file, it might be useful to declare an abstract table for use as
   --  the parent of other concrete tables, to share fields.
   --  This concept is similar to the use of abstract root types in a tagged
   --  type hierarchy.

   procedure Set_Active (Self : in out Table_Description; Active : Boolean);
   function Is_Active (Self : Table_Description) return Boolean;
   --  A special marker that indicates whether special treatement should be
   --  done on a table.
   --  By default, all tables are marked as active, but you could use this
   --  marker to filter out some tables. For instance, when you generate code
   --  from the database schema, the user might want to ignore some tables.
   --  You would mark this table as inactive.

   function Super_Table (Self : Table_Description) return Table_Description;
   --  If the table derives from an abstract table (see Is_Abstract), this will
   --  return that other table. All fields from that other tables are also
   --  valid for Self.

   procedure For_Each_Field
     (Self              : Table_Description;
      Callback          : access procedure (F : in out Field);
      Include_Inherited : Boolean := False);
   --  For all fields in the table, calls Callback.
   --  By default, the fields inherited from an abstract table are not list,
   --  you need to set Include_Inherited to True if you want to access them.

   procedure For_Each_FK
     (Self     : Table_Description;
      Callback : access procedure
        (From, To : Field; Id : Natural; Ambiguous : Boolean));
   --  For all foreign keys that reference another table from Self.
   --  For instance:
   --     CREATE TABLE person
   --        (father INTEGER REFERENCES person(id),
   --         mother INTEGER REFERENCES person(id),
   --         field1 INTEGER,
   --         field2 INTEGER,
   --         FOREIGN KEY (field1, field2) REFERENCES registers(f, m));
   --
   --  would result in four calls to Callback:
   --     * (From=person.father,  To=person.id,   Id=1)
   --     * (From=person.mother,  To=person.id,   Id=2)
   --     * (From=person.field1,  To=registers.f, Id=3)
   --     * (From=person.field2,  To=registers.m, Id=3)
   --
   --  The Id can be used to group foreign keys together to know which ones are
   --  part of the same tuple.
   --
   --  Ambiguous is set to True if there are multiple different foreign keys to
   --  the same table, as is the case for ids 1 and 2 in the above example

   function Field_From_Name
     (Self  : Table_Description'Class; Name  : String) return Field;
   --  Return the field with the specified name.
   --  This also looks for the field in the table we inherit from.

   function Get_PK (Self : Table_Description'Class) return Field;
   --  Assuming there is a single primary key in the table, returns it.
   --  Otherwise, returns No_Field

   ------------
   -- Schema --
   ------------
   --  The whole schema for the database, including all tables

   type DB_Schema is private;
   --  The schema of a database.
   --  This describes all the tables, their fields, the foreign key references,
   --  and various other attributes of the database. This can be queried from
   --  an existing database, or loaded from text files.

   No_Schema : constant DB_Schema;

   Invalid_Table : exception;

   function Get_Table
     (Self : DB_Schema; Name : String) return Table_Description;
   --  Retrieve a table description by name.
   --  Invalid_Table is raised if there is no such table

   procedure For_Each_Table
     (Self     : DB_Schema;
      Callback : access procedure (T : in out Table_Description));
   --  For all tables in the database, calls Callback

   ---------------
   -- Schema IO --
   ---------------

   type Schema_IO is abstract tagged null record;
   --  An object to read and write a schema to various media

   function Read_Schema (Self : Schema_IO) return DB_Schema is abstract;
   --  Retrieve the database schema

   procedure Write_Schema (Self : Schema_IO; Schema : DB_Schema) is abstract;
   --  Write the schema

   type DB_Schema_IO is new Schema_IO with record
      DB : Database_Connection;
   end record;
   overriding function Read_Schema (Self : DB_Schema_IO) return DB_Schema;
   overriding procedure Write_Schema
     (Self : DB_Schema_IO; Schema : DB_Schema);
   --  Read or write the schema to a live database.
   --  "Writing" to the database means creating the appropriate tables and
   --  views (if DB is set, otherwise output statements to stdout)

   type File_Schema_IO is new Schema_IO with record
      File : GNATCOLL.VFS.Virtual_File;
   end record;
   overriding function Read_Schema (Self : File_Schema_IO) return DB_Schema;
   overriding procedure Write_Schema
     (Self : File_Schema_IO; Schema : DB_Schema);
   --  Read or write the schema from a file.
   --  See GNATCOLL documentation for the format of this file.
   --  This will write to stdout if the file is No_File

   function New_Schema_IO
     (File : GNATCOLL.VFS.Virtual_File) return File_Schema_IO'Class;
   function New_Schema_IO (DB : Database_Connection) return DB_Schema_IO'Class;
   --  Return a new schema io. This is similar to creating a variable and
   --  assigning its File or DB field, but is easier to use:
   --      Schema := New_Schema_IO (Create ("file.txt")).Read_Schema;

   Invalid_File : exception;

   procedure Load_Data
     (DB     : access Database_Connection_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Schema : DB_Schema := No_Schema;
      Replace_Newline : Boolean := True);
   procedure Load_Data
     (DB       : access Database_Connection_Record'Class;
      Data     : String;
      Schema   : DB_Schema := No_Schema;
      Location : String := "data";
      Replace_Newline : Boolean := True);
   --  Load data from a file or from memory into the database.
   --  This should be used for initial fixtures when you create a new database,
   --  so in general after a call to Write_Schema.
   --  The format of the file is documented in the GNATCOLL documentation.
   --  The exact commands used for the actual insertion depends on the DBMS
   --  backend, and are optimized as much as possible.
   --  The schema must be specified if your input file potentially includes
   --  cross-references between tables (values starting with "&").
   --
   --  You need to call Commit_Or_Rollback to actually commit the data into the
   --  database, so that a single transaction is used for all data loading when
   --  there are multiple files to load.
   --
   --  If Replace_Newline is True, then a "\n" string will be replaced by an
   --  actual ASCII.LF when stored in the database.

private
   use GNATCOLL.Refcount, GNATCOLL.Refcount.Weakref;

   type Abstract_Table_Description is new Weak_Refcounted with null record;
   package Tables_Ref is new Weakref_Pointers (Abstract_Table_Description);
   type Table_Description is new Tables_Ref.Ref with null record;

   ------------
   -- Fields --
   ------------

   type Field_Description is new Weak_Refcounted with record
      Name        : GNAT.Strings.String_Access;
      Typ         : Field_Type;
      Id          : Positive;
      Description : GNAT.Strings.String_Access;
      Default     : GNAT.Strings.String_Access;
      PK          : Boolean;  --  Part of the primary key ?
      Not_Null    : Boolean;
      Indexed     : Boolean;  --  Do we need an index ?
      FK          : Boolean;  --  Whether this is part of a foreign key
      Table       : Tables_Ref.Weak_Ref;
      Active      : Boolean := True;
   end record;

   overriding procedure Free (Self : in out Field_Description);

   package Fields_Ref is new Weakref_Pointers (Field_Description);
   type Field is new Fields_Ref.Ref with null record;

   No_Field : constant Field := (Fields_Ref.Null_Ref with null record);

   package Field_Lists is new Ada.Containers.Doubly_Linked_Lists (Field);
   type Field_List is new Field_Lists.List with null record;

   Empty_Field_List : constant Field_List :=
     (Field_Lists.Empty_List with null record);

   type Field_Pair is record
      From : Field;
      To   : Field;  --  No field if pointing to foreign primary key
   end record;
   package Pair_Lists is new Ada.Containers.Doubly_Linked_Lists (Field_Pair);

   type Foreign_Key_Description is new Refcounted with record
      To_Table        : Tables_Ref.Weak_Ref;
      --  Needed, since a pair.To might be No_Field, in which case we would
      --  not have this info.

      Revert_Name     : GNAT.Strings.String_Access;
      Fields          : Pair_Lists.List;
      Ambiguous       : Boolean;
   end record;
   --  A foreign key from one table to another
   --      From_Table (From_Attributes) REFERENCES To_Table (To_Attributes)
   --  Ambiguous is set to True when a To_Table (To_Attribute) tuple appear in
   --  several foreign keys e.g.
   --     (who_contact) REFERENCES contact(id)
   --     (contact)     REFERENCES contact(id)

   overriding procedure Free (Self : in out Foreign_Key_Description);

   package Foreign_Refs is new Smart_Pointers (Foreign_Key_Description);
   type Foreign_Key is new Foreign_Refs.Ref with null record;

   function To_Table (FK : Foreign_Key) return Table_Description'Class;
   --  The table that is referenced by the foreign key

   package Foreign_Keys is new Ada.Containers.Doubly_Linked_Lists
     (Foreign_Key);

   -----------------------
   -- Table_Description --
   -----------------------

   type Table_Description_Record is new Abstract_Table_Description with record
      Name        : GNAT.Strings.String_Access := null;
      Row         : GNAT.Strings.String_Access := null;  --  possibly null

      Kind        : Relation_Kind := Kind_Table;
      Id          : Positive := 1;
      Description : GNAT.Strings.String_Access := null;
      Fields      : Field_List := Empty_Field_List;
      Is_Abstract : Boolean := False;
      FK          : Foreign_Keys.List := Foreign_Keys.Empty_List;

      Active      : Boolean := True;

      Has_PK      : Boolean := False;
      --  Whether the table has a primary key

      Super_Table : Table_Description := No_Table;
      --  The table from which we inherit fields
   end record;

   type TDR is access all Table_Description_Record'Class;
   overriding procedure Free (Self : in out Table_Description_Record);

   package Tables_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Table_Description, "<", "=");

   ------------
   -- Schema --
   ------------

   type DB_Schema is record
      Tables  : Tables_Maps.Map;
   end record;

   No_Schema : constant DB_Schema := (Tables  => Tables_Maps.Empty_Map);

   No_Table : constant Table_Description :=
     (Tables_Ref.Null_Ref with null record);
end GNATCOLL.SQL.Inspect;
