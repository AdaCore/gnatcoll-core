------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2011-2017, AdaCore                     --
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

pragma Ada_2012;
private with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Hash_Case_Insensitive;
with GNATCOLL.SQL.Exec;           use GNATCOLL.SQL.Exec;
with GNATCOLL.VFS;
with GNAT.Regexp;                 use GNAT.Regexp;
private with GNATCOLL.Refcount;
private with GNAT.Strings;

package GNATCOLL.SQL.Inspect is
   --  Work around issue with the Ada containers: the tampering checks
   --  mean that the container might be corrupted if used from multiple
   --  tasks, even in read-only.
   --  pragma Suppress (Tampering_Check);

   type Table_Description is tagged private;
   type Field is tagged private;
   type Field_List is tagged private;

   package String_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (String, Ada.Strings.Hash_Case_Insensitive,
      Ada.Strings.Equal_Case_Insensitive,
      Ada.Strings.Equal_Case_Insensitive);

   Invalid_Type : exception;
   --  Raise by Read_Schema when some unknown type is used.

   function Quote_Keyword (Str : String) return String;
   --  If Str is a keyword (or special token) for the DBMS, surround it with
   --  quotes. Otherwise, return Str as is

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

   function Get_Actual_Type (Self : Field) return Field_Mapping_Access;
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
      Callback : access procedure (T : in out Table_Description);
      Alphabetical : Boolean := True);
   --  For all tables in the database, calls Callback.
   --  The order is either alphabetical, or in the order used in the textual
   --  model description.

   ---------------
   -- Schema IO --
   ---------------

   type Schema_IO is abstract tagged null record;
   --  An object to read and write a schema to various media

   function Read_Schema
      (Self : Schema_IO;
       DB   : not null Database_Connection) return DB_Schema is abstract;
   --  Retrieve the database schema

   procedure Write_Schema
      (Self   : Schema_IO;
       DB     : not null Database_Connection;
       Schema : DB_Schema) is abstract;
   --  Write the schema

   type DB_Schema_IO is new Schema_IO with record
      Filter : Regexp := GNAT.Regexp.Compile (".*");
   end record;
   overriding function Read_Schema
      (Self : DB_Schema_IO;
       DB   : not null Database_Connection) return DB_Schema;
   overriding procedure Write_Schema
     (Self   : DB_Schema_IO;
      DB     : not null Database_Connection;
      Schema : DB_Schema);
   --  Read or write the schema to a live database.
   --  "Writing" to the database means creating the appropriate tables and
   --  views (if DB is set, otherwise output statements to stdout).
   --  Only tables matching Filter are output.

   type File_Schema_IO is new Schema_IO with record
      File : GNATCOLL.VFS.Virtual_File;
      Omit_Schema : String_Sets.Set;
   end record;
   overriding function Read_Schema
      (Self : File_Schema_IO;
       DB   : not null Database_Connection) return DB_Schema;
   function Read_Schema
     (Self : File_Schema_IO; Data : String) return DB_Schema;
   overriding procedure Write_Schema
     (Self   : File_Schema_IO;
      DB     : not null Database_Connection;
      Schema : DB_Schema);
   procedure Write_Schema
     (Self   : File_Schema_IO;
      DB     : not null Database_Connection;
      Schema : DB_Schema;
      Puts   : access procedure (S : String);
      Align_Columns : Boolean := True;
      Show_Comments : Boolean := True);
   --  Read or write the schema from a file.
   --  See GNATCOLL documentation for the format of this file.
   --  This will write to Puts parameter if the Self.File is No_File

   function New_Schema_IO
     (File : GNATCOLL.VFS.Virtual_File) return File_Schema_IO'Class;
   function New_Schema_IO return DB_Schema_IO'Class;
   --  Return a new schema io. This is similar to creating a variable and
   --  assigning its File field, but is easier to use:
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

   procedure Load_Data
     (File   : GNATCOLL.VFS.Virtual_File;
      Puts   : access procedure (S : String));
   --  Load the initial data from File, and dumps it to Output without
   --  pretty-printing or comments.

private
   use GNATCOLL.Refcount;

   type Abstract_Table_Description is tagged null record;
   procedure Free (Self : in out Abstract_Table_Description) is null;
   procedure Free_Dispatch (Self : in out Abstract_Table_Description'Class);
   package Tables_Ref
      is new Shared_Pointers (Abstract_Table_Description'Class, Free_Dispatch);
   type Table_Description is new Tables_Ref.Ref with null record;

   ------------
   -- Fields --
   ------------

   type Field_Properties is record
      PK       : Boolean := False;
      Not_Null : Boolean := False;  --  (true for a PK, implicitly)
      Unique   : Boolean := False;  --  Unique field
      Indexed  : Boolean := False;  --  Do we need an index ?
      Noindex  : Boolean := False;  --  Force disabling of indexes
      Case_Insensitive : Boolean := False;
   end record;
   --  The various properties that can be set for a field in a table.

   type Field_Description is record
      Name        : GNAT.Strings.String_Access;
      Typ         : Field_Mapping_Access;
      Id          : Positive;
      Description : GNAT.Strings.String_Access;
      Default     : GNAT.Strings.String_Access;
      Props       : Field_Properties;
      FK          : Boolean;  --  Whether this is part of a foreign key
      Table       : Tables_Ref.Weak_Ref;
      Active      : Boolean := True;
   end record;

   procedure Free (Self : in out Field_Description);

   package Fields_Ref is new Shared_Pointers (Field_Description, Free);
   type Field is new Fields_Ref.Ref with null record;

   No_Field : constant Field := (Fields_Ref.Null_Ref with null record);

   package Field_Lists is new Ada.Containers.Vectors (Natural, Field);
   type Field_List is new Field_Lists.Vector with null record;

   Empty_Field_List : constant Field_List :=
     (Field_Lists.Empty_Vector with null record);

   type Field_Pair is record
      From : Field;
      To   : Field;  --  No field if pointing to foreign primary key
   end record;
   package Pair_Lists is new Ada.Containers.Vectors (Natural, Field_Pair);

   type Foreign_Key_Description is record
      To_Table        : Tables_Ref.Weak_Ref;
      --  Needed, since a pair.To might be No_Field, in which case we would
      --  not have this info.

      Revert_Name     : GNAT.Strings.String_Access;
      Fields          : Pair_Lists.Vector;
      Ambiguous       : Boolean;
   end record;
   --  A foreign key from one table to another
   --      From_Table (From_Attributes) REFERENCES To_Table (To_Attributes)
   --  Ambiguous is set to True when a To_Table (To_Attribute) tuple appear in
   --  several foreign keys e.g.
   --     (who_contact) REFERENCES contact(id)
   --     (contact)     REFERENCES contact(id)

   procedure Free (Self : in out Foreign_Key_Description);

   package Foreign_Refs is new Shared_Pointers (Foreign_Key_Description, Free);
   type Foreign_Key is new Foreign_Refs.Ref with null record;

   function To_Table (FK : Foreign_Key) return Table_Description'Class;
   --  The table that is referenced by the foreign key

   package Foreign_Keys is new Ada.Containers.Vectors
     (Natural, Foreign_Key);

   package String_Lists is new Ada.Containers.Indefinite_Vectors
     (Natural, String);

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
      FK          : Foreign_Keys.Vector := Foreign_Keys.Empty_Vector;

      Indexes     : String_Lists.Vector;
      --  The list of multi-column indexes (that are declared in their own line
      --  in the table description). This contains strings like:
      --     "field1,field2,field3|index_name"

      Uniques     : String_Lists.Vector;
      --  The list of multi-column unique constraints (that are declared in
      --  their own line in the table description). This contains strings like:
      --     "field1,field2,field3|constraint_name"

      Active      : Boolean := True;

      Has_PK      : Boolean := False;
      --  Whether the table has a primary key

      Super_Table : Table_Description :=
         (Tables_Ref.Null_Ref with null record);
      --  The table from which we inherit fields
   end record;

   type TDR is access all Table_Description_Record'Class;
   for TDR'Size use Standard'Address_Size;
   for TDR'Storage_Pool use Tables_Ref.Element_Access'Storage_Pool;
   overriding procedure Free (Self : in out Table_Description_Record);

   package Tables_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Table_Description, "<", "=");
   package Tables_Lists is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => String);

   ------------
   -- Schema --
   ------------

   type DB_Schema is record
      Tables  : Tables_Maps.Map;

      Ordered_Tables : Tables_Lists.Vector;
      --  In the order in which the user defined them in the description file.
   end record;

   No_Schema : constant DB_Schema :=
     (Tables         => Tables_Maps.Empty_Map,
      Ordered_Tables => Tables_Lists.Empty_Vector);

   No_Table : constant Table_Description :=
     (Tables_Ref.Null_Ref with null record);
end GNATCOLL.SQL.Inspect;
