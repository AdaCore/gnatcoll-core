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

pragma Ada_2012;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Sets;
with GNATCOLL.Refcount;       use GNATCOLL.Refcount;
with GNATCOLL.Strings;        use GNATCOLL.Strings;

package GNATCOLL.SQL_Impl is
   --  Work around issue with the Ada containers: the tampering checks
   --  mean that the container might be corrupted if used from multiple
   --  tasks, even in read-only.
   --      pragma Suppress (Tampering_Check);

   type Cst_String_Access is access constant String;
   --  Various aspects of a database description (table names, field names,...)
   --  are represented as string. To limit the number of memory allocation and
   --  deallocation (and therefore increase speed), this package uses such
   --  strings as Cst_String_Access. These strings are never deallocation, and
   --  should therefore be pointed to "aliased constant String" in your
   --  code, as in:
   --       Name : aliased constant String := "mysubquery";
   --       Q : SQL_Query := SQL_Select
   --          (Fields => ...,
   --           From   => Subquery (SQL_Select (...),
   --                               Name => Name'Access));

   Null_String : aliased constant String := "NULL";

   K_Delta : constant := 0.01;
   K_Decimals : constant := 2;   --  must match K_Delta above
   K_Digits : constant := 14;
   type T_Money is delta K_Delta digits K_Digits;
   --  The base type to represent money in a database. The exact mapping
   --  depends on the DBMS (for postgreSQL, this is "numeric(14,2)").

   ---------------
   -- Formatter --
   ---------------

   type Formatter is abstract tagged null record;
   --  A formatter provides DBMS-specific formatting for SQL statements.
   --  Each backend has its peculiarities, and these are handled through
   --  various instances of Formatter.

   function Boolean_Image (Self : Formatter; Value : Boolean) return String;
   function Money_Image (Self : Formatter; Value : T_Money) return String;
   --  Return an image of the various basic types suitable for the DBMS.
   --  For instance, sqlite does not support boolean fields, which are thus
   --  mapped to integers at the lowest level, even though the Ada layer still
   --  manipulates Booleans.
   --  If you override these, you will likely want to also override
   --  Boolean_Value (DBMS_Forward_Cursor).

   function String_Image
     (Self : Formatter; Value : String; Quote : Boolean) return String;
   --  Escape every apostrophe character "'".
   --  Useful for strings in SQL commands where "'" means the end
   --  of the current string.
   --  This is not suitable for use for prepared queries, which should not be
   --  quoted.
   --  If Quote is False, Value is returned as is (suitable for prepared
   --  queries). Otherwise, Value is surrounded by quote characters, and every
   --  special character in Value are also protected.

   function Boolean_Value (Self : Formatter; Value : String) return Boolean
      is (Boolean'Value (Value));
   function Money_Value (Self : Formatter; Value : String) return T_Money
      is (T_Money'Value (Value));
   --  Convert back to Ada type

   function Field_Type_Autoincrement
     (Self : Formatter) return String is abstract;
   --  Return the SQL type to use for auto-incremented fields.
   --  Such a field is always a primary key, so this information is also
   --  returend as part of the type (this is mandatory for sqlite in
   --  particular).

   function Field_Type_Money
     (Self : Formatter) return String is abstract;
   --  Return the SQL type to use for money fields depending on DBMS

   function Supports_Timezone (Self  : Formatter) return Boolean;
   --  Whether the formatter supports time zones for times. Default is True.

   function Parameter_String
     (Self       : Formatter;
      Index      : Positive;
      Type_Descr : String) return String is ("?");
   --  Return the character to put before a parameter in a SQL statement, when
   --  the value will be substituted at run time.
   --  Typ describes the type of the parameter, and is returned by the
   --  SQL_Parameter primitive operation Describe_Type;

   procedure Append_To_String_And_Cast
     (Self     : Formatter;
      Field    : String;
      Result   : in out XString;
      SQL_Type : String) is abstract;
   --  Outputs Field to the string, while doing a type cast to SQL_Type if the
   --  latter is specified.

   generic
      type Base_Type is digits <>;
   function Any_Float_To_SQL
     (Self : Formatter'Class; Value : Base_Type; Quote : Boolean)
     return String;

   ----------------
   -- Parameters --
   ----------------
   --  Support for parameters when executing SQL queries.
   --  These types should always be created as part of Field_Types package
   --  instantiations.
   --  See GNATCOLL.SQL.Exec

   type SQL_Parameter_Type is abstract tagged null record;

   procedure Free (Self : in out SQL_Parameter_Type) is null;
   --  Free memory used by Self

   function Image
      (Self   : SQL_Parameter_Type;
       Format : Formatter'Class) return String is abstract;
   --  Marshall the parameter to a string, to pass it to the DBMS.
   --  Use the formatter's primitives to encode basic types when possible.

   procedure Free_Dispatch (Self : in out SQL_Parameter_Type'Class);
   package Parameters is new GNATCOLL.Refcount.Shared_Pointers
      (SQL_Parameter_Type'Class, Release => Free_Dispatch);
   type SQL_Parameter_Ptr is new Parameters.Ref with null record;
   --  A smart pointer that contains a parameter

   --------------------
   -- Field mappings --
   --------------------
   --  This type describes how a database description file's types map
   --  to Ada and SQL types.
   --  You need to call GNATCOLL.SQL.Inspect.Register_Field_Mapping.

   type Field_Mapping is abstract tagged null record;
   type Field_Mapping_Access is access all Field_Mapping'Class;

   function SQL_Type_Name
      (Self   : Field_Mapping;
       Format : not null access Formatter'Class) return String is abstract;
   --  Return the name of the SQL type to use in databases.
   --  Some types are mapped differently depending on the DBMS (for instance
   --  for auto-increment fields).
   --  This name will be used in a "CREATE TABLE" statement. It is also used
   --  to encode parameters in queries, as in "$1::integer" (postgresql).

   function Ada_Field_Type_Name
      (Self : Field_Mapping) return String is abstract;
   --  The fully qualified Ada field types , as should be used in generated
   --  code, for instance "GNATCOLL.SQL.SQL_Field_Integer"

   function Maps_Schema_Type
     (Self : in out Field_Mapping; Schema : String) return Boolean
     is abstract;
   --  Schema is the type read in a schema description file.
   --  This function should return True if Self handles this type. It can then
   --  modify some fields in Self (a copy of Self will be made on return and
   --  stored, so having multiple fields of the same type works as expected).
   --  The first registered mapping that return True will be used to create
   --  the Ada code describing the database.
   --  Schema is always lower cased.

   procedure Register_Field_Mapping (Self : Field_Mapping'Class);
   --  Register a new field type, so that users can create their own field
   --  types.

   function Mapping_From_Schema (Schema : String) return Field_Mapping_Access;
   --  Return the mapping matching a type seen in a database description file

   type Null_Record is null record;
   --  Convenience type to instantiate Field_Types below.

   Invalid_Schema : exception;
   --  Raises by Maps_Schema_Type, when the schema description contains an
   --  invalid field type

   -------------------------------------
   -- General declarations for tables --
   -------------------------------------
   --  The following declarations are needed to be able to declare the
   --  following generic packages. They are repeated in GNATCOLL.SQL for ease
   --  of use.

   type Table_Names is record
      Name     : Cst_String_Access;

      Instance : Cst_String_Access;
      Instance_Index : Integer := -1;
      --  The name of the instance is either Instance (if not null), or
      --  computed from the index (see Numbered_Tables above) if not -1, or the
      --  name of the table
   end record;
   No_Names : constant Table_Names := (null, null, -1);
   --  Describes a table (by its name), and the name of its instance. This is
   --  used to find all tables involved in a query, for the auto-completion. We
   --  do not store instances of SQL_Table'Class directly, since that would
   --  involve several things:
   --     - extra Initialize/Adjust/Finalize calls
   --     - Named_Field_Internal would need to embed a pointer to a table, as
   --       opposed to just its names, and therefore must be a controlled type.
   --       This makes the automatic package more complex, and makes the field
   --       type controlled, which is also a lot more costly.
   --  The contents of this type is the same as the discriminants for SQL_Table
   --  and SQL_Field (but unfortunately cannot be used directly as the
   --  discriminant).

   function Instance_Name (Names : Table_Names) return String;
   --  Return the name of the instance for that table.

   function Hash (Self : Table_Names) return Ada.Containers.Hash_Type;
   package Table_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Table_Names, Hash, "=", "=");

   type SQL_Table_Or_List is abstract tagged private;
   --  Either a single table or a group of tables

   procedure Append_Tables
     (Self : SQL_Table_Or_List; To : in out Table_Sets.Set) is null;
   --  Append all the tables referenced in Self to To

   procedure Append_To_String
     (Self       : SQL_Table_Or_List;
      Format     : Formatter'Class;
      Result     : in out XString;
      Show_Types : Boolean) is abstract;
   --  Convert the table to a string

   type SQL_Single_Table (Instance : GNATCOLL.SQL_Impl.Cst_String_Access;
                          Instance_Index : Integer)
      is abstract new SQL_Table_Or_List with private;
   --  Any type of table, or result of join between several tables. Such a
   --  table can have fields

   type SQL_Table_List is new SQL_Table_Or_List with private;
   --  Holds one or more tables. This is meant as an internal type for
   --  gnatcoll, to store tables while it builds the tree representing a
   --  query.

   Empty_Table_List : constant SQL_Table_List;

   function Is_Empty (Self : SQL_Table_List) return Boolean;
   --  Whether there are any tables is Self

   function "&" (Left, Right : SQL_Table_List) return SQL_Table_List;
   function "&" (Left, Right : SQL_Single_Table'Class) return SQL_Table_List;
   function "&" (Left : SQL_Table_List; Right : SQL_Single_Table'Class)
     return SQL_Table_List;
   function "+" (Left : SQL_Single_Table'Class) return SQL_Table_List;
   --  Helpers to build a list of tables

   procedure Append_To_String
      (Self       : SQL_Table_List;
       Format     : Formatter'Class;
       Separator  : String;
       Result     : in out GNATCOLL.Strings.XString;
       Show_Types : Boolean);
   --  Append to the existing Result string the list of all tables,
   --  with Separator between each. For instance:
   --      Table1 ', ' Table2 ', ' Table3
   --  If List is empty, nothing is appended

   -------------------------------------
   -- General declarations for fields --
   -------------------------------------

   type SQL_Assignment is private;

   type SQL_Field_Or_List is abstract tagged null record;
   --  Either a single field or a list of fields

   procedure Append_To_String
     (Self       : SQL_Field_Or_List;
      Format     : Formatter'Class;
      Result     : in out XString;
      Long       : Boolean;
      Show_Types : Boolean) is abstract;
   --  Convert the field to a string.
   --  If Long is true, a fully qualified name is used (table.name), otherwise
   --  just the field name is used.
   --  If Show_Types is true, and the field represents a static value, its
   --  is also included when the DBMS supports it (for instance 1::integer on
   --  postgres).

   type SQL_Field_List is new SQL_Field_Or_List with private;
   Empty_Field_List : constant SQL_Field_List;
   --  A list of fields, as used in a SELECT query ("field1, field2");

   procedure Append_To_String
     (Self       : SQL_Field_List;
      Format     : Formatter'Class;
      Separator  : String;
      Result     : in out XString;
      Long       : Boolean;
      Show_Types : Boolean);
   overriding procedure Append_To_String
     (Self       : SQL_Field_List;
      Format     : Formatter'Class;
      Result     : in out XString;
      Long       : Boolean;
      Show_Types : Boolean);

   type SQL_Field (Table : Cst_String_Access;
                   Instance : Cst_String_Access;
                   Name : Cst_String_Access;
                   Instance_Index : Integer)
      is abstract new SQL_Field_Or_List with null record;
   --  A field that comes directly from the database. It can be within a
   --  specific table instance, but we still need to know the name of the table
   --  itself for the autocompletion.
   --  (Table,Instance) might be null if the field is a constant.
   --  The discriminants are used to get the name of the table when displaying
   --  the field, while permitting static constructs like:
   --      Ta_Names : constant Cst_String_Access := ...;
   --      type T_Names (Instance : Cst_String_Access)
   --          is new SQL_Table (Ta_Names, Instance, -1)
   --      with record
   --         Id : SQL_Field_Integer (Ta_Names, Instance, -1);
   --      end record;
   --  so that one can define multiple representations of the Names table, as
   --  in:
   --     T1 : T_Names (null);       --  Default, name will be "names"
   --     T2 : T_Names (Ta_Names2);  --  An alias
   --  In both cases, the fields T1.Id and T2.Id automatically know how to
   --  display themselves as "names.id" and "names2.id". This does not
   --  require memory allocation and is thus more efficient.

   overriding procedure Append_To_String
     (Self       : SQL_Field;
      Format     : Formatter'Class;
      Result     : in out XString;
      Long       : Boolean;
      Show_Types : Boolean);

   procedure Append_Tables (Self : SQL_Field; To : in out Table_Sets.Set);
   --  Append the table(s) referenced by Self to To.
   --  This is used for auto-completion later on

   procedure Append_If_Not_Aggregate
     (Self         : SQL_Field;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);
   --  Append all fields referenced by Self if Self is not the result of an
   --  aggregate function. This is used for autocompletion of "group by".
   --  Is_Aggregate is set to True if Self is an aggregate, untouched otherwise

   procedure Append (List : in out SQL_Field_List; Field : SQL_Field'Class);

   function "&" (Left, Right : SQL_Field'Class) return SQL_Field_List;
   function "&" (Left, Right : SQL_Field_List) return SQL_Field_List;
   function "&"
     (Left : SQL_Field_List; Right : SQL_Field'Class) return SQL_Field_List;
   function "&"
     (Left : SQL_Field'Class; Right : SQL_Field_List) return SQL_Field_List;
   --  Create lists of fields

   function "+" (Left : SQL_Field'Class) return SQL_Field_List;
   --  Create a list with a single field

   package Field_List is new Ada.Containers.Indefinite_Vectors
     (Natural, SQL_Field'Class);

   function First (List : SQL_Field_List) return Field_List.Cursor;
   --  Return the first field contained in the list

   --------------------
   -- Field pointers --
   --------------------
   --  A smart pointer that frees memory whenever the field is no longer needed

   type SQL_Field_Pointer is private;
   No_Field_Pointer : constant SQL_Field_Pointer;
   --  A smart pointer

   type SQL_Field_Array is array (Natural range <>) of SQL_Field_Pointer;

   function "+" (Field : SQL_Field'Class) return SQL_Field_Pointer;
   --  Create a new pointer. Memory will be deallocated automatically

   procedure Append
     (List : in out SQL_Field_List'Class; Field : SQL_Field_Pointer);
   --  Append a new field to the list

   procedure Append_To_String
     (Self       : SQL_Field_Pointer;
      Format     : Formatter'Class;
      Result     : in out XString;
      Long       : Boolean;
      Show_Types : Boolean);
   procedure Append_Tables
     (Self : SQL_Field_Pointer; To : in out Table_Sets.Set);
   procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_Pointer;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);
   --  See doc for SQL_Field

   ----------------
   -- Field data --
   ----------------
   --  There are two kinds of fields: one is simple fields coming straight from
   --  the database ("table.field"), the other are fields computed through this
   --  API ("field1 || field2", Expression ("field"), "field as name"). The
   --  latter need to allocate memory to store their contents, and are stored
   --  in a refcounted type internally, so that we can properly manage memory.

   type SQL_Field_Internal is abstract tagged null record;
   --  Data that can be stored in a field

   procedure Free (Self : in out SQL_Field_Internal) is null;
   procedure Free_Dispatch (Self : in out SQL_Field_Internal'Class);
   procedure Append_To_String
     (Self       : SQL_Field_Internal;
      Format     : Formatter'Class;
      Result     : in out XString;
      Long       : Boolean;
      Show_Types : Boolean) is abstract;
   procedure Append_Tables
     (Self : SQL_Field_Internal; To : in out Table_Sets.Set) is null;
   procedure Append_If_Not_Aggregate
     (Self         : access SQL_Field_Internal;   --  for dispatching
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean) is null;
   --  The three subprograms are equivalent to the ones for SQL_Field. When a
   --  field contains some data, it will simply delegate the calls to the above
   --  subprograms.
   --  Self_Field is added to the list. Self_Field.Get must be equal to Self

   package Field_Pointers is new Shared_Pointers
      (SQL_Field_Internal'Class, Free_Dispatch);
   subtype SQL_Field_Internal_Access is Field_Pointers.Element_Access;

   generic
      type Base_Field is abstract new SQL_Field with private;
   package Data_Fields is
      type Field is new Base_Field with record
         Data : Field_Pointers.Ref;
      end record;

      overriding procedure Append_To_String
        (Self       : Field;
         Format     : Formatter'Class;
         Result     : in out XString;
         Long       : Boolean;
         Show_Types : Boolean);
      overriding procedure Append_Tables
        (Self : Field; To : in out Table_Sets.Set);
      overriding procedure Append_If_Not_Aggregate
        (Self         : Field;
         To           : in out SQL_Field_List'Class;
         Is_Aggregate : in out Boolean);
   end Data_Fields;
   --  Mixin inheritand for a field, to add specific user data to them. This
   --  user data is refcounted. Field just acts as a proxy for Data, and
   --  delegates all its operations to Data.

   ----------------------------------------
   -- General declarations for criterias --
   ----------------------------------------

   type SQL_Criteria is private;
   No_Criteria : constant SQL_Criteria;

   procedure Append_To_String
     (Self   : SQL_Criteria;
      Format : Formatter'Class;
      Long   : Boolean := True;
      Result : in out XString);
   procedure Append_Tables (Self : SQL_Criteria; To : in out Table_Sets.Set);
   procedure Append_If_Not_Aggregate
     (Self         : SQL_Criteria;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);
   --  The usual semantics for these subprograms (see SQL_Field)

   type SQL_Criteria_Data is abstract tagged null record;
   --  The data contained in a criteria. You can create new versions of it if
   --  you need to create new types of criterias

   procedure Free (Self : in out SQL_Criteria_Data) is null;
   procedure Free_Dispatch (Self : in out SQL_Criteria_Data'Class);
   procedure Append_To_String
     (Self   : SQL_Criteria_Data;
      Format : Formatter'Class;
      Long   : Boolean := True;
      Result : in out XString) is abstract;
   procedure Append_Tables
     (Self : SQL_Criteria_Data; To : in out Table_Sets.Set) is null;
   procedure Append_If_Not_Aggregate
     (Self         : SQL_Criteria_Data;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean) is null;
   --  See description of these subprograms for a SQL_Criteria

   package SQL_Criteria_Pointers
      is new Shared_Pointers (SQL_Criteria_Data'Class, Free_Dispatch);

   subtype SQL_Criteria_Data_Access is SQL_Criteria_Pointers.Element_Access;

   procedure Set_Data
     (Self : in out SQL_Criteria; Data : SQL_Criteria_Data'Class);
   function Get_Data (Self : SQL_Criteria) return SQL_Criteria_Data_Access;
   --  Set the data associated with Self.
   --  This is only needed when you implement your own kinds of criteria, not
   --  when writing SQL queries.

   Op_Equal         : aliased constant String := "=";
   Op_Not_Equal     : aliased constant String := "<>";
   Op_Less          : aliased constant String := "<";
   Op_Less_Equal    : aliased constant String := "<=";
   Op_Greater       : aliased constant String := ">";
   Op_Greater_Equal : aliased constant String := ">=";
   Op_Distinct      : aliased constant String := " IS DISTINCT FROM ";
   Op_Not_Distinct  : aliased constant String := " IS NOT DISTINCT FROM ";
   Op_Is            : aliased constant String := " IS ";
   Op_Is_Not        : aliased constant String := " IS NOT ";
   Op_Like          : aliased constant String := " LIKE ";
   Op_ILike         : aliased constant String := " ILIKE ";
   Op_Not_Like      : aliased constant String := " NOT LIKE ";
   Op_Not_ILike     : aliased constant String := " NOT ILIKE ";
   Op_Overlaps      : aliased constant String := " OVERLAPS ";
   Op_Any           : aliased constant String := " = ANY (";
   Op_Parenthesis   : aliased constant String := ")";
   --  Op_Distinct and Op_Not_Distinct are not supported on sqlite. Instead,
   --  the latter provides "IS" and "IS NOT" that play a similar role (and
   --  are not supported by postgresql).

   function Compare
     (Left, Right : SQL_Field'Class;
      Op          : Cst_String_Access;
      Suffix      : Cst_String_Access := null)
      return SQL_Criteria;
   --  Used to write comparison operations. This is a low-level implementation,
   --  which should only be used when writing your own criterias, not when
   --  writing queries.
   --  The operation is written as
   --     Left Op Right Suffix

   function Compare1
     (Field       : SQL_Field'Class;
      Op          : Cst_String_Access;
      Suffix      : Cst_String_Access := null)
      return SQL_Criteria;
   --  Apply a function to a field, as in:
   --     Op Field Suffix         (Op or Suffix can contain parenthesis)

   function Row_Compare
      (Row1, Row2 : SQL_Single_Table'Class;
       Op : not null Cst_String_Access) return SQL_Criteria;
    --  Row comparison operators (standard SQL but do not work with sqlite)

   ------------------------------------------
   -- General declarations for assignments --
   ------------------------------------------

   No_Assignment : constant SQL_Assignment;

   function "&" (Left, Right : SQL_Assignment) return SQL_Assignment;
   --  Concat two assignments

   procedure Append_Tables (Self : SQL_Assignment; To : in out Table_Sets.Set);
   procedure Append_To_String
     (Self       : SQL_Assignment;
      Format     : Formatter'Class;
      With_Field : Boolean;
      Result     : in out XString);
   --  The usual semantics for these subprograms (see fields)

   procedure To_List (Self : SQL_Assignment; List : out SQL_Field_List);
   --  Return the list of values in Self as a list of fields. This is used for
   --  statements likes "INSERT INTO ... SELECT list"

   procedure Get_Fields (Self : SQL_Assignment; List : out SQL_Field_List);
   --  Return the list of fields impacted by the assignments

   function Create (F1, F2 : SQL_Field'Class) return SQL_Assignment;
   --  A generic way to create assignments

   -------------------
   -- Query results --
   -------------------

   generic
      type Base_Type is digits <>;
   function Any_Float_Value
      (Format : Formatter'Class; S : String) return Base_Type;
   --  Parsing the result of a SQL query as float

   -------------------
   -- Custom fields --
   -------------------
   --  The following package can be used to create your own field types, based
   --  on specific Ada types. It creates various subprograms for ease of use
   --  when writing queries, as well as subprograms to more easily bind SQL
   --  functions manipulating this type.

   pragma Warnings (Off, """Stored_To_Ada"" is not referenced");
   generic
      type Ada_Type (<>) is private;
      --  The type used to represent values of that field in Ada programs.

      type Stored_Ada_Type is private;   --  often the same as Ada_Type
      with function Ada_To_Stored (Value : Ada_Type) return Stored_Ada_Type;
      with function Stored_To_Ada (Value : Stored_Ada_Type) return Ada_Type;

      with procedure Free (Self : in out Stored_Ada_Type) is null;
      --  How values should be stored internally, in particular in parameters.
      --  ??? We can't provide default values for these in current Ada, which
      --  makes the package more work to instantiate. Perhaps we could use
      --  some traits package here, which would be reusable in other contexts
      --  like containers.

      with function To_SQL
        (Format : Formatter'Class;
         Value  : Stored_Ada_Type;
         Quote  : Boolean) return String;
      --  Converts Ada_Type to a value suitable to pass to SQL.
      --
      --  Quote is set to True if the output should protect special SQL
      --  characters. This is used when creating SQL Queries, as in:
      --      Where => Table.Field = "value"
      --  for instance. Quote will be False though when the value is passed
      --  as a separate parameter when executing the query (which is the
      --  recommended approach), as in:
      --      Where => Table.Field = Text_Param (1)

      with function From_SQL
         (Format  : Formatter'Class;
          Value   : String) return Ada_Type;
      --  Converts a string read back from the SQL DBMS to Ada.

      Ada_Field_Type : String;
      --  Fully qualified name for the field types, in Ada. This name will
      --  be used in generated code.

      type Field_Data is private;
      --  Extra data to store in fields, like constraints applied to it for
      --  instance (maximum length,...). As a convenience, you could use the
      --  Null_Record type defined above.

      with function Schema_Type_Check
         (Schema : String; Data : out Field_Data) return Boolean;
      --  Parses a type string (always lower-cased) read in a database
      --  description file.
      --  Returns True if this type applies to this package, and sets Data
      --  as appropriate.
      --  In general, multiple names can be used in the schema to map to the
      --  same type, so we can support database-specific types. It is also
      --  possible for users to override builtin types.

      with function SQL_Type (Data : Field_Data) return String;
      --  The name of the SQL type. When possible, this should include the
      --  constraints, like "character(2)", or "timestamp(2)".
      --  This function will often receive Data with default values, for
      --  instance to generate "$1::type".

   package Field_Types is

      type Field is new SQL_Field with record
         Constraints : Field_Data;
      end record;
      Null_Field : constant Field;

      --------------------------------------------------
      -- Mappings (schema description -> Ada and SQL) --
      --------------------------------------------------

      type Mapping is new Field_Mapping with record
         Constraints : Field_Data;
      end record;
      overriding function SQL_Type_Name
         (Self   : Mapping;
          Format : not null access Formatter'Class) return String;
      overriding function Ada_Field_Type_Name (Self : Mapping) return String
         is (Ada_Field_Type);
      overriding function Maps_Schema_Type
         (Self : in out Mapping; Schema : String) return Boolean
         is (Schema_Type_Check (Schema, Self.Constraints));
      --  This type is automatically registered, so that gnatcoll_db2ada knows
      --  about the type created in instances of this package

      ----------------
      -- Parameters --
      ----------------

      type Parameter is new SQL_Parameter_Type with record
         Val     : Stored_Ada_Type;
      end record;
      overriding procedure Free (Self : in out Parameter);
      overriding function Image
         (Self   : Parameter;
          Format : Formatter'Class) return String
         is (To_SQL (Format, Self.Val, Quote => False));
      --  The parameters used to dynamically substitute values when executing
      --  queries.

      function Param (Index : Positive) return Field'Class;
      --  Return a special string that will be inserted in the query, and
      --  can be substituted with an actual value when the query is executed.
      --  This is used to parameterize queries. In particular, this allows you
      --  to prepare a general form of the query, as in:
      --      SELECT * FROM table WHERE table.field1 = ?1
      --  and execute this several times, substituting a different value
      --  every time.
      --  This is more efficient in general (since the statement is prepared
      --  only once, although the preparation cannot take advantage of special
      --  knowledge related to the value), and safer (no need to worry about
      --  specially quoting the actual value, which GNATCOLL would do for you
      --  but potentially there might still be issues).
      --  The exact string inserted depends on the DBMS.

      function As_Param (Value : Ada_Type) return SQL_Parameter_Ptr;
      --  Pass a specific value to the database

      ------------
      -- Fields --
      ------------

      function From_Table
        (Self  : Field;
         Table : SQL_Single_Table'Class) return Field'Class;
      --  Returns field applied to the table, as in Table.Field.
      --  In general, this is not needed, except when Table is the result of a
      --  call to Rename on a table generated by a call to Left_Join for
      --  instance. In such a case, the list of valid fields for Table is not
      --  known, and we do not have primitive operations to access those, so
      --  this function makes them accessible. However, there is currently no
      --  check that Field is indeed valid for Table.

      function Expression (Value : Ada_Type) return Field'Class;
      function Expression_From_Stored
         (Value : Stored_Ada_Type) return Field'Class;
      --  Create a constant field

      function From_String (SQL : String) return Field'Class;
      --  Similar to the above, but the parameter is assumed to be proper SQL
      --  already (so for instance no quoting or special-character quoting
      --  would occur for strings). This function just indicates to GNATCOLL
      --  how the string should be interpreted

      function Cast (Self : SQL_Field'Class) return Field'Class;
      --  Convert a field, as in:
      --       "CAST (Field AS sql_type)"

      function "&"
        (Field : SQL_Field'Class; Value : Ada_Type) return SQL_Field_List;
      function "&"
        (Value : Ada_Type; Field : SQL_Field'Class) return SQL_Field_List;
      function "&"
        (List : SQL_Field_List; Value : Ada_Type) return SQL_Field_List;
      function "&"
        (Value : Ada_Type; List : SQL_Field_List) return SQL_Field_List;
      --  Create lists of fields

      function "="  (Left : Field; Right : Field'Class) return SQL_Criteria
         is (Compare (Left, Right, Op_Equal'Access));
      function "/=" (Left : Field; Right : Field'Class) return SQL_Criteria
         is (Compare (Left, Right, Op_Not_Equal'Access));
      function "<"  (Left : Field; Right : Field'Class) return SQL_Criteria
         is (Compare (Left, Right, Op_Less'Access));
      function "<=" (Left : Field; Right : Field'Class) return SQL_Criteria
         is (Compare (Left, Right, Op_Less_Equal'Access));
      function ">"  (Left : Field; Right : Field'Class) return SQL_Criteria
         is (Compare (Left, Right, Op_Greater'Access));
      function ">=" (Left : Field; Right : Field'Class) return SQL_Criteria
         is (Compare (Left, Right, Op_Greater_Equal'Access));
      function "="  (Left : Field; Right : Ada_Type) return SQL_Criteria
         is (Compare (Left, Expression (Right), Op_Equal'Access));
      function "/=" (Left : Field; Right : Ada_Type) return SQL_Criteria
         is (Compare (Left, Expression (Right), Op_Not_Equal'Access));
      function "<"  (Left : Field; Right : Ada_Type) return SQL_Criteria
         is (Compare (Left, Expression (Right), Op_Less'Access));
      function "<=" (Left : Field; Right : Ada_Type) return SQL_Criteria
         is (Compare (Left, Expression (Right), Op_Less_Equal'Access));
      function ">"  (Left : Field; Right : Ada_Type) return SQL_Criteria
         is (Compare (Left, Expression (Right), Op_Greater'Access));
      function ">=" (Left : Field; Right : Ada_Type) return SQL_Criteria
         is (Compare (Left, Expression (Right), Op_Greater_Equal'Access));
      --  Compare fields and values

      function Greater_Than
        (Left : SQL_Field'Class; Right : Field) return SQL_Criteria
        is (Compare (Left, Right, Op_Greater'Access));
      function Greater_Or_Equal
        (Left : SQL_Field'Class; Right : Field) return SQL_Criteria
        is (Compare (Left, Right, Op_Greater_Equal'Access));
      function Equal
        (Left : SQL_Field'Class; Right : Field) return SQL_Criteria
        is (Compare (Left, Right, Op_Equal'Access));
      function Less_Than
        (Left : SQL_Field'Class; Right : Field) return SQL_Criteria
        is (Compare (Left, Right, Op_Less'Access));
      function Less_Or_Equal
        (Left : SQL_Field'Class; Right : Field) return SQL_Criteria
        is (Compare (Left, Right, Op_Less_Equal'Access));
      function Greater_Than
        (Left : SQL_Field'Class; Right : Ada_Type) return SQL_Criteria
        is (Compare (Left, Expression (Right), Op_Greater'Access));
      function Greater_Or_Equal
        (Left : SQL_Field'Class; Right : Ada_Type) return SQL_Criteria
        is (Compare (Left, Expression (Right), Op_Greater_Equal'Access));
      function Equal
        (Left : SQL_Field'Class; Right : Ada_Type) return SQL_Criteria
        is (Compare (Left, Expression (Right), Op_Equal'Access));
      function Less_Than
        (Left : SQL_Field'Class; Right : Ada_Type) return SQL_Criteria
        is (Compare (Left, Expression (Right), Op_Less'Access));
      function Less_Or_Equal
        (Left : SQL_Field'Class; Right : Ada_Type) return SQL_Criteria
        is (Compare (Left, Expression (Right), Op_Less_Equal'Access));
      --  Same as "<", "<=", ">", ">=" and "=", but these can be used with the
      --  result of aggregate fields for instance. In general, you should not
      --  use these to work around typing issues (for instance comparing a text
      --  field with 1234)

      function Distinct_From (Left, Right: Field'Class) return SQL_Criteria
         is (Compare (Left, Right, Op_Distinct'Access));
      function Not_Distinct_From (Left, Right: Field'Class) return SQL_Criteria
         is (Compare (Left, Right, Op_Not_Distinct'Access));
      --  Compare two values.
      --  If one of them is null, Equal and Not_Equal would return null,
      --  instead of True or False. But "DISTINCT FROM" would return false if
      --  both are null, and true if only one is null.

      function "=" (Self : Field; Value : Ada_Type) return SQL_Assignment;
      function "=" (Self : Field; To : Field'Class) return SQL_Assignment;
      --  Set Field to the value of To

      --  Assign a new value to the value

      generic
         Name : String;
      function Operator (Field1, Field2 : Field'Class) return Field'Class;
      --  An operator between two fields, that return a field of the new type

      generic
         type Scalar is (<>);
         Name   : String;
         Prefix : String := "";
         Suffix : String := "";
      function Scalar_Operator
        (Self : Field'Class; Operand : Scalar) return Field'Class;
      --  An operator between a field and a constant value, as in
      --      field + interval '2 days'
      --           where  Name   is "+"
      --                  Prefix is "interval '"
      --                  Suffix is " days'"

      generic
         Name : String;
      function SQL_Function return Field'Class;
      --  A parameter-less sql function, as in "CURRENT_TIMESTAMP"

      generic
         type Argument_Type is abstract new SQL_Field with private;
         Name   : String;
         Suffix : String := ")";
      function Apply_Function (Self : Argument_Type'Class) return Field'Class;
      --  Applying a function to a field, as in  "LOWER (field)", where
      --     Name   is "LOWER ("
      --     Suffix is ")"

      generic
         type Argument1_Type is abstract new SQL_Field with private;
         type Argument2_Type is abstract new SQL_Field with private;
         Name   : String;
         Suffix : String := ")";
         Sep    : String := ", ";
      function Apply_Function2
         (Arg1 : Argument1_Type'Class;
          Arg2 : Argument2_Type'Class)
         return Field'Class;
      --  Applying a function to two fields, and return another field, as in:
      --      "FUNC (field1, field2)" where
      --      Name   is "FUNC ("
      --      Sep    is ", "
      --      Suffix is ")"

      -------------------
      -- Query results --
      -------------------

      function Parse_From_SQL
         (Format  : Formatter'Class;
          Value   : String) return Ada_Type
         renames From_SQL;
      --  Make the formal parameter visible to users of this package

   private
      pragma Warnings (Off, "*is read but never assigned*");
      Default_Constraints : Field_Data;
      pragma Warnings (On, "*is read but never assigned*");
      --  Uninitialized, using default values

      pragma Warnings (Off, "*may be referenced before it has a value");
      Null_Field : constant Field :=
        (Table    => null,
         Instance => null,
         Instance_Index => -1,
         Constraints => Default_Constraints,
         Name     => Null_String'Access);
      pragma Warnings (On, "*may be referenced before it has a value");
   end Field_Types;

private

   -----------------
   -- Field lists --
   -----------------

   package Field_List_Pointers is new GNATCOLL.Refcount.Shared_Pointers
      (Field_List.Vector);
   --  See comment for Table_List_Pointers

   subtype Field_List_Ref is Field_List_Pointers.Ref;

   type SQL_Field_List is new SQL_Field_Or_List with record
      List : Field_List_Ref;
   end record;

   ------------
   -- Tables --
   ------------

   type SQL_Table_Or_List is abstract tagged null record;

   type SQL_Single_Table (Instance : Cst_String_Access;
                          Instance_Index : Integer)
      is abstract new SQL_Table_Or_List with null record;
   --  instance name, might be null when this is the same name as the table.
   --  This isn't used for lists, but is used for all other types of tables
   --  (simple, left join, subqueries) so is put here for better sharing.

   ------------------
   -- Tables lists --
   ------------------
   --  The various "&" operator try to optimize the number of copies of the
   --  standard container we do, by reserving an initial capacity, and reusing
   --  existing lists when they are not shared. To do this, they test the
   --  actual refcount value.
   --  This should be thread safe in all reasonable uses in practice:
   --  * when a query is built locally in a subprogram, there is obviously no
   --    issue.
   --  * when a query is built at elaboration time and shared across tasks,
   --    this is also safe, since we are no longer comparing refcount (but
   --    relying on GNATCOLL.Refcount to do proper memory management).
   --  * when parts of the query (like a list of fields for instance) are
   --    declared as a global constant, and then used locally to build a
   --    query:
   --        Global : constant SQL_Field_List := ....;
   --        procedure Bla is
   --           Q : SQL_Query := SQL_Select (Global & Table.Field, ...);
   --        end Bla;
   --    then Global has a refcount of 2 in the call and will not be shared.

   package Table_List is new Ada.Containers.Indefinite_Vectors
     (Natural, SQL_Single_Table'Class);

   package Table_List_Pointers is
     new GNATCOLL.Refcount.Shared_Pointers (Table_List.Vector);
   --  Store the actual data for a SQL_Table_List in a different block (using
   --  a smart pointer for reference counting), since otherwise all the calls
   --  to "&" result in a copy of the list (per design of the Ada05 containers)
   --  which shows up as up to 20% of the number of calls to malloc on the
   --  testsuite).

   subtype Table_List_Data is Table_List_Pointers.Ref;

   type SQL_Table_List is new SQL_Table_Or_List with record
      Data : Table_List_Data;
   end record;
   overriding procedure Append_To_String
     (Self       : SQL_Table_List;
      Format     : Formatter'Class;
      Result     : in out XString;
      Show_Types : Boolean);
   overriding procedure Append_Tables
     (Self : SQL_Table_List; To : in out Table_Sets.Set);
   --  Append all the tables referenced in Self to To

   Empty_Table_List : constant SQL_Table_List :=
      (SQL_Table_Or_List with Data => Table_List_Pointers.Null_Ref);

   ---------------
   -- Criterias --
   ---------------

   type SQL_Criteria is record
      Criteria : SQL_Criteria_Pointers.Ref;
   end record;
   --  SQL_Criteria must not be tagged, otherwise we have subprograms that are
   --  primitive for two types. This would also be impossible for users to
   --  declare a variable of type SQL_Criteria.

   No_Criteria : constant SQL_Criteria :=
     (Criteria => SQL_Criteria_Pointers.Null_Ref);

   --------------------
   -- Field pointers --
   --------------------

   package SQL_Field_Pointers is new Shared_Pointers (SQL_Field'Class);
   type SQL_Field_Pointer is new SQL_Field_Pointers.Ref with null record;
   No_Field_Pointer : constant SQL_Field_Pointer :=
      (SQL_Field_Pointers.Null_Ref with null record);

   -----------------
   -- Assignments --
   -----------------

   type Assignment_Item is record
      Field    : SQL_Field_Pointer;
      --  The modified field

      To_Field : SQL_Field_Pointer;
      --  Its new value (No_Field_Pointer sets to NULL)
   end record;

   package Assignment_Lists is new Ada.Containers.Vectors
      (Natural, Assignment_Item);

   type SQL_Assignment is record
      List : Assignment_Lists.Vector;
   end record;

   No_Assignment : constant SQL_Assignment :=
     (List => Assignment_Lists.Empty_Vector);

   Empty_Field_List : constant SQL_Field_List :=
     (SQL_Field_Or_List with List => Field_List_Pointers.Null_Ref);

end GNATCOLL.SQL_Impl;
