-----------------------------------------------------------------------
--                           G N A T C O L L                         --
--                                                                   --
--                 Copyright (C) 2005-2008, AdaCore                  --
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

--  This package provides an object-oriented, high-level interface to SQL
--  queries.
--  Instead of using static strings to write queries, this package allows you
--  to write queries using Ada function calls. It helps to ensure the syntactic
--  validity of the resulting queries, and, some packages can automatically be
--  generated from your database (see below), ensures type-safety and that the
--  query only references existing fields of the database. An example of such a
--  query is:
--
--     Q : SQL_Query :=
--           SQL_Select
--             (Fields => Table1.Field1 & Table2.Field2,
--              From   => Table1 & Table2,
--              Where  => Table1.Field3 = Table2.Field4);
--
--  This checks, among other things, that Field3 and Field4 are of the same
--  type.
--  This package itself does not provide a way to execute a query on a given
--  database. See GNATCOLL.SQL.Exec for such facilities.
--  As a result, this package is independent of any DBMS, and in fact does not
--  even require one to be installed on your system.
--
--  Automatic generation of database description
--  =============================================
--
--  This package depends on having types and subprograms that describe the
--  structure of the database. Writting such packages manually is tedious and
--  error prone. Instead, you should use the gnatcoll_db2ada tool to
--  automatically generate this description before each compilation. This
--  ensures that any SQL query in your application only references fields that
--  do exist in the database, and therefore helps detect at compilation time a
--  lot of possible errors that would otherwise only be detected at run time.
--
--  These generated packages should contain the following, for each table in
--  your database:
--
--     Ta_Table_Name : aliased constant String := "table_name";
--     package T_Table is
--        N_Field1 : aliased constant String := "field1";
--        N_Field2 : aliased constant String := "field2";
--        type Table (Instance : Cst_String_Access)
--           is new SQL_Table (Ta_Table_Name'Access, Instance) with
--        record
--           Field1 : SQL_Field_Integer
--              (Ta_Table_Name'Access, Instance, N_Field1'Access);
--           Field2 : SQL_Field_Integer
--              (Ta_Table_Name'Access, Instance, N_Field2'Access);
--        end record;
--
--         function FK (Self : Table; Foreign : SQL_Table'Class)
--            return SQL_Criteria;
--
--     end T_Table;
--
--  Finally, a default instance of the table that can be used in the queries:
--      Table : T_Table.Table (null);
--
--  FK is a subprogram to retrieve the foreign keys between two tables, to
--  simplify the writting of the sql queries. This is optional, and if you are
--  maintaining this package by hand you might not want to generate these.
--
--  The reason to use a package like the above is to avoid naming conflicts
--  between the functions generated for the fields, and the name of the
--  instances (in the example above, if another table was called Field1, and we
--  weren't using a package, we would have a naming conflict).
--
--  This way, a user might write a query with two instances of the table with
--  the following code (which uses the Ada2005 dotted notation, although this
--  isn't mandatory):
--      AI : T_Sales_Entity.Table := T_Sales_Entity.Table
--        (Rename (Sales_Entity, "foo"));
--      SQL_Select
--        (Fields => AI.Field1 & Action_Item.Field1,
--         From   => AI & Action_Item,
--         Where  => AI.FK (Action_Item))

with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Finalization;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with GNAT.Strings;           use GNAT.Strings;

package GNATCOLL.SQL is

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

   type SQL_Criteria is private;
   type SQL_Query is private;

   ------------
   -- Tables --
   ------------

   type SQL_Table_Or_List is abstract tagged private;
   --  Either a single table or a group of tables

   function To_String (Self : SQL_Table_Or_List) return String is abstract;
   --  Convert the table to a string

   type SQL_Single_Table (Instance : Cst_String_Access)
      is abstract new SQL_Table_Or_List with private;
   --  Any type of table, or result of join between several tables. Such a
   --  table can have fields

   type SQL_Table_List is new SQL_Table_Or_List with private;
   Empty_Table_List : constant SQL_Table_List;
   --  A list of tables, as used in a SELECT query ("a, b")

   type SQL_Table (Table_Name, Instance : Cst_String_Access)
      is abstract new SQL_Single_Table with private;
   overriding function To_String (Self : SQL_Table) return String;
   --  A table representing a field of a specific table

   type SQL_Unchecked_Table_Access is access constant SQL_Table'Class;

   type SQL_Table_Access is access all SQL_Table'Class;
   procedure Free (A : in out SQL_Table_Access);
   --  Needs to be freed explicitely

   function FK
     (Self : SQL_Table; Foreign : SQL_Table'Class) return SQL_Criteria;
   --  Criteria to use when joining the two instances

   function "&" (Left, Right : SQL_Table_List) return SQL_Table_List;
   function "&" (Left, Right : SQL_Single_Table'Class) return SQL_Table_List;
   function "&" (Left : SQL_Table_List; Right : SQL_Single_Table'Class)
                 return SQL_Table_List;
   function "+" (Left : SQL_Single_Table'Class) return SQL_Table_List;
   --  Create a list of tables, suitable for use in a SELECT query.
   --  Note the operator "+" to create a list with a single element
   --  For efficiency reasons, these operators try to reuse one of the lists
   --  passed in parameter, append to it, and return it. That limits the number
   --  of copies to be done, and thus the number of system calls to malloc.

   ------------
   -- Fields --
   ------------

   type SQL_Field_Or_List is abstract tagged private;

   type SQL_Field_List is new SQL_Field_Or_List with private;
   Empty_Field_List : constant SQL_Field_List;
   --  A list of fields, as used in a SELECT query ("field1, field2");

   function To_String
     (Self : SQL_Field_List; Long : Boolean := True)  return String;

   type SQL_Field (Table, Instance, Name : Cst_String_Access)
      is abstract new SQL_Field_Or_List with private;
   --  A single field

   function To_String
     (Self : SQL_Field; Long : Boolean := True)  return String;

   function As
     (Field : SQL_Field'Class; Name : String) return SQL_Field'Class;
   --  Rename a field in the output. This is equivalent to "field AS name".
   --  The result is such that it can only be put in a list of fields, nothing
   --  else.

   function Desc (Field : SQL_Field'Class) return SQL_Field'Class;
   function Asc  (Field : SQL_Field'Class) return SQL_Field'Class;
   --  Specify a specific sort order. This is only used in the Order_By clause
   --  of a Select statement

   type SQL_Field_Integer is new SQL_Field with private;
   type SQL_Field_Text    is new SQL_Field with private;
   type SQL_Field_Time    is new SQL_Field with private;
   type SQL_Field_Boolean is new SQL_Field with private;
   type SQL_Field_Float   is new SQL_Field with private;

   function Field
     (Table : SQL_Single_Table'Class;
      Field : SQL_Field_Integer) return SQL_Field_Integer'Class;
   function Field
     (Table : SQL_Single_Table'Class;
      Field : SQL_Field_Text) return SQL_Field_Text'Class;
   function Field
     (Table : SQL_Single_Table'Class;
      Field : SQL_Field_Time) return SQL_Field_Time'Class;
   function Field
     (Table : SQL_Single_Table'Class;
      Field : SQL_Field_Boolean) return SQL_Field_Boolean'Class;
   function Field
     (Table : SQL_Single_Table'Class;
      Field : SQL_Field_Float) return SQL_Field_Float'Class;
   --  Returns field applied to the table, as in Table.Field.
   --  In general, this is not needed, except when Table is the result of a
   --  call to Rename on a table generated by a call to Left_Join for instance.
   --  In such a case, the list of valid fields for Table is not known, and we
   --  do not have primitive operations to access those, so this function makes
   --  them accessible. However, there is currently no check that Field is
   --  indeed valid for Table.
   --  The type of returned value is the same type of field as Field, ie a
   --  SQL_Field_Integer if Field is a SQL_Field_Integer for instance

   Null_Field_Text    : constant SQL_Field_Text;
   Null_Field_Integer : constant SQL_Field_Integer;
   Null_Field_Boolean : constant SQL_Field_Boolean;

   function From_String (Expression : String) return SQL_Field_Text'Class;
   function From_Integer (Expression : String) return SQL_Field_Integer'Class;
   --  Create a field from sql core. Expression is an SQL statement, no check
   --  is done though.

   function Expression (Value : String)  return SQL_Field_Text'Class;
   function Expression (Value : Integer) return SQL_Field_Integer'Class;
   function Expression (Value : Boolean) return SQL_Field_Boolean'Class;
   function Expression (Value : Float)   return SQL_Field_Float'Class;
   function Expression
     (Value : Ada.Calendar.Time; Date_Only : Boolean := False)
      return SQL_Field_Time'Class;
   --  Create constant fields (for a select statement for instance). The
   --  expression is surrounded by quotes, and special characters are
   --  escaped as needed

   function As_Days (Count : Natural) return SQL_Field_Time'Class;
   --  An expression representing a number of days

   function At_Time_Zone
     (Field : SQL_Field_Time'Class; TZ : String) return SQL_Field_Time'Class;
   --  Convert a 'timestamp with time zone' expression to another time zone

   function Expression_Or_Null (Value : String) return SQL_Field_Text'Class;
   --  Same as above but if the Value is "NULL", returns NULL instead of 'NULL'

   function "&" (Left, Right : SQL_Field_List)        return SQL_Field_List;
   function "&" (Left, Right : SQL_Field'Class)       return SQL_Field_List;
   function "&"
     (Left : SQL_Field_List; Right : SQL_Field'Class) return SQL_Field_List;
   function "&"
     (Left : SQL_Field'Class; Right : SQL_Field_List) return SQL_Field_List;
   function "+" (Left : SQL_Field'Class)              return SQL_Field_List;

   function "&" (List : SQL_Field_List; Value : String)  return SQL_Field_List;
   function "&" (List : SQL_Field'Class; Value : String) return SQL_Field_List;
   function "&" (Value : String; List : SQL_Field_List)  return SQL_Field_List;
   function "&" (Value : String; List : SQL_Field'Class) return SQL_Field_List;

   function "&" (List : SQL_Field_List; Value : Integer) return SQL_Field_List;
   function "&" (Value : Integer; List : SQL_Field_List) return SQL_Field_List;
   function "&"
     (List : SQL_Field'Class; Value : Integer) return SQL_Field_List;
   function "&"
     (Value : Integer; List : SQL_Field'Class) return SQL_Field_List;

   function "&" (List : SQL_Field_List; Value : Boolean) return SQL_Field_List;
   function "&" (Value : Boolean; List : SQL_Field_List) return SQL_Field_List;
   function "&"
     (List : SQL_Field'Class; Value : Boolean) return SQL_Field_List;
   function "&"
     (Value : Boolean; List : SQL_Field'Class) return SQL_Field_List;
   --  Create a list of fields, suitable for use in a SELECT query

   function Concat (Fields : SQL_Field_List) return SQL_Field'Class;
   --  Converts the list into a concatenation of fields, as in:
   --     "prefix " || foo.bar || "suffix"

   function Tuple (Fields : SQL_Field_List) return SQL_Field'Class;
   --  Return the list of fields as a tuple, ie (field1, field2)

   function Coalesce (Fields : SQL_Field_List) return SQL_Field'Class;
   --  Returns the first of its arguments that is not null
   --     Coalesce (value1, value2, ...)

   function Extract
     (Field : SQL_Field'Class; Attribute : String) return SQL_Field'Class;
   --  Return the result of "extract (attribute from field)"

   function To_Char
     (Field : SQL_Field_Time; Format : String) return SQL_Field'Class;
   --  Format a date field, as in "to_char (field, "format")"

   function Lower
     (Field : SQL_Field_Text'Class) return SQL_Field_Text'Class;
   --  Returns "lower (field)"

   function Current_Date return SQL_Field_Time'Class;
   function Now return SQL_Field_Time'Class;
   --  Return the current date

   function "-"
     (Field1, Field2 : SQL_Field_Time'Class) return SQL_Field_Time'Class;
   function "-"
     (Field1 : SQL_Field_Time'Class; Days : Integer)
      return SQL_Field_Time'Class;
   --  Return the different between two dates

   type Aggregate_Function is new String;
   Func_Count    : constant Aggregate_Function := "count";
   Func_Distinct : constant Aggregate_Function := "distinct";
   Func_Min      : constant Aggregate_Function := "min";
   Func_Max      : constant Aggregate_Function := "max";
   Func_Concat   : constant Aggregate_Function := "concat";
   Func_Bool_And : constant Aggregate_Function := "bool_and";
   Func_Bool_Or  : constant Aggregate_Function := "bool_or";
   --  Func_Distinct is not useful in general, since the various calls to
   --  SQL_Select below have their own Distinct parameter. However, it is
   --  useful in constructs such as "count (distinct a.b)", which can be
   --  constructed as Apply (Func_Count, Apply (Func_Distinct, "a.b"))
   --
   --  If you need to compare the count, for instance, you should use a
   --  syntax similar to:
   --     Greater_Or_Equal (Apply (Func_Count, field_name), 2)

   function Apply
     (Func   : Aggregate_Function;
      Fields : SQL_Field_List) return SQL_Field'Class;
   function Apply
     (Func     : Aggregate_Function;
      Criteria : SQL_Criteria) return SQL_Field'Class;
   function Apply
     (Func  : Aggregate_Function;
      Field : SQL_Field'Class) return SQL_Field'Class;
   --  Apply an aggregate function to a field. Other fields in the result of
   --  the query should be grouped. Each element of Fields is taken as one of
   --  the arguments to Func.
   --  The result of this function is an untyped field. If you need to compare
   --  this result with some other field or value, you should use the
   --  functions Greater_Than, Less_Than, ... below, rather than the usual
   --  operators.

   ---------------------
   -- Case statements --
   ---------------------
   --  SQL can have case statements in the field part of a select statement.
   --  For instance,  SELECT CASE WHEN a = b THEN a ELSE '' END  FROM ...

   type When_List is private;

   function SQL_Case
     (List : When_List; Else_Clause : SQL_Field'Class := Null_Field_Text)
      return SQL_Field'Class;
   --  Return a case statement made of one or several WHEN clause.
   --  If none of the WHEN clause matches, Else_Clause will be executed
   --  instead

   function SQL_When
     (Criteria : SQL_Criteria; Field : SQL_Field'Class) return When_List;
   --  Display Field if Criteria is true

   function "&" (List1, List2 : When_List) return When_List;
   --  Concatenate two WHEN statements

   ---------------------
   -- Array of fields --
   ---------------------
   --  This array plays a similar role to a field_list. The idea is that you
   --  can explicitly specify the index of each file, and thus ensure more
   --  consistency in your application. For instance, you would do the
   --  following:
   --     Field_First_Name : constant := 0;
   --     Field_Last_Name  : constant := 1;
   --     SQL_Select (Fields => To_List
   --                           ((Field_First_Name => +Create ("first"),
   --                             Field_Last_Name  => +Create ("last"))),
   --                 From   => ...);
   --  and you can then retrieve the specific fields in each row of the result
   --  by using the constant indexes, which ensures more consistency.
   --  The first element in the array should always be 0 for that purpose.

   type SQL_Field_Pointer is private;
   --  A smart pointer

   function "+" (Field : SQL_Field'Class) return SQL_Field_Pointer;
   --  Create a new pointer. Memory will be deallocated automatically

   type SQL_Field_Array is array (Natural range <>) of SQL_Field_Pointer;

   function To_List (Fields : SQL_Field_Array) return SQL_Field_List;
   --  Convert the array into a list

   --------------
   -- Criteria --
   --------------

   No_Criteria : constant SQL_Criteria;

   function "=" (Left, Right : SQL_Field_Integer'Class) return SQL_Criteria;
   function "=" (Left, Right : SQL_Field_Text'Class)    return SQL_Criteria;
   function "=" (Left, Right : SQL_Field_Boolean'Class) return SQL_Criteria;
   function "=" (Left, Right : SQL_Field_Float'Class)   return SQL_Criteria;
   function "=" (Left, Right : SQL_Field_Time'Class)    return SQL_Criteria;

   function "="
     (Left : SQL_Field_Integer; Right : Integer) return SQL_Criteria;
   function "="
     (Left : SQL_Field_Text; Right : String)  return SQL_Criteria;
   function "="
     (Left : SQL_Field_Boolean; Right : Boolean) return SQL_Criteria;
   function "="
     (Left : SQL_Field_Float; Right : Float)   return SQL_Criteria;

   function "="
     (Left : SQL_Field_Time; Right : Ada.Calendar.Time) return SQL_Criteria;
   function Date_Equal
     (Left : SQL_Field_Time; Right : Ada.Calendar.Time) return SQL_Criteria;
   --  The first one also compares times, whereas the second only compares
   --  dates

   function "/=" (Left, Right : SQL_Field_Integer'Class) return SQL_Criteria;
   function "/=" (Left, Right : SQL_Field_Text'Class)    return SQL_Criteria;
   function "/=" (Left, Right : SQL_Field_Boolean'Class) return SQL_Criteria;
   function "/=" (Left, Right : SQL_Field_Float'Class)   return SQL_Criteria;
   function "/=" (Left, Right : SQL_Field_Time'Class)    return SQL_Criteria;

   function "/="
     (Left : SQL_Field_Integer; Right : Integer) return SQL_Criteria;
   function "/="
     (Left : SQL_Field_Text; Right : String)  return SQL_Criteria;
   function "/="
     (Left : SQL_Field_Boolean; Right : Boolean) return SQL_Criteria;
   function "/="
     (Left : SQL_Field_Float; Right : Float)   return SQL_Criteria;
   function "/="
     (Left : SQL_Field_Time; Right : Ada.Calendar.Time) return SQL_Criteria;

   function "<" (Left, Right : SQL_Field_Integer'Class) return SQL_Criteria;
   function "<" (Left, Right : SQL_Field_Text'Class)    return SQL_Criteria;
   function "<" (Left, Right : SQL_Field_Boolean'Class) return SQL_Criteria;
   function "<" (Left, Right : SQL_Field_Float'Class)   return SQL_Criteria;
   function "<" (Left, Right : SQL_Field_Time'Class)    return SQL_Criteria;

   function "<"
     (Left : SQL_Field_Integer; Right : Integer) return SQL_Criteria;
   function "<"
     (Left : SQL_Field_Float;   Right : Float)   return SQL_Criteria;

   function "<"
     (Left : SQL_Field_Time;    Right : Ada.Calendar.Time) return SQL_Criteria;
   function Date_Less_Than
     (Left : SQL_Field_Time;    Right : Ada.Calendar.Time) return SQL_Criteria;
   --  The first one also compares times, whereas the second only compares
   --  dates

   function ">" (Left, Right : SQL_Field_Integer'Class) return SQL_Criteria;
   function ">" (Left, Right : SQL_Field_Text'Class)    return SQL_Criteria;
   function ">" (Left, Right : SQL_Field_Boolean'Class) return SQL_Criteria;
   function ">" (Left, Right : SQL_Field_Float'Class)   return SQL_Criteria;
   function ">" (Left, Right : SQL_Field_Time'Class)    return SQL_Criteria;

   function ">"
     (Left : SQL_Field_Integer; Right : Integer) return SQL_Criteria;
   function ">"
     (Left : SQL_Field_Float; Right : Float)   return SQL_Criteria;
   function ">"
     (Left : SQL_Field_Text; Right : String)   return SQL_Criteria;

   function Greater_Than
     (Left : SQL_Field'Class; Right : Integer) return SQL_Criteria;
   function Greater_Or_Equal
     (Left : SQL_Field'Class; Right : Integer) return SQL_Criteria;
   function Equal
     (Left : SQL_Field'Class; Right : Boolean) return SQL_Criteria;
   --  Same as ">" and ">=", but usable for instance for aggregate fields
   --  resulting from the use of Apply

   function ">"
     (Left : SQL_Field_Time; Right : Ada.Calendar.Time) return SQL_Criteria;
   function Date_Greater_Than
     (Left : SQL_Field_Time; Right : Ada.Calendar.Time) return SQL_Criteria;
   --  The first one also compares times, whereas the second only compares
   --  dates

   function Greater_Than
     (Left  : SQL_Field'Class;
      Right : SQL_Field_Time'Class) return SQL_Criteria;
   function Less_Than
     (Left  : SQL_Field'Class;
      Right : SQL_Field_Time'Class) return SQL_Criteria;
   --  Same as ">" but usable for instance for aggregate fields
   --  resulting from the use of Apply

   function "<=" (Left, Right : SQL_Field_Integer'Class) return SQL_Criteria;
   function "<=" (Left, Right : SQL_Field_Text'Class)    return SQL_Criteria;
   function "<=" (Left, Right : SQL_Field_Boolean'Class) return SQL_Criteria;
   function "<=" (Left, Right : SQL_Field_Float'Class)   return SQL_Criteria;
   function "<=" (Left, Right : SQL_Field_Time'Class)    return SQL_Criteria;

   function "<="
     (Left : SQL_Field_Integer; Right : Integer) return SQL_Criteria;
   function "<="
     (Left : SQL_Field_Float;   Right : Float)   return SQL_Criteria;

   function "<="
     (Left : SQL_Field_Time;    Right : Ada.Calendar.Time) return SQL_Criteria;
   function Date_Less_Or_Equal
     (Left : SQL_Field_Time;    Right : Ada.Calendar.Time) return SQL_Criteria;
   --  The first one also compares times, whereas the second only compares
   --  dates

   function ">=" (Left, Right : SQL_Field_Integer'Class) return SQL_Criteria;
   function ">=" (Left, Right : SQL_Field_Text'Class)    return SQL_Criteria;
   function ">=" (Left, Right : SQL_Field_Boolean'Class) return SQL_Criteria;
   function ">=" (Left, Right : SQL_Field_Float'Class)   return SQL_Criteria;
   function ">=" (Left, Right : SQL_Field_Time'Class)    return SQL_Criteria;

   function ">="
     (Left : SQL_Field_Integer; Right : Integer) return SQL_Criteria;
   function ">="
     (Left : SQL_Field_Float;   Right : Float)   return SQL_Criteria;

   function ">="
     (Left : SQL_Field_Time;    Right : Ada.Calendar.Time) return SQL_Criteria;
   function Date_Greater_Or_Equal
     (Left : SQL_Field_Time;    Right : Ada.Calendar.Time) return SQL_Criteria;
   --  The first one also compares times, whereas the second only compares
   --  dates.

   function "and" (Left, Right : SQL_Criteria) return SQL_Criteria;
   function "or"  (Left, Right : SQL_Criteria) return SQL_Criteria;
   --  Combine two criterias

   function "and"
     (Left : SQL_Criteria; Right : SQL_Field_Boolean) return SQL_Criteria;
   function "or"
     (Left : SQL_Criteria; Right : SQL_Field_Boolean) return SQL_Criteria;
   --  Combine two criterias, one of which is for a boolean test. This is just
   --  to simplify the writting

   function "not" (Left : SQL_Field_Boolean) return SQL_Criteria;
   --  Test that Left is False. This can also be done with an explicit call to
   --  "=" above

   function SQL_In
     (Self : SQL_Field'Class; List : SQL_Field_List) return SQL_Criteria;
   function SQL_In
     (Self : SQL_Field'Class; List : String) return SQL_Criteria;
   function SQL_In
     (Self : SQL_Field'Class; Subquery : SQL_Query) return SQL_Criteria;
   function SQL_Not_In
     (Self : SQL_Field'Class; List : SQL_Field_List) return SQL_Criteria;
   function SQL_Not_In
     (Self : SQL_Field'Class; Subquery : SQL_Query) return SQL_Criteria;
   --  Whether Self is equal to any of the values in List

   function Ilike
     (Self : SQL_Field_Text; Str : String) return SQL_Criteria;
   function Ilike
     (Self : SQL_Field_Text; Field : SQL_Field'Class) return SQL_Criteria;
   function Like
     (Self : SQL_Field_Text; Str : String) return SQL_Criteria;
   function Not_Ilike
     (Self : SQL_Field_Text; Str : String) return SQL_Criteria;
   function Not_Like
     (Self : SQL_Field_Text; Str : String) return SQL_Criteria;
   --  Return a resp. case-insensitive or case-sensitive pattern matching.
   --  Right is automatically quoted. However, you are responsible for
   --  putting the meta-character % at the right places in Right.

   function Is_Null     (Self : SQL_Field'Class) return SQL_Criteria;
   function Is_Not_Null (Self : SQL_Field'Class) return SQL_Criteria;
   --  Test whether a field is null or not (ie unset or set)

   function Overlaps (Left, Right : SQL_Field'Class) return SQL_Criteria;
   --  Whether the range specified in Left overlaps the range specified in
   --  Right.

   -----------------
   -- Assignments --
   -----------------

   type SQL_Assignment is private;

   No_Assignment : constant SQL_Assignment;

   function "="
     (Field : SQL_Field_Text'Class; Value : String) return SQL_Assignment;
   function "="
     (Field : SQL_Field_Integer'Class; Value : Integer) return SQL_Assignment;
   function "="
     (Field : SQL_Field_Boolean'Class; Value : Boolean) return SQL_Assignment;
   function "="
     (Field : SQL_Field_Time'Class;
      Value : Ada.Calendar.Time) return SQL_Assignment;
   function "="
     (Field : SQL_Field_Float'Class; Value : Float) return SQL_Assignment;
   --  Set field to a specific value

   function "="
     (Field : SQL_Field'Class; To : SQL_Field'Class) return SQL_Assignment;
   --  Set Field to the value of To

   function To_Null (Field : SQL_Field'Class) return SQL_Assignment;
   --  Set field to null

   function "&" (Left, Right : SQL_Assignment) return SQL_Assignment;
   --  Create a list of assignments

   -------------
   -- Queries --
   -------------

   type SQL_Left_Join_Table is new SQL_Single_Table with private;
   --  A special kind of table that represents a join between two tables

   function Rename
     (Self : SQL_Left_Join_Table; Name : Cst_String_Access)
      return SQL_Left_Join_Table'Class;
   --  Returns a new instance of Self, with a different name.
   --  No deallocation is ever done for Name, see Cst_String_Access

   function Left_Join
     (Full    : SQL_Single_Table'Class;
      Partial : SQL_Single_Table'Class;
      On      : SQL_Criteria := No_Criteria) return SQL_Left_Join_Table;
   --  Performs a left join between the two tables. It behaves like a standard
   --  join, but if a row from Full doesn't match any row in Partial, a virtual
   --  row full of NULL is added to Partial, and returned in the join.
   --  If On is not specified and the two tables are simple tables (ie not the
   --  result of another join), it is automatically completed based on the
   --  foreign keys joining the two tables. This completion relies on the FK
   --  primitive operations for the tables, so if you have not implemented them
   --  the auto-completion will not work.

   function Join
     (Table1 : SQL_Single_Table'Class;
      Table2 : SQL_Single_Table'Class;
      On     : SQL_Criteria := No_Criteria) return SQL_Left_Join_Table;
   --  Join the two tables

   function SQL_Select
     (Fields   : SQL_Field_Or_List'Class;
      From     : SQL_Table_Or_List'Class := Empty_Table_List;
      Where    : SQL_Criteria := No_Criteria;
      Group_By : SQL_Field_Or_List'Class := Empty_Field_List;
      Having   : SQL_Criteria := No_Criteria;
      Order_By : SQL_Field_Or_List'Class := Empty_Field_List;
      Limit    : Integer := -1;
      Offset   : Integer := -1;
      Distinct : Boolean := False) return SQL_Query;
   --  Select one or more fields from one or more tables

   function SQL_Insert
     (Values : SQL_Assignment;
      Where  : SQL_Criteria := No_Criteria) return SQL_Query;
   --  Insert a new row in the table specified by the left-hand side of
   --  the assignments. All these left-hand side fields must belong to the same
   --  table, or the query is ambiguous and will raise a Program_Error.
   --  The right-hand side of the assignments, though, can either be constants
   --  or fields from other tables. When other tables are referenced, the
   --  insert statement is transformed into an INSERT with a subquery (see
   --  below), and WHERE is used as the WHERE claused for that subquery.

   function SQL_Insert
     (Fields : SQL_Field_Or_List'Class;
      Values : SQL_Query) return SQL_Query;
   --  Insert a new row in the table. The list of values come from a subquery

   function SQL_Insert_Default_Values
     (Table : SQL_Table'Class) return SQL_Query;
   --  Insert a new row in the table using default values for all fields

   function SQL_Update
     (Table : SQL_Table'Class;
      Set   : SQL_Assignment;
      Where : SQL_Criteria := No_Criteria;
      From  : SQL_Table_Or_List'Class := Empty_Table_List) return SQL_Query;
   --  Update the contents of a table.
   --  Where specifies which rows of the table are affected by the change.
   --  From should be used if Where references other tables. It can be
   --  auto-completed

   function SQL_Delete
     (From  : SQL_Table'Class;
      Where : SQL_Criteria := No_Criteria) return SQL_Query;
   --  Deletes all fields matching WHERE in the table FROM

   function SQL_Begin    return SQL_Query;
   function SQL_Rollback return SQL_Query;
   function SQL_Commit   return SQL_Query;
   --  Support for transactions

   function SQL_Lock (Table : SQL_Table'Class) return SQL_Query;
   --  Lock a table. This is a postgres extension

   procedure Auto_Complete
     (Self                   : in out SQL_Query;
      Auto_Complete_From     : Boolean := True;
      Auto_Complete_Group_By : Boolean := True);
   --  Automatically complete missing fields in the query, based on other
   --  fields.
   --  For a Select query, this includes the list of tables in From if
   --  Auto_Complete_From is true, and the list of fields in GROUP BY if
   --  Auto_Complete_Group_By is true.

   -----------------------
   -- subqueries tables --
   -----------------------
   --  These tables represent subqueries

   type Subquery_Table is new SQL_Single_Table with private;

   function Subquery
     (Query : SQL_Query; Table_Name : Cst_String_Access) return Subquery_Table;
   --  Create a temporary subquery table, as in:
   --    select * from b, (select ...) a where ...
   --    A := Subquery ("select ...", "a");
   --  Table_Name is never freed, and should therefore point to a "aliased
   --  constant String" in your code
   --  See the various inherited Field subprograms to reference specific fields
   --  from the result of the query.

   overriding function To_String (Self : Subquery_Table) return String;

   ---------------------------
   -- Conversion to strings --
   ---------------------------

   function To_String
     (Self : SQL_Criteria; Long : Boolean := True) return Unbounded_String;
   function To_String (Self : SQL_Query)       return Unbounded_String;
   function To_String
     (Self : SQL_Assignment; With_Field : Boolean)  return String;
   --  Transform Self into a valid SQL string

private

   -------------------------
   -- Table and instances --
   -------------------------

   type Table_Names is record
      Name     : Cst_String_Access;
      Instance : Cst_String_Access;
   end record;
   No_Names : constant Table_Names := (null, null);
   --  Describes a table (by its name), and the name of its instance. This is
   --  used to find all tables involved in a query, for the auto-completion. We
   --  do not store instances of SQL_Table'Class directly, since that would
   --  involve several things:
   --     - extra Initialize/Adjust/Finalize calls
   --     - Named_Field_Internal would need to embed a pointer to a table, as
   --       opposed to just its names, and therefore must be a controlled type.
   --       This makes the automatic package more complex, and makes the field
   --       type controlled, which is also a lot more costly.

   function Hash (Self : Table_Names) return Ada.Containers.Hash_Type;
   package Table_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Table_Names, Hash, "=", "=");

   type SQL_Table_Or_List is abstract tagged null record;
   procedure Append_Tables
     (Self : SQL_Table_Or_List; To : in out Table_Sets.Set) is null;
   --  Append all the tables referenced in Self to To

   type SQL_Single_Table (Instance : Cst_String_Access)
      is abstract new SQL_Table_Or_List with null record;
   --  instance name, might be null when this is the same name as the table.
   --  This isn't used for lists, but is used for all other types of tables
   --  (simple, left join, subqueries) so is put here for better sharing.

   type SQL_Table (Table_Name, Instance : Cst_String_Access)
      is abstract new SQL_Single_Table (Instance) with null record;
   overriding procedure Append_Tables
     (Self : SQL_Table; To : in out Table_Sets.Set);

   ------------------
   -- Tables lists --
   ------------------

   package Table_List is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (SQL_Single_Table'Class);

   type Table_List_Internal is record
      Refcount : Natural := 1;
      List     : Table_List.List;
   end record;
   type Table_List_Internal_Access is access all Table_List_Internal;
   --  Store the actual data for a SQL_Table_List in a different block (using
   --  a smart pointer for reference counting), since otherwise all the calls
   --  to "&" result in a copy of the list (per design of the Ada05 containers)
   --  which shows up as up to 20% of the number of calls to malloc on the
   --  testsuite).

   type Table_List_Data is new Ada.Finalization.Controlled with record
      Data : Table_List_Internal_Access;
   end record;
   overriding procedure Adjust (Self : in out Table_List_Data);
   overriding procedure Finalize (Self : in out Table_List_Data);

   type SQL_Table_List is new SQL_Table_Or_List with record
      Data : Table_List_Data;
   end record;
   overriding function To_String (Self : SQL_Table_List)  return String;
   overriding procedure Append_Tables
     (Self : SQL_Table_List; To : in out Table_Sets.Set);
   --  Append all the tables referenced in Self to To

   Empty_Table_List : constant SQL_Table_List :=
     (SQL_Table_Or_List
      with Data => (Ada.Finalization.Controlled with null));

   --------------------
   -- Field pointers --
   --------------------

   type Field_Access is access all SQL_Field'Class;
   type Field_Pointer_Data is record
      Refcount : Natural := 1;
      Field    : Field_Access;
   end record;
   type Field_Pointer_Data_Access is access Field_Pointer_Data;
   type SQL_Field_Pointer is new Ada.Finalization.Controlled with record
      Data : Field_Pointer_Data_Access;
   end record;
   procedure Adjust   (Self : in out SQL_Field_Pointer);
   procedure Finalize (Self : in out SQL_Field_Pointer);

   ----------------
   -- Field data --
   ----------------
   --  There are two kinds of fields: one is simple fields coming straight from
   --  the database ("table.field"), the other are fields computed through this
   --  API ("field1 || field2", Expression ("field"), "field as name"). The
   --  latter need to allocate memory to store their contents, and are stored
   --  in a refcounted type internally.

   type SQL_Field_Internal is abstract tagged record
      Refcount : Natural := 1;
   end record;
   type SQL_Field_Internal_Access is access all SQL_Field_Internal'Class;

   procedure Free (Data : in out SQL_Field_Internal) is null;
   --  Free memory associated with Data

   function To_String
     (Self : SQL_Field_Internal; Long : Boolean) return String is abstract;
   --  Return a displayable version of the field

   procedure Append_Tables
     (Self : SQL_Field_Internal; To : in out Table_Sets.Set) is null;
   --  Append all tables referenced by Self to To if they are not already there

   procedure Append_If_Not_Aggregate
     (Self         : access SQL_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean) is null;
   --  Append all fields referenced by Self if Self is not the result of an
   --  aggregate function. This is used for autocompletion of "group by".
   --  Is_Aggregate is set to True if Self is an aggregate, untouched otherwise

   type Field_Data is new Ada.Finalization.Controlled with record
      Data : SQL_Field_Internal_Access;
   end record;
   overriding procedure Adjust   (Self : in out Field_Data);
   overriding procedure Finalize (Self : in out Field_Data);
   --  Refcounted internal data for some types of fields

   -----------
   -- Field --
   -----------
   --  This type hierarchy for fields includes several types. It could be made
   --  smaller, but the goals are to keep the declaration of simple fields
   --  ("table.field") as simple as possible, and avoid using controlled types
   --  for those for maximum efficiency.

   type SQL_Field_Or_List is abstract tagged null record;

   type SQL_Field (Table, Instance, Name : Cst_String_Access)
     is abstract new SQL_Field_Or_List with null record;
   --  A field that comes directly from the database. It can be within a
   --  specific table instance, but we still need to know the name of the table
   --  itself for the autocompletion.
   --  (Table,Instance) might be null if the field is a constant

   procedure Append_Tables (Self : SQL_Field; To : in out Table_Sets.Set);
   --  Append the table(s) referenced by Self to To

   procedure Append_If_Not_Aggregate
     (Self         : SQL_Field;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);
   --  Append all fields referenced by Self if Self is not the result of an
   --  aggregate function. This is used for autocompletion of "group by".
   --  Is_Aggregate is set to True if Self is an aggregate, untouched otherwise

   type SQL_Field_Integer is new SQL_Field with null record;
   type SQL_Field_Text    is new SQL_Field with null record;
   type SQL_Field_Time    is new SQL_Field with null record;
   type SQL_Field_Boolean is new SQL_Field with null record;
   type SQL_Field_Float   is new SQL_Field with null record;
   --  A field coming straight from the database

   type SQL_Field_Integer_Build is new SQL_Field_Integer with record
      Data : Field_Data;
   end record;
   overriding function To_String
     (Self : SQL_Field_Integer_Build; Long : Boolean := True)  return String;
   overriding procedure Append_Tables
     (Self : SQL_Field_Integer_Build; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_Integer_Build;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

   type SQL_Field_Text_Build is new SQL_Field_Text with record
      Data : Field_Data;
   end record;
   overriding function To_String
     (Self : SQL_Field_Text_Build; Long : Boolean := True)  return String;
   overriding procedure Append_Tables
     (Self : SQL_Field_Text_Build; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_Text_Build;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

   type SQL_Field_Time_Build is new SQL_Field_Time with record
      Data : Field_Data;
   end record;
   overriding function To_String
     (Self : SQL_Field_Time_Build; Long : Boolean := True)  return String;
   overriding procedure Append_Tables
     (Self : SQL_Field_Time_Build; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_Time_Build;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

   type SQL_Field_Boolean_Build is new SQL_Field_Boolean with record
      Data : Field_Data;
   end record;
   overriding function To_String
     (Self : SQL_Field_Boolean_Build; Long : Boolean := True)  return String;
   overriding procedure Append_Tables
     (Self : SQL_Field_Boolean_Build; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_Boolean_Build;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

   type SQL_Field_Float_Build is new SQL_Field_Float with record
      Data : Field_Data;
   end record;
   overriding function To_String
     (Self : SQL_Field_Float_Build; Long : Boolean := True)  return String;
   overriding procedure Append_Tables
     (Self : SQL_Field_Float_Build; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_Float_Build;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);
   --  A field, constructed in the application (for instance a field
   --  within a join table, see also Expression)

   type SQL_Field_Any is new SQL_Field with record
      Data : Field_Data;
   end record;
   overriding function To_String
     (Self : SQL_Field_Any; Long : Boolean := True)  return String;
   overriding procedure Append_Tables
     (Self : SQL_Field_Any; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_Any;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

   package Field_List is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (SQL_Field'Class);
   type SQL_Field_List is new SQL_Field_Or_List with record
      List : Field_List.List;
   end record;
   procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_List;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);
   --  Append all fields referenced in Self to To, if Self is not the result of
   --  an aggregate function

   --------------------------
   --  Named field data --
   --------------------------
   --  Instantiation of the above for specific types of fields, created for
   --  instance via Expression, From_String, or operators on time. Such fields
   --  are still typed

   type Named_Field_Internal is new SQL_Field_Internal with record
      Table : Table_Names := No_Names;

      Value : GNAT.Strings.String_Access;
      --  The expression representing the field in SQL

      Operator : GNAT.Strings.String_Access;
      --  null unless we have an operator on several fields ("-" for instance)

      List     : SQL_Field_List;
   end record;
   type Named_Field_Internal_Access is access all Named_Field_Internal'Class;
   overriding procedure Free (Self : in out Named_Field_Internal);
   overriding function To_String
     (Self : Named_Field_Internal; Long : Boolean) return String;
   overriding procedure Append_Tables
     (Self : Named_Field_Internal; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : access Named_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

   -------------------
   -- As field data --
   -------------------
   --  Used when a field is renamed via "anything AS name"

   type As_Field_Internal is new SQL_Field_Internal with record
      As      : GNAT.Strings.String_Access;
      Renamed : SQL_Field_Pointer;
   end record;
   type As_Field_Internal_Access is access all As_Field_Internal'Class;
   overriding procedure Free (Self : in out As_Field_Internal);
   overriding function To_String
     (Self : As_Field_Internal; Long : Boolean) return String;
   overriding procedure Append_Tables
     (Self : As_Field_Internal; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : access As_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

   --------------------------
   -- Multiple args fields --
   --------------------------
   --  Several fields grouped into one via functions, operators or other. Such
   --  fields are not typed ("field1 operator field2 operator field3 ...")

   type Multiple_Args_Field_Internal is new SQL_Field_Internal with record
      Func_Name      : Cst_String_Access;
      Separator      : Cst_String_Access;
      Suffix         : GNAT.Strings.String_Access;
      In_Parenthesis : Boolean := False;
      List           : Field_List.List;
   end record;
   type Multiple_Args_Field_Internal_Access is access all
     Multiple_Args_Field_Internal'Class;
   overriding function To_String
     (Self : Multiple_Args_Field_Internal; Long : Boolean) return String;
   overriding procedure Append_Tables
     (Self : Multiple_Args_Field_Internal; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : access Multiple_Args_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);
   overriding procedure Free (Self : in out Multiple_Args_Field_Internal);

   -----------------------
   -- Aggregrate fields --
   -----------------------
   --  Representing an sql aggregate function

   type Aggregate_Field_Internal is new SQL_Field_Internal with record
      Func     : GNAT.Strings.String_Access;
      Params   : Field_List.List;
      Criteria : SQL_Criteria;
   end record;
   type Aggregate_Field_Internal_Access
     is access all Aggregate_Field_Internal'Class;
   overriding procedure Free (Self : in out Aggregate_Field_Internal);
   overriding function To_String
     (Self : Aggregate_Field_Internal; Long : Boolean) return String;
   overriding procedure Append_Tables
     (Self : Aggregate_Field_Internal; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : access Aggregate_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

   -----------------
   -- Sort fields --
   -----------------
   --  Fields used in the "ORDER BY" clauses

   type Sorted_Field_Internal is new SQL_Field_Internal with record
      Ascending : Boolean;
      Sorted    : SQL_Field_Pointer;
   end record;
   type Sorted_Field_Internal_Access is access all Sorted_Field_Internal'Class;
   overriding function To_String
     (Self : Sorted_Field_Internal; Long : Boolean) return String;
   overriding procedure Append_Tables
     (Self : Sorted_Field_Internal; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : access Sorted_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

   --------------
   -- Criteria --
   --------------

   type SQL_Criteria_Type is (Criteria_Equal,
                              Criteria_Not_Equal,
                              Criteria_Less_Than,
                              Criteria_Less_Or_Equal,
                              Criteria_Greater_Than,
                              Criteria_Greater_Or_Equal,
                              Criteria_And,
                              Criteria_Or,
                              Criteria_Like,
                              Criteria_Ilike,
                              Criteria_Not_Like,
                              Criteria_Not_Ilike,
                              Criteria_Overlaps,
                              Criteria_In,
                              Criteria_Not_In,
                              Criteria_Null,
                              Criteria_Not_Null);
   subtype Field_Criteria
     is SQL_Criteria_Type range Criteria_Equal .. Criteria_Greater_Or_Equal;
   subtype Criteria_Criteria
     is SQL_Criteria_Type range Criteria_And .. Criteria_Or;
   subtype Like_Criteria
     is SQL_Criteria_Type range Criteria_Like .. Criteria_Not_Ilike;
   subtype Null_Criteria
     is SQL_Criteria_Type range Criteria_Null .. Criteria_Not_Null;

   type SQL_Criteria_Data;
   type SQL_Criteria_Data_Access is access SQL_Criteria_Data;

   type Controlled_SQL_Criteria is new Ada.Finalization.Controlled with record
      Data : SQL_Criteria_Data_Access;
   end record;
   procedure Finalize (Self : in out Controlled_SQL_Criteria);
   procedure Adjust   (Self : in out Controlled_SQL_Criteria);

   procedure Append_Tables
     (Self : SQL_Criteria; To : in out Table_Sets.Set);
   --  Append all tables referenced in Self to To

   type SQL_Criteria is record
      Criteria : Controlled_SQL_Criteria;
   end record;
   --  SQL_Criteria must not be tagged, otherwise we have subprograms that are
   --  primitive for two types

   package Criteria_List is new Ada.Containers.Doubly_Linked_Lists
     (SQL_Criteria);

   type SQL_Criteria_Data (Op : SQL_Criteria_Type) is record
      Refcount : Natural := 1;
      case Op is
         when Field_Criteria | Like_Criteria | Criteria_Overlaps =>
            Arg1 : SQL_Field_Pointer;
            Arg2 : SQL_Field_Pointer;

         when Criteria_Criteria =>
            Criterias : Criteria_List.List;

         when Criteria_In | Criteria_Not_In =>
            Arg       : SQL_Field_Pointer;
            List      : SQL_Field_List;
            Subquery  : SQL_Query;
            In_String : Ada.Strings.Unbounded.Unbounded_String;

         when Null_Criteria =>
            Arg3 : SQL_Field_Pointer;
      end case;
   end record;
   No_Criteria : constant SQL_Criteria :=
     (Criteria => (Ada.Finalization.Controlled with Data => null));

   ----------------------
   --  Case statements --
   ----------------------

   type When_List_Item is record
      Criteria : SQL_Criteria;
      Field    : SQL_Field_Pointer;
   end record;

   package When_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (When_List_Item);
   type When_List is record
      List : When_Lists.List;
   end record;

   type Case_Stmt_Internal is new SQL_Field_Internal with record
      Criteria    : When_List;
      Else_Clause : SQL_Field_Pointer;
   end record;
   type Case_Stmt_Internal_Access is access all Case_Stmt_Internal'Class;
   overriding function To_String
     (Self : Case_Stmt_Internal; Long : Boolean) return String;
   overriding procedure Append_Tables
     (Self : Case_Stmt_Internal; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : access Case_Stmt_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);
   --  For all functions with multiple arguments (Concat, Coalesce,...)

   ---------------
   -- Left join --
   ---------------

   type Join_Table_Internal is record
      Refcount     : Natural := 1;
      Tables       : SQL_Table_List;
      On           : SQL_Criteria;
      Is_Left_Join : Boolean;
   end record;
   type Join_Table_Internal_Access is access all Join_Table_Internal;
   type Join_Table_Data is new Ada.Finalization.Controlled with record
      Data : Join_Table_Internal_Access;
   end record;
   overriding procedure Adjust (Self : in out Join_Table_Data);
   overriding procedure Finalize (Self : in out Join_Table_Data);
   --  The contents of a join table is in a smart pointer. That way, we avoid
   --  duplicating the data (especially the Ada2005 containers) whenever we
   --  "Adjust" a SQL_Left_Join_Table, which saves a number of system calls to
   --  malloc() and free()

   type SQL_Left_Join_Table is new SQL_Single_Table with record
      Data   : Join_Table_Data;
   end record;

   function To_String (Self : SQL_Left_Join_Table) return String;
   overriding procedure Append_Tables
     (Self : SQL_Left_Join_Table; To : in out Table_Sets.Set);

   -----------------
   -- Assignments --
   -----------------

   type Assignment_Item is record
      Refcount : Natural := 1;
      Field    : SQL_Field_Pointer;
      Value    : GNAT.Strings.String_Access;
      To_Field : SQL_Field_Pointer;
   end record;

   type Assignment_Item_Access is access all Assignment_Item;

   type Assignment_Controlled is new Ada.Finalization.Controlled with record
      Data : Assignment_Item_Access;
   end record;
   procedure Adjust   (Self : in out Assignment_Controlled);
   procedure Finalize (Self : in out Assignment_Controlled);

   package Assignment_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Assignment_Controlled);

   type SQL_Assignment is record
      List : Assignment_Lists.List;
   end record;

   No_Assignment : constant SQL_Assignment :=
     (List => Assignment_Lists.Empty_List);

   procedure Append_Tables
     (Self : SQL_Assignment; To : in out Table_Sets.Set);

   -------------
   -- Queries --
   -------------

   type Query_Contents is abstract tagged record
      Refcount : Natural := 1;
   end record;
   type SQL_Query_Contents_Access is access all Query_Contents'Class;
   procedure Free (Self : in out Query_Contents) is null;
   function To_String
     (Self : Query_Contents) return Unbounded_String is abstract;
   procedure Auto_Complete
     (Self                   : in out Query_Contents;
      Auto_Complete_From     : Boolean := True;
      Auto_Complete_Group_By : Boolean := True) is null;

   type Controlled_SQL_Query is new Ada.Finalization.Controlled with record
      Data : SQL_Query_Contents_Access;
   end record;
   procedure Finalize (Self : in out Controlled_SQL_Query);
   procedure Adjust   (Self : in out Controlled_SQL_Query);

   type SQL_Query is record
      Contents : Controlled_SQL_Query;
   end record;
   No_Query : constant SQL_Query :=
     (Contents => (Ada.Finalization.Controlled with null));

   type Query_Select_Contents is new Query_Contents with record
      Fields       : SQL_Field_List;
      Tables       : SQL_Table_List;
      Extra_Tables : Table_Sets.Set;  --  auto completed tables
      Criteria     : SQL_Criteria;
      Group_By     : SQL_Field_List;
      Having       : SQL_Criteria;
      Order_By     : SQL_Field_List;
      Limit        : Integer;
      Offset       : Integer;
      Distinct     : Boolean;
   end record;
   type Query_Select_Contents_Access is access all Query_Select_Contents'Class;
   overriding function To_String
     (Self : Query_Select_Contents) return Unbounded_String;
   overriding procedure Auto_Complete
     (Self                   : in out Query_Select_Contents;
      Auto_Complete_From     : Boolean := True;
      Auto_Complete_Group_By : Boolean := True);

   type Query_Insert_Contents is new Query_Contents with record
      Into           : Table_Names := No_Names;
      Default_Values : Boolean := False;
      Fields         : SQL_Field_List;
      Values         : SQL_Assignment;
      Where          : SQL_Criteria;
      Subquery       : SQL_Query := No_Query;
   end record;
   type Query_Insert_Contents_Access is access all Query_Insert_Contents'Class;
   overriding function To_String
     (Self : Query_Insert_Contents) return Unbounded_String;
   overriding procedure Auto_Complete
     (Self                   : in out Query_Insert_Contents;
      Auto_Complete_From     : Boolean := True;
      Auto_Complete_Group_By : Boolean := True);

   type Query_Update_Contents is new Query_Contents with record
      Table      : SQL_Table_List;
      Set        : SQL_Assignment;
      Where      : SQL_Criteria;
      From       : SQL_Table_List;
      Extra_From : Table_Sets.Set; --  from auto complete
   end record;
   type Query_Update_Contents_Access is access all Query_Update_Contents'Class;
   overriding function To_String
     (Self : Query_Update_Contents) return Unbounded_String;
   overriding procedure Auto_Complete
     (Self                   : in out Query_Update_Contents;
      Auto_Complete_From     : Boolean := True;
      Auto_Complete_Group_By : Boolean := True);

   type Query_Delete_Contents is new Query_Contents with record
      Table : SQL_Table_List;
      Where : SQL_Criteria;
   end record;
   type Query_Delete_Contents_Access is access all Query_Delete_Contents'Class;
   overriding function To_String
     (Self : Query_Delete_Contents) return Unbounded_String;

   type Simple_Query_Contents is new Query_Contents with record
      Command : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Simple_Query_Contents_Access is access all Simple_Query_Contents'Class;
   overriding function To_String
     (Self : Simple_Query_Contents) return Unbounded_String;

   ---------------------
   -- Subquery tables --
   ---------------------

   type Subquery_Table is new SQL_Single_Table with record
      Query : SQL_Query;
   end record;

   ------------------------------------
   --  Null field deferred constants --
   ------------------------------------

   Null_String : aliased constant String := "NULL";

   Null_Field_Integer : constant SQL_Field_Integer :=
     (Table    => null,
      Instance => null,
      Name     => Null_String'Access);
   Null_Field_Text : constant SQL_Field_Text :=
     (Table    => null,
      Instance => null,
      Name     => Null_String'Access);
   Null_Field_Boolean : constant SQL_Field_Boolean :=
     (Table    => null,
      Instance => null,
      Name     => Null_String'Access);

   Empty_Field_List : constant SQL_Field_List :=
     (SQL_Field_Or_List with List => Field_List.Empty_List);

end GNATCOLL.SQL;
