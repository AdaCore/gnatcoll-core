------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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
with Ada.Finalization;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with GNATCOLL.SQL_Impl;      use GNATCOLL.SQL_Impl;

package GNATCOLL.SQL is

   subtype SQL_Criteria is GNATCOLL.SQL_Impl.SQL_Criteria;

   type SQL_Query is tagged private;
   --  A tagged type representing a query. This is a tagged type so that you
   --  can use the dotted notation of Ada05 to call its primitive operations,
   --  but you should not extend it

   subtype Cst_String_Access is GNATCOLL.SQL_Impl.Cst_String_Access;

   ------------
   -- Tables --
   ------------

   subtype SQL_Table_Or_List is GNATCOLL.SQL_Impl.SQL_Table_Or_List;
   --  Either a single table or a group of tables

   subtype SQL_Single_Table is GNATCOLL.SQL_Impl.SQL_Single_Table;
   --  Any type of table, or result of join between several tables. Such a
   --  table can have fields

   type SQL_Table_List is new SQL_Table_Or_List with private;
   Empty_Table_List : constant SQL_Table_List;
   --  A list of tables, as used in a SELECT query ("a, b")

   type SQL_Table (Table_Name, Instance : GNATCOLL.SQL_Impl.Cst_String_Access;
                   Instance_Index : Integer)
      is abstract new SQL_Single_Table with private;
   function To_String (Self : SQL_Table'Class) return String;
   overriding function To_String
     (Self : SQL_Table; Format : Formatter'Class) return String;
   --  A table representing a field of a specific table.
   --  If Instance is specified (i.e. not null), the FROM clause will include:
   --        SELECT ... FROM Table_Name Instance, ...
   --  Otherwise, if Instance_Index is not -1, the FROM clause will include:
   --        SELECT ... FROM Table_Name T<index>, ...
   --        ie a generic name for the table.
   --  Otherwise, the FROM clause will include:
   --        SELECT ... FROM Table_Name, ...
   --
   --  The goal is to ensure unicity of the table in a query (for instance if a
   --  table occurs several times in the FROM clause). So if you have a table
   --  Names, which could occur several times in a query, you could either
   --  provide explicit renaming of it, as in:
   --      Aliased_Name : aliased constant String := "aliased_name";
   --      Aliased_Table : T_Names (Instance => Aliased_Name'Access);
   --
   --      Q := SQL_Select (Fields => Aliased_Table.Name,
   --                       From   => Aliased_Table, ...)
   --
   --  This will work fine in most cases. However, in some cases (automatically
   --  generated queries for instance), you might not know in advance how many
   --  of those renamings you will need, and therefore cannot create all the
   --  "aliased constant String" in advance.
   --  In such a case, using the Instance_Index might provide an easier way.
   --
   --      Aliased : T_Names (Instance => null, Instance_Index => 1); --  "t1"
   --      Q := SQL_Select (Fields => Aliased.Name,
   --                       From   => Aliased, ...)

   type SQL_Unchecked_Table_Access is access constant SQL_Table'Class;

   type SQL_Table_Access is access all SQL_Table'Class;
   procedure Free (A : in out SQL_Table_Access);
   --  Needs to be freed explicitely

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

   subtype SQL_Field_List is GNATCOLL.SQL_Impl.SQL_Field_List;
   Empty_Field_List : constant SQL_Field_List :=
     GNATCOLL.SQL_Impl.Empty_Field_List;
   --  A list of fields, as used in a SELECT query ("field1, field2");

   subtype SQL_Field is GNATCOLL.SQL_Impl.SQL_Field;
   --  A single field

   function As
     (Field : SQL_Field'Class; Name : String) return SQL_Field'Class;
   --  Rename a field in the output. This is equivalent to "field AS name".
   --  The result is such that it can only be put in a list of fields, nothing
   --  else.

   function Desc (Field : SQL_Field'Class) return SQL_Field'Class;
   function Asc  (Field : SQL_Field'Class) return SQL_Field'Class;
   --  Specify a specific sort order. This is only used in the Order_By clause
   --  of a Select statement

   package Integer_Fields is new Field_Types
     (Integer, Integer_To_SQL, Parameter_Integer);
   type SQL_Field_Integer is new Integer_Fields.Field with null record;
   Null_Field_Integer : constant SQL_Field_Integer;
   function Integer_Param (Index : Positive) return Integer_Fields.Field'Class
                           renames Integer_Fields.Param;

   package Text_Fields is new Field_Types
     (String, String_To_SQL, Parameter_Text);
   type SQL_Field_Text is new Text_Fields.Field with null record;
   Null_Field_Text : constant SQL_Field_Text;
   function Text_Param (Index : Positive) return Text_Fields.Field'Class
                        renames Text_Fields.Param;

   package Boolean_Fields is new Field_Types
     (Boolean, Boolean_To_SQL, Parameter_Boolean);
   type SQL_Field_Boolean is new Boolean_Fields.Field with null record;
   Null_Field_Boolean : constant SQL_Field_Boolean;
   function Boolean_Param (Index : Positive) return Boolean_Fields.Field'Class
                           renames Boolean_Fields.Param;

   package Float_Fields is new Field_Types
     (Float, Float_To_SQL, Parameter_Float);
   type SQL_Field_Float is new Float_Fields.Field with null record;
   Null_Field_Float : constant SQL_Field_Float;
   function Float_Param (Index : Positive) return Float_Fields.Field'Class
                         renames Float_Fields.Param;

   subtype T_Money is GNATCOLL.SQL_Impl.T_Money;
   function "=" (T1, T2 : T_Money) return Boolean
      renames GNATCOLL.SQL_Impl."=";
   function "+" (T1, T2 : T_Money) return T_Money
      renames GNATCOLL.SQL_Impl."+";
   function "-" (T1, T2 : T_Money) return T_Money
      renames GNATCOLL.SQL_Impl."-";
   function "<" (T1, T2 : T_Money) return Boolean
      renames GNATCOLL.SQL_Impl."<";
   function "<=" (T1, T2 : T_Money) return Boolean
      renames GNATCOLL.SQL_Impl."<=";
   function ">" (T1, T2 : T_Money) return Boolean
      renames GNATCOLL.SQL_Impl.">";
   function ">=" (T1, T2 : T_Money) return Boolean
      renames GNATCOLL.SQL_Impl.">=";
   --  Make this type visible here, so that users do not have to explicitly
   --  'with' GNATCOLL.SQL_Impl.

   package Money_Fields is new Field_Types
     (T_Money, Money_To_SQL, Parameter_Money);
   type SQL_Field_Money is new Money_Fields.Field with null record;
   Null_Field_Money : constant SQL_Field_Money;
   function Money_Param (Index : Positive) return Money_Fields.Field'Class
                         renames Money_Fields.Param;

   package Time_Fields is new Field_Types
     (Ada.Calendar.Time, Time_To_SQL, Parameter_Time);
   type SQL_Field_Time is new Time_Fields.Field with null record;
   Null_Field_Time : constant SQL_Field_Time;
   function Time_Param (Index : Positive) return Time_Fields.Field'Class
                        renames Time_Fields.Param;
   --  A timestamp, ie date + time

   package Date_Fields is new Field_Types
     (Ada.Calendar.Time, Date_To_SQL, Parameter_Date);
   type SQL_Field_Date is new Date_Fields.Field with null record;
   Null_Field_Date : constant SQL_Field_Date;
   function Date_Param (Index : Positive) return Date_Fields.Field'Class
                        renames Date_Fields.Param;
   --  Only includes the date, not the time

   function From_String
     (Expression : String) return Text_Fields.Field'Class
      renames Text_Fields.From_String;
   --  Create a field from sql core. Expression is an SQL statement, no check
   --  is done though.

   function Expression
     (Value : String) return Text_Fields.Field'Class
      renames Text_Fields.Expression;
   function Expression
     (Value : Integer) return Integer_Fields.Field'Class
      renames Integer_Fields.Expression;
   function Expression
     (Value : Boolean) return Boolean_Fields.Field'Class
      renames Boolean_Fields.Expression;
   function Expression
     (Value : Float) return Float_Fields.Field'Class
      renames Float_Fields.Expression;
   function Expression
     (Value : Ada.Calendar.Time) return Time_Fields.Field'Class
     renames Time_Fields.Expression;
   function Expression
     (Value : Ada.Calendar.Time) return Date_Fields.Field'Class
     renames Date_Fields.Expression;
   --  Create constant fields (for a select statement for instance). The
   --  expression is surrounded by quotes, and special characters are
   --  escaped as needed

   function As_Days
     (Count : Natural) return Time_Fields.Field'Class;
   function As_Days
     (Count : Natural) return Date_Fields.Field'Class;
   --  An expression representing a number of days

   function Expression_Or_Null
     (Value : String) return Text_Fields.Field'Class;
   --  Same as above but if the Value is "NULL", returns NULL instead of 'NULL'

   function "+" (Left : SQL_Field'Class) return SQL_Field_List
                 renames GNATCOLL.SQL_Impl."+";

   function "&" (Left, Right : SQL_Field_List) return SQL_Field_List
                 renames GNATCOLL.SQL_Impl."&";
   function "&" (Left, Right : SQL_Field'Class) return SQL_Field_List
                 renames GNATCOLL.SQL_Impl."&";
   function "&"
     (Left : SQL_Field_List; Right : SQL_Field'Class) return SQL_Field_List
      renames GNATCOLL.SQL_Impl."&";
   function "&"
     (Left : SQL_Field'Class; Right : SQL_Field_List) return SQL_Field_List
      renames GNATCOLL.SQL_Impl."&";
   function "&" (List : SQL_Field_List; Value : String)  return SQL_Field_List
                 renames Text_Fields."&";
   function "&" (List : SQL_Field'Class; Value : String) return SQL_Field_List
                 renames Text_Fields."&";
   function "&" (Value : String; List : SQL_Field_List)  return SQL_Field_List
                 renames Text_Fields."&";
   function "&" (Value : String; List : SQL_Field'Class) return SQL_Field_List
                 renames Text_Fields."&";
   function "&" (List : SQL_Field_List; Value : Integer) return SQL_Field_List
                 renames Integer_Fields."&";
   function "&" (Value : Integer; List : SQL_Field_List) return SQL_Field_List
                 renames Integer_Fields."&";
   function "&" (List : SQL_Field'Class; Value : Integer) return SQL_Field_List
                 renames Integer_Fields."&";
   function "&" (Value : Integer; List : SQL_Field'Class) return SQL_Field_List
                 renames Integer_Fields."&";
   function "&" (List : SQL_Field_List; Value : Boolean) return SQL_Field_List
                 renames Boolean_Fields."&";
   function "&" (Value : Boolean; List : SQL_Field_List) return SQL_Field_List
                 renames Boolean_Fields."&";
   function "&" (List : SQL_Field'Class; Value : Boolean) return SQL_Field_List
                 renames Boolean_Fields."&";
   function "&" (Value : Boolean; List : SQL_Field'Class) return SQL_Field_List
                 renames Boolean_Fields."&";
   --  Create a list of fields, suitable for use in a SELECT query

   function "-" is new Time_Fields.Operator ("-");
   function "+" is new Time_Fields.Operator ("+");

   function "-" is new Date_Fields.Operator ("-");
   function "+" is new Date_Fields.Operator ("+");

   function "-" is new Integer_Fields.Scalar_Operator (Integer, "-");
   function "+" is new Integer_Fields.Scalar_Operator (Integer, "+");
   function "*" is new Integer_Fields.Scalar_Operator (Integer, "*");
   function "/" is new Integer_Fields.Scalar_Operator (Integer, "/");

   function Current_Date is new Date_Fields.SQL_Function ("current_date");
   function Current_Timestamp
      is new Time_Fields.SQL_Function ("current_timestamp");
   --  Return the current date or timestamp

   function Absolute
     (Field : Integer_Fields.Field'Class) return Integer_Fields.Field'Class;
   function Lower
     (Field : Text_Fields.Field'Class) return Text_Fields.Field'Class;
   function Initcap
     (Field : Text_Fields.Field'Class) return Text_Fields.Field'Class;
   --  Return the corresponding SQL function applied on Field

   function Cast_To_String
     (Field : SQL_Field'Class) return Text_Fields.Field'Class;
   --  Convert any field type to a text field

   function Cast_To_Date
     (Field : SQL_Field_Time'Class) return Date_Fields.Field'Class;
   --  Convert a time to a date

   function At_Time_Zone
     (Field : Time_Fields.Field'Class; TZ : String)
      return Time_Fields.Field'Class;
   --  Convert a 'timestamp with time zone' expression to another time zone

   function To_Char
     (Field : Time_Fields.Field'Class; Format : String)
      return Text_Fields.Field'Class;
   --  Format a date field, as in "to_char (field, "format")"

   function Extract
     (Field : Time_Fields.Field'Class; Attribute : String)
      return Time_Fields.Field'Class;
   function Extract
     (Field : Date_Fields.Field'Class; Attribute : String)
      return Date_Fields.Field'Class;
   --  Return the result of "extract (attribute from field)"

   -------------------------
   -- Aggregate functions --
   -------------------------

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

   ---------------------------------
   -- Functions on list of fields --
   ---------------------------------
   --  The following functions apply to lists of fields, and return a single
   --  field. A generic version is provided so that you can implement your own.

   generic
      Func_Name : String := "";
      Separator : String := ",";
      Suffix    : String := "";
   function Field_List_Function
     (Fields : SQL_Field_List) return SQL_Field'Class;
   --  A function that applies to multiple fields, as in
   --     Func_Name [Separator Field1]* Suffix
   --  For instance, "coalesce (a, b, c)"
   --  The parenthesis must be part of Func_Name and Suffix if they are needed.

   function Concat (Fields : SQL_Field_List) return SQL_Field'Class;
   --  Converts the list into a concatenation of fields, as in:
   --     "prefix " || foo.bar || "suffix"

   function Tuple (Fields : SQL_Field_List) return SQL_Field'Class;
   --  Return the list of fields as a tuple, ie (field1, field2)

   function Coalesce (Fields : SQL_Field_List) return SQL_Field'Class;
   --  Returns the first of its arguments that is not null
   --     Coalesce (value1, value2, ...)

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

   subtype SQL_Field_Pointer is GNATCOLL.SQL_Impl.SQL_Field_Pointer;
   function "+" (Field : SQL_Field'Class) return SQL_Field_Pointer
                 renames GNATCOLL.SQL_Impl."+";
   --  Create a new pointer. Memory will be deallocated automatically

   type SQL_Field_Array is array (Natural range <>) of SQL_Field_Pointer;

   function To_List (Fields : SQL_Field_Array) return SQL_Field_List;
   --  Convert the array into a list

   --------------
   -- Criteria --
   --------------
   --  Most of the comparison operators ("<=", "<", "=", ">=", ">") are
   --  automatically inherited from gnatcoll.sql_impl types by the
   --  SQL_Field_Text, SQL_Field_Integer,... types. They thus do not appear in
   --  this API, although they are usable directly within your code.
   --  There is however one case where these attributes are not inherited: the
   --  result of the functions like As_Days, Lower,... are of a type
   --  Text_Fields.Field, not SQL_Field_Text. As a result, their operators are
   --  not directly visible in your package. You should add
   --      use type Text_Fields.Field;
   --  in your package to make the operators visible.

   No_Criteria : constant SQL_Criteria := GNATCOLL.SQL_Impl.No_Criteria;

   function "=" (C1, C2 : SQL_Criteria) return Boolean
                 renames GNATCOLL.SQL_Impl."=";

   function Greater_Than
     (Left : SQL_Field'Class; Right : Integer) return SQL_Criteria
      renames Integer_Fields.Greater_Than;
   function Greater_Or_Equal
     (Left : SQL_Field'Class; Right : Integer) return SQL_Criteria
      renames Integer_Fields.Greater_Or_Equal;
   function Equal
     (Left : SQL_Field'Class; Right : Boolean) return SQL_Criteria
      renames Boolean_Fields.Equal;
   --  Same as ">" and ">=", but usable for instance for aggregate fields
   --  resulting from the use of Apply

   function "and" (Left, Right : SQL_Criteria) return SQL_Criteria;
   function "or"  (Left, Right : SQL_Criteria) return SQL_Criteria;
   --  Combine two criterias

   function "and"
     (Left : SQL_Criteria; Right : Boolean_Fields.Field'Class)
      return SQL_Criteria;
   function "or"
     (Left : SQL_Criteria; Right : Boolean_Fields.Field'Class)
      return SQL_Criteria;
   --  Combine two criterias, one of which is for a boolean test. This is just
   --  to simplify the writting

   function "not" (Left : Boolean_Fields.Field'Class) return SQL_Criteria;
   --  Test that Left is False. This can also be done with an explicit call to
   --  "=" above

   function "not" (Self : SQL_Criteria) return SQL_Criteria;
   --  Return the opposite of Self

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

   function Any
     (Self, Str : Text_Fields.Field'Class) return SQL_Criteria;
   --  "Self = ANY (Str)"

   function Ilike
     (Self : Text_Fields.Field'Class; Str : String) return SQL_Criteria;
   function Ilike
     (Self : Text_Fields.Field'Class; Field : SQL_Field'Class)
      return SQL_Criteria;
   function Like
     (Self : Text_Fields.Field'Class; Str : String) return SQL_Criteria;
   function Not_Ilike
     (Self : Text_Fields.Field'Class; Str : String) return SQL_Criteria;
   function Not_Like
     (Self : Text_Fields.Field'Class; Str : String) return SQL_Criteria;
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
   --  The operator "=" is inherited from gnatcoll-sql_impl for all fields
   --  (either between two fields, or between a field and a scalar value).

   subtype SQL_Assignment is GNATCOLL.SQL_Impl.SQL_Assignment;
   No_Assignment : constant SQL_Assignment;

   function "=" (Left, Right : SQL_Assignment) return Boolean
                 renames GNATCOLL.SQL_Impl."=";
   --  Compare two assignments (this makes the implicit equality visible to
   --  users of this package who haven't "use"d GNATCOLL.SQL_Impl

   function "&" (Left, Right : SQL_Assignment) return SQL_Assignment
                 renames GNATCOLL.SQL_Impl."&";
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
      On      : SQL_Criteria) return SQL_Left_Join_Table;
   --  Performs a left join between the two tables. It behaves like a standard
   --  join, but if a row from Full doesn't match any row in Partial, a virtual
   --  row full of NULL is added to Partial, and returned in the join.

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
      Distinct : Boolean := False;
      Auto_Complete : Boolean := False) return SQL_Query;
   --  Select one or more fields from one or more tables
   --  If Auto_Complete is true, the resulting query is auto-completed just as
   --  if you had called the Auto_Complete subprogram. This is put here so that
   --  you can have global SQL_Query constants, pre-completed

   function SQL_Union
     (Query1, Query2 : SQL_Query;
      Order_By : SQL_Field_Or_List'Class := Empty_Field_List;
      Limit    : Integer := -1;
      Offset   : Integer := -1;
      Distinct : Boolean := False) return SQL_Query;
   --  Join the two queries with a Union.
   --  The Limit, Offset and Order_By parameters for each query will be
   --  ignored by the DBMS. When the union is itself used in another union,
   --  only the outer-most union will have its Order_By, Limit and Offset
   --  taken into account.

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
   -- Extending queries --
   -----------------------
   --  It is often convenient to have slightly similar versions of queries, but
   --  with a few differences. For instance, you might want to prepare a
   --  first version of the query, and then have a second version with
   --  additional criteria.
   --     Q : SQL_Query := SQL_Select (...);
   --     P : Prepared_Statement := Prepare (Q);
   --     Q2 : SQL_Query := Q.Where_And (...);

   function Where_And
     (Query : SQL_Query; Where : SQL_Criteria) return SQL_Query;
   function Where_Or
     (Query : SQL_Query; Where : SQL_Criteria) return SQL_Query;
   --  Add a new "and" or "or" clause to the query (which must be a SELECT
   --  query). The result is a separate query which can be modified
   --  independently of Query.
   --  This does not auto-complete the result query, even if the original
   --  query had been auto-completed.

   function Order_By
     (Query : SQL_Query; Order_By : SQL_Field_Or_List'Class)
      return SQL_Query;
   --  Adds extra field in the order_by part of the query. These are added
   --  *before* the order_by clause of Query, so that they take priority

   function Distinct (Query : SQL_Query) return SQL_Query;
   --  Remove duplicate rows in the result of query

   function Offset (Query : SQL_Query; Offset : Natural) return SQL_Query;
   function Limit (Query : SQL_Query; Limit : Natural) return SQL_Query;
   --  Modifies the "limit" and "offset" in the query. This is useful if you
   --  need to repeat the query several times to get various pages of results

   -----------------------
   -- subqueries tables --
   -----------------------
   --  These tables represent subqueries as used in a "FROM" list.
   --  There is no support for using subqueries in the list of fields: it is
   --  just more efficient to perform two separate queries in such a case.
   --  Example of use:
   --     N_Sorted : aliased constant String := "sorted";
   --     Sorted : constant Subquery_Table :=
   --       Subquery (SQL_Select (Config.Name, ...), N_Sorted'Access);
   --     Sorted_Config   : constant SQL_Field_Text'Class :=
   --       SQL_Field_Text (Config.Name.From_Table (Sorted));
   --
   --  You can then use the table Sorted in any SQL_Select query, and access
   --  its fields via Sorted_Config.

   type Subquery_Table is new SQL_Single_Table with private;

   function Subquery
     (Query : SQL_Query'Class; Table_Name : Cst_String_Access)
      return Subquery_Table;
   --  Create a temporary subquery table, as in:
   --    select * from b, (select ...) a where ...
   --    A := Subquery ("select ...", "a");
   --  Table_Name is never freed, and should therefore point to a "aliased
   --  constant String" in your code
   --  See the various inherited Field subprograms to reference specific fields
   --  from the result of the query.

   overriding function To_String
     (Self : Subquery_Table; Format : Formatter'Class) return String;

   ---------------------------
   -- Conversion to strings --
   ---------------------------

   function To_String
     (Self : SQL_Query; Format : Formatter'Class) return Unbounded_String;
   --  Transform Self into a valid SQL string

private

   -------------------------
   -- Table and instances --
   -------------------------

   type SQL_Table (Table_Name, Instance : Cst_String_Access;
                   Instance_Index : Integer)
   is abstract new
     SQL_Single_Table (Instance, Instance_Index) with null record;
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
   overriding function To_String
     (Self : SQL_Table_List; Format : Formatter'Class)  return String;
   overriding procedure Append_Tables
     (Self : SQL_Table_List; To : in out Table_Sets.Set);
   --  Append all the tables referenced in Self to To

   Empty_Table_List : constant SQL_Table_List :=
     (SQL_Table_Or_List
      with Data => (Ada.Finalization.Controlled with null));

   -----------
   -- Field --
   -----------
   --  This type hierarchy for fields includes several types. It could be made
   --  smaller, but the goals are to keep the declaration of simple fields
   --  ("table.field") as simple as possible, and avoid using controlled types
   --  for those for maximum efficiency.

   procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_List;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);
   --  Append all fields referenced in Self to To, if Self is not the result of
   --  an aggregate function

   --------------
   -- Criteria --
   --------------

   type SQL_Criteria_Type is (Criteria_And,
                              Criteria_Or,
                              Criteria_In,
                              Criteria_Not_In,
                              Criteria_Null,
                              Criteria_Not_Null,
                              Criteria_Not);
   subtype Criteria_Criteria
     is SQL_Criteria_Type range Criteria_And .. Criteria_Or;
   subtype Null_Criteria
     is SQL_Criteria_Type range Criteria_Null .. Criteria_Not_Null;

   package Criteria_List is new Ada.Containers.Doubly_Linked_Lists
     (SQL_Criteria);

   type SQL_Criteria_Data (Op : SQL_Criteria_Type) is
      new GNATCOLL.SQL_Impl.SQL_Criteria_Data with record
      case Op is
         when Criteria_Criteria =>
            Criterias : Criteria_List.List;

         when Criteria_In | Criteria_Not_In =>
            Arg       : SQL_Field_Pointer;
            List      : SQL_Field_List;
            Subquery  : SQL_Query;
            In_String : Ada.Strings.Unbounded.Unbounded_String;

         when Null_Criteria =>
            Arg3 : SQL_Field_Pointer;

         when Criteria_Not =>
            Criteria : SQL_Criteria;
      end case;
   end record;

   overriding function To_String
     (Self   : SQL_Criteria_Data;
      Format : Formatter'Class;
      Long   : Boolean := True) return String;
   overriding procedure Append_Tables
     (Self : SQL_Criteria_Data; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : SQL_Criteria_Data;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

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
     (Self   : Case_Stmt_Internal;
      Format : Formatter'Class;
      Long   : Boolean) return String;
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

   overriding function To_String
     (Self : SQL_Left_Join_Table; Format : Formatter'Class) return String;
   overriding procedure Append_Tables
     (Self : SQL_Left_Join_Table; To : in out Table_Sets.Set);

   -----------------
   -- Assignments --
   -----------------

   No_Assignment : constant SQL_Assignment :=
     GNATCOLL.SQL_Impl.No_Assignment;

   -------------
   -- Queries --
   -------------

   type Query_Contents is abstract tagged record
      Refcount : Natural := 1;
   end record;
   type SQL_Query_Contents_Access is access all Query_Contents'Class;
   procedure Free (Self : in out Query_Contents) is null;
   function To_String
     (Self   : Query_Contents;
      Format : Formatter'Class) return Unbounded_String is abstract;
   procedure Auto_Complete
     (Self                   : in out Query_Contents;
      Auto_Complete_From     : Boolean := True;
      Auto_Complete_Group_By : Boolean := True) is null;

   type Controlled_SQL_Query is new Ada.Finalization.Controlled with record
      Data : SQL_Query_Contents_Access;
   end record;
   procedure Finalize (Self : in out Controlled_SQL_Query);
   procedure Adjust   (Self : in out Controlled_SQL_Query);

   type SQL_Query is tagged record
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
     (Self   : Query_Select_Contents;
      Format : Formatter'Class) return Unbounded_String;
   overriding procedure Auto_Complete
     (Self                   : in out Query_Select_Contents;
      Auto_Complete_From     : Boolean := True;
      Auto_Complete_Group_By : Boolean := True);

   type Query_Union_Contents is new Query_Contents with record
      Q1, Q2       : SQL_Query;
      Order_By     : SQL_Field_List;
      Limit        : Integer;
      Offset       : Integer;
      Distinct     : Boolean;
   end record;
   type Query_Union_Contents_Access is access all Query_Union_Contents'Class;
   overriding function To_String
     (Self   : Query_Union_Contents;
      Format : Formatter'Class) return Unbounded_String;

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
     (Self   : Query_Insert_Contents;
      Format : Formatter'Class) return Unbounded_String;
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
     (Self   : Query_Update_Contents;
      Format : Formatter'Class) return Unbounded_String;
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
     (Self   : Query_Delete_Contents;
      Format : Formatter'Class) return Unbounded_String;

   type Simple_Query_Contents is new Query_Contents with record
      Command : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Simple_Query_Contents_Access is access all Simple_Query_Contents'Class;
   overriding function To_String
     (Self   : Simple_Query_Contents;
      Format : Formatter'Class) return Unbounded_String;

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
     (Integer_Fields.Null_Field with null record);
   Null_Field_Text : constant SQL_Field_Text :=
     (Text_Fields.Null_Field with null record);
   Null_Field_Boolean : constant SQL_Field_Boolean :=
     (Boolean_Fields.Null_Field with null record);
   Null_Field_Float : constant SQL_Field_Float :=
     (Float_Fields.Null_Field with null record);
   Null_Field_Money : constant SQL_Field_Money :=
     (Money_Fields.Null_Field with null record);
   Null_Field_Time : constant SQL_Field_Time :=
     (Time_Fields.Null_Field with null record);
   Null_Field_Date : constant SQL_Field_Date :=
     (Date_Fields.Null_Field with null record);

end GNATCOLL.SQL;
