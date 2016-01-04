------------------------------------------------------------------------------
--                             M O D E L I N G                              --
--                                                                          --
--                     Copyright (C) 2010-2016, AdaCore                     --
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

--  An Object Relationship Management API.
--  This API creates a high-level Ada interface to a SQL database.
--  This package only contains base types that are further derived in an
--  automatically generated package that depends on your database schema. You
--  should not have to read the generated package, all comments are described
--  here.
--  Some subprograms are commented out in this package, and are here for
--  documentation purposes only. They illustrate what the automatically
--  generated package contains (see gnatcoll_db2ada documentation).

with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with GNATCOLL.SQL;          use GNATCOLL.SQL;
with GNATCOLL.SQL.Exec;     use GNATCOLL.SQL.Exec;

package GNATCOLL.SQL.Orm is

   Dynamic_Fetching : Boolean := True;
   --  If this variable is true, the Orm.* packages will automatically
   --  perform SQL queries to fetch missing information. Otherwise, you will
   --  get an exception Field_Not_Available, and you'll need to adjust the
   --  Select_Related for the manager. Setting this to False is mostly useful
   --  for debugging purposes, and helps optimize the queries.

   Field_Not_Available : exception;
   --  Raised when an sql query would need to be performed to retrieve a field,
   --  but Dynamic_Fetching is False.

   Self_Referencing : exception;
   --  Raised when an element is referencing itself, but it hasn't been
   --  inserted in the database yet. For instance:
   --      E := New_...;
   --      Set_FK (E, E);
   --      Session.Flush;
   --  The row cannot be inserted in the database, because we need to know
   --  E's id before we can insert E.
   --  The solution here is to first insert E without the self-reference, flush
   --  the session, and then set the self-reference (and possibly flush again).

   No_Update : aliased constant String := "#$%@%";
   --  A string that is unlikely to be specified by a user in a field. This is
   --  then used as the default value for various fields below, so that we
   --  know whether that field is being modified or not.

   type Manager is abstract tagged private;
   --  A high-level interface to the database. Through a Manager, you
   --  construct a query that is then run on the database and returns
   --  the list of results.
   --  Specialized children of Manager are provided for each of the tables in
   --  the database, to return the appropriate list of elements.
   --
   --  The manager does not cache the result of the query. As a result, the
   --  same manager can be reused multiple time to retrieve the same kind of
   --  elements. We do not have to decide in advance whether we want a forward
   --  or direct list, nor whether we want a standard or detached component.
   --  This reuse gives a more efficient API, since we can prepare in advance
   --  (possibly at elaboration time) a series of managers and reuse them if
   --  needed.
   --  ??? When we have prepared parameterized statements in GNATCOLL.SQL, we
   --  could take advantage of them here as well, and replace the actual values
   --  only at the last moment.
   --
   --  The managers that are automatically define a function Get that returns
   --  a specific row from a table (by primary key). This function is just a
   --  shortcut which creates a manager and retrieves a list of rows (limited
   --  to a single row for efficiency). This subprograms assumes there is a
   --  matching element. If you are not sure, retrieve the list directly and
   --  check whether it is empty.

   procedure Order_By (Self : in out Manager'Class; By : SQL_Field_List);
   procedure Order_By (Self : in out Manager'Class; By : SQL_Field'Class);
   pragma Inline (Order_By);
   --  Specifies a specific sort order.
   --  The generated managers will contain a version of the functions above.
   --  They return a copy of the Manager, so you can then manipulate both
   --  Self and the result independently.

   procedure Limit
     (Self : in out Manager'Class; Count : Natural; From : Natural := 0);
   pragma Inline (Limit);
   --  Return a copy of Self that will only retrieve a specific number of
   --  rows

   procedure Distinct (Self : in out Manager'Class);
   pragma Inline (Distinct);
   --  Return a copy of Self that ensures that no two rows in the result are
   --  the same.

   procedure Select_Related
      (Self  : in out Manager'Class;
       Depth : Natural;
       Follow_Left_Join : Boolean := False);
   pragma Inline (Select_Related);
   --  The query performed by Self will also return fields from other tables
   --  linked through a foreign key.
   --  For instance, if A.Field is a foreign key to B.PK, the resulting
   --  manager will retrieve the values for A, but also the related values
   --  from B, which can result in major speed ups when processing the
   --  results.
   --  Depth indicates how many levels of foreign keys are followed. If it is
   --  0, no related table will be fetched.
   --  This subprogram will include all related tables. For finer control, see
   --  the Select_Related operation that the manager defines and that allow
   --  you to specify which table to link. If you have called the version with
   --  Depth, though, the other version will have no effect.

   function Select_Related (Self : Manager'Class) return Natural;
   pragma Inline (Select_Related);
   --  Whether we should also query related tables. 0 indicates that we should
   --  not, and other result indicates to which depth

   procedure Filter (Self : in out Manager'Class; Condition : SQL_Criteria);
   pragma Inline (Filter);
   --  Filter the data returned by Self (returns a copy of Self).
   --  Condition can be used when you need to reference another table than the
   --  one manipulated by Self. If you only want to impact that table, it is
   --  better to use the dedicated version of Filter, which provides more type
   --  safety and will be easier to write in general.

   procedure Query (Self   : Manager'Class;
                    Query  : out SQL_Query;
                    Fields : SQL_Field_List;
                    From   : SQL_Table_List;
                    Criteria : SQL_Criteria := No_Criteria);
   --  Return the query to execute for this manager.
   --  Fields are the list of fields that should be retrieved by the
   --  query.
   --  From is the list of JOIN and LEFT JOIN that should be performed for
   --  the query. The query is autocompleted, so technically only the
   --  LEFT JOIN are needed.
   --  ??? Result should be cached, but making Self "in out" is not convenient

   ------------------------
   -- Lists and elements --
   ------------------------
   --  Each child package defines a manager specific to one table, and two
   --  kinds of lists: Forward_List and Direct_List. In both cases, the number
   --  of SQL queries is exactly the same. However, depending on the SQL
   --  backend, the former might be faster since we do not have to get all
   --  results in memory at once (in particular much faster with sqlite). In
   --  exchange, the list can be traversed only once.
   --
   --  Each package also provides two types of elements: standard components or
   --  detached components. In both cases, the number of queries is exactly the
   --  same. The former is more efficient since we do not create a record with
   --  the value of all fields (in particular avoiding copies of strings and
   --  parsing times, when these fields might not be needed), but we cannot
   --  move the list forward when using standard components. Detached
   --  components can be kept forever but require more memory management.

   --  Each component has a set of primitive operations to query the value of
   --  the fields. They can automatically lookup related records (performing
   --  SQL queries automatically if necessary). These lookups can be eliminated
   --  by using the appropriate value for Select_Related (for the manager).
   --  In the case of the detached components, the result value is also cached,
   --  thus multiple calls to the primitive operation will result in at most
   --  once SQL query. For standard components, a SQL query might be performed
   --  every time (in the spirit of keeping these components as light as
   --  possible). To avoid the extra queries, you should store the result in
   --  a temporary variable and reuse that as appropriate.
   --
   --  The list always holds a handle to the connection (which therefore is not
   --  released to the pool until the list is destroyed). As a result you
   --  should not store such a list in a datastructure (which in addition would
   --  be inefficient).

   Cursor_Has_Moved : exception;
   --  Raised when retrieving a value from an element, but the database cursor
   --  has changed in between. This does not occur with detached elements.

   type Forward_List;   --  extends GNATCOLL.SQL.Exec.Forward_Cursor
   type Direct_List;    --  extends GNATCOLL.SQL.Exec.Direct_Cursor
   --  The two types of lists that can be created by a manager. They influence
   --  efficiency of the code (forward_list: only moving forward, but only one
   --  row fetched at a time depending on DBMS; direct_list: moving in any
   --  order, but all data needs to be stored in memory)

   --------------------
   -- Implementation --
   --------------------
   --  The following types and subprograms are only useful for the package
   --  automatically generated by gnatcoll_db2ada to provide the ORM API for
   --  your schema. We need to make these types public so that the generated
   --  package (which will be in another package hierarchy) has access to them.

   type Shared_List_Data is record
      Session   : Session_Type;
      Follow_LJ : Boolean;  --  Whether to follow LEFT JOIN automatically
   end record;
   type Shared_List_Data_Access is access constant Shared_List_Data;

   procedure Copy (Self : Manager'Class; Into : in out Manager'Class);
   pragma Inline (Copy);
   --  Copy all fields from Self into Into

   type Counts is array (Natural range <>, Boolean range <>) of Field_Index;

   type Alias_Array is array (Natural range <>) of Integer;
   --  This array is used to compute the name of tables in queries. This is
   --  complex because a given table might occur several times, especially
   --  when select_related(...) is used. The table names are precomputed here
   --  for efficiency.
   --  Semantically, we have a nested list structure (the way it would be
   --  implemented in list), where each level indicates the aliases for tables.
   --  The first instance of all tables should have its full name, so that it
   --  is easier for users to write Filter or Order_By clauses.
   --
   --  For instance (in square in the alias index to use):
   --      links[-1]   ---meta---> associations[-1] ---end1---> assoc_end[-1]
   --                                               \--end2---> assoc_end[1]
   --                  \--end1---> models[-1] ---from---> model_names[-1]
   --                                         \--meta---> models[2]
   --                  \--end2---> models[0] ---from---> model_names[3]
   --                                         \--meta---> models[4]
   --
   --  This is stored as a flat array on the Ada side, because each level
   --  might have a varying number of entries, some of which are pointers to
   --  sublists, others are integers for the table aliases. In this table, the
   --  first item for each level is a alias number, others are jumps to other
   --  parts of the array.
   --
   --  Aliases : array (Integer range <>) of Integer :=
   --      (-1,4,16,?,   # links, meta index, end1 index,  end2 index
   --       -1,8,12,     # at index 4: meta is alias -1, end1 index, end2 index
   --       ...)
   --  The table is computed for the worst case, ie the deepest select_related
   --  we might use and with Follow_Left_Join. To keep queries shorter, alias
   --  numbers are assigned breadth first.

   No_Shared_Lists_Data : constant Shared_List_Data :=
     (Session    => No_Session,
      Follow_LJ  => False);
   --  Common information shared by the list types

   type Forward_List is new Forward_Cursor with record
      Data  : aliased Shared_List_Data;
      Depth : Natural;  --  Depth for Select_Related
   end record;
   No_Forward_List : constant Forward_List :=
      (No_Element with Data => No_Shared_Lists_Data, Depth => 0);
   --  Depth cannot be part of Data, since the same List is shared by all
   --  levels of elements (a Config (depth 0) points to a platform (depth 1),
   --  and yet they share the same list that contains the data).

   type Direct_List is new Direct_Cursor with record
      Data  : aliased Shared_List_Data;
      Depth : Natural;  --  Depth for Select_Related
   end record;
   No_Direct_List : constant Direct_List :=
      (No_Direct_Element with Data => No_Shared_Lists_Data, Depth => 0);

   function Follow_LJ (Self : Manager'Class) return Boolean;
   pragma Inline (Follow_LJ);
   --  Whether the m anager follows LEFT JOIN

private
   type Manager is abstract tagged record
      Where          : SQL_Criteria := No_Criteria;
      Order_By       : SQL_Field_List;
      Limit_Count    : Integer := -1;
      Offset         : Integer := -1;
      Distinct       : Boolean := False;
      Select_Related : Natural := 0;
      Follow_LJ      : Boolean := False;
   end record;
end GNATCOLL.SQL.Orm;
