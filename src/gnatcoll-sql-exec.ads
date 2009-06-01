-----------------------------------------------------------------------
--                               G N A T C O L L                     --
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

--  This package provides subprograms to interact with a database. It provides
--  a DBMS-agnostic API, which is further specialized in children packages for
--  each supported DBMS system.
--  There are various reasons to use this package preferrably to the low-level
--  package specific to each DBMS:
--    - your code is not specialized for a specific system, and can therefore
--      more easily be moved from one system to another
--    - automatic connection and reconnection to the database.
--      If the connection is lost, this package will automatically attempt to
--      reconnect (it also automatically connects the first time a query is
--      emitted).
--    - task safe
--    - automatic transactions
--      whenever you start modifying the database, a transaction is
--      automatically started. That helps ensure that only self-consistent
--      changes are performed, and is more efficient on most DBMS. No command
--      is sent to the DBMS if the current transaction is in a failure state,
--      which is also more efficient
--    - support for local caching of some queries
--      Very often, a database will contain tables whose contents rarely
--      changes, and corresponds to Ada enumerations. In such cases, this
--      package can cache the result locally to save round-trips to the DBMS
--      system.
--    - logging support.
--      All sql queries that are executed are traced through GNATCOLL.Traces

with Ada.Calendar;
private with Ada.Finalization;
private with GNAT.Strings;

package GNATCOLL.SQL.Exec is

   Perform_Queries : Boolean := True;
   --  If False, no operation is performed on the database, but the queries
   --  are logged as if the operation took place. This is only intended for
   --  automatic testsuites.

   DBMS_Postgresql : constant String := "postgresql";
   DBMS_Sqlite : constant String := "sqlite";
   --  The various database backends that are supported.
   --  GNATCOLL does not use these constants itself, but they are provided to
   --  serve as a common vocabulary for applications to describe the backend
   --  they want to use. These contents should be passed to Setup_Database (see
   --  below), and then on to the factory for Get_Task_Connection.
   --  You can create your own backends if needed, and thus create your own
   --  string constants for your application, if you want.

   -------------
   -- Cursors --
   -------------

   type Forward_Cursor is tagged private;
   No_Element : constant Forward_Cursor;
   --  A cursor that iterates over all rows of the result of an SQL query. A
   --  single row can be queried at a time, and there is no possibility to go
   --  back to a previous row (since not all DBMS backends support this).
   --  This type automatically takes care of memory management and frees its
   --  memory when no longer in use.
   --  This type is tagged only so that you can override it in your own
   --  applications (an example is to add an Element primitive operation which
   --  converts the current row into a specific Ada record for ease of use)

   type Abstract_DBMS_Forward_Cursor is abstract tagged private;
   type Abstract_Cursor_Access is
     access all Abstract_DBMS_Forward_Cursor'Class;
   --  Internal contents of a cursor.
   --  Instead of overriding Cursor directly, the support packages for the DBMS
   --  must override this type, so users do not have to use unconstrained types
   --  in their code, thus allowing "Result : Cursor" declarations.
   --  In practice, DBMS-specific backends will derive from
   --  gnatcoll-sql-exec-dbms_cursor, which defines the required primitive ops

   --------------------------
   -- Database_Description --
   --------------------------
   --  Data common to all the concurrent connections to the database.

   type Database_Description is private;
   --  Describes how to access a database, and stores global caches associated
   --  with that database.

   procedure Setup_Database
     (Description   : out Database_Description;
      Database      : String;
      User          : String := "";
      Host          : String := "";
      Password      : String := "";
      DBMS          : String := DBMS_Postgresql;
      Cache_Support : Boolean := False);
   --  Register the address of the database, for use by all other subprograms
   --  in this package.
   --  If Cache_Support is True, then some SQL queries can be cached locally,
   --  and reused by Execute above when its Use_Cache parameter is True. If
   --  Cache_Support is False, then no caching is ever done.

   procedure Free (Description : in out Database_Description);
   --  Free memory associated with description.
   --  This should only be called when the last database connection was closed,
   --  since each connection keeps a handle on the description

   function Get_Host     (Description : Database_Description) return String;
   function Get_User     (Description : Database_Description) return String;
   function Get_Database (Description : Database_Description) return String;
   function Get_Password (Description : Database_Description) return String;
   function Get_DBMS     (Description : Database_Description) return String;
   --  Return the connection components for the database

   -------------------------
   -- Database_Connection --
   -------------------------

   type Database_Connection_Record is abstract tagged private;
   type Database_Connection is access all Database_Connection_Record'Class;
   --  A thread-specific access to a database. Each thread, in an application,
   --  should have its own access to the database, so that transactions really
   --  are thread-specific. This also stores the result of the last query
   --  executed, and takes care of creating and cancelling transactions when
   --  needed.
   --  This type is really an access to some data, so that all subprograms
   --  below can take IN parameters. This simplifies user-code, which can
   --  therefore contain functions.
   --  This abstract type is specialized in GNATCOLL.SQL.Postgres and other
   --  child packages.

   function Check_Connection
     (Connection : access Database_Connection_Record'Class) return Boolean;
   --  Attempt to connect to the database, and return True if the connection
   --  was successful. Calling this subprogram is optional, since it will be
   --  done automatically when calling Execute (see below). This can however be
   --  used to ensure that the database works properly.

   procedure Fetch
     (Result     : out Forward_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : String);
   procedure Fetch
     (Result     : out Forward_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : GNATCOLL.SQL.SQL_Query);
   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      Query      : GNATCOLL.SQL.SQL_Query);
   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      Query      : String);
   --  Submit a query to the database, log it and wait for the result.
   --  Logs the query, as needed.
   --  The query can either be written directly as a string, or through a
   --  SQL_Query (which is encouraged, since it provides additional safety).
   --
   --  We used procedures instead of functions here for several reasons: that
   --  allows you to extend the Cursor type without overridding these
   --  procedures, this is slightly more efficient (since Cursor is a
   --  controlled type), and that forces the user to declare a local variable,
   --  rather than use Value (Execute (...), ...), which might have
   --  unpredictable results depending on when the controlled type is
   --  finalized. This also makes it easier to have your own specialized
   --  Execute functions in your application that return specific types of
   --  cursor, without requiring possibly costly copies of the result to
   --  convert from one type to another.
   --
   --  The names differ (Fetch and Execute) depending on whether the result is
   --  read or not. This is so that you can use dotted notation, as in:
   --       Curs.Fetch (Connection, Query)
   --  which reads better than Curs.Execute (Connection, Query), since we
   --  execute the query, not the cursor.
   --
   --  Result is always first reset to No_Element, so any custom field you
   --  might have will also be reset

   function Connect_And_Execute
     (Connection  : access Database_Connection_Record;
      Query       : String;
      Is_Select   : Boolean;
      Direct      : Boolean) return Abstract_Cursor_Access is abstract;
   --  This is mostly an internal subprogram, overridden by all DBMS-specific
   --  backends.
   --  If the connection to the database has not been made yet, connect to it.
   --  Then perform the query, reconnecting once if the connection failed.
   --  Will return null if the connection to the database is bad.
   --  If the query is the empty string, this procedure only connects to
   --  the database and checks the connection. It returns null if the
   --  connection is no longer valid.
   --  If Direct is true, a direct_cursor is created, otherwise a
   --  Forward_Cursor. The connection is allowed to return a direct cursor even
   --  if the user only wanted a forward_cursor, but the opposite is not
   --  allowed.

   procedure Close
     (Connection : access Database_Connection_Record) is abstract;
   procedure Free (Connection : in out Database_Connection);
   --  Close the connection to the database, if needed

   function Error
     (Connection : access Database_Connection_Record)
      return String is abstract;
   --  Return the last error message set by the database

   function Success
     (Connection : access Database_Connection_Record) return Boolean;
   --  Whether the last query succeeded. Note that when a query that modifies
   --  the database failed, no further query that modifies the database can be
   --  executed until the current transaction has been rolled back. This
   --  mirrors the standard behavior of postgres, and avoids sending a query
   --  that would not be executed anyway.

   procedure Set_Failure
     (Connection : access Database_Connection_Record'Class;
      Error_Msg  : String := "");
   --  Mark the transaction as failed. In general, this does not need to be
   --  done, but is needed when you expect for instance a SELECT to return at
   --  least one row, but it doesn't return any after an insertion.
   --  Error_Msg has the same semantics as for Rollback. If it isn't specified,
   --  this subprogram will test whether the database itself currently reports
   --  an error, and use that one instead.

   procedure Rollback
     (Connection : access Database_Connection_Record'Class;
      Error_Msg  : String := "");
   --  If a transaction is still active on Connection, then cancel it. This
   --  should be called as the last operation before the threads ends, to
   --  clean up the connection. The user must explicitly commit the transaction
   --  at an appropriate time.
   --  This resets the "Success" status to True.
   --  If Error_Msg is specified, that will be the message returned by
   --  Last_Error_Message, which users can later use to know why the
   --  transaction was aborted.

   function Last_Error_Message
     (Connection : access Database_Connection_Record'Class) return String;
   --  Reports the last error message on this connection (ie the one that
   --  made the transaction fail)

   function In_Transaction
     (Connection : access Database_Connection_Record'Class) return Boolean;
   --  Return True if a transaction is taking place (ie at least one
   --  modification to the database took place, and was not COMMIT'd or
   --  ROLLBACK'd.

   procedure Commit_Or_Rollback
     (Connection : access Database_Connection_Record'Class);
   --  Commit or rollback the current transaction, depending on whether we had
   --  an error. This does not affect the result of Success (unless COMMIT
   --  itself fails), so that you can still know afterward whether the
   --  transaction was committed or not.

   procedure Invalidate_Cache
     (Connection : access Database_Connection_Record'Class);
   --  Invalid all caches associated with the database (for all connections).
   --  Some queries can be cached (see Execute below) for  more efficiency.

   function Get_Task_Connection
     (Description : Database_Description;
      Factory     : access function
        (Desc : Database_Description) return Database_Connection;
      Username    : String := "")
      return Database_Connection;
   --  Return the database connection specific to the current task. A new one
   --  is created if none existed yet, and the connection to the database is
   --  done automatically.
   --  If the thread is not connected yet, a new connection is created through
   --  Factory.
   --  The newly created connection and Username are then passed to
   --  Reset_Connection (see below).

   procedure Reset_Connection
     (Description : Database_Description;
      Connection  : access Database_Connection_Record'Class;
      Username    : String := "");
   --  Reset the contents of Connection, and binds it to Description. In
   --  general, it is better to use Get_Task_Connection which does the
   --  necessary things, but when not in a multi-tasking application it is
   --  more efficient to have one "global" variable representing the single
   --  connection, and initialize it with this procedure
   --
   --  Username is used when tracing calls to the database. It is not the same
   --  as the user used to log in the database (typically, the username would
   --  be set to a unique identifier for the current application user, for
   --  instance the login name, whereas the application would always use a
   --  common user/password to log in the database)

   function Get_Description
     (Connection : access Database_Connection_Record'Class)
      return Database_Description;
   --  Return the description of the database to which we are connected

   ------------------------------------------
   -- Retrieving results - Forward cursors --
   ------------------------------------------
   --  The following subprograms represent a way to access the various
   --  columns returned by a query. A single row can be accessed at a time,
   --  since not all DBMS systems provide ways to query all results in memory
   --  at once (which might also not be efficient in the case of big tables).
   --
   --  These subprograms do not provide the same generality that DBMS-specific
   --  functions would, but represent with the most frequent use done with a
   --  result.

   type Field_Index is new Natural;

   function Processed_Rows (Self : Forward_Cursor) return Natural;
   --  The number of rows that were returned so far by the cursor. Every time
   --  you call Next, this is incremented by 1. If you looped until Has_Row
   --  returned False, this gives you the total number of rows in the result
   --  (which can not be computed without traversing all the results).
   --  If the query you executed is a DELETE, INSERT or UPDATE, this returns
   --  the number of rows modified by the query.

   function Has_Row (Self : Forward_Cursor) return Boolean;
   --  Whether there is a row to process. Fetching all the results from a query
   --  is done in a loop similar to:
   --      Cursor := Execute (...)
   --      while Has_Row (Cursor) loop
   --         ...
   --         Next (Cursor);
   --      end loop;

   procedure Next (Self : in out Forward_Cursor);
   --  Moves to the next row of results. This is not implemented as a function,
   --  since once the cursor was moved to the next field, there is no way to
   --  move back to the previous row.

   function Value (Self : Forward_Cursor; Field : Field_Index) return String;
   function Boolean_Value
     (Self : Forward_Cursor; Field : Field_Index) return Boolean;
   function Integer_Value
     (Self    : Forward_Cursor;
      Field   : Field_Index;
      Default : Integer := Integer'First) return Integer;
   function Float_Value
     (Self : Forward_Cursor; Field : Field_Index) return Float;
   function Time_Value
     (Self  : Forward_Cursor; Field : Field_Index) return Ada.Calendar.Time;
   --  Return a specific cell, converted to the appropriate format

   function Is_Null
     (Self  : Forward_Cursor; Field : Field_Index) return Boolean;
   --  True if the corresponding cell is not set

   function Last_Id
     (Connection : access Database_Connection_Record'Class;
      Self       : Forward_Cursor;
      Field      : SQL_Field_Integer) return Integer;
   --  Return the value set for field in the last INSERT command on that
   --  connection.
   --  Field must be an automatically incremented field (or a sql sequence).
   --  Returns -1 if the id could not be queried (perhaps the previous insert
   --  failed or was never committed). When the last_id could not be retrieved,
   --  the connection is set to the failure state
   --  Depending on the backend, this id might be computed through a sql query,
   --  so it is better to cache it if you need to reuse it several times.

   function Field_Count (Self : Forward_Cursor) return Field_Index;
   --  The number of fields per row in Res

   function Field_Name
     (Self : Forward_Cursor; Field : Field_Index) return String;
   --  The name of a specific field in a row of Res

   -----------------------------------------
   -- Retrieving results - Direct cursors --
   -----------------------------------------

   type Direct_Cursor is new Forward_Cursor with private;
   No_Direct_Element : constant Direct_Cursor;
   --  A direct cursor is a cursor that keeps all its results in memory, and
   --  gives access to any of the rows in any order.
   --  As opposed to a Forward_Cursor, you can iterate several times over the
   --  results. On the other hand, a direct_cursor uses more memory locally, so
   --  might not be the best choice systematically.

   function Rows_Count
     (Self : Direct_Cursor) return Natural renames Processed_Rows;
   --  Return total number of rows in result.
   --  Processed_Rows will always return the number read from the database

   procedure First (Self : in out Direct_Cursor);
   procedure Last  (Self : in out Direct_Cursor);
   --  Moves the cursor on the first or last row of results;

   procedure Absolute (Self : in out Direct_Cursor; Row : Positive);
   --  Moves the cursor on the specific row of results.
   --  The first row is numbered 1

   procedure Relative (Self : in out Direct_Cursor; Step : Integer);
   --  Moves the cursor by a specified number of rows. Step can be negative to
   --  move backward. Using Step=1 is the same as using Next

   procedure Fetch
     (Result     : out Direct_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : String;
      Use_Cache  : Boolean);
   procedure Fetch
     (Result     : out Direct_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : GNATCOLL.SQL.SQL_Query;
      Use_Cache  : Boolean);
   overriding procedure Fetch
     (Result     : out Direct_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : String);
   overriding procedure Fetch
     (Result     : out Direct_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : GNATCOLL.SQL.SQL_Query);
   --  Execute the query, and get all results in memory.
   --  If Use_Cache is True, and you are executing a SELECT query, the result
   --  of a previous execution of that query will be reused rather than
   --  executed again. If it was never executed, it will be cached for later
   --  use (no caching takes place if Use_Cache is False). This should mostly
   --  be used for queries to table that almost never changes, ie that store
   --  "enumeration types". The cache must be specifically invalidated (see
   --  Invalidate_Cache) to reset it, although it will also expire
   --  automatically and be refreshed after a while.

   --------------------------------------------
   -- Getting info about the database schema --
   --------------------------------------------
   --  The following subprograms will provide a view of the database schema (ie
   --  the set of tables and their fields, and the relationships between the
   --  tables).

   procedure Foreach_Table
     (Connection : access Database_Connection_Record;
      Callback   : access procedure (Name, Description : String)) is abstract;
   --  Find all tables in the database.
   --  For each, call Callback. Description is the comment that was optionally
   --  stored in the database to describe the role of the table (generally
   --  through a COMMENT command, which depends on the type of database you are
   --  using).

   procedure Foreach_Field
     (Connection : access Database_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Name        : String;
         Typ         : String;
         Index       : Natural;
         Description : String)) is abstract;
   --  For each attribute of the table, call Callback. Index is the attribute
   --  index in the table (column number). Description is the comment that was
   --  set when the attribute was created (for DBMS systems that support it),
   --  and can be the empty string

   procedure Foreach_Foreign_Key
     (Connection : access Database_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Index             : Positive;
         Local_Attribute   : Integer;
         Foreign_Table     : String;
         Foreign_Attribute : Integer)) is abstract;
   --  For each foreign key in Table_Name: calls the Callback for each
   --  attribute part of that key. For instance, if the key is a tuple of
   --  attributes pointing into a foreign table, the callback will be called
   --  twice, once for each attribute in the tuple. The index will be the same
   --  in the two calls to help identify foreign keys that are made of multiple
   --  attributes

   -------------------------
   -- Errors and Warnings --
   -------------------------

   procedure Print_Warning
     (Connection : access Database_Connection_Record'Class; Str : String);
   procedure Print_Error
     (Connection : access Database_Connection_Record'Class; Str : String);
   --  Print a warning or message to the appropriate GNATCOLL.Traces stream.

private

   type Database_Description_Record is record
      Host     : GNAT.Strings.String_Access;
      Dbname   : GNAT.Strings.String_Access;
      User     : GNAT.Strings.String_Access;
      Password : GNAT.Strings.String_Access;
      DBMS     : GNAT.Strings.String_Access;

      Caching : Boolean := False;
   end record;
   type Database_Description is access all Database_Description_Record;

   type Database_Connection_Record is abstract tagged record
      DB             : Database_Description;
      Success        : Boolean := True;
      In_Transaction : Boolean := False;
      Username       : GNAT.Strings.String_Access;
      Error_Msg      : GNAT.Strings.String_Access;
   end record;

   type Abstract_DBMS_Forward_Cursor is abstract tagged record
      Refcount : Natural := 1;
   end record;

   type Forward_Cursor is new Ada.Finalization.Controlled with record
      Res : Abstract_Cursor_Access;
   end record;
   overriding procedure Adjust   (Self : in out Forward_Cursor);
   overriding procedure Finalize (Self : in out Forward_Cursor);

   type Direct_Cursor is new Forward_Cursor with null record;
   --  The contents is of type Abstract_DBMS_Direct_Cursor, defined in
   --  GNATCOLL.SQL.Exec_Private, and implemented by each backend. All
   --  primitive ops forward to this contents

   No_Element : constant Forward_Cursor :=
     (Ada.Finalization.Controlled with null);
   No_Direct_Element : constant Direct_Cursor :=
     (Ada.Finalization.Controlled with null);

end GNATCOLL.SQL.Exec;
