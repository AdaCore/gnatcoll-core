------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2014, AdaCore                     --
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
--
--  There are various ways to execute queries and get their results:
--    - two types of cursors (forward_cursor or direct_cursor) dependeing on
--      whether you want to keep all results in memory or not. Using direct
--      cursors is more flexible, but slower since there is a need for a lot
--      more memory allocations. Examples of timing (executing 10_000 times a
--      query joining two tables, returning 400 rows).
--         sqlite:
--           FORWARD_CURSOR, getting first row     => 0.707530000 s
--           FORWARD_CURSOR, iterating on all rows => 3.193714000 s
--           DIRECT_CURSOR, getting first row      => 5.541400000 s
--           DIRECT_CURSOR, iterating on all rows  => 5.600546000 s
--
--    - Prepared statements on the client
--      Statements are generally written with GNATCOLL.SQL. They then need to
--      be converted to String to be sent to the DBMS server. This conversion
--      (along with possible auto-completion of the query) takes some non
--      negligible amount of time. You can thus prepare such queries once and
--      for all. Here is an example on the same query as above, but the query
--      is prepared once, and then executed 10_000 times. This is only looking
--      at the first row in the result, so should be compared with the first
--      line above.
--         sqlite:
--           FORWARD_CURSOR => 0.413184000 s
--           DIRECT_CURSOR  => 5.398043000 s
--
--    - Prepared statements on the server
--      In addition to the above client-side preparation, most DBMS systems
--      support the notion of analyzing the query on the server, and optimize
--      it there. Such a preparation is valid for a specific connection (so
--      several preparations will be needed if you have multiple concurrent
--      connections to the database (but this API takes care of that
--      automatically for you). This can provide significant speed up. When
--      using direct_cursor, we still need to perform a lot of memory
--      allocations to store the results in memory
--      (400 rows * 2 columns * 10_000 iterations allocations in the example)
--         sqlite:
--           FORWARD_CURSOR =>  0.047700000 s
--           DIRECT_CURSOR  =>  5.014961000 s
--
--    - Caching on the client side
--      Last, this API is able to cache the result of a query locally on the
--      client, and thus save any round-trip to the server. At the cost of some
--      memory, it provides the fastest possible access to the data. This is
--      only usable with prepared statements, and only makes sense for direct
--      cursors, since by definition the cache should be iterable several
--      times). Note also that using server-side preparation does not
--      significantly speed things up, since the statement is executed only
--      once anyway. Here, we are only doing 400 * 2 allocations to store
--      results in memory.
--         sqlite:
--             DIRECT_CURSOR =>  0.015502000 s

with Ada.Calendar;
with System;
private with Ada.Finalization;
private with GNATCOLL.Refcount;
with GNAT.Strings;

package GNATCOLL.SQL.Exec is

   Perform_Queries : Boolean := True;
   --  If False, no operation is performed on the database, but the queries
   --  are logged as if the operation took place. This is only intended for
   --  automatic testsuites.

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

   ----------------
   -- Parameters --
   ----------------
   --  All database systems support a way of writing queries without using
   --  specific values. Instead, those are bound when the query is executed.
   --  Thus, in sqlite, the query might look like:
   --      SELECT * FROM table WHERE table.field1 = ?1
   --  whereas in postgreSQL it would be
   --      SELECT * FROM table WHERE table.field1 = $1
   --
   --  Such queries are created in GNATCOLL through the use of
   --  GNATCOLL.SQL.Text_Param, GNATCOLL.SQL.Integer_Param,...

   type SQL_Parameter (Typ : Parameter_Type := Parameter_Integer) is record
      case Typ is
         when Parameter_Integer   => Int_Val : Integer;
         when Parameter_Json      => Json_Val  : access constant String;
         when Parameter_XML       => XML_Val   : access constant String;
         when Parameter_Text      => Str_Val : access constant String;
            --  references external string, to avoid an extra copy
         when Parameter_Boolean   => Bool_Val : Boolean;
         when Parameter_Float     => Float_Val : Float;
         when Parameter_Time      => Time_Val  : Ada.Calendar.Time;
         when Parameter_Date      => Date_Val  : Ada.Calendar.Time;
         when Parameter_Character => Char_Val : Character;
         when Parameter_Money     => Money_Val : T_Money;
      end case;
   end record;

   Null_Parameter : constant SQL_Parameter;

   function "+" (Value : access constant String) return SQL_Parameter;
   function "+" (Value : Integer) return SQL_Parameter;
   function "+" (Value : Boolean) return SQL_Parameter;
   function "+" (Value : Float) return SQL_Parameter;
   function "+" (Value : Character) return SQL_Parameter;
   function "+" (Time : Ada.Calendar.Time) return SQL_Parameter;
   function "+" (Value : T_Money) return SQL_Parameter;

   type SQL_Parameters is array (Positive range <>) of SQL_Parameter;
   No_Parameters : constant SQL_Parameters;

   function Image
     (Format : Formatter'Class; Param : SQL_Parameter) return String;
   function Image
     (Format : Formatter'Class; Params : SQL_Parameters)
      return String;
   --  Return a displayable version of the parameters list

   --------------------------
   -- Database_Description --
   --------------------------
   --  Data common to all the concurrent connections to the database.

   type Error_Reporter is abstract tagged private;
   type Error_Reporter_Access is access all Error_Reporter'Class;
   --  This type is used by the various methods that need to report SQL
   --  errors or issues with the database. Not all the primitive operations
   --  are used by all database backends (for instance, postgresql does not
   --  report an error for a corrupted database).

   type Database_Description_Record
     (Caching : Boolean;
      Errors  : access Error_Reporter'Class) is abstract tagged private;
   type Database_Description is access all Database_Description_Record'Class;
   --  Describes how to access a database, and stores global caches associated
   --  with that database.
   --  This type is derived in each of the DBMS specific packages. See
   --  GNATCOLL.SQL.Sqlite and GNATCOLL.SQL.Postgres for instance.
   --
   --  If Cache is true, some statements will be cached locally in the
   --  connection (see the parameter Use_Cache for the Prepare subprograms
   --  below).

   type Database_Connection_Record
     (Descr : access Database_Description_Record'Class;
      Always_Use_Transactions : Boolean)
   is abstract new Formatter with private;
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
   --  Always_Use_Transactions is used internally to indicate whether GNATCOLL
   --  should always start a SQL transaction even for SELECT statements. This
   --  might result in significant speed ups for some DBMS (sqlite)

   function Build_Connection
     (Self : access Database_Description_Record)
      return Database_Connection is abstract;
   --  Returns a new object to represent connection to the database.
   --  On return, no connection to the DBMS has been made (this will
   --  be done lazily by the turned object).
   --  If instead you want to reuse an existing connection, you should use
   --  Reset_Connection below.

   procedure Free (Description : in out Database_Description_Record) is null;
   procedure Free (Description : in out Database_Description);
   --  Free memory associated with description.
   --  This should only be called when the last database connection was closed,
   --  since each connection keeps a handle on the description

   --------------------
   -- Error_Reporter --
   --------------------

   procedure Free (Self : in out Error_Reporter) is null;
   --  Free the memory used by Self

   procedure On_Database_Corrupted
     (Self       : in out Error_Reporter;
      Connection : access Database_Connection_Record'Class) is null;
   --  Called when the database is corrupted.
   --  A call to On_Error will also occur.

   procedure On_Warning
     (Self       : in out Error_Reporter;
      Connection : access Database_Connection_Record'Class;
      Message    : String) is null;
   --  Called when a warning is emitted by the database.

   procedure On_Error
     (Self       : in out Error_Reporter;
      Connection : access Database_Connection_Record'Class;
      Message    : String) is null;
   --  Called when an error is emitted by the database.

   -------------------------
   -- Database_Connection --
   -------------------------

   function Check_Connection
     (Connection : access Database_Connection_Record) return Boolean;
   --  Attempt to connect to the database, and return True if the connection
   --  was successful. Calling this subprogram is optional, since it will be
   --  done automatically when calling Execute (see below). This can however be
   --  used to ensure that the database works properly.

   procedure Fetch
     (Result     : out Forward_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : String;
      Params     : SQL_Parameters := No_Parameters);
   procedure Fetch
     (Result     : out Forward_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : GNATCOLL.SQL.SQL_Query;
      Params     : SQL_Parameters := No_Parameters);
   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      Query      : GNATCOLL.SQL.SQL_Query;
      Params     : SQL_Parameters := No_Parameters);
   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      Query      : String;
      Params     : SQL_Parameters := No_Parameters);
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

   function Insert_And_Get_PK
     (Connection : access Database_Connection_Record'Class;
      Query      : GNATCOLL.SQL.SQL_Query;
      Params     : SQL_Parameters := No_Parameters;
      PK         : SQL_Field_Integer) return Integer;
   function Insert_And_Get_PK
     (Connection : access Database_Connection_Record;
      Query      : String;
      Params     : SQL_Parameters := No_Parameters;
      PK         : SQL_Field_Integer) return Integer;
   --  Execute the INSERT statement, and retrieve the primary key of the
   --  newly inserted row. This is similar, but more efficient, to calling
   --      Fetch (Result, Connection, Query, Params);
   --      return Last_Id (Result, Connection, Field);
   --  The primary key must be an integer field.
   --  The function also exists for prepared queries.

   procedure Close
     (Connection : access Database_Connection_Record) is abstract;
   procedure Free (Connection : in out Database_Connection);
   --  Close the connection to the database, if needed.
   --  Only Free needs to be called, and it will automatically call Close.

   procedure Mark_As_Closed
      (Connection : access Database_Connection_Record'Class;
       Closed     : Boolean);
   function Was_Closed
     (Connection : access Database_Connection_Record'Class) return Boolean;
   --  This is for internal use only, marks the connection as closed.
   --  No further operation should be performed on it, in particular
   --  finalization of prepared statements.
   --  It is valid to pass a freed pointer to Was_Closed

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

   procedure Automatic_Transactions
     (Connection : access Database_Connection_Record'Class;
      Active     : Boolean := True);
   function Automatic_Transactions
     (Connection : access Database_Connection_Record'Class) return Boolean;
   --  Activate (which is the default) or deactivate automatic SQL
   --  transactions. When enabled, the first SQL statement that potentially
   --  modifies the database (basically other than a SELECT) will start a
   --  transaction first (with BEGIN). It is however, your responsibility to
   --  finally do a Commit or Rollback.
   --  When disabled, transactions will never be started automatically (but
   --  you can use Start_Transaction to start one).
   --  It is recommended to change this setting when you just retrieved a new
   --  connection, not while executing SQL statements.

   function Start_Transaction
     (Connection : access Database_Connection_Record'Class)
      return Boolean;
   --  Start a new transaction, if not already in one. This does not need to be
   --  called in general, since transactions are automatically started when you
   --  modify the contents of the database, but you might need to start one
   --  manually in some cases (declaring a cursor with "DECLARE .. CURSOR" for
   --  instance).
   --  Return True if a transaction was started, False if one was already in
   --  progress.

   function In_Transaction
     (Connection : access Database_Connection_Record'Class) return Boolean;
   --  Return True if a transaction is taking place (ie at least one
   --  modification to the database took place, and was not COMMIT'd or
   --  ROLLBACK'd.

   procedure Commit_Or_Rollback
     (Connection : access Database_Connection_Record'Class);
   procedure Commit
     (Connection : access Database_Connection_Record'Class)
      renames Commit_Or_Rollback;
   --  Commit or rollback the current transaction, depending on whether we had
   --  an error. This does not affect the result of Success (unless COMMIT
   --  itself fails), so that you can still know afterward whether the
   --  transaction was committed or not.

   procedure Force_Connect
     (Connection : access Database_Connection_Record) is abstract;
   --  Force a connection to the DBMS. Normally, this connection is done
   --  automatically the first time an SQL command is executed, but it might
   --  be needed sometimes to force a connection earlier.

   procedure Force_Disconnect
     (Connection : access Database_Connection_Record) is abstract;
   --  Force an immediate disconnection of the connection to the DBMS. This
   --  does not perform any cleanup action, and is intended only for fault
   --  injection during application testing.

   function Connected_On
     (Connection : access Database_Connection_Record)
      return Ada.Calendar.Time is abstract;
   --  Timestamp for the connection to the server. This is used to detect
   --  when a connection has been reconnected (for instance because it was lost
   --  at some point).

   procedure Invalidate_Cache;
   --  Invalid all caches associated with the database (for all connections).
   --  Some queries can be cached (see Execute below) for more efficiency.

   procedure Reset_Connection
     (Connection  : access Database_Connection_Record'Class;
      Username    : String := "");
   --  Reset the contents of Connection.
   --  This terminates any on-going transaction and resets various internal
   --  fields.
   --  In general, it is better to use Tasking.Get_Task_Connection which does
   --  the necessary things, but when not in a multi-tasking application it is
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

   function Can_Alter_Table_Constraints
     (Self : access Database_Connection_Record)
      return Boolean is abstract;
   --  Whether it is possible to add constraints to an existing table.
   --  This is intended for use when creating tables (in GNATCOLL.SQL.Inspect)

   function Has_Pragmas
     (Self : access Database_Connection_Record) return Boolean is abstract;
   --  Whether the database knows about the "PRAGMA" command.

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

   function Current (Self : Forward_Cursor) return Positive;
   --  Index of the current row. The first row is always numbered 1

   function Value (Self : Forward_Cursor; Field : Field_Index) return String;
   function Boolean_Value
     (Self : Forward_Cursor; Field : Field_Index) return Boolean;

   function Integer_Value
     (Self    : Forward_Cursor;
      Field   : Field_Index;
      Default : Integer) return Integer;
   function Integer_Value
     (Self    : Forward_Cursor;
      Field   : Field_Index) return Integer;
   --  Reads a value as an integer. The second version might raise a
   --  Constraint_Error if the field is null or does not contain an integer.
   --  The first version will return the default instead.

   function Float_Value
     (Self    : Forward_Cursor;
      Field   : Field_Index;
      Default : Float) return Float;
   function Float_Value
     (Self : Forward_Cursor; Field : Field_Index) return Float;
   --  Reads a value as a float. The second version might raise a
   --  Constraint_Error if the field is null or does not contain a float.
   --  The first version will return the default instead.

   function Money_Value
     (Self : Forward_Cursor; Field : Field_Index)
     return T_Money;
   function Time_Value
     (Self  : Forward_Cursor; Field : Field_Index) return Ada.Calendar.Time;
   function Json_Text_Value
     (Self  : Forward_Cursor; Field : Field_Index) return String;
   function XML_Text_Value
     (Self  : Forward_Cursor; Field : Field_Index) return String;
   --  Return a specific cell, converted to the appropriate format

   function Is_Null
     (Self  : Forward_Cursor; Field : Field_Index) return Boolean;
   --  True if the corresponding cell is not set

   function Last_Id
     (Self       : Forward_Cursor;
      Connection : access Database_Connection_Record'Class;
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

   overriding procedure Fetch
     (Result     : out Direct_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : String;
      Params     : SQL_Parameters := No_Parameters);
   overriding procedure Fetch
     (Result     : out Direct_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : GNATCOLL.SQL.SQL_Query;
      Params     : SQL_Parameters := No_Parameters);
   --  Execute the query, and get all results in memory.

   -------------------------
   -- Prepared statements --
   -------------------------
   --  Prepared statements are a way to optimize your application and its
   --  queries. There are several levels of preparation:
   --
   --    * Create parts of queries in advance, for instance a SQL_Field_List.
   --      This does not save a lot of CPU time, but saves a few system calls
   --      to malloc. This does not need any of the following subprograms.
   --
   --    * Precompute (and auto-complete) sql queries generated from
   --      GNATCOLL.SQL. That API is rather heavy, and computing
   --      auto-completion might be time consuming. This preparation is only
   --      client side and does not involve the DBMS.
   --
   --    * DBMS systems all have a way to prepare statements (on the server
   --      this time). This involves optimizing the query and how it should be
   --      executed. Such prepared statements, however, are only valid while
   --      the connection to the database lasts (or until you explicitly close
   --      the prepared statement.
   --
   --    * GNATCOLL.SQL.Exec is also able to cache (on the client) the result
   --      of some queries. This way, you avoid communication with the DBMS
   --      altogether, which provides significant speed up for often executed
   --      queries (like tables of valid values for fields, aka enumerations).
   --
   --  When combined, both of these will significantly speed up execution of
   --  queries. However, there is often little point in running exactly the
   --  same query several times. For this reason, queries can be parameterized,
   --  where the parameters can be changed before each execution. Most DBMS
   --  support this efficiently
   --
   --  Preparing statements in memory
   ----------------------------------
   --  The first time the resulting statement is executed, the internal tree
   --  structure will be converted to a string, and kept as is afterward.
   --  The tree structure is then freed.
   --  This saves memory, and is more efficient since you are saving a lot in
   --  terms of malloc and functions returning strings.
   --  The memory is automatically freed when the statement goes out of scope.
   --
   --  It is better to use such a Cached_Statement rather than simply storing
   --  the conversion to a string yourself:
   --      QS : constant String := To_String (DB, QS);
   --  The conversion to string depends on the specific database backend you
   --  are using (for instance, sqlite does not encode booleans the same way
   --  that postgreSQL does).
   --  Thus the conversion to string needs to be done only when you have an
   --  actual connection, and thus cannot be done at the library level.
   --
   --  Caching statement results
   -----------------------------
   --  If Use_Cache is True, and you are executing a SELECT query, the result
   --  of a previous execution of that query will be reused rather than
   --  executed again. If it was never executed, it will be cached for later
   --  use (no caching takes place if Use_Cache is False). This should mostly
   --  be used for queries to tables that almost never change, ie that store
   --  "enumeration types". The cache must be specifically invalidated (see
   --  Invalidate_Cache) to reset it, although it will also expire
   --  automatically and be refreshed after a while.
   --
   --  Preparing statements on the server
   --------------------------------------
   --  If On_Server is true, then a connection-specific preparation is also
   --  done on the server, for further optimization. Otherwise, the
   --  result of this call is to generate the string representation (and auto
   --  completion) of the query only once, and reuse that later on (that still
   --  provides a significant speed up). This also provides a way to cache the
   --  result of the query locally on the client.
   --  In general, On_Server should only be set if the query contains
   --  parameters (since otherwise it is too specialized to be worth keeping
   --  in memory).
   --
   --  There is little gain in having both Use_Cache and On_Server be true: the
   --  query is executed only once (until the cache expires) on the server
   --  anyway.
   --
   --  Name is used in the logs (and sometimes in the DBMS) to uniquely show
   --  the statement. If unspecified, an automatic name is computed.
   --
   --  Global prepared statements
   -------------------------------
   --
   --  The idea is that Prepared_Statement could be global variables prepared
   --  during the elaboration. Internally, they are accessed from within a
   --  protected record, so it is safe to have them as global variables even in
   --  a multi-threaded application. It is however possible to only use these
   --  as local variables, if a little inefficient since the conversion from
   --  SQL structures to a string has to be done each time. On the other hand,
   --  it saves memory since you don't need to keep the prepared statements for
   --  ever in memory.

   type Prepared_Statement is tagged private;
   No_Prepared : constant Prepared_Statement;
   --  A precomputed SQL statement, on the client side.
   --  This type is reference counted and will automatically free memory or
   --  release DBMS resources when it goes out of scope.

   function Prepare
     (Query         : SQL_Query;
      Auto_Complete : Boolean := False;
      Use_Cache     : Boolean := False;
      On_Server     : Boolean := False;
      Name          : String := "") return Prepared_Statement;
   function Prepare
     (Query         : String;
      Use_Cache     : Boolean := False;
      On_Server     : Boolean := False;
      Name          : String := "") return Prepared_Statement;
   --  Prepare the statement for multiple executions.
   --  If Auto_Complete is true, the query is first auto-completed.

   procedure Fetch
     (Result     : out Direct_Cursor;
      Connection : access Database_Connection_Record'Class;
      Stmt       : Prepared_Statement'Class;
      Params     : SQL_Parameters := No_Parameters);
   procedure Fetch
     (Result     : out Forward_Cursor;
      Connection : access Database_Connection_Record'Class;
      Stmt       : Prepared_Statement'Class;
      Params     : SQL_Parameters := No_Parameters);
   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      Stmt       : Prepared_Statement'Class;
      Params     : SQL_Parameters := No_Parameters);
   --  Execute a prepared statement on the connection.

   function Insert_And_Get_PK
     (Connection : access Database_Connection_Record;
      Stmt       : Prepared_Statement'Class;
      Params     : SQL_Parameters := No_Parameters;
      PK         : SQL_Field_Integer) return Integer;
   --  Execute a prepared insert statement, and return the Id of the newly
   --  inserted row. See documentation for Insert_And_Get_PK for non-prepared
   --  statements.
   --  Stmt must be used at least once through this function before you use
   --  Execute or Fetch on it, otherwise it might be incorrectly prepared
   --  (missing returned value) and you would not get the id of the row
   --  as expected.

   --------------------------------------------
   -- Getting info about the database schema --
   --------------------------------------------
   --  The following subprograms will provide a view of the database schema (ie
   --  the set of tables and their fields, and the relationships between the
   --  tables).

   type Relation_Kind is (Kind_Table, Kind_View);

   procedure Foreach_Table
     (Connection : access Database_Connection_Record;
      Callback   : access procedure
        (Name, Description : String; Kind : Relation_Kind)) is abstract;
   --  Find all tables in the database.
   --  For each, call Callback. Description is the comment that was optionally
   --  stored in the database to describe the role of the table (generally
   --  through a COMMENT command, which depends on the type of database you are
   --  using).

   procedure Foreach_Field
     (Connection : access Database_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Name           : String;
         Typ            : String;
         Index          : Natural;
         Description    : String;
         Default_Value  : String;
         Is_Primary_Key : Boolean;
         Not_Null       : Boolean)) is abstract;
   --  For each attribute of the table, call Callback. Index is the attribute
   --  index in the table (column number). Description is the comment that was
   --  set when the attribute was created (for DBMS systems that support it),
   --  and can be the empty string.
   --  Default_Value is the default value for the attribute (the empty string
   --  is used if there is no default)
   --  Is_Primary_Key is set to True if the field is part of the primary key
   --  for this table.
   --  Not_Null is set to true if the attribute cannot be null

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
   --  This subprograms are for internal implementation only

   procedure Print_Warning
     (Connection : access Database_Connection_Record'Class; Str : String);
   procedure Print_Error
     (Connection : access Database_Connection_Record'Class; Str : String);
   procedure Report_Database_Corrupted
     (Connection : access Database_Connection_Record'Class);
   --  Print a warning or message to the appropriate GNATCOLL.Traces stream.

   -------------------------
   -- Private subprograms --
   -------------------------
   --  These subprograms are meant to be overridden by specific implementations
   --  for each DBMS. You should not use them directly in your applications,
   --  since the subprograms above wrap them better.

   type DBMS_Stmt is new System.Address;
   No_DBMS_Stmt : constant DBMS_Stmt;
   --  A statement prepared on the server. This is only valid for a specific
   --  connection.

   function Connect_And_Execute
     (Connection  : access Database_Connection_Record;
      Is_Select   : Boolean;
      Direct      : Boolean;
      Query       : String         := "";
      Stmt        : DBMS_Stmt      := No_DBMS_Stmt;
      Params      : SQL_Parameters := No_Parameters)
      return Abstract_Cursor_Access is abstract;
   --  This is mostly an internal subprogram, overridden by all DBMS-specific
   --  backends.
   --  If the connection to the database has not been made yet, connect to it.
   --  Then perform the query or prepared statement, reconnecting once if the
   --  connection failed. (If Stmt is set, Query is ignored).
   --  Will return null if the connection to the database is bad.
   --  If the query is the empty string, this procedure only connects to
   --  the database and checks the connection. It returns null if the
   --  connection is no longer valid.
   --  If Direct is true, a direct_cursor is created, otherwise a
   --  Forward_Cursor. The connection is allowed to return a direct cursor even
   --  if the user only wanted a forward_cursor, but the opposite is not
   --  allowed.

   function Connect_And_Prepare
     (Connection : access Database_Connection_Record;
      Query      : String;
      Name       : String;
      Direct     : Boolean)
      return DBMS_Stmt;
   --  Prepare a statement on the server, and return a handle to it. This is
   --  only valid for the specific Connection. This function can return null
   --  if prepared statements are not supported on that DBMS.
   --  Connection to the database is first done if needed

   function Execute
     (Connection  : access Database_Connection_Record;
      Prepared    : DBMS_Stmt;
      Is_Select   : Boolean;
      Direct      : Boolean;
      Params      : SQL_Parameters := No_Parameters)
      return Abstract_Cursor_Access;
   --  Execute a prepared statement on the server

   procedure Finalize
     (Connection : access Database_Connection_Record;
      Prepared   : DBMS_Stmt) is null;
   --  Free memory used by Prepared on the server

   procedure Reset
     (Connection : access Database_Connection_Record;
      Prepared   : DBMS_Stmt) is null;
   --  Reset the prepared statement so that the next call to Element returns
   --  the first row

   procedure Post_Execute_And_Log
     (R          : access Abstract_DBMS_Forward_Cursor'Class;
      Connection : access Database_Connection_Record'Class;
      Query      : String;
      Prepared   : Prepared_Statement'Class := No_Prepared;
      Is_Select  : Boolean;
      Params     : SQL_Parameters := No_Parameters);
   --  Mark the connection as success or failure depending on R.
   --  Logs the query

   procedure Set_SQL_Suffix
      (Prepared : Prepared_Statement'Class;
       Suffix   : String);
   --  SQL command added to Prepared's own SQL (for instance, " RETURNING..."
   --  in postgreSQL). This has no effect if the statement has already been
   --  prepared on the server.

   function Has_SQL_Suffix
      (Prepared : Prepared_Statement'Class) return Boolean;
   --  True if Prepared is either already prepared on the server, or already
   --  has a suffix defined.

   function Is_Prepared_On_Server_Supported
     (Connection : access Database_Connection_Record) return Boolean;
   --  True if Prepared supported on the server for this connection

private

   type Error_Reporter is abstract tagged null record;

   type Database_Description_Record
     (Caching : Boolean;
      Errors  : access Error_Reporter'Class) is abstract tagged null record;

   type Database_Connection_Record
     (Descr : access Database_Description_Record'Class;
      Always_Use_Transactions : Boolean)
     is abstract new Formatter with record
      Success        : Boolean := True;
      In_Transaction : Boolean := False;
      Username       : GNAT.Strings.String_Access;
      Error_Msg      : GNAT.Strings.String_Access;
      Automatic_Transactions : Boolean := True;
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

   Null_Parameter : constant SQL_Parameter :=
     (Typ => Parameter_Integer, Int_Val => Integer'First);
   No_Parameters : constant SQL_Parameters (1 .. 0) :=
     (others => Null_Parameter);

   -------------------------
   -- Prepared statements --
   -------------------------

   No_DBMS_Stmt : constant DBMS_Stmt := DBMS_Stmt (System.Null_Address);
   --  A statement prepared on the server. This is only valid for a specific
   --  connection.

   type Prepared_In_Session;
   type Prepared_In_Session_List is access all Prepared_In_Session;
   type Prepared_In_Session is record
      Stmt         : DBMS_Stmt := No_DBMS_Stmt;
      DB           : Database_Connection;  --  The connection used to prepare
      DB_Timestamp : Ada.Calendar.Time;
      --  The DB.Connected_On when the statement was prepared. Used to detect
      --  whether we need to re-prepare it.

      Next         : Prepared_In_Session_List;
   end record;

   type Cache_Id is new Natural;
   No_Cache_Id : constant Cache_Id := Cache_Id'Last;

   type Prepared_Statement_Data is new GNATCOLL.Refcount.Refcounted with record
      Query      : SQL_Query;   --  Reset to null once prepared
      Query_Str  : GNAT.Strings.String_Access;

      Suffix_Str : GNAT.Strings.String_Access;
      --  An extra (DBMS dependent) suffix to add to the SQL. Changing this
      --  invalidates the prepared statement.

      Is_Select : Boolean;

      Use_Cache     : Boolean := False;
      Cached_Result : Cache_Id := No_Cache_Id;

      On_Server : Boolean := False;
      Name      : GNAT.Strings.String_Access;
      Prepared  : Prepared_In_Session_List;
   end record;
   --  This type stores a statement as a string, to save time and memory.
   --  It is reference counted, so that it is automatically released when no
   --  longer needed.

   overriding procedure Free (Self : in out Prepared_Statement_Data);

   package Prepared_Statements is new GNATCOLL.Refcount.Smart_Pointers
     (Prepared_Statement_Data);
   type Prepared_Statement is new Prepared_Statements.Ref with null record;

   No_Prepared : constant Prepared_Statement :=
     (Prepared_Statements.Null_Ref with null record);

end GNATCOLL.SQL.Exec;
