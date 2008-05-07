-----------------------------------------------------------------------
--                               G N A T C O L L                     --
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

--  This package provides subprograms to interact with a database. It provides
--  a DBMS-agnostic API, which is further specialized in children packages for
--  each support DBMS system.
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
with System;

package GNATCOLL.SQL.Exec is

   Perform_Queries : Boolean := True;
   --  If False, no operation is performed on the database, but the queries
   --  are logged as if the operation took place. This is only intended for
   --  automatic testsuites.

   DBMS_Postgresql : constant String := "postgresql";
   --  The various database backends that are supported.
   --  GNATCOLL does not use these constants itself, but they are provided to
   --  serve as a common vocabulary for applications to describe the backend
   --  they want to use. These contents should be passed to Setup_Database (see
   --  below), and then on to the factory for Get_Task_Connection.
   --  You can create your own backends if needed, and thus create your own
   --  string constants for your application, if you want.

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

   type Query_Result is tagged private;
   --  The result of a query, as returned by a DBMS. This type automatically
   --  takes care of memory management, and frees its memory when no longer in
   --  use.

   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      R          : out Query_Result;
      Query      : String;
      Use_Cache  : Boolean := False);
   --  Submit a query to the database, log it and wait for the result.
   --  Logs the query, as needed.
   --  If Use_Cache is True, and the same query was executed before, its
   --  result will be reused rather than executed again. If it was never
   --  executed, it will be cached for later use (no caching takes place if
   --  Use_Cache is False). This should mostly be used for queries to table
   --  that almost never changes, ie that store "enumeration types". The cache
   --  must be specifically invalidated (see Invalidate_Cache) to reset it.

   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      R          : out Query_Result;
      Query      : GNATCOLL.SQL.SQL_Query;
      Use_Cache  : Boolean := False);
   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      Query      : GNATCOLL.SQL.SQL_Query;
      Use_Cache  : Boolean := False);
   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      Query      : String;
      Use_Cache  : Boolean := False);
   --  Same as above

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

   function Get_Host     (Description : Database_Description) return String;
   function Get_User     (Description : Database_Description) return String;
   function Get_Database (Description : Database_Description) return String;
   function Get_Password (Description : Database_Description) return String;
   function Get_DBMS     (Description : Database_Description) return String;
   --  Return the connection components for the database

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

   ------------------------
   -- Retrieving results --
   ------------------------
   --  The following subprograms represent a way to access the various rows and
   --  columns returned by a query. They do not provide the same generality
   --  that DBMS-specific functions would, but represent with the most frequent
   --  use done with a result.

   type Tuple_Index is new Natural;
   type Field_Index is new Natural;

   function Tuple_Count (Res : Query_Result) return Tuple_Index;
   --  The number of rows in the result, or that were impacted by a database
   --  modification

   function Value
     (Res   : Query_Result;
      Tuple : Tuple_Index;
      Field : Field_Index) return String;
   function Address_Value
     (Res   : Query_Result;
      Tuple : Tuple_Index;
      Field : Field_Index) return System.Address;  --  A C string
   function Boolean_Value
     (Res   : Query_Result;
      Tuple : Tuple_Index;
      Field : Field_Index) return Boolean;
   function Integer_Value
     (Res   : Query_Result;
      Tuple : Tuple_Index;
      Field : Field_Index;
      Default : Integer := Integer'First) return Integer;
   function Float_Value
     (Res   : Query_Result;
      Tuple : Tuple_Index;
      Field : Field_Index) return Float;
   function Time_Value
     (Res   : Query_Result;
      Tuple : Tuple_Index;
      Field : Field_Index) return Ada.Calendar.Time;
   --  Return a specific cell, converted to the appropriate format

   function Is_Null
     (Res   : Query_Result;
      Tuple : Tuple_Index;
      Field : Field_Index) return Boolean;
   --  True if the corresponding cell is not set

   function Last_Id
     (Connection : access Database_Connection_Record'Class;
      Res        : Query_Result;
      Field      : SQL_Field_Integer) return Integer;
   --  Return the value set for field in the last INSERT command on that
   --  connection.
   --  Field must be an automatically incremented field (or a sql sequence).
   --  Returns -1 if the id could not be queried (perhaps the previous insert
   --  failed or was never committed). When the last_id could not be retrieved,
   --  the connection is set to the failure state
   --  Depending on the backend, this id might be computed through a sql query,
   --  so it is better to cache it if you need to reuse it several times.

   function Field_Count (Res : Query_Result) return Field_Index;
   --  The number of fields per row in Res

   function Field_Name (Res : Query_Result; Field : Field_Index) return String;
   --  The name of a specific field in a row of Res

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

   -----------------------------------
   -- Specializing for various DBMS --
   -----------------------------------
   --  The following types and subprograms need to be overridden to specialize
   --  this API for various DBMS

   type Query_Result_Content is abstract tagged private;
   type Query_Result_Content_Access is access all Query_Result_Content'Class;
   --  Internal contents of a query_result.
   --  Instead of overriding Query_Result directly, the support packages for
   --  the DBMS must override this type, so that Query_Result is not visibly
   --  tagged and users do not have to use unconstrained types in their code,
   --  thus allowing "Result : Query_Result" declarations.

   procedure Connect_And_Execute
     (Connection  : access Database_Connection_Record;
      Query       : String;
      R           : out Query_Result_Content_Access;
      Is_Select   : Boolean) is abstract;
   --  If the connection to the database has not been made yet, connect to it.
   --  Then perform the query, reconnecting once if the connection failed.
   --  R should be left to null if the connection to the database is bad.
   --  If the query is the empty string, this procedure should only connect to
   --  the database and check the connection.

   function Error
     (Connection : access Database_Connection_Record)
      return String is abstract;
   --  Return the last error message set by the database

   function Is_Success
     (Result : Query_Result_Content) return Boolean is abstract;
   --  Whether the corresponding query succeeded

   function Error_Msg
     (Result : Query_Result_Content) return String is abstract;
   --  Return the error message associated with the query

   function Status
     (Result : Query_Result_Content) return String is abstract;
   --  Return a string describing the status of the query. This is used for
   --  logging purposes.

   procedure Finalize (Result : in out Query_Result_Content) is abstract;
   --  Free the memory used by Result

   function Tuple_Count
     (Res : Query_Result_Content) return Tuple_Index is abstract;
   --  Return the number of rows impacted (ie modified or returned) by the
   --  query. Is_Select indicates whether the query was a "SELECT" (ie rows are
   --  returned) or other (ie rows are modified)

   function Value
     (Res   : Query_Result_Content;
      Tuple : Tuple_Index;
      Field : Field_Index) return String is abstract;
   function Address_Value
     (Res   : Query_Result_Content;
      Tuple : Tuple_Index;
      Field : Field_Index) return System.Address is abstract;
   function Boolean_Value
     (Res   : Query_Result_Content;
      Tuple : Tuple_Index;
      Field : Field_Index) return Boolean;
   function Integer_Value
     (Res   : Query_Result_Content;
      Tuple : Tuple_Index;
      Field : Field_Index) return Integer;
   function Float_Value
     (Res   : Query_Result_Content;
      Tuple : Tuple_Index;
      Field : Field_Index) return Float;
   function Time_Value
     (Res   : Query_Result_Content;
      Tuple : Tuple_Index;
      Field : Field_Index) return Ada.Calendar.Time;
   function Is_Null
     (Res   : Query_Result_Content;
      Tuple : Tuple_Index;
      Field : Field_Index) return Boolean is abstract;
   function Last_Id
     (Connection : access Database_Connection_Record'Class;
      Res        : Query_Result_Content;
      Field      : SQL_Field_Integer) return Integer is abstract;
   function Field_Count
     (Res : Query_Result_Content) return Field_Index is abstract;
   function Field_Name
     (Res : Query_Result_Content; Field : Field_Index) return String
     is abstract;
   --  See matching subprograms for Query_Result. The default implementation of
   --  the subprograms converts from a string to the appropriate type.
   --  Constraint_Error is raised if the field does not contain an appropriate
   --  value.

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

   type Query_Result_Content is abstract tagged record
      Refcount : Natural := 1;
   end record;

   type Query_Result is new Ada.Finalization.Controlled with record
      Res : Query_Result_Content_Access;
   end record;
   overriding procedure Adjust   (Self : in out Query_Result);
   overriding procedure Finalize (Self : in out Query_Result);

end GNATCOLL.SQL.Exec;
