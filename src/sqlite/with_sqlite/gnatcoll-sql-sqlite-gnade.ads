------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
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

--  This file provides a very low-level interface to sqlite.
--  It is not intented for end users

with Interfaces.C.Strings;
with System;

package GNATCOLL.SQL.Sqlite.Gnade is

   type Database is private;
   No_Database : constant Database;
   --  A connection to a sqlite database

   function Image (DB : Database) return String;
   --  Display the details on the connection (logging only)

   type Open_Flags is mod 2**32;
   Open_Readonly  : constant Open_Flags := 16#00001#;
   Open_Readwrite : constant Open_Flags := 16#00002#;
   Open_Create    : constant Open_Flags := 16#00004#;
   Open_Nomutex   : constant Open_Flags := 16#08000#;
   Open_Fullmutex : constant Open_Flags := 16#10000#;
   --  How a database should be open. Read and Readwrite require that the
   --  database already exist, but when combined with Create the database will
   --  be created if it doesn't already exist.
   --  If the OPEN_NOMUTEX flag is set, then the database connection opens in
   --  the multi-thread threading mode as long as the single-thread mode has
   --  not been set at compile-time or start-time. If the OPEN_FULLMUTEX flag
   --  is set then the database connection opens in the serialized threading
   --  mode unless single-thread was previously selected at compile-time or
   --  start-time.

   Open_In_Memory   : constant String := ":memory:";
   Open_Tmp_On_Disk : constant String := "";
   --  Special database names that can be used with Open

   type Result_Codes is
     (Sqlite_OK,
      Sqlite_Error,       --   SQL error or missing database
      Sqlite_Internal,    -- Internal logic error in SQLite
      Sqlite_Perm,        -- Access permission denied
      Sqlite_Abort,       --  Callback routine requested an abort
      Sqlite_Busy,        --  The database file is locked
      Sqlite_Locked,      --  A table in the database is locked
      Sqlite_Nomem,       --  A malloc() failed
      Sqlite_Readonly,    --  Attempt to write a readonly database
      Sqlite_Interrupt,   --  Operation terminated by sqlite3_interrupt()
      Sqlite_Ioerr,       --  Some kind of disk I/O error occurred
      Sqlite_Corrupt,     --  The database disk image is malformed
      Sqlite_Notfound,    --  NOT USED. Table or record not found
      Sqlite_Full,        --  Insertion failed because database is full
      Sqlite_Cantopen,    --  Unable to open the database file
      Sqlite_Protocol,    --  NOT USED. Database lock protocol error
      Sqlite_Empty,       --  Database is empty
      Sqlite_Schema,      --  The database schema changed
      Sqlite_Toobig,      --  String or BLOB exceeds size limit
      Sqlite_Constraint,  --  Abort due to constraint violation
      Sqlite_Mismatch,    --  Data type mismatch
      Sqlite_Misuse,      --  Library used incorrectly
      Sqlite_Nolfs,       --  Uses OS features not supported on host
      Sqlite_Auth,        --  Authorization denied
      Sqlite_Format,      --  Auxiliary database format error
      Sqlite_Range,       --  2nd parameter to sqlite3_bind out of range
      Sqlite_Notadb,      --  File opened that is not a database file
      Sqlite_Row,         --  sqlite3_step() has another row ready
      Sqlite_Done,        --  sqlite3_step() has finished executing
      Sqlite_Locked_Sharedcache,
      Sqlite_Ioerr_Read,
      Sqlite_Ioerr_Short_Read,
      Sqlite_Ioerr_Write,
      Sqlite_Ioerr_Fsync,
      Sqlite_Ioerr_Dir_Fsync,
      Sqlite_Ioerr_Truncate,
      Sqlite_Ioerr_Fstat,
      Sqlite_Ioerr_Unlock,
      Sqlite_Ioerr_Rdlock,
      Sqlite_Ioerr_Delete,
      Sqlite_Ioerr_Blocked,
      Sqlite_Ioerr_Nomem,
      Sqlite_Ioerr_Access,
      Sqlite_Ioerr_Checkreservedblock,
      Sqlite_Ioerr_Lock,
      Sqlite_Ioerr_Close,
      Sqlite_Ioerr_Dir_Close);

   ---------------------------------------
   --  Opening and closing a connection --
   ---------------------------------------

   procedure Open
     (DB       : out Database;
      Filename : String := Open_In_Memory;
      Flags    : Open_Flags := Open_Readwrite or Open_Create;
      Status   : out Result_Codes);
   --  Open the Filename database. Filename is interpreted as UTF-8.
   --  If If the filename is ":memory:", then a private, temporary in-memory
   --  database is created for the connection. This in-memory database will
   --  vanish when the database connection is closed. Future versions of SQLite
   --  might make use of additional special filenames that begin with the ":"
   --  character. It is recommended that when a database filename actually does
   --  begin with a ":" character you should prefix the filename with a
   --  pathname such as "./" to avoid ambiguity.
   --  If the filename is an empty string, then a private, temporary on-disk
   --  database will be created. This private database will be automatically
   --  deleted as soon as the database connection is closed.
   --
   --  Status is different from Sqlite_OK in case of error (See Error_Msg then)

   function Error_Msg (DB : Database) return String;
   --  Return the error message for the most recent query on DB. This is not
   --  thread safe, and might return the error message from another thread.

   procedure Close (DB : Database);
   --  Close the connection to the database.
   --  This finalizes all prepared statements, as needed

   type Busy_Handler_Callback is access function
     (Data : System.Address; Count : Integer) return Integer;
   pragma Convention (C, Busy_Handler_Callback);

   procedure Busy_Handler
     (DB   : Database;
      Cb   : Busy_Handler_Callback;
      Data : System.Address := System.Null_Address);
   pragma Import (C, Busy_Handler, "sqlite3_busy_handler");
   --  This routine sets a callback function that might be invoked whenever an
   --  attempt is made to open a database table that another thread or process
   --  has locked.
   --
   --  If the busy callback is NULL, then SQLITE_BUSY or SQLITE_IOERR_BLOCKED
   --  is returned immediately upon encountering the lock. If the busy callback
   --  is not NULL, then the callback will be invoked with two arguments.
   --
   --  The first argument to the handler is a copy of Data pointer which
   --  is the third argument to Busy_Handler. The second argument to
   --  the handler callback is the number of times that the busy handler has
   --  been invoked for this locking event. If the busy callback returns 0,
   --  then no additional attempts are made to access the database and
   --  SQLITE_BUSY or SQLITE_IOERR_BLOCKED is returned. If the callback returns
   --  non-zero, then another attempt is made to open the database for reading
   --  and the cycle repeats.

   type Logger is access procedure
     (Data       : System.Address;
      Error_Code : Result_Codes;
      Message    : Interfaces.C.Strings.chars_ptr);
   pragma Convention (C, Logger);

   procedure Set_Config_Log
     (Func : Logger;
      Data : System.Address := System.Null_Address);
   --  Set the function used by sqlite3_log to log error messages.
   --  Data will be passed as is to the function (its first parameter

   procedure Set_Config_Memstatus (Collect_Stats : Boolean);
   --  This option takes single argument of type int, interpreted as a boolean,
   --  which enables or disables the collection of memory allocation
   --  statistics. When memory allocation statistics are disabled, the
   --  following SQLite interfaces become non-operational:
   --    * sqlite3_memory_used()
   --    * sqlite3_memory_highwater()
   --    * sqlite3_soft_heap_limit64()
   --    * sqlite3_status()
   --  Memory allocation statistics are enabled by default unless SQLite is
   --  compiled with SQLITE_DEFAULT_MEMSTATUS=0 in which case memory allocation
   --  statistics are disabled by default.

   procedure Set_Config_Single_Thread;
   --  This option sets the threading mode to Single-thread. In other words, it
   --  disables all mutexing and puts SQLite into a mode where it can only be
   --  used by a single thread. If SQLite is compiled with the
   --  SQLITE_THREADSAFE=0 compile-time option then it is not possible to
   --  change the threading mode from its default value of Single-thread and so
   --  sqlite3_config() will return SQLITE_ERROR if called with the
   --  SQLITE_CONFIG_SINGLETHREAD configuration option.

   procedure Set_Config_Multi_Thread;
   --  This option sets the threading mode to Multi-thread. In other words, it
   --  disables mutexing on database connection and prepared statement
   --  objects. The application is responsible for serializing access to
   --  database connections and prepared statements. But other mutexes are
   --  enabled so that SQLite will be safe to use in a multi-threaded
   --  environment as long as no two threads attempt to use the same database
   --  connection at the same time. If SQLite is compiled with the
   --  SQLITE_THREADSAFE=0 compile-time option then it is not possible to set
   --  the Multi-thread threading mode and sqlite3_config() will return
   --  SQLITE_ERROR if called with the SQLITE_CONFIG_MULTITHREAD configuration
   --  option.

   procedure Set_Config_Serialized;
   --  This option sets the threading mode to Serialized. In other words, this
   --  option enables all mutexes including the recursive mutexes on database
   --  connection and prepared statement objects. In this mode (which is the
   --  default when SQLite is compiled with SQLITE_THREADSAFE=1) the SQLite
   --  library will itself serialize access to database connections and
   --  prepared statements so that the application is free to use the same
   --  database connection or the same prepared statement in different threads
   --  at the same time. If SQLite is compiled with the SQLITE_THREADSAFE=0
   --  compile-time option then it is not possible to set the Serialized
   --  threading mode and sqlite3_config() will return SQLITE_ERROR if called
   --  with the SQLITE_CONFIG_SERIALIZED configuration option.

   ----------------
   -- Statements --
   ----------------

   type Statement is private;
   No_Statement : constant Statement;
   --  A prepared SQL statement.

   procedure Prepare
     (DB     : Database;
      SQL    : String;  --  UTF-8 encoded
      Stmt   : out Statement;
      Status : out Result_Codes);
   --  Prepare (ie precompile) the SQL statement

   procedure Step
     (Stmt   : in out Statement;
      Status : out Result_Codes);
   --  Evaluate the next row in the result.
   --  Status set to Sqlite_Done indicates that the Step should no longer be
   --  called on that Stmt
   --  Sqlite_Row indicates that a new row is available

   function Column_Double (Stmt : Statement; Col : Natural) return Float;
   function Column_Int    (Stmt : Statement; Col : Natural) return Integer;
   function Column_Text   (Stmt : Statement; Col : Natural) return String;
   --  Get the value stored in a column. This is only valid if the last call
   --  to Step returned Sqlite_Row

   function Column_C_Text
     (Stmt : Statement; Col : Natural) return Interfaces.C.Strings.chars_ptr;
   function Column_Bytes (Stmt : Statement; Col : Natural) return Natural;
   --  Direct access to the C value (which is zero-terminated), and its
   --  length. Pointer is valid until the next call to Step or the next
   --  conversion of the value (by calling one of the other Column_* functions)
   --  Do not free result

   type Sqlite_Types is (Sqlite_Integer,
                         Sqlite_Float,
                         Sqlite_Text,
                         Sqlite_Blob,
                         Sqlite_Null);

   function Column_Type (Stmt : Statement; Col : Natural) return Sqlite_Types;
   --  Return the initial type of the column (hower, if you query it with one
   --  of the functiond above this might change the actual type of the column)

   function Column_Count (Stmt : Statement) return Natural;
   --  Return the number of columns in the result, 0 for an UPDATE, INSERT or
   --  DELETE

   function Column_Name (Stmt : Statement; Col : Natural) return String;
   --  Return the name of the specific column (or the value of the "AS" if one
   --  was specified.

   procedure Bind_Double
     (Stmt : Statement; Index : Integer; Value : Interfaces.C.double);

   procedure Bind_Int
     (Stmt : Statement; Index : Integer; Value : Interfaces.C.int);

   procedure Bind_Null (Stmt : Statement; Index : Integer);

   type Text_Destructor is access procedure (Str : in out System.Address);
   pragma Convention (C, Text_Destructor);

   procedure Bind_Text
     (Stmt : Statement; Index : Integer;
      Str : System.Address; N_Bytes : Natural;
      Destructor : Text_Destructor := null);
   --  Define the values for the parameters.
   --  The Destructor is called to free the memory when the parameter is bound
   --  to another value or destroyed. The default is that we do not free memory
   --  at all (and thus we assume the string is static).

   procedure Finalize (Stmt : Statement);
   --  Finalize and free the memory occupied by stmt

   function Reset (Stmt : Statement) return Result_Codes;
   --  Reset the statement, so that next call to step() returns the first row

   procedure Clear_Bindings (Stmt : Statement);
   --  Use this routine to reset all bound host parameters to NULL

   function Last_Insert_Rowid (DB : Database) return Long_Long_Integer;
   --  This routine returns the rowid of the most recent successful INSERT into
   --  the database from the database connection in the first argument. If no
   --  successful INSERTs have ever occurred on that database connection, zero
   --  is returned.
   --  If a separate thread performs a new INSERT on the same database
   --  connection while the last_insert_rowid function is running and thus
   --  changes the last insert rowid, then the value returned by
   --  last_insert_rowid is unpredictable and might not equal either the old or
   --  the new last insert rowid.

   function Changes (DB : Database) return Natural;
   --  Returns the number of database rows that were changed or inserted or
   --  deleted by the most recently completed SQL statement on the database.
   --  Only changes that are directly specified by the INSERT, UPDATE, or
   --  DELETE statement are counted.

   function DB_Handle (Stmt : Statement) return Database;
   --  Return the database connection on which the statement was prepared

   type Result_Table is private;
   No_Table : constant Result_Table;

   procedure Get_Table
     (DB     : Database;
      SQL    : String;
      Result : out Result_Table;
      Status : out Result_Codes;
      Error  : out Interfaces.C.Strings.chars_ptr);
   --  Execute a query on the server, and get all the rows at the once.
   --  Error must be freed by the caller

   procedure Free_Table (Result : in out Result_Table);
   --  Free the table

   function Get_Value
     (Result : Result_Table;
      Row, Column : Natural) return Interfaces.C.Strings.chars_ptr;
   --  Return the value at a specific row/column (or Null_Ptr)
   --  Column and Row start at 0

   function Get_Rows (Result : Result_Table) return Natural;
   function Get_Columns (Result : Result_Table) return Natural;
   --  Return the number of rows and columns in the table

   function Get_Column_Name
     (Result : Result_Table; Column : Natural) return String;
   --  Return the name of a specific column. Column starts at 0

   -----------------------
   -- Online backup API --
   -----------------------

   type Sqlite3_Backup is new System.Address;

   function Backup_Init
     (Pdest : Database;      --  destination database handle
      Pdest_Name : String;   --  destination database name
      Psource : Database;    --  source database handle
      Psource_Name : String) --  source database name
      return Sqlite3_Backup;
   --  The D and N arguments to sqlite3_backup_init(D,N,S,M) are the database
   --  connection associated with the destination database and the database
   --  name, respectively.
   --  The database name is "main" for the main database, "temp" for the
   --  temporary database, or the name specified after the AS keyword in an
   --  ATTACH statement for an attached database.
   --  The S and M arguments passed to sqlite3_backup_init(D,N,S,M)
   --  identify the database connection and database name of the source
   --  database, respectively. The source and destination database
   --  connections (parameters S and D) must be different or else
   --  sqlite3_backup_init(D,N,S,M) will fail with an error.
   --
   --  If an error occurs within sqlite3_backup_init(D,N,S,M), then NULL
   --  is returned and an error code and error message are stored in the
   --  destination database connection D. The error code and message for
   --  the failed call to sqlite3_backup_init() can be retrieved using
   --  the sqlite3_errcode(), sqlite3_errmsg(), and/or sqlite3_errmsg16()
   --  functions. A successful call to sqlite3_backup_init() returns a pointer
   --  to an sqlite3_backup object. The sqlite3_backup object may be used
   --  with the sqlite3_backup_step() and sqlite3_backup_finish() functions
   --  to perform the specified backup operation.

   function Backup_Step
     (Bkp : Sqlite3_Backup;
      Npage : Integer := -1) return Result_Codes;
   pragma Import (C, Backup_Step, "sqlite3_backup_step");
   --  Copy npages (or all if -1)
   --  Function sqlite3_backup_step(B,N) will copy up to N pages between the
   --  source and destination databases specified by sqlite3_backup object
   --  B. If N is negative, all remaining source pages are copied. If
   --  sqlite3_backup_step(B,N) successfully copies N pages and there are
   --  still more pages to be copied, then the function returns SQLITE_OK.

   function Backup_Finish (Bkp : Sqlite3_Backup) return Result_Codes;
   pragma Import (C, Backup_Finish, "sqlite3_backup_finish");
   --  Finish the backup and free memory

   function Backup_Remaining (Bkp : Sqlite3_Backup) return Integer;
   pragma Import (C, Backup_Remaining, "sqlite3_backup_remaining");

   function Backup_Page_Count (Bkp : Sqlite3_Backup) return Integer;
   pragma Import (C, Backup_Page_Count, "sqlite3_backup_pagecount");

private
   type Database_Record is null record; --  Must be null, hides sqlite data
   type Database is access Database_Record;
   pragma Convention (C, Database);
   No_Database : constant Database := null;

   type Statement is new System.Address;
   No_Statement : constant Statement := Statement (System.Null_Address);

   type Result_Table is record
      Values  : System.Address;
      Rows    : Natural;
      Columns : Natural;
   end record;

   No_Table : constant Result_Table := (System.Null_Address, 0, 0);

   for Sqlite_Types use (Sqlite_Integer => 1,
                         Sqlite_Float   => 2,
                         Sqlite_Text    => 3,
                         Sqlite_Blob    => 4,
                         Sqlite_Null    => 5);

   for Result_Codes use
     (Sqlite_OK                       => 0,
      Sqlite_Error                    => 1,
      Sqlite_Internal                 => 2,
      Sqlite_Perm                     => 3,
      Sqlite_Abort                    => 4,
      Sqlite_Busy                     => 5,
      Sqlite_Locked                   => 6,
      Sqlite_Nomem                    => 7,
      Sqlite_Readonly                 => 8,
      Sqlite_Interrupt                => 9,
      Sqlite_Ioerr                    => 10,
      Sqlite_Corrupt                  => 11,
      Sqlite_Notfound                 => 12,
      Sqlite_Full                     => 13,
      Sqlite_Cantopen                 => 14,
      Sqlite_Protocol                 => 15,
      Sqlite_Empty                    => 16,
      Sqlite_Schema                   => 17,
      Sqlite_Toobig                   => 18,
      Sqlite_Constraint               => 19,
      Sqlite_Mismatch                 => 20,
      Sqlite_Misuse                   => 21,
      Sqlite_Nolfs                    => 22,
      Sqlite_Auth                     => 23,
      Sqlite_Format                   => 24,
      Sqlite_Range                    => 25,
      Sqlite_Notadb                   => 26,
      Sqlite_Row                      => 100,
      Sqlite_Done                     => 101,
      Sqlite_Locked_Sharedcache       => 6  +  1 * 256,
      Sqlite_Ioerr_Read               => 10 +  1 * 256,
      Sqlite_Ioerr_Short_Read         => 10 +  2 * 256,
      Sqlite_Ioerr_Write              => 10 +  3 * 256,
      Sqlite_Ioerr_Fsync              => 10 +  4 * 256,
      Sqlite_Ioerr_Dir_Fsync          => 10 +  5 * 256,
      Sqlite_Ioerr_Truncate           => 10 +  6 * 256,
      Sqlite_Ioerr_Fstat              => 10 +  7 * 256,
      Sqlite_Ioerr_Unlock             => 10 +  8 * 256,
      Sqlite_Ioerr_Rdlock             => 10 +  9 * 256,
      Sqlite_Ioerr_Delete             => 10 + 10 * 256,
      Sqlite_Ioerr_Blocked            => 10 + 11 * 256,
      Sqlite_Ioerr_Nomem              => 10 + 12 * 256,
      Sqlite_Ioerr_Access             => 10 + 13 * 256,
      Sqlite_Ioerr_Checkreservedblock => 10 + 14 * 256,
      Sqlite_Ioerr_Lock               => 10 + 15 * 256,
      Sqlite_Ioerr_Close              => 10 + 16 * 256,
      Sqlite_Ioerr_Dir_Close          => 10 + 17 * 256);
   pragma Convention (C, Result_Codes);

   pragma Import (C, Column_Double,     "sqlite3_column_double");
   pragma Import (C, Column_Int,        "sqlite3_column_int");
   pragma Import (C, Column_C_Text,     "sqlite3_column_text");
   pragma Import (C, Column_Bytes,      "sqlite3_column_bytes");
   pragma Import (C, Changes,           "sqlite3_changes");
   pragma Import (C, Column_Type,       "sqlite3_column_type");
   pragma Import (C, Column_Count,      "sqlite3_column_count");
   pragma Import (C, DB_Handle,         "sqlite3_db_handle");

end GNATCOLL.SQL.Sqlite.Gnade;
