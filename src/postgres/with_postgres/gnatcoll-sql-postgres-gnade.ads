-------------------------------------------------------------------------------
--                                                                           --
--                      GNADE  : GNu Ada Database Environment                --
--                                                                           --
--  Copyright (C) 2000-2003, Juergen Pfeifer <juergen.pfeifer@gmx.net>       --
--  Copyright (C) 2004-2011, AdaCore                                         --
--                                                                           --
--  GNADE is free software;  you can redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with GNAT;  see file COPYING.  If not, write --
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
--  MA 02111-1307, USA.                                                      --
--                                                                           --
--  As a special exception,  if other files  instantiate  generics from this --
--  unit, or you link  this unit with other files  to produce an executable, --
--  this  unit  does not  by itself cause  the resulting  executable  to  be --
--  covered  by the  GNU  General  Public  License.  This exception does not --
--  however invalidate  any other reasons why  the executable file  might be --
--  covered by the  GNU Public License.                                      --
--                                                                           --
--  GNADE is implemented to work with GNAT, the GNU Ada compiler.            --
-------------------------------------------------------------------------------

--  This file is adapted from the GNADE package, with local changes, in
--  particular in the namespace.
--  This binding is not intended to provide a Layer that hides the details
--  of PostgreSQL, instead the opposite is intended. This binding exposes
--  the same functionality as the C interface libpq.
--
--  It contains the low-level binding to the postgreSQL library, and is not
--  recommended for end users (the higher-level API in GNATCOLL.SQL is easier
--  to use, more portable across DBMS and provides type-safety). However, this
--  package can be used to provide an easier transition of existing code to
--  the higher-level API.

with System.Storage_Elements;
with Ada.Finalization;
with Interfaces.C.Strings;
with GNATCOLL.SQL_Impl;

package GNATCOLL.SQL.Postgres.Gnade is
   PostgreSQL_Error : exception;

   NAMEDATALEN : constant := 32;
   --  The maximum length for system identifiers (e.g. table names etc.)

   type OID is new Interfaces.C.unsigned;
   --  PostreSQL assigns a unique Object ID (OID) to a row.

   InvalidOID : constant OID;
   --  This denotes an invalid OID.

   subtype TypeID is OID;
   --  Types are objects in the database and have IDs.
   InvalidTypeID : constant TypeID;

   type Field_Index is new Natural;
   --  Field indices in general start with 0 in PostgreSQL

   type Tuple_Index is new Natural;
   --  Tuple indices in general start with 0 in PostgreSQL

   type ConnStatus is (CONNECTION_OK,
                       CONNECTION_BAD,
                       CONNECTION_STARTED,
                       CONNECTION_MADE,
                       CONNECTION_AWAITING_RESPONSE,
                       CONNECTION_AUTH_OK,
                       CONNECTION_SETENV);
   pragma Convention (C, ConnStatus);

   subtype SynchConnStatus is
     ConnStatus range CONNECTION_OK .. CONNECTION_BAD;
   --  These values are possible for synchronous connections.

   type Backend_PID is new Interfaces.C.int;
   --  This type is used to identify the backend server process handling the
   --  database connection.

   type ExecStatus is (PGRES_EMPTY_QUERY,
                       PGRES_COMMAND_OK,
                       PGRES_TUPLES_OK,
                       PGRES_COPY_OUT,
                       PGRES_COPY_IN,
                       PGRES_BAD_RESPONSE,
                       PGRES_NONFATAL_ERROR,
                       PGRES_FATAL_ERROR);
   pragma Convention (C, ExecStatus);

   type Database (Parameters :  access String) is tagged limited private;
   --  You must pass the PostgreSQP connection string as discriminant
   --  when you instantiate a new Database object. The connection to
   --  the database will be made during the initialization of the object.
   --  The connection will be closed automatically when the Database objects
   --  goes out of scope (will be "finalized").

   procedure Reset      (DB : Database'Class);
   --  This function will close the connection to the backend and attempt
   --  to reestablish a new connection to the same postmaster, using all
   --  the same parameters previously used. This may be useful for error
   --  recovery if a working connection is lost.
   pragma Inline (Reset);

   function  Name       (DB : Database'Class) return String;
   --  Returns the database name of the connection
   pragma Inline (Name);

   function  User       (DB : Database'Class) return String;
   --  Returns the user name of the connection
   pragma Inline (User);

   function  Password   (DB : Database'Class) return String;
   --  Returns the password of the connection
   pragma Inline (Password);

   function  Host       (DB : Database'Class) return String;
   --  Returns the server host name of the connection
   pragma Inline (Host);

   function  Port       (DB : Database'Class) return String;
   --  Returns the port of the connection
   pragma Inline (Port);

   function  TTY        (DB : Database'Class) return String;
   --  Returns the debug tty of the connection
   pragma Inline (TTY);

   function  Options    (DB : Database'Class) return String;
   --  Returns the backend options used in the connection
   pragma Inline (Options);

   function  Error      (DB : Database'Class) return String;
   --  Returns the error message most recently generated by an operation
   --  on the connection
   pragma Inline (Error);

   function  Server_PID (DB : Database'Class) return Backend_PID;
   --  Returns the process ID of the backend server handling this connection
   pragma Inline (Server_PID);

   function  Status     (DB : Database'Class) return ConnStatus;
   --  Returns the status of the connection.
   --  Only two of the ConnStatus values are seen outside of an asynchronous
   --  connection procedure - CONNECTION_OK or CONNECTION_BAD. A good
   --  connection to the database has the status CONNECTION_OK. A failed
   --  connection attempt is signaled by status CONNECTION_BAD. Ordinarily,
   --  an OK status will remain so until PQfinish, but a communications
   --  failure might result in the status changing to CONNECTION_BAD
   --  prematurely. In that case the application could try to recover by
   --  calling the Reset procedure.
   pragma Inline (Status);

   --  -----------------------------------------------------------------------
   --  Management of Large Objects (BLOBs)
   --  Cf http://www.postgresql.org/docs/current/interactive/lo-interfaces.html
   --  (Added by Audran Le Baron <lebaron@act-europe.fr>)
   --  -----------------------------------------------------------------------

   type File_Mode is mod 16#100000000#;
   for File_Mode'Size use Integer'Size;
   INV_WRITE : constant File_Mode := 16#00020000#;
   INV_READ  : constant File_Mode := 16#00040000#;

   type File_Descriptor is new Integer;
   Invalid_FD : constant File_Descriptor := -1;

   Seek_Set : constant := 0;
   Seek_Cur : constant := 1;
   Seek_End : constant := 2;

   function BLOB_Create (DB   : Database'Class;
                         Mode : File_Mode)
                         return OID;
   --  Create a Large Object. Read and write modes can be combined by OR'ing
   --  the correspondant flags. Return the OID of the newly created BLOB.
   --  Oid lo_creat (PGconn *conn, int mode)
   pragma Inline (BLOB_Create);

   function BLOB_Import (DB           : Database'Class;
                         In_File_Name : String) return OID;
   --  Import a Large Object into the database
   --  and return the corresponding OID.
   --  Oid lo_import(PGconn *conn, const char *filename)
   pragma Inline (BLOB_Import);

   function BLOB_Export (DB            : Database'Class;
                         Object_Id     : OID;
                         Out_File_Name : String) return Boolean;
   --  Export the given Large Object from the database to the specified file.
   --  Return False upon error, True otherwise.
   --  int lo_export(PGconn *conn, Oid lobjId, const char *filename)
   pragma Inline (BLOB_Export);

   function BLOB_Open (DB        : Database'Class;
                       Object_Id : OID;
                       Mode      : File_Mode)
                       return File_Descriptor;
   --  Open the given BLOB. Return the corresponding file descriptor.
   --  int lo_open  (PGconn *conn, Oid lobjId, int mode)
   pragma Inline (BLOB_Open);

   function BLOB_Write (DB : Database'Class;
                        FD : File_Descriptor;
                        A  : System.Address;
                        N  : Integer)
                        return Integer;
   --  Write N bytes from A to large object FD.
   --  Return the number of bytes actually written.
   --  int lo_write (PGconn *conn, int fd, const char *buf, size_t len)
   pragma Inline (BLOB_Write);

   function BLOB_Read (DB : Database'Class;
                       FD : File_Descriptor;
                       A  : System.Address;
                       N  : Integer)
                       return Integer;
   --  Read N bytes from large object FD to A.
   --  Return the number of bytes actually read.
   --  int lo_read  (PGconn *conn, int fd, char *buf, size_t len)
   pragma Inline (BLOB_Read);

   function BLOB_Lseek (DB     : Database'Class;
                        FD     : File_Descriptor;
                        Offset : Integer;
                        Origin : Integer)
                        return Integer;
   --  Move the current location pointer for the large object FD
   --  to the new location specified by offset.
   --  The valid values for Origin are:
   --     SEEK_SET (seek from object start),
   --     SEEK_CUR (seek from current position),
   --     SEEK_END (seek from object end).
   --  Return the new location pointer.
   --  int lo_lseek (PGconn *conn, int fd, int offset, int whence)
   pragma Inline (BLOB_Lseek);

   function BLOB_Tell (DB : Database'Class;
                       FD : File_Descriptor)
                       return Integer;
   --  Return the current read or write location of large object FD.
   --  If there is an error, the return value is negative.
   --  int lo_tell  (PGconn *conn, int fd)
   pragma Inline (BLOB_Tell);

   function BLOB_Close (DB : Database'Class;
                        FD : File_Descriptor)
                        return Boolean;
   --  Close the given large object (FD).
   --  Return True upon success, False otherwise.
   --  int lo_close (PGconn *conn, int fd)
   pragma Inline (BLOB_Close);

   function BLOB_Unlink (DB        : Database'Class;
                         Object_Id : OID) return Boolean;
   --  Remove the given Large Object from the database.
   --  Return True upon success, False otherwise.
   --  int lo_unlink(PGconn *conn, Oid lobjId)
   pragma Inline (BLOB_Unlink);

   --  -----------------------------------------------------------------------

   type Result is private;
   --  This is the abstraction of a query result set.
   --  You must call Clear to free the contents once you no longer need it

   procedure Clear (Res : in out Result);
   --  Free the memory used by Result

   procedure Execute
     (Res    : in out Result;
      DB     : Database'Class;
      Query  : String;
      Format : GNATCOLL.SQL_Impl.Formatter'Class;
      Params : SQL_Parameters := No_Parameters);
   --  Submit a query to Postgres and wait for the result

   procedure Prepare
     (Res       : out Result;
      DB        : Database'Class;
      Stmt_Name : String;
      Query     : String);
   pragma Inline (Prepare);
   --  Prepare the statement for execution

   procedure Exec_Prepared
     (Res       : out Result;
      DB        : Database'Class;
      Stmt_Name : String;
      Format    : GNATCOLL.SQL_Impl.Formatter'Class;
      Params    : SQL_Parameters := No_Parameters);
   --  Execute a prepared statement

   function Status (Res    : Result)     return ExecStatus;
   --  Returns the result status of the query
   --  If the result status is PGRES_TUPLES_OK, then the routines described
   --  below can be used to retrieve the tuples returned by the query.
   --  Note that a SELECT that happens to retrieve zero tuples still shows
   --  PGRES_TUPLES_OK. PGRES_COMMAND_OK is for commands that can never
   --  return tuples (INSERT, UPDATE, etc.). A response of PGRES_EMPTY_QUERY
   --  often exposes a bug in the client software.

   function Status (Status : ExecStatus) return String;
   --  Converts the enumerated type returned by PQresultStatus into a
   --  string constant describing the status code

   function Status (Res    : Result)     return String;
   --  Combination of the above two elementary functions. Get the string
   --  constant describing the status code directly from the Result
   pragma Inline (Status);

   function Error  (Res    : Result)     return String;
   --  Returns the error message associated with the query, or an empty
   --  string if there was no error.
   --
   --  Immediately following an Execute or GetResult call, Error (on the
   --  Database object) will return the same string as Error (on the Result).
   --  However, a Result will retain its error message until destroyed,
   --  whereas the Database's error message will change when subsequent
   --  operations are done. Use Error (on the Result) when you want to know
   --  the status associated with a particular Result; use Error (on the
   --  Database) when you want to know the status from the latest operation
   --  on the connection.
   pragma Inline (Error);

   function Tuple_Count (Res   : Result) return Tuple_Index;
   --  Returns the number of tuples (instances) in the query result
   pragma Inline (Tuple_Count);

   function Field_Count (Res   : Result) return Field_Index;
   --  Returns the number of fields (attributes) in each tuple of the
   --  query result
   pragma Inline (Field_Count);

   function Field_Name  (Res   : Result;
                         Index : Field_Index) return String;
   --  Returns the field (attribute) name associated with the given field
   --  index.
   pragma Inline (Field_Name);

   procedure Field_Lookup (Res   : Result;
                           Name  : String;
                           Index : out Field_Index;
                           Found : out Boolean);
   --  Returns the field (attribute) index associated with the given
   --  field name. If the field was found, the 'Found' parameter is set
   --  to True and the 'Index' parameter will contain the fields index.
   --  Otherwise 'Found' is set to False.
   pragma Inline (Field_Lookup);

   function Is_Binary (Res   : Result;
                       Index : Field_Index) return Boolean;
   --  Returns True if the Result contains binary tuple data, False if it
   --  contains ASCII data.
   pragma Inline (Is_Binary);

   function Field_Type  (Res   : Result;
                         Index : Field_Index) return TypeID;
   --  Returns the field type associated with the given field index.
   --  The TypeID returned is an internal coding of the type.
   pragma Inline (Field_Type);

   procedure Value (Res     : Result;
                    Tuple   : Tuple_Index;
                    Field   : Field_Index;
                    Pointer : out System.Address);
   --  For most queries, the value referenced by Pointer is a C-String
   --  representation of the attribute value. But if Is_Binary is True,
   --  the value referenced by Pointer is the binary representation of the type
   --  in the internal format of the backend server. It is then the
   --  programmer's responsibility to cast and convert the data to the
   --  correct Ada type. The Pointer returned points to storage that
   --  is part of the Result object. One should not modify it, and one
   --  must explicitly copy the value into other storage if it is to be
   --  used past the lifetime of the Result object itself.

   function C_Value
     (Res   : Result;
      Tuple : Tuple_Index := 0;
      Field : Field_Index := 0) return Interfaces.C.Strings.chars_ptr;
   --  Returns the C string

   function Value (Res   : Result;
                   Tuple : Tuple_Index := 0;
                   Field : Field_Index := 0) return String;
   --  Returns a single field (attribute) value of one tuple of a Result
   --  as a String. If this is a binary field please use the more general
   --  Value procedure above.

   function Boolean_Value (Res   : Result;
                           Tuple : Tuple_Index := 0;
                           Field : Field_Index := 0) return Boolean;
   pragma Inline (Boolean_Value);
   --  Same as above, but returns a boolean

   function Value (Res        : Result;
                   Tuple      : Tuple_Index := 0;
                   Field_Name : String) return String;
   --  Returns a single field (attribute) value of one tuple of a Result
   --  as a String. If this is a binary field please use the more general
   --  Value procedure above.
   pragma Inline (Value);

   function Boolean_Value (Res        : Result;
                           Tuple      : Tuple_Index := 0;
                           Field_Name : String) return Boolean;
   pragma Inline (Boolean_Value);
   --  Same as above but returns a boolean

   function Integer_Value (Res     : Result;
                           Tuple   : Tuple_Index := 0;
                           Field   : Field_Index := 0;
                           Default : Integer := Integer'First) return Integer;
   pragma Inline (Integer_Value);
   --  Same as above but returns an integer.
   --  If the field does not contain an integer, Default is returned if it
   --  was specified, or an exception is raised otherwise.

   function Array_Field (Value : String; Field : Positive) return String;
   --  Given a postgres array value "{1, 2, 3}", return the Index-th value.
   --  Constraint_Error is raised upon reaching the end of the array

   function Field_Size  (Res   : Result;
                         Field : Field_Index) return Integer;
   --  Returns the size in bytes of the field associated with the given
   --  field index.
   --  Field_Size returns the space allocated for this field in a database
   --  tuple, in other words the size of the server's binary representation
   --  of the data type. -1 is returned if the field is variable size.
   pragma Inline (Field_Size);

   function Field_Modification  (Res   : Result;
                                 Field : Field_Index) return Integer;
   --  Returns the type-specific modification data of the field associated
   --  with the given field index.
   pragma Inline (Field_Modification);

   function Field_Length (Res   : Result;
                          Tuple : Tuple_Index;
                          Field : Field_Index) return Natural;
   --  Returns the length of a field (attribute) in bytes.
   --  This is the actual data length for the particular data value, that is
   --  the size of the object retrieved by the Value function. Note that for
   --  ASCII-represented values, this size has little to do with the binary
   --  size reported by Field_Size.
   pragma Inline (Field_Length);

   function Is_Null (Res   : Result;
                     Tuple : Tuple_Index;
                     Field : Field_Index) return Boolean;
   --  Tests a field for a NULL entry. This function returns True if the
   --  field contains a NULL, False if it contains a non-null value.
   --  (Note that the Value function will return an empty string for a NULL
   --  field.)
   pragma Inline (Is_Null);

   function Command_Status (Res : Result) return String;
   --  Returns the command status string from the SQL command that generated
   --  the Result.
   pragma Inline (Command_Status);

   function Command_Tuples (Res : Result) return String;
   --  Returns the number of rows affected by the SQL command.
   --  If the SQL command that generated the Result was INSERT, UPDATE or
   --  DELETE, this returns a string containing the number of rows affected.
   --  If the command was anything else, it returns the empty string

   function Command_Tuples (Res : Result) return Natural;
   --  Returns the number of rows affected by the SQL command.
   --  If the SQL command that generated the Result was INSERT, UPDATE or
   --  DELETE, this returns the number of rows affected.
   --  If the command was anything else, it returns 0.
   pragma Inline (Command_Tuples);

   function OID_Value (Res : Result) return OID;
   --  Returns the object id of the tuple inserted, if the SQL command was
   --  an INSERT. Otherwise, returns InvalidOid.
   pragma Inline (OID_Value);

   procedure Make_Empty_Result (Res    : out Result;
                                DB     : Database'Class;
                                Status : ExecStatus := PGRES_EMPTY_QUERY);
   --  Constructs an empty Result object with the given status
   --  This is libpq's internal routine to allocate and initialize an empty
   --  Result object. It is exported because some applications find it useful
   --  to generate result objects (particularly objects with error status)
   --  themselves. If the Database connection is valid and status indicates
   --  an error, the connection's current errorMessage is copied into the
   --  Result.
   pragma Inline (Make_Empty_Result);

   --  -----------------------------------------------------------------------
   --  Procedures and functions for Asynchronous query processing
   --  -----------------------------------------------------------------------
   procedure Set_Non_Blocking (DB : Database'Class);
   --  Sets the state of the connection to non-blocking
   --  When a database connection has been set to non-blocking mode and
   --  Execute is called, it will temporarily set the state of the connection
   --  to blocking until the Execute completes.

   function Is_Non_Blocking (DB : Database'Class) return Boolean;
   --  Returns the blocking status of the database connection.

   function Send_Query (DB    : Database'Class;
                        Query : String) return Boolean;
   --  Submit a query to Postgres without waiting for the result(s). True is
   --  returned if the query was successfully dispatched, False if not (in
   --  which case, use function Error to get more information about the
   --  failure).
   --  After successfully calling Send_Query, call Get_Result one or more
   --  times to obtain the query results. Send_Query may not be called again
   --  (on the same connection) until Get_Result has returned with completion
   --  flag set to True, indicating that the query is done.

   procedure Get_Result (DB   : Database'Class;
                         Res  : out Result;
                         Done : out Boolean);
   --  Wait for the next result from a prior Send_Query, and return it.
   --  Done is set to True when the query is complete and there will be no
   --  more results, in this case Res will not be set. When Done is set to
   --  False, the Res contains a Result and processing is not finished.

   function Consume_Input (DB : Database'Class) return Boolean;
   --  If input is available from the backend, consume it.
   --  Consume_Input normally returns True indicating "no error", but returns
   --  False if there was some kind of trouble (in which case function Error
   --  should be used). Note that the result does not say whether any input
   --  data was actually collected. After calling Consume_Input, the
   --  application may check Is_Busy and/or Notifies to see if their state
   --  has changed.
   --
   --  Consume_Input may be called even if the application is not prepared to
   --  deal with a result or notification just yet. The routine will read
   --  available data and save it in a buffer, thereby causing a select(2)
   --  read-ready indication to go away. The application can thus use
   --  Consume_Input to clear the select condition immediately, and then
   --  examine the results at leisure.

   function  Flush (DB : Database'Class) return Boolean;
   --  Attempt to flush any data queued to the backend, returns True if
   --  successful (or if the send queue is empty) or False if it failed for
   --  some reason.
   --  Flush needs to be called on a non-blocking connection before calling
   --  select to determine if a responce has arrived. If True is returned it
   --  ensures that there is no data queued to the backend that has not
   --  actually been sent. Only applications that have used Set_Non_Blocking
   --  have a need for this.

   function  Is_Busy (DB : Database'Class) return Boolean;
   --  Returns True if a query is busy, that is, Get_Result would block
   --  waiting for input. A False return indicates that Get_Result can be
   --  called with assurance of not blocking.
   --  Is_Busy will not itself attempt to read data from the backend;
   --  therefore Consume_Input must be invoked first, or the busy state will
   --  never end.

   function  Socket (DB : Database'Class) return Interfaces.C.int;
   --  Obtain the file descriptor number for the backend connection socket.
   --  A valid descriptor will be >= 0; a result of -1 indicates that no
   --  backend connection is currently open.
   --

   function  Request_Cancel (DB : Database'Class) return Boolean;
   --  Request that Postgres abandon processing of the current query.
   --  The return value is True if the cancel request was successfully
   --  dispatched, False if not. (If not, function Error tells why not.)
   --  Successful dispatch is no guarantee that the request will have any
   --  effect, however. Regardless of the return value of Request_Cancel,
   --  the application must continue with the normal result-reading sequence
   --  using Get_Result. If the cancellation is effective, the current query
   --  will terminate early and return an error result. If the cancellation
   --  fails (say, because the backend was already done processing the query),
   --  then there will be no visible result at all.
   --  Note that if the current query is part of a transaction, cancellation
   --  will abort the whole transaction.
   --  Request_Cancel can safely be invoked from a signal handler. So, it is
   --  also possible to use it in conjunction with plain Execute, if the
   --  decision to cancel can be made in a signal handler. Note that
   --  Request_Cancel will have no effect if the connection is not currently
   --  open or the backend is not currently processing a query.

   type Notification is
      record
         Relation_Name : String (1 .. NAMEDATALEN);
         Notifier      : Backend_PID;
      end record;
   pragma Convention (C, Notification);

   procedure Notifies (DB      : Database'Class;
                       Message : out Notification;
                       Done    : out Boolean);
   --  Returns the next notification from a list of unhandled notification
   --  messages received from the backend. Done is set to False if there are
   --  no pending notifications. In this case Message is not be set. If Done
   --  is set to False, Message contains a valid Notification. Once a
   --  notification is returned from Notifies, it is considered handled and
   --  will be removed from the list of notifications.

   --  -----------------------------------------------------------------------
   --  Helpers
   --  -----------------------------------------------------------------------
   function Quote_Identifier (Identifier : String) return String;
   pragma Inline (Quote_Identifier);

private
   use Ada.Finalization;

   InvalidOID : constant OID := 0;
   InvalidTypeID : constant TypeID := TypeID (InvalidOID);

   type Connection_Handle is new System.Storage_Elements.Integer_Address;
   Null_Connection : constant Connection_Handle := 0;
   function Connect (Params : access String) return Connection_Handle;

   --  According to the PostgreSQL documentation, starting with Version 7
   --  PostgreSQL is threadsafe, the connection object should only be used
   --  by one thread at a time; Result object may be used concurrent.
   --  So we protect the PostgreSQL connection by this protected type.
   protected type PGConn (Parameters : access String) is
      function Handle return Connection_Handle;
      procedure Reset;
      function Error return String;
      function Status return  ConnStatus;
      procedure Close;
      function PID return Backend_PID;
      procedure Execute
        (Res     : in out Result;
         Success : out Boolean;
         Query   : String;
         Format  : GNATCOLL.SQL_Impl.Formatter'Class;
         Params  : SQL_Parameters := No_Parameters);
      procedure Prepare
        (Res       : out Result;
         Stmt_Name : String;
         Query     : String);
      procedure Exec_Prepared
        (Res       : out Result;
         Stmt_Name : String;
         Format    : GNATCOLL.SQL_Impl.Formatter'Class;
         Params    : SQL_Parameters := No_Parameters);
      function BLOB_Create (Mode : File_Mode) return OID;
      function BLOB_Import (In_File_Name : String) return OID;
      function BLOB_Export (Object_Id     : OID;
                            Out_File_Name : String) return Boolean;
      function BLOB_Open (Object_Id : OID;
                          Mode      : File_Mode) return File_Descriptor;
      function BLOB_Write (FD : File_Descriptor;
                           A  : System.Address;
                           N  : Integer) return Integer;
      function BLOB_Read (FD : File_Descriptor;
                          A  : System.Address;
                          N  : Integer) return Integer;
      function BLOB_Lseek (FD     : File_Descriptor;
                           Offset : Integer;
                           Origin : Integer) return Integer;
      function BLOB_Tell (FD : File_Descriptor) return Integer;
      function BLOB_Close (FD : File_Descriptor) return Boolean;
      function BLOB_Unlink (Object_Id : OID) return Boolean;
      --  --------------------------------------------------------------------
      procedure Empty_Result (Res    : out Result;
                              Status : ExecStatus);
      procedure SetNonBlocking;
      function  IsNonBlocking return Boolean;
      function SendQuery (Query : String) return Boolean;
      procedure GetResult (Res : out Result; Done : out Boolean);
      function ConsumeInput return Boolean;
      function Flush return Boolean;
      function IsBusy return Boolean;
      function Socket return Interfaces.C.int;
      function RequestCancel return Boolean;
      procedure Notifies (Message : out Notification;
                          Done    : out Boolean);
   private
      C_Connection : Connection_Handle := Connect (Parameters);
   end PGConn;
   type Connection_Pointer is access all PGConn;

   type Database (Parameters : access String) is new Limited_Controlled with
      record
         Connection : Connection_Pointer := new PGConn (Parameters);
      end record;

   procedure Initialize (Object : in out Database);
   procedure Finalize   (Object : in out Database);

   type PGresult is new System.Storage_Elements.Integer_Address;
   Null_Result : constant PGresult := 0;

   type Natural_Access is access Natural;

   type Result is record
      Res : PGresult := Null_Result;
   end record;

end GNATCOLL.SQL.Postgres.Gnade;
