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

with Ada.Calendar;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with GNAT.Calendar;
with GNAT.Sockets;                 use GNAT.Sockets;
with GNAT.Strings;                 use GNAT.Strings;

with Interfaces.C.Strings;         use Interfaces.C.Strings;

with GNATCOLL.SQL.Postgres.Gnade;  use GNATCOLL.SQL.Postgres.Gnade;
with GNATCOLL.SQL.Exec_Private;    use GNATCOLL.SQL.Exec_Private;
with GNATCOLL.Traces;              use GNATCOLL.Traces;
with GNATCOLL.Utils;               use GNATCOLL.Utils;

package body GNATCOLL.SQL.Postgres.Builder is
   Me_Query  : constant Trace_Handle := Create ("SQL");

   Use_Cursors : constant Boolean := False;
   --  Whether to use "DECLARE name CURSOR ..." to use cursors for Forward
   --  cursors. Although this might save some memory since we do not have to
   --  have all results in memory, this is in fact *much* slower, so is
   --  disabled for now. Possible improvements would be to fetch several rows
   --  at once in the cursor, but even that does not seem to improve things too
   --  much.

   type Postgresql_DBMS_Stmt_Record is record
      Cursor : Ada.Strings.Unbounded.Unbounded_String;
      --  Name of the associated cursor

   end record;
   type Postgresql_DBMS_Stmt is access all Postgresql_DBMS_Stmt_Record;

   function Convert is new Ada.Unchecked_Conversion
     (Postgresql_DBMS_Stmt, DBMS_Stmt);
   function Convert is new Ada.Unchecked_Conversion
     (DBMS_Stmt, Postgresql_DBMS_Stmt);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Postgresql_DBMS_Stmt_Record, Postgresql_DBMS_Stmt);

   type Database_Access is access GNATCOLL.SQL.Postgres.Gnade.Database;

   type Postgresql_Connection_Record is
     new GNATCOLL.SQL.Exec.Database_Connection_Record with
      record
         Connection_String : GNAT.Strings.String_Access;
         Postgres          : Database_Access;

         Cursor            : Natural := 0;
         --  Id for the current cursor
         --  This is used to create the name of cursors for dbms statements,
         --  when no name is provided by the user.

         Connected_On      : Ada.Calendar.Time := GNAT.Calendar.No_Time;
      end record;
   type Postgresql_Connection is access all Postgresql_Connection_Record'Class;
   overriding procedure Close
     (Connection : access Postgresql_Connection_Record);
   overriding function Parameter_String
     (Self  : Postgresql_Connection_Record;
      Index : Positive;
      Typ   : Parameter_Type) return String;
   overriding function Can_Alter_Table_Constraints
     (Self : access Postgresql_Connection_Record) return Boolean;
   overriding function Has_Pragmas
     (Self : access Postgresql_Connection_Record) return Boolean;
   overriding function Connect_And_Execute
     (Connection  : access Postgresql_Connection_Record;
      Is_Select   : Boolean;
      Direct      : Boolean;
      Query       : String         := "";
      Stmt        : DBMS_Stmt      := No_DBMS_Stmt;
      Params      : SQL_Parameters := No_Parameters)
      return Abstract_Cursor_Access;
   overriding function Connected_On
     (Connection : access Postgresql_Connection_Record)
      return Ada.Calendar.Time;
   overriding function Connect_And_Prepare
     (Connection : access Postgresql_Connection_Record;
      Query      : String;
      Name       : String;
      Direct     : Boolean)
      return DBMS_Stmt;
   overriding function Execute
     (Connection  : access Postgresql_Connection_Record;
      Prepared    : DBMS_Stmt;
      Is_Select   : Boolean;
      Direct      : Boolean;
      Params      : SQL_Parameters := No_Parameters)
      return Abstract_Cursor_Access;
   overriding procedure Force_Connect
     (Connection : access Postgresql_Connection_Record);
   overriding procedure Force_Disconnect
     (Connection : access Postgresql_Connection_Record);
   overriding function Insert_And_Get_PK
     (Connection : access Postgresql_Connection_Record;
      Query      : String;
      Params     : SQL_Parameters := No_Parameters;
      PK         : SQL_Field_Integer) return Integer;
   overriding function Insert_And_Get_PK
     (Connection : access Postgresql_Connection_Record;
      Stmt       : Prepared_Statement'Class;
      Params     : SQL_Parameters := No_Parameters;
      PK         : SQL_Field_Integer) return Integer;
   overriding function String_Image
     (Self : Postgresql_Connection_Record; Value : String; Quote : Boolean)
      return String;
   overriding function Field_Type_Autoincrement
     (Self : Postgresql_Connection_Record) return String;
   overriding function Field_Type_Money
     (Self : Postgresql_Connection_Record) return String;
   overriding function Error
     (Connection : access Postgresql_Connection_Record) return String;
   overriding procedure Foreach_Table
     (Connection : access Postgresql_Connection_Record;
      Callback   : access procedure
        (Name, Description : String; Kind : Relation_Kind));
   overriding procedure Foreach_Field
     (Connection : access Postgresql_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Name           : String;
         Typ            : String;
         Index          : Natural;
         Description    : String;
         Default_Value  : String;
         Is_Primary_Key : Boolean;
         Not_Null       : Boolean));
   overriding procedure Foreach_Foreign_Key
     (Connection : access Postgresql_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Index             : Positive;
         Local_Attribute   : Integer;
         Foreign_Table     : String;
         Foreign_Attribute : Integer));
   overriding procedure Finalize
     (Connection : access Postgresql_Connection_Record;
      Prepared   : DBMS_Stmt);
   --  Reset:
   --  The prepared statement is "DECLARE ... CURSOR" so there is nothing to
   --  reset. The cursor itself is created as part of the iteration

   generic
      type Base is abstract new DBMS_Forward_Cursor with private;
   package Postgresql_Cursors is
      type Cursor is abstract new Base with record
         Res     : GNATCOLL.SQL.Postgres.Gnade.Result;

         Current : GNATCOLL.SQL.Postgres.Gnade.Tuple_Index := 0;
         --  Always 0 for Forward_Cursor
      end record;

      overriding function Current (Self : Cursor) return Positive;
      overriding function Error_Msg (Self : Cursor) return String;
      overriding function Status (Self : Cursor) return String;
      overriding function Is_Success (Self : Cursor) return Boolean;
      overriding procedure Finalize (Self : in out Cursor);
      overriding function Value
        (Self  : Cursor; Field : GNATCOLL.SQL.Exec.Field_Index) return String;
      overriding function C_Value
        (Self  : Cursor; Field : GNATCOLL.SQL.Exec.Field_Index)
         return chars_ptr;
      overriding function Boolean_Value
        (Self  : Cursor; Field : GNATCOLL.SQL.Exec.Field_Index) return Boolean;
      overriding function Is_Null
        (Self  : Cursor; Field : GNATCOLL.SQL.Exec.Field_Index) return Boolean;
      overriding function Last_Id
        (Self       : Cursor;
         Connection : access Database_Connection_Record'Class;
         Field      : SQL_Field_Integer) return Integer;
      overriding function Field_Count
        (Self : Cursor) return GNATCOLL.SQL.Exec.Field_Index;
      overriding function Field_Name
        (Self  : Cursor; Field : GNATCOLL.SQL.Exec.Field_Index) return String;
   end Postgresql_Cursors;
   --  Build cursors using a Result internally for various information

   ------------------------
   -- Postgresql_Cursors --
   ------------------------

   package body Postgresql_Cursors is
      overriding function Error_Msg (Self : Cursor) return String is
      begin
         return Error (Self.Res);
      end Error_Msg;

      overriding function Status (Self : Cursor) return String is
      begin
         return Status (Self.Res);
      end Status;

      overriding function Is_Success (Self : Cursor) return Boolean is
      begin
         return Status (Self.Res) = PGRES_TUPLES_OK
           or else Status (Self.Res) = PGRES_COMMAND_OK;
      end Is_Success;

      overriding procedure Finalize (Self : in out Cursor) is
      begin
         Clear (Self.Res);
      end Finalize;

      overriding function Current (Self : Cursor) return Positive is
      begin
         return Integer (Self.Current) + 1;
      end Current;

      overriding function Value
        (Self  : Cursor;
         Field : GNATCOLL.SQL.Exec.Field_Index) return String is
      begin
         return Value (Self.Res, Self.Current,
                       GNATCOLL.SQL.Postgres.Gnade.Field_Index (Field));
      end Value;

      overriding function C_Value
        (Self  : Cursor;
         Field : GNATCOLL.SQL.Exec.Field_Index) return chars_ptr is
      begin
         return C_Value
           (Self.Res, Self.Current,
            GNATCOLL.SQL.Postgres.Gnade.Field_Index (Field));
      end C_Value;

      overriding function Boolean_Value
        (Self  : Cursor;
         Field : GNATCOLL.SQL.Exec.Field_Index) return Boolean is
      begin
         return Boolean_Value
           (Self.Res, Self.Current,
            GNATCOLL.SQL.Postgres.Gnade.Field_Index (Field));
      end Boolean_Value;

      overriding function Is_Null
        (Self  : Cursor;
         Field : GNATCOLL.SQL.Exec.Field_Index) return Boolean is
      begin
         return Is_Null
           (Self.Res,
            Self.Current,
            GNATCOLL.SQL.Postgres.Gnade.Field_Index (Field));
      end Is_Null;

      overriding function Last_Id
        (Self       : Cursor;
         Connection : access Database_Connection_Record'Class;
         Field      : SQL_Field_Integer) return Integer
      is
         pragma Unreferenced (Self);
         Q        : SQL_Query;
         Res2     : Forward_Cursor;
      begin
         --  Do not depend on OIDs, since the table might not have them (by
         --  default, recent versions of postgreSQL disable them. Instead, we
         --  use the currval() function which returns the last value set for a
         --  sequence within the current connection.

         Q := SQL_Select
           (Fields => From_String ("currval('" & Field.Table.all
            & "_" & Field.Name.all & "_seq')"));

         Res2.Fetch (Connection, Q);
         if Has_Row (Res2) then
            return Integer_Value (Res2, 0);
         end if;
         return -1;
      end Last_Id;

      overriding function Field_Count
        (Self : Cursor) return GNATCOLL.SQL.Exec.Field_Index is
      begin
         return GNATCOLL.SQL.Exec.Field_Index (Field_Count (Self.Res));
      end Field_Count;

      overriding function Field_Name
        (Self  : Cursor;
         Field : GNATCOLL.SQL.Exec.Field_Index) return String is
      begin
         return Field_Name
           (Self.Res, GNATCOLL.SQL.Postgres.Gnade.Field_Index (Field));
      end Field_Name;
   end Postgresql_Cursors;

   package Forward_Cursors is new Postgresql_Cursors (DBMS_Forward_Cursor);
   type Postgresql_Cursor is new Forward_Cursors.Cursor with record
      Stmt   : Postgresql_DBMS_Stmt;
      Must_Free_Stmt : Boolean := False;

      Processed_Rows : Natural := 0;
      Connection : Postgresql_Connection;
      Nested_Transactions : Boolean := False;
      Has_Row    : Boolean := True;
   end record;
   type Postgresql_Cursor_Access is access all Postgresql_Cursor'Class;

   overriding procedure Finalize (Self : in out Postgresql_Cursor);
   overriding function Processed_Rows
     (Self : Postgresql_Cursor) return Natural;
   overriding function Has_Row
     (Self : Postgresql_Cursor) return Boolean;
   overriding procedure Next (Self : in out Postgresql_Cursor);

   function Declare_Cursor
     (Query : String; Stmt : Postgresql_DBMS_Stmt) return String;
   --  SQL command to declare a cursor

   package Direct_Cursors is new Postgresql_Cursors (DBMS_Direct_Cursor);
   type Postgresql_Direct_Cursor is new Direct_Cursors.Cursor with record
      Rows    : Natural := 0;
   end record;
   type Postgresql_Direct_Cursor_Access
     is access all Postgresql_Direct_Cursor'Class;

   overriding function Processed_Rows
     (Self : Postgresql_Direct_Cursor) return Natural;
   overriding function Has_Row
     (Self : Postgresql_Direct_Cursor) return Boolean;
   overriding procedure Next   (Self : in out Postgresql_Direct_Cursor);
   overriding procedure First (Self : in out Postgresql_Direct_Cursor);
   overriding procedure Last  (Self : in out Postgresql_Direct_Cursor);
   overriding procedure Absolute
     (Self : in out Postgresql_Direct_Cursor; Row : Positive);
   overriding procedure Relative
     (Self : in out Postgresql_Direct_Cursor; Step : Integer);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNATCOLL.SQL.Postgres.Gnade.Database, Database_Access);

   function Get_Connection_String
     (Description   : Database_Description;
      With_Password : Boolean) return String;
   --  Create a connection string from the database description

   generic
      with procedure Perform
        (Res    : out Result;
         Query  : String;
         Stmt   : DBMS_Stmt;
         Params : SQL_Parameters := No_Parameters);
   procedure Connect_And_Do
     (Connection : access Postgresql_Connection_Record;
      Query      : String;
      Stmt       : DBMS_Stmt;
      Res        : out Result;
      Success    : out Boolean;
      Params     : SQL_Parameters := No_Parameters);
   --  (Re)connect to the database if needed, and perform the action. If the
   --  result of the action is successfull (as per exec status in Res), Success
   --  is set to True and Res to the last result. Otherwise, Success is set to
   --  False.

   ----------------------
   -- Build_Connection --
   ----------------------

   function Build_Connection
     (Descr : access Postgres_Description'Class) return Database_Connection
   is
   begin
      return new Postgresql_Connection_Record
        (Descr, Always_Use_Transactions => False);
   end Build_Connection;

   -----------
   -- Error --
   -----------

   function Error
     (Connection : access Postgresql_Connection_Record) return String is
   begin
      if Connection.Postgres = null then
         return "No connection to database";
      else
         return Error (Connection.Postgres.all);
      end if;
   end Error;

   ---------------------------
   -- Get_Connection_String --
   ---------------------------

   function Get_Connection_String
     (Description   : Database_Description;
      With_Password : Boolean) return String
   is
      Descr : constant Postgres_Description_Access :=
        Postgres_Description_Access (Description);
      User   : constant String := Descr.User.all;
      Host   : constant String := Descr.Host.all;
      Passwd : constant String := Descr.Password.all;

      function Escape (Str : String) return String;
      function Escape (Str : String) return String is
         Len : Natural := 0;
      begin
         for S in Str'Range loop
            if Str (S) = ''' or else Str (S) = '\' then
               Len := Len + 2;
            else
               Len := Len + 1;
            end if;
         end loop;

         return Result : String (Str'First .. Str'First + Len - 1) do
            Len := Result'First;
            for S in Str'Range loop
               if Str (S) = ''' or else Str (S) = '\' then
                  Result (Len) := '\';
                  Result (Len + 1) := Str (S);
                  Len := Len + 2;
               else
                  Result (Len) := Str (S);
                  Len := Len + 1;
               end if;
            end loop;
         end return;
      end Escape;

      Str : Unbounded_String  := To_Unbounded_String
        ("dbname='" & Escape (Descr.Dbname.all) & "'");
   begin
      if User /= "" then
         Append (Str, " user='" & Escape (User) & "'");
      end if;

      if Host /= "" then
         Append (Str, " host='" & Escape (Host) & "'");
      end if;

      if Descr.Port /= -1 then
         Append (Str, " port=" & Image (Descr.Port, Min_Width => 1));
      end if;

      if With_Password and then Passwd /= "" then
         Append (Str, " password='" & Escape (Passwd) & "'");
      end if;

      case Descr.SSL is
         when Disable => Append (Str, " sslmode=disable");
         when Allow   => Append (Str, " sslmode=allow");
         when Prefer  => Append (Str, " sslmode=prefer");
         when Require => Append (Str, " sslmode=require");
      end case;

      return To_String (Str);
   end Get_Connection_String;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (Connection : access Postgresql_Connection_Record) is
   begin
      --  Since we have a controlled type, we just have to deallocate memory to
      --  deallocate memory allocated by postgres
      if Connection /= null then
         Free (Connection.Connection_String);
         Unchecked_Free (Connection.Postgres);
      end if;
   end Close;

   -------------------
   -- Force_Connect --
   -------------------

   overriding procedure Force_Connect
     (Connection : access Postgresql_Connection_Record)
   is
   begin
      if Connection.Postgres = null then
         Print_Warning
           (Connection,
            "Connecting to the database "
            & Get_Connection_String (Get_Description (Connection), False));

         if Connection.Connection_String = null then
            Connection.Connection_String := new String'
              (Get_Connection_String (Get_Description (Connection), True));
         end if;

         Connection.Postgres := new GNATCOLL.SQL.Postgres.Gnade.Database
           (Connection.Connection_String);
         Connection.Connected_On := Ada.Calendar.Clock;
      else
         Print_Warning
           (Connection,
            "Reconnecting to the database "
            & Get_Connection_String (Get_Description (Connection), False));
         Reset (Connection.Postgres.all);
         Connection.Connected_On := Ada.Calendar.Clock;
      end if;
   end Force_Connect;

   ----------------------
   -- Force_Disconnect --
   ----------------------

   overriding procedure Force_Disconnect
     (Connection : access Postgresql_Connection_Record)
   is
      Sock : Socket_Type;
      function To_Socket is
        new Ada.Unchecked_Conversion (Interfaces.C.int, Socket_Type);

   begin
      if Connection.Postgres = null then
         Print_Warning
           (Connection, "Can't disconnect null connection");
         return;
      end if;

      Sock := To_Socket (Connection.Postgres.Socket);
      if Sock = No_Socket then
         Print_Warning (Connection, "Not connected");
      else
         --  Keep the socket descriptor valid, but ensure all reads will fail

         Shutdown_Socket (Sock);
      end if;
   end Force_Disconnect;

   --------------------
   -- Connect_And_Do --
   --------------------

   procedure Connect_And_Do
     (Connection : access Postgresql_Connection_Record;
      Query      : String;
      Stmt       : DBMS_Stmt;
      Res        : out Result;
      Success    : out Boolean;
      Params      : SQL_Parameters := No_Parameters)
   is
      Res_Status : ExecStatus;
      First_Try  : Integer;

   begin
      if Connection.Postgres /= null
           and then Status (Connection.Postgres.all)
                                  = CONNECTION_OK
      then
         --  If the connection is already established, first try to send the
         --  query directly, then try to reconnect if the connection was
         --  not OK.

         First_Try := 1;

      else
         --  If not connected, or the connection failed, go straight to the
         --  second try, where we force a reconnection before sending the
         --  query.

         First_Try := 2;
      end if;

      for Try in First_Try .. 2 loop
         Clear (Res);

         --  Reconnect if needed

         if Try = 2 then
            Force_Connect (Connection);
         end if;

         Success := (Status (Connection.Postgres.all) = CONNECTION_OK);

         --  If connection status is bad, and we just tried to reconnect,
         --  report error now.

         if Try = 2 then
            if not Success then
               Print_Error
                 (Connection, "Cannot connect to PostgreSQL database "
                  & " Connection String is """
                  & Get_Connection_String
                    (Get_Description (Connection), False)
                  & """");
               Close (Connection);
               Connection.Postgres := null;
               return;
            end if;
         end if;

         --  Empty query: only check connection status

         if Query = "" and then Stmt = No_DBMS_Stmt then
            return;
         end if;

         --  Here if we have a presumed working connection

         Perform (Res, Query, Stmt, Params);

         Res_Status := Status (Res);
         case Res_Status is
            when PGRES_COMMAND_OK |
                 PGRES_TUPLES_OK  |
                 PGRES_COPY_OUT   |
                 PGRES_COPY_IN    |
                 PGRES_COPY_BOTH  =>
               Success := True;
               return;

            when PGRES_NONFATAL_ERROR
               | PGRES_FATAL_ERROR
               | PGRES_EMPTY_QUERY =>

               --  If the connection is still good, that just means the request
               --  was invalid. Do not try to reconnect in this case, since
               --  that would kill any transaction BEGIN..COMMIT we are in the
               --  process of doing.

               if Status (Connection.Postgres.all) = CONNECTION_OK then
                  Success := False;
                  return;

               end if;

            when others =>
               null;
         end case;

         --  If this was the first attempt, then we'll now retry the connection

         if Try = 1 then
            Print_Warning
              (Connection,
               "Query failed with status " & Res_Status'Img);
            Print_Warning
              (Connection,
               "DB status is " & Status (Connection.Postgres.all)'Img
               & ", reconnecting");
         end if;
      end loop;
   end Connect_And_Do;

   --------------------
   -- Declare_Cursor --
   --------------------

   function Declare_Cursor
     (Query : String; Stmt : Postgresql_DBMS_Stmt) return String is
   begin
      return "DECLARE " & To_String (Stmt.Cursor)
        & " SCROLL CURSOR FOR " & Query;
   end Declare_Cursor;

   ------------------
   -- Connected_On --
   ------------------

   overriding function Connected_On
     (Connection : access Postgresql_Connection_Record)
      return Ada.Calendar.Time is
   begin
      return Connection.Connected_On;
   end Connected_On;

   -------------------------
   -- Connect_And_Prepare --
   -------------------------

   overriding function Connect_And_Prepare
     (Connection : access Postgresql_Connection_Record;
      Query      : String;
      Name       : String;
      Direct     : Boolean)
      return DBMS_Stmt
   is
      P_Stmt : Postgresql_DBMS_Stmt;

      procedure Perform
        (Res    : out Result;
         Query  : String;
         Stmt   : DBMS_Stmt;
         Params : SQL_Parameters := No_Parameters);

      -------------
      -- Perform --
      -------------

      procedure Perform
        (Res    : out Result;
         Query  : String;
         Stmt   : DBMS_Stmt;
         Params : SQL_Parameters := No_Parameters)
      is
         pragma Assert (Stmt = No_DBMS_Stmt);

         CName : constant String := To_String (P_Stmt.Cursor);
      begin
         if Active (Me_Query) then
            if Name /= "" then
               Trace (Me_Query, "PQprepare(" & Name & ")");
            else
               Trace (Me_Query, "PQprepare(" & CName & ", " & Query & ")");
            end if;
         end if;

         --  Older versions of PostgreSQL (prior to 8.0) do not have a specific
         --  PQprepare(), so for compatibility we issue a PREPARE statement
         --  instead.

         if Active (Me_Query) then
            Trace (Me_Query, "PREPARE " & CName & " AS " & Query);
         end if;

         Execute (Res, Connection.Postgres.all,
                  "PREPARE " & CName & " AS " & Query, Connection.all,
                  Params);
         --  Prepare (Res, Connection.Postgres.all, CName, Query);
      end Perform;

      procedure Do_Perform is new Connect_And_Do (Perform);

      Res : Result;
      Success : Boolean;
      Was_Started : Boolean;
      pragma Unreferenced (Was_Started);

   --  Start of processing for Connect_And_Prepare

   begin
      P_Stmt := new Postgresql_DBMS_Stmt_Record;

      if Name /= "" then
         P_Stmt.Cursor := To_Unbounded_String ("cursor_" & Name);
      else
         Connection.Cursor := Connection.Cursor + 1; --  ??? Concurrency
         P_Stmt.Cursor := To_Unbounded_String
           ("cursor_" & Image (Connection.Cursor, 0));
      end if;

      if Direct or else not Use_Cursors then
         Do_Perform (Connection,
           Query, No_DBMS_Stmt, Res, Success);
      else
         Was_Started := Start_Transaction (Connection);
         Do_Perform (Connection,
           Declare_Cursor (Query, P_Stmt), No_DBMS_Stmt, Res, Success);
      end if;

      Clear (Res);

      if Success then
         return Convert (P_Stmt);
      else
         if Active (Me_Query) then
            Trace (Me_Query, "PQprepared failed: " & Error (Connection));
         end if;
         Unchecked_Free (P_Stmt);
         return No_DBMS_Stmt;
      end if;
   end Connect_And_Prepare;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Connection  : access Postgresql_Connection_Record;
      Prepared    : DBMS_Stmt;
      Is_Select   : Boolean;
      Direct      : Boolean;
      Params      : SQL_Parameters := No_Parameters)
      return Abstract_Cursor_Access
   is
      R    : Postgresql_Cursor_Access;
      DR   : Postgresql_Direct_Cursor_Access;
      Res  : Result;
      Stmt : constant Postgresql_DBMS_Stmt := Convert (Prepared);
      Name : constant String := To_String (Stmt.Cursor);
   begin
      --  For a direct_cursor, this will execute the query. For a
      --  forward_cursor this will declare the cursor on the DBMS

      --  Trace (Me_Query, "PQexecPrepared(" & Name & ")");

      Exec_Prepared
        (Res, Connection.Postgres.all, Name, Connection.all, Params);

      if Direct or not Use_Cursors then
         DR := new Postgresql_Direct_Cursor;
         DR.Res := Res;
         if Is_Select then
            DR.Rows := Natural (Tuple_Count (Res));
         else
            DR.Rows := Natural'(Command_Tuples (Res));
         end if;

         --  Extra trace that might be useful from time to time, but is often
         --  just noise because GNATCOLL.SQL.Exec will already display the
         --  result of the query.
         --  Post_Execute_And_Log
         --    (DR, Connection, "PQexecPrepared(" & Name & ")",
         --     No_Prepared, Is_Select => Is_Select, Params => Params);

         return Abstract_Cursor_Access (DR);

      else
         R            := new Postgresql_Cursor;
         R.Connection := Postgresql_Connection (Connection);
         R.Res        := Res;
         R.Stmt       := Stmt;

         --   Post_Execute_And_Log
         --     (R, Connection, "PQexecPrepared(" & Name & ")", No_Prepared,
         --      Is_Select => Is_Select, Params => Params);

         Next (R.all);  --  Read first row
         return Abstract_Cursor_Access (R);
      end if;
   end Execute;

   -----------------------
   -- Insert_And_Get_PK --
   -----------------------

   overriding function Insert_And_Get_PK
     (Connection : access Postgresql_Connection_Record;
      Query      : String;
      Params     : SQL_Parameters := No_Parameters;
      PK         : SQL_Field_Integer) return Integer
   is
      R : Forward_Cursor;
   begin
      R.Fetch (Connection,
               Query & " RETURNING " & PK.To_String (Connection.all),
               Params);
      return Integer_Value (R, 0);
   end Insert_And_Get_PK;

   -----------------------
   -- Insert_And_Get_PK --
   -----------------------

   overriding function Insert_And_Get_PK
     (Connection : access Postgresql_Connection_Record;
      Stmt       : Prepared_Statement'Class;
      Params     : SQL_Parameters := No_Parameters;
      PK         : SQL_Field_Integer) return Integer
   is
      R : Forward_Cursor;
   begin
      if not Stmt.Has_SQL_Suffix then
         --  ??? Assuming the suffix was already the same (should be since
         --  this is the primary key).
         Set_SQL_Suffix
            (Stmt, " RETURNING " & PK.To_String (Connection.all));
      end if;

      R.Fetch (Connection, Stmt, Params);
      return Integer_Value (R, 0);
   end Insert_And_Get_PK;

   -------------------------
   -- Connect_And_Execute --
   -------------------------

   function Connect_And_Execute
     (Connection  : access Postgresql_Connection_Record;
      Is_Select   : Boolean;
      Direct      : Boolean;
      Query       : String         := "";
      Stmt        : DBMS_Stmt      := No_DBMS_Stmt;
      Params      : SQL_Parameters := No_Parameters)
      return Abstract_Cursor_Access
   is
      procedure Perform
        (Res    : out Result;
         Query  : String;
         Stmt   : DBMS_Stmt;
         Params : SQL_Parameters := No_Parameters);

      -------------
      -- Perform --
      -------------

      procedure Perform
        (Res    : out Result;
         Query  : String;
         Stmt   : DBMS_Stmt;
         Params : SQL_Parameters := No_Parameters) is
      begin
         if Stmt /= No_DBMS_Stmt then
            Exec_Prepared (Res,
              Connection.Postgres.all,
              To_String (Convert (Stmt).Cursor),
              Connection.all,
              Params);
         else
            Execute (Res,
              Connection.Postgres.all, Query, Connection.all, Params);
         end if;
      end Perform;

      procedure Do_Perform is new Connect_And_Do (Perform);

      DR : Postgresql_Direct_Cursor_Access;
      R  : Postgresql_Cursor_Access;
      Success : Boolean;
      Res : Result;
      Create_Direct : constant Boolean :=
        Direct
        or else not Is_Select
        or else not Use_Cursors
        or else Query = ""
        or else Stmt /= No_DBMS_Stmt;

   --  Start of processing for Connect_And_Execute

   begin
      if Create_Direct then
         DR := new Postgresql_Direct_Cursor;
      else
         R            := new Postgresql_Cursor;
         R.Connection := Postgresql_Connection (Connection);
      end if;

      if Create_Direct then
         Do_Perform (Connection, Query, Stmt, Res, Success, Params);
      else
         R.Nested_Transactions := Start_Transaction (Connection);

         R.Stmt := new Postgresql_DBMS_Stmt_Record;
         R.Must_Free_Stmt := True;
         Connection.Cursor := Connection.Cursor + 1; --  ??? Concurrency ?
         R.Stmt.Cursor := To_Unbounded_String
           ("stmt" & Image (Connection.Cursor, 0));

         Do_Perform
           (Connection,
            Declare_Cursor (Query, R.Stmt),
            No_DBMS_Stmt,
            Res,
            Success,
            Params);
      end if;

      if Connection.Postgres = null then
         return null;
      end if;

      if Create_Direct then
         DR.Res := Res;

         if Success and then Query /= "" then
            if Is_Select then
               DR.Rows := Natural (Tuple_Count (Res));
            else
               DR.Rows := Natural'(Command_Tuples (Res));
            end if;
         end if;

         return Abstract_Cursor_Access (DR);

      else
         Next (R.all);
         return Abstract_Cursor_Access (R);
      end if;
   end Connect_And_Execute;

   --------------------
   -- Processed_Rows --
   --------------------

   overriding function Processed_Rows
     (Self : Postgresql_Direct_Cursor) return Natural is
   begin
      return Self.Rows;
   end Processed_Rows;

   --------------------
   -- Processed_Rows --
   --------------------

   overriding function Processed_Rows
     (Self : Postgresql_Cursor) return Natural is
   begin
      return Self.Processed_Rows;
   end Processed_Rows;

   -------------------
   -- Foreach_Table --
   -------------------

   overriding procedure Foreach_Table
     (Connection : access Postgresql_Connection_Record;
      Callback   : access procedure
        (Name, Description : String; Kind : Relation_Kind))
   is
      R     : Forward_Cursor;
      Kind  : Relation_Kind;
   begin
      R.Fetch
        (Connection,
         "SELECT pg_class.relname, pg_description.description,"
         & " pg_class.relkind"
         & " FROM (pg_class left join pg_description"
         & "         on  pg_description.objoid = pg_class.oid"
         & "         and pg_description.objsubid = 0),"
         & "      pg_namespace"
         & " WHERE relnamespace=pg_namespace.oid"
         & "   AND pg_namespace.nspname='public'"
         & "   AND pg_class.relkind ~ '[rv]'"
         & " ORDER BY pg_class.relname");

      while Has_Row (R) loop
         if Value (R, 2) = "r" then
            Kind := Kind_Table;
         else
            Kind := Kind_View;
         end if;

         Callback (Name        => Value (R, 0),
                   Description => Value (R, 1),
                   Kind        => Kind);
         Next (R);
      end loop;
   end Foreach_Table;

   -------------------
   -- Foreach_Field --
   -------------------

   overriding procedure Foreach_Field
     (Connection : access Postgresql_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Name           : String;
         Typ            : String;
         Index          : Natural;
         Description    : String;
         Default_Value  : String;
         Is_Primary_Key : Boolean;
         Not_Null       : Boolean))
   is
      R, R2 : Forward_Cursor;

      procedure Process_Fields (PK : String);
      --  Process all the strings. PK is a postgreSQL array representing the
      --  list of primary keys

      procedure Process_Fields (PK : String) is
         Is_PK : Boolean;
         Current : Integer;
         Field : Positive;
         Key   : Integer;
      begin
         while Has_Row (R) loop
            Field := 1;
            Current := Integer_Value (R, 2);

            begin
               loop
                  Key := Integer'Value (Array_Field (PK, Field));

                  if Key = Current then
                     Is_PK := True;
                     exit;
                  end if;

                  Field := Field + 1;
               end loop;

            exception
               when Constraint_Error =>
                  Is_PK := False; --  no more fields in primary key
            end;

            Callback
              (Name           => Value (R, 0),
               Typ            => Value (R, 1),
               Index          => Current,
               Description    => Value (R, 3),
               Not_Null       => Boolean_Value (R, 4),
               Default_Value  => Value (R, 5),
               Is_Primary_Key => Is_PK);
            Next (R);
         end loop;
      end Process_Fields;

   begin
      R2.Fetch
        (Connection,
         "SELECT  pg_constraint.conkey"   --  1 attribute tuple
         & " from pg_constraint,"
         & "   pg_class"
         & " where conrelid=pg_class.oid"
         & "   and pg_class.relname='" & Table_Name & "'"
         & "   and pg_constraint.contype='p'");

      R.Fetch
        (Connection,
         "SELECT pg_attribute.attname,"       --  0 att name
         & "     pg_catalog.format_type(atttypid, atttypmod),"  --  1 att type
         & "     pg_attribute.attnum,"        --  2 attribute index in table
         & "     pg_description.description," --  3 field doc
         & "     pg_attribute.attnotnull,"    --  4 not null ?
         & "     (SELECT substring"
         & "        (pg_catalog.pg_get_expr(d.adbin, d.adrelid) for 128)"
         & "         FROM pg_catalog.pg_attrdef d"
         & "      WHERE d.adrelid = pg_attribute.attrelid"
         & "      AND d.adnum = pg_attribute.attnum"
         & "      AND pg_attribute.atthasdef)"  --  5 default
         & " FROM (pg_attribute left join pg_description"
         & "          on pg_description.objoid   = pg_attribute.attrelid"
         & "         and pg_description.objsubid = pg_attribute.attnum),"
         & "      pg_type, pg_class"
         & " WHERE atttypid = pg_type.OID"
         & "   AND pg_attribute.attnum > 0"
         & "   AND pg_class.relname='" & Table_Name & "'"
         & "   AND pg_class.oid = pg_attribute.attrelid"
         & "   AND not pg_attribute.attisdropped"
         & " ORDER BY pg_attribute.attname");

      if R2.Has_Row then
         Process_Fields (Value (R2, 0));
      else
         Process_Fields ("");
      end if;
   end Foreach_Field;

   -------------------------
   -- Foreach_Foreign_Key --
   -------------------------

   procedure Foreach_Foreign_Key
     (Connection : access Postgresql_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Index             : Positive;
         Local_Attribute   : Integer;
         Foreign_Table     : String;
         Foreign_Attribute : Integer))
   is
      R     : Forward_Cursor;
      Index : Natural := 1;
   begin
      R.Fetch
        (Connection,
         "SELECT  pg_constraint.contype,"  --  0 constraint type ('f', 'p',...)
         & " pg_constraint.conname,"       --  1 constraint name
         & " pg_class.relname,"            --  2 class name
         & " pg_constraint.conkey,"        --  3 attribute tuple
         & " pg_class2.relname,"           --  4 foreign table if any
         & " pg_constraint.confkey"        --  5 foreign attribute tuple
         & " from (pg_constraint left join pg_class pg_class2"
         & "   on pg_constraint.confrelid=pg_class2.oid),"
         & "   pg_class"
         & " where conrelid=pg_class.oid"
         & "   and pg_class.relname='" & Table_Name & "'"
         & "   and pg_constraint.contype='f'"
         & " order by pg_constraint.conkey");

      while Has_Row (R) loop
         declare
            Attr_Array   : constant String := Value (R, 3);
            Foreign      : constant String := Value (R, 4);
            Foreign_Attr : constant String := Value (R, 5);
            Key1, Key2   : Integer;
            Field        : Positive := 1;
         begin
            loop
               Key1 := Integer'Value (Array_Field (Attr_Array, Field));
               Key2 := Integer'Value (Array_Field (Foreign_Attr, Field));

               Callback
                 (Index             => Index,
                  Local_Attribute   => Key1,
                  Foreign_Table     => Foreign,
                  Foreign_Attribute => Key2);

               Field := Field + 1;
            end loop;

         exception
            when Constraint_Error =>
               --  no more fields in key tuples
               null;
         end;

         Index := Index + 1;
         Next (R);
      end loop;
   end Foreach_Foreign_Key;

   -------------
   -- Has_Row --
   -------------

   overriding function Has_Row
     (Self : Postgresql_Direct_Cursor) return Boolean is
   begin
      return Self.Current < Tuple_Count (Self.Res);
   end Has_Row;

   -------------
   -- Has_Row --
   -------------

   overriding function Has_Row
     (Self : Postgresql_Cursor) return Boolean
   is
   begin
      return Self.Has_Row;
   end Has_Row;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Postgresql_Cursor) is
      Count : constant Natural := 1;
      Str : constant String :=
        "FETCH" & Count'Img & " FROM " & To_String (Self.Stmt.Cursor);
   begin
      Execute (Self.Res, Self.Connection.Postgres.all, Str,
               Self.Connection.all);
      if Status (Self.Res) /= PGRES_TUPLES_OK then
         Post_Execute_And_Log
           (Self'Unrestricted_Access, Self.Connection, Str, Is_Select => True);
      end if;

      Self.Has_Row := Tuple_Count (Self.Res) /= 0;

      if Self.Has_Row then
         Self.Processed_Rows := Self.Processed_Rows + Count;
      end if;
   end Next;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Postgresql_Direct_Cursor) is
   begin
      Self.Current := Self.Current + 1;
   end Next;

   -----------
   -- First --
   -----------

   overriding procedure First (Self : in out Postgresql_Direct_Cursor) is
   begin
      Self.Current := 0;
   end First;

   ----------
   -- Last --
   ----------

   overriding procedure Last  (Self : in out Postgresql_Direct_Cursor) is
   begin
      Self.Current := Tuple_Index (Self.Rows - 1);
   end Last;

   --------------
   -- Absolute --
   --------------

   overriding procedure Absolute
     (Self : in out Postgresql_Direct_Cursor; Row : Positive) is
   begin
      Self.Current := Tuple_Index (Row - 1);
   end Absolute;

   --------------
   -- Relative --
   --------------

   overriding procedure Relative
     (Self : in out Postgresql_Direct_Cursor; Step : Integer) is
   begin
      Self.Current := Tuple_Index
        (Integer'Min
           (Integer'Max (Integer (Self.Current) + Step, 0), Self.Rows - 1));
   end Relative;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Postgresql_Cursor) is
      Close : constant String := "CLOSE " & To_String (Self.Stmt.Cursor);
   begin
      Execute (Self.Res, Self.Connection.Postgres.all, Close,
               Self.Connection.all);
      Post_Execute_And_Log
        (Self'Access, Self.Connection, Close, No_Prepared, False);

      if Self.Nested_Transactions then
         --  ??? What if something has started a transaction in between ?
         Commit_Or_Rollback (Self.Connection);
      end if;

      if Self.Must_Free_Stmt then
         Finalize (Self.Connection, Convert (Self.Stmt));
      end if;

      Forward_Cursors.Finalize (Forward_Cursors.Cursor (Self));
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Connection : access Postgresql_Connection_Record;
      Prepared   : DBMS_Stmt)
   is
      Stmt : Postgresql_DBMS_Stmt := Convert (Prepared);
      Res : Result;
   begin
      if Stmt /= null then
         declare
            Str : constant String := "DEALLOCATE " & To_String (Stmt.Cursor);
         begin
            Execute (Res, Connection.Postgres.all, Str, Connection.all);
            if Active (Me_Query) then
               Trace (Me_Query, Str & " (" & Status (Res) & ")");
            end if;
         end;
         Clear (Res);
         Unchecked_Free (Stmt);
      end if;
   end Finalize;

   ----------------------
   -- Parameter_String --
   ----------------------

   overriding function Parameter_String
     (Self  : Postgresql_Connection_Record;
      Index : Positive;
      Typ   : Parameter_Type) return String
   is
      pragma Unreferenced (Self);
   begin
      case Typ is
         when Parameter_Text | Parameter_Character =>
            return '$' & Image (Index, 0) & "::text";
         when Parameter_Json =>
            return '$' & Image (Index, 0) & "::json";
         when Parameter_XML =>
            return '$' & Image (Index, 0) & "::xml";
         when Parameter_Integer =>
            return '$' & Image (Index, 0) & "::integer";
         when Parameter_Boolean =>
            return '$' & Image (Index, 0) & "::boolean";
         when Parameter_Float =>
            return '$' & Image (Index, 0) & "::float";
         when Parameter_Time =>
            --  Don't know how to say "::time with time zon"
            return '$' & Image (Index, 0);
         when Parameter_Date =>
            return '$' & Image (Index, 0) & "::date";
         when Parameter_Money =>
            return '$' & Image (Index, 0) & "::numeric";
      end case;
   end Parameter_String;

   ------------------------------
   -- Field_Type_Autoincrement --
   ------------------------------

   overriding function Field_Type_Autoincrement
     (Self : Postgresql_Connection_Record) return String
   is
      pragma Unreferenced (Self);
   begin
      return "SERIAL PRIMARY KEY";
   end Field_Type_Autoincrement;

   ------------------------------
   -- Field_Type_Money --
   ------------------------------

   overriding function Field_Type_Money
     (Self : Postgresql_Connection_Record) return String
   is
      pragma Unreferenced (Self);
   begin
      return "NUMERIC (" & K_Digits'Img & "," & K_Decimals'Img & ")";
   end Field_Type_Money;

   ----------------------------
   -- Has_Postgresql_Support --
   ----------------------------

   function Has_Postgresql_Support return Boolean is
   begin
      return True;
   end Has_Postgresql_Support;

   ------------------
   -- String_Image --
   ------------------

   overriding function String_Image
     (Self : Postgresql_Connection_Record; Value : String; Quote : Boolean)
      return String
   is
      pragma Unreferenced (Self);
      Num_Of_Apostrophes : constant Natural :=
        Ada.Strings.Fixed.Count (Value, "'");
      Num_Of_Backslashes : constant Natural :=
        Ada.Strings.Fixed.Count (Value, "\");
      New_Str            : String
        (Value'First .. Value'Last + Num_Of_Apostrophes + Num_Of_Backslashes);
      Index              : Natural := Value'First;
      Prepend_E          : Boolean := False;
   begin
      if not Quote then
         return Value;
      end if;

      if Num_Of_Apostrophes = 0
        and then Num_Of_Backslashes = 0
      then
         return "'" & Value & "'";
      end if;

      for I in Value'Range loop
         if Value (I) = ''' then
            New_Str (Index .. Index + 1) := "''";
            Index := Index + 1;
         elsif Value (I) = '\' then
            New_Str (Index .. Index + 1) := "\\";
            Prepend_E := True;
            Index := Index + 1;
         else
            New_Str (Index) := Value (I);
         end if;
         Index := Index + 1;
      end loop;

      if Prepend_E then
         return "E'" & New_Str & "'";
      else
         return "'" & New_Str & "'";
      end if;
   end String_Image;

   ---------------------------------
   -- Can_Alter_Table_Constraints --
   ---------------------------------

   overriding function Can_Alter_Table_Constraints
     (Self : access Postgresql_Connection_Record) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return True;
   end Can_Alter_Table_Constraints;

   -----------------
   -- Has_Pragmas --
   -----------------

   overriding function Has_Pragmas
     (Self : access Postgresql_Connection_Record) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Has_Pragmas;

end GNATCOLL.SQL.Postgres.Builder;
