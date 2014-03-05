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

with Ada.Characters.Handling;      use Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNATCOLL.SQL.Sqlite.Gnade;    use GNATCOLL.SQL.Sqlite.Gnade;
with GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Exec_Private;    use GNATCOLL.SQL.Exec_Private;
with GNATCOLL.Traces;              use GNATCOLL.Traces;
with GNATCOLL.Utils;               use GNATCOLL.Utils;
with GNAT.Calendar;
with Interfaces.C.Strings;         use Interfaces.C.Strings;
with System;                       use System;

package body GNATCOLL.SQL.Sqlite.Builder is
   Me : constant Trace_Handle := Create ("SQL.SQLITE");
   Me_Log : constant Trace_Handle := Create ("SQL.SQLITE.LOG");

   procedure Logger
     (Data       : System.Address;
      Error_Code : Result_Codes;
      Message    : Interfaces.C.Strings.chars_ptr);
   pragma Convention (C, Logger);
   --  Logs error messages from sqlite (see sqlite3_log)

   type Sqlite_Connection_Record is
     new GNATCOLL.SQL.Exec.Database_Connection_Record with
      record
         DB           : GNATCOLL.SQL.Sqlite.Gnade.Database;
         Connected_On : Ada.Calendar.Time := GNAT.Calendar.No_Time;
      end record;
   overriding procedure Force_Connect
     (Connection : access Sqlite_Connection_Record);
   overriding procedure Force_Disconnect
     (Connection : access Sqlite_Connection_Record);
   overriding function Supports_Timezone
     (Self  : Sqlite_Connection_Record) return Boolean;
   overriding function Boolean_Image
     (Self : Sqlite_Connection_Record; Value : Boolean) return String;
   overriding function Money_Image
     (Self : Sqlite_Connection_Record; Value : T_Money) return String;
   overriding function Parameter_String
     (Self  : Sqlite_Connection_Record;
      Index : Positive;
      Typ   : Parameter_Type) return String;
   overriding procedure Close
     (Connection : access Sqlite_Connection_Record);
   overriding function Field_Type_Autoincrement
     (Self : Sqlite_Connection_Record) return String;
   overriding function Field_Type_Money
     (Self : Sqlite_Connection_Record) return String;
   overriding function Can_Alter_Table_Constraints
     (Self : access Sqlite_Connection_Record) return Boolean;
   overriding function Has_Pragmas
     (Self : access Sqlite_Connection_Record) return Boolean;
   overriding function Check_Connection
     (Self : access Sqlite_Connection_Record) return Boolean;
   overriding function Connect_And_Execute
     (Connection  : access Sqlite_Connection_Record;
      Is_Select   : Boolean;
      Direct      : Boolean;
      Query       : String         := "";
      Stmt        : DBMS_Stmt      := No_DBMS_Stmt;
      Params      : SQL_Parameters := No_Parameters)
      return Abstract_Cursor_Access;
   overriding function Connected_On
     (Connection : access Sqlite_Connection_Record) return Ada.Calendar.Time;
   overriding function Connect_And_Prepare
     (Connection : access Sqlite_Connection_Record;
      Query      : String;
      Name       : String;
      Direct     : Boolean) return DBMS_Stmt;
   overriding function Execute
     (Connection  : access Sqlite_Connection_Record;
      Prepared    : DBMS_Stmt;
      Is_Select   : Boolean;
      Direct      : Boolean;
      Params      : SQL_Parameters := No_Parameters)
      return Abstract_Cursor_Access;
   overriding procedure Reset
     (Connection : access Sqlite_Connection_Record;
      Prepared   : DBMS_Stmt);
   overriding procedure Finalize
     (Connection : access Sqlite_Connection_Record;
      Prepared   : DBMS_Stmt);
   overriding function Error
     (Connection : access Sqlite_Connection_Record) return String;
   overriding procedure Foreach_Table
     (Connection : access Sqlite_Connection_Record;
      Callback   : access procedure
        (Name, Description : String; Kind : Relation_Kind));
   overriding procedure Foreach_Field
     (Connection : access Sqlite_Connection_Record;
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
     (Connection : access Sqlite_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Index             : Positive;
         Local_Attribute   : Integer;
         Foreign_Table     : String;
         Foreign_Attribute : Integer));

   type Sqlite_Cursor is new DBMS_Forward_Cursor with record
      DB             : access Sqlite_Connection_Record'Class;
      Stmt           : Statement;

      Free_Stmt      : Boolean := False;
      --  Whether the statement needs to be finalized; This will be false for
      --  a statement prepared explicitly by the user on the server. In this
      --  case, the statement will be reset instead.

      Processed_Rows : Natural := 0;
      Last_Status    : Result_Codes;  --  Last status of Step
   end record;
   type Sqlite_Cursor_Access is access all Sqlite_Cursor'Class;

   overriding function Current (Self : Sqlite_Cursor) return Positive;
   overriding function Error_Msg  (Self : Sqlite_Cursor) return String;
   overriding function Status     (Self : Sqlite_Cursor) return String;
   overriding function Is_Success (Self : Sqlite_Cursor) return Boolean;
   overriding procedure Finalize  (Self : in out Sqlite_Cursor);
   overriding function Processed_Rows (Self : Sqlite_Cursor) return Natural;
   overriding function Value
     (Self  : Sqlite_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return String;
   overriding function C_Value
     (Self  : Sqlite_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return chars_ptr;
   overriding function Is_Null
     (Self  : Sqlite_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return Boolean;
   overriding function Last_Id
     (Self       : Sqlite_Cursor;
      Connection : access Database_Connection_Record'Class;
      Field      : SQL_Field_Integer) return Integer;
   overriding function Field_Count
     (Self : Sqlite_Cursor) return GNATCOLL.SQL.Exec.Field_Index;
   overriding function Field_Name
     (Self : Sqlite_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return String;
   overriding function Has_Row (Self : Sqlite_Cursor) return Boolean;
   overriding procedure Next   (Self : in out Sqlite_Cursor);
   overriding function Boolean_Value
     (Self : Sqlite_Cursor; Field : Field_Index) return Boolean;
   overriding function Money_Value
     (Self  : Sqlite_Cursor; Field : Field_Index) return T_Money;

   package Direct_Cursors is new Generic_Direct_Cursors (Sqlite_Cursor);
   type Sqlite_Direct_Cursor is new Direct_Cursors.Direct with null record;
   type Sqlite_Direct_Cursor_Access is access all Sqlite_Direct_Cursor'Class;

   function Is_Whitespace (C : Character) return Boolean;
   --  Whether C is a white space character

   procedure Skip_Whitespace (S : String; Pos : in out Integer);
   procedure Skip_To_Whitespace (S : String; Pos : in out Integer);
   --  Skip to or until the next whitespace character. Pos is left on the
   --  first whitespace character or on the first character after the spaces

   function Unchecked_Convert is new Ada.Unchecked_Conversion
     (Statement, DBMS_Stmt);
   function Unchecked_Convert is new Ada.Unchecked_Conversion
     (DBMS_Stmt, Statement);

   overriding function Is_Prepared_On_Server_Supported
     (Connection : access Sqlite_Connection_Record) return Boolean;
   --  We allow transactions prepared on the server, but there are several
   --  restrictions with sqlite:
   --     - M410-030: when we execute a statement prepared on the server, this
   --       seems to prevent the deletion of the database on Windows.
   --       This might however be because we were missing a proper close to
   --       sqlite3_close.
   --     - executing the same statements multiple times nested will fail.
   --       For instance, do a:
   --            for r in query1(params):
   --                for r in query1(params2):
   --                     pass
   --       means the outer loop will only return a single result.

   ----------------------
   -- Check_Connection --
   ----------------------

   overriding function Check_Connection
     (Self : access Sqlite_Connection_Record) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return True;
   end Check_Connection;

   -------------------------------------
   -- Is_Prepared_On_Server_Supported --
   -------------------------------------

   overriding function Is_Prepared_On_Server_Supported
     (Connection : access Sqlite_Connection_Record) return Boolean
   is
      pragma Unreferenced (Connection);
   begin
      return True;
   end Is_Prepared_On_Server_Supported;

   ---------------
   -- Error_Msg --
   ---------------

   overriding function Error_Msg (Self : Sqlite_Cursor) return String is
   begin
      return Error_Msg (DB_Handle (Self.Stmt));
   end Error_Msg;

   ------------
   -- Status --
   ------------

   overriding function Status (Self : Sqlite_Cursor) return String is
   begin
      if Self.Last_Status = Sqlite_Done
        or else Self.Last_Status = Sqlite_Row
      then
         return "";
      else
         return Result_Codes'Image (Self.Last_Status);
      end if;
   exception
      when others =>
         return "ERROR";
   end Status;

   ----------------
   -- Is_Success --
   ----------------

   overriding function Is_Success
     (Self : Sqlite_Cursor) return Boolean is
   begin
      return Self.Last_Status = Sqlite_OK
        or else Self.Last_Status = Sqlite_Row
        or else Self.Last_Status = Sqlite_Done;
   end Is_Success;

   -----------
   -- Error --
   -----------

   function Error
     (Connection : access Sqlite_Connection_Record) return String is
   begin
      if Connection.DB = No_Database then
         return "No connection to database";
      else
         return Error_Msg (Connection.DB);
      end if;
   end Error;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Sqlite_Cursor) is
      Status : Result_Codes;
   begin
      if Self.Stmt /= No_Statement
        and then
          (Self.DB = null
           or else not Was_Closed (Self.DB))
      then
         if Self.Free_Stmt then
            Finalize (Self.Stmt);
         else
            --  Clear bindings is useless, since we never need to free memory
            --  (even strings are passed by access)

            --  Clear_Bindings (Self.Stmt);

            Status := Reset (Self.Stmt);
            if Status /= Sqlite_OK then
               Trace (Me, "Error when reseting cursor to free LOCKS: "
                      & Status'Img);
            end if;
         end if;
         Self.Stmt := No_Statement;
         Self.DB := null;
      end if;
   end Finalize;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset
     (Connection : access Sqlite_Connection_Record;
      Prepared   : DBMS_Stmt)
   is
      Status : Result_Codes;
      pragma Unreferenced (Connection, Status);
   begin
      Status := Reset (Unchecked_Convert (Prepared));
   end Reset;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Connection : access Sqlite_Connection_Record;
      Prepared   : DBMS_Stmt)
   is
      pragma Unreferenced (Connection);
   begin
      Finalize (Unchecked_Convert (Prepared));
   end Finalize;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (Connection : access Sqlite_Connection_Record) is
   begin
      Trace (Me, "Closing connection to sqlite");
      Mark_As_Closed (Connection, Closed => True);
      Close (Connection.DB, Finalize_Prepared_Statements => True);
      Connection.DB := No_Database;
   end Close;

   -------------------
   -- Force_Connect --
   -------------------

   overriding procedure Force_Connect
     (Connection : access Sqlite_Connection_Record)
   is
      Status : Result_Codes;
   begin
      --  With sqlite, we do not need to try and reconnect, since there is no
      --  no network involved. We either have a connection, or not

      if Connection.DB = No_Database then
         Print_Warning
           (Connection,
            "Connecting to sqlite database "
            & Sqlite_Description_Access
              (Get_Description (Connection)).Dbname.all);

         declare
            Name : constant String :=
              Sqlite_Description_Access
                (Get_Description (Connection)).Dbname.all;
         begin
            --  We let sqlite create the database (even an empty one) as
            --  needed. Applications that need to know whether the schema
            --  has been created should either check earlier whether the
            --  file exists, or can use "pragma user_version" to check the
            --  version of the schema.
            Open
              (DB       => Connection.DB,
               Filename => Name,
               Flags    => Open_Readwrite or Open_Create or Open_Nomutex,
               Status   => Status);
            Connection.Connected_On := Ada.Calendar.Clock;

            --  Controls SQLITE_FCNTL_CHUNK_SIZE setting in sqlite. This helps
            --  avoid fragmentation by growing/shrinking the database file in
            --  SQLITE_FCNTL_CHUNK_SIZE increments.

--              File_Control
--                (Connection.DB, "" & ASCII.NUL,
--                 SQLITE_FCNTL_CHUNK_SIZE, 1024 * 1024);

         end;

         if Status /= Sqlite_OK then
            Print_Error
              (Connection,
               "Could not connect to database: " & Error_Msg (Connection.DB));
            Connection.Close;   --  avoid memory leaks
         else
            Mark_As_Closed (Connection, Closed => False);
            Set_Busy_Timeout (Connection.DB, Max_Ms_On_Busy);

            --  Make sure that with appropriate versions of sqlite (>= 3.6.19)
            --  we do enforce foreign keys constraints

            Execute (Connection, "PRAGMA foreign_keys=ON");
         end if;
      end if;
   end Force_Connect;

   ----------------------
   -- Force_Disconnect --
   ----------------------

   overriding procedure Force_Disconnect
     (Connection : access Sqlite_Connection_Record)
   is
      pragma Unreferenced (Connection);
   begin
      --  No network connection involved

      null;
   end Force_Disconnect;

   -------------------------
   -- Connect_And_Prepare --
   -------------------------

   overriding function Connect_And_Prepare
     (Connection : access Sqlite_Connection_Record;
      Query      : String;
      Name       : String;
      Direct     : Boolean) return DBMS_Stmt
   is
      pragma Unreferenced (Direct);
      Stmt   : Statement;
      Status : Result_Codes;
   begin
      --  We cannot prepare direct_cursor, since we are using sqlite3_get_table
      --  and that doesn't provide support for prepared statements
      --  ??? We should not be using sqlite3_get_table, apparently it is being
      --  phased out

      if Query = "" then
         return No_DBMS_Stmt;
      end if;

      Force_Connect (Connection);

      if Connection.DB = No_Database then
         return No_DBMS_Stmt;
      end if;

      Prepare (Connection.DB, Query, Stmt, Status);

      if Active (Me) and then Name /= "" then
         --  The full query was already displayed in Compute_Statement
         Trace (Me, "PREPARE " & Name);
      end if;

      if Status /= Sqlite_OK then
         Trace (Me, "Connect_And_Prepare failed to prepare statement for "
                & Query & ASCII.LF & Error_Msg (Connection.DB));
         Finalize (Stmt);
         return No_DBMS_Stmt;
      end if;

      return Unchecked_Convert (Stmt);
   end Connect_And_Prepare;

   ------------------
   -- Connected_On --
   ------------------

   overriding function Connected_On
     (Connection : access Sqlite_Connection_Record) return Ada.Calendar.Time is
   begin
      return Connection.Connected_On;
   end Connected_On;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Connection  : access Sqlite_Connection_Record;
      Prepared    : DBMS_Stmt;
      Is_Select   : Boolean;
      Direct      : Boolean;
      Params      : SQL_Parameters := No_Parameters)
      return Abstract_Cursor_Access
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Sqlite_Cursor'Class, Sqlite_Cursor_Access);
      Res    : Sqlite_Cursor_Access;
      Res2   : Sqlite_Direct_Cursor_Access;
      Stmt   : Statement;
      Last_Status : Result_Codes;
      Tmp_Data : array (Params'Range) of GNAT.Strings.String_Access;
      Money_Int : Integer;
   begin
      --  Since we have a prepared statement, the connection already exists, no
      --  need to recreate.
      --  We always need to create a forward cursor, which will possibly be
      --  used to initialize the direct cursor.

      Stmt := Unchecked_Convert (Prepared);

      for P in Params'Range loop
         if Params (P) = Null_Parameter then
            Bind_Null (Stmt, P);
         else
            case Params (P).Typ is
            when Parameter_Text =>
               Bind_Text (Stmt, P, Params (P).Str_Val.all'Address,
                          Params (P).Str_Val'Length);
            when Parameter_Json =>
               Bind_Text (Stmt, P, Params (P).Json_Val.all'Address,
                          Params (P).Json_Val'Length);
            when Parameter_XML =>
               Bind_Text (Stmt, P, Params (P).XML_Val.all'Address,
                          Params (P).XML_Val'Length);
            when Parameter_Character =>
               Bind_Text (Stmt, P, Params (P).Char_Val'Address, 1);
            when Parameter_Integer =>
               Bind_Int (Stmt, P, Interfaces.C.int (Params (P).Int_Val));
            when Parameter_Float =>
               Bind_Double
                 (Stmt, P, Interfaces.C.double (Params (P).Float_Val));
            when Parameter_Boolean =>
               Bind_Int
                 (Stmt, P,
                  Interfaces.C.int (Boolean'Pos (Params (P).Bool_Val)));
            when Parameter_Time =>
               Tmp_Data (P) := new String'
                 (Time_To_SQL
                    (Connection.all, Params (P).Time_Val, Quote => False));
               Bind_Text
                 (Stmt, P, Tmp_Data (P).all'Address, Tmp_Data (P)'Length);

            when Parameter_Date =>
               Tmp_Data (P) := new String'
                 (Date_To_SQL
                    (Connection.all, Params (P).Time_Val, Quote => False));
               Bind_Text
                 (Stmt, P, Tmp_Data (P).all'Address, Tmp_Data (P)'Length);

            when Parameter_Money =>
               --  In SQLite, Money type will be mapped as integer
               Money_Int := Integer (Params (P).Money_Val / K_Delta);
               Bind_Int (Stmt, P, Interfaces.C.int (Money_Int));
            end case;
         end if;
      end loop;

      Step (Stmt, Last_Status);

      --  Free the memory we just allocated. We should ideally clear the
      --  bindings at this stage, but:
      --     using Bind_Null is forbidden because the statement is executing
      --     Clear_Bindings will be called automatically when the statement
      --       is Finalized anyway.

      for P in Params'Range loop
         case Params (P).Typ is
            when Parameter_Time | Parameter_Date =>
               Free (Tmp_Data (P));

            when others =>
               null;
         end case;
      end loop;

      case Last_Status is
         when Sqlite_OK | Sqlite_Row | Sqlite_Done =>
            Res := new Sqlite_Cursor;
            Res.Stmt := Stmt;
            Res.DB := Connection;
            Res.Free_Stmt := False;
            Res.Last_Status := Last_Status;

            if Is_Select then
               Res.Processed_Rows := 0;
            else
               Res.Processed_Rows := Changes (Connection.DB);
            end if;

         when Sqlite_Corrupt =>
            Report_Database_Corrupted (Connection);
            return null;

         when others =>
            Print_Warning
              (Connection,
               "Error while executing query, status="
               & Last_Status'Img);
            return null;
      end case;

      if not Direct then
         return Abstract_Cursor_Access (Res);
      end if;

      --  For direct cursors, we now need to actually read all the results, and
      --  store them in memory

      Res2 := new Sqlite_Direct_Cursor;
      Res2.Initialize (Sqlite_Cursor (Res.all)'Access);
      Res.Stmt := No_Statement;
      Res.DB := null;
      Unchecked_Free (Res);

      return Abstract_Cursor_Access (Res2);
   end Execute;

   -------------------------
   -- Connect_And_Execute --
   -------------------------

   overriding function Connect_And_Execute
     (Connection  : access Sqlite_Connection_Record;
      Is_Select   : Boolean;
      Direct      : Boolean;
      Query       : String         := "";
      Stmt        : DBMS_Stmt      := No_DBMS_Stmt;
      Params      : SQL_Parameters := No_Parameters)
      return Abstract_Cursor_Access
   is
      Res    : Abstract_Cursor_Access := null;
      P_Stmt : DBMS_Stmt := Stmt;
   begin
      if Stmt = No_DBMS_Stmt then
         P_Stmt := Connect_And_Prepare (Connection, Query, "", Direct);
      else
         P_Stmt := Stmt;
      end if;

      if P_Stmt /= No_DBMS_Stmt then
         Res := Execute
           (Connection => Connection,
            Prepared   => P_Stmt,
            Is_Select  => Is_Select,
            Direct     => Direct,
            Params     => Params);

         --  If the statement was prepared locally (Stmt = No_DBMS_Stmt) then
         --  finalize it now if needed.

         if Res /= null then
            if Res.all in Sqlite_Direct_Cursor'Class then
               Get_Cursor (Sqlite_Direct_Cursor_Access (Res).all).Free_Stmt :=
                 Stmt = No_DBMS_Stmt;
            else
               Sqlite_Cursor_Access (Res).Free_Stmt := Stmt = No_DBMS_Stmt;
            end if;

         elsif Stmt = No_DBMS_Stmt then
            --  P_Stmt is no longer accessible, and yet if we don't finalize it
            --  we are in effect keeping a transaction (or read transaction)
            --  open.
            Finalize (Connection, P_Stmt);
         end if;
      end if;

      return Res;
   end Connect_And_Execute;

   --------------------
   -- Processed_Rows --
   --------------------

   overriding function Processed_Rows (Self : Sqlite_Cursor) return Natural is
   begin
      return Self.Processed_Rows;
   end Processed_Rows;

   -----------
   -- Value --
   -----------

   overriding function Value
     (Self  : Sqlite_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return String is
   begin
      return Column_Text (Self.Stmt, Natural (Field));
   end Value;

   -------------
   -- C_Value --
   -------------

   overriding function C_Value
     (Self  : Sqlite_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return chars_ptr is
   begin
      return Column_C_Text (Self.Stmt, Natural (Field));
   end C_Value;

   -------------
   -- Is_Null --
   -------------

   overriding function Is_Null
     (Self  : Sqlite_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return Boolean is
   begin
      return Column_Type (Self.Stmt, Natural (Field)) = Sqlite_Null;
   end Is_Null;

   -------------
   -- Last_Id --
   -------------

   overriding function Last_Id
     (Self       : Sqlite_Cursor;
      Connection : access Database_Connection_Record'Class;
      Field      : SQL_Field_Integer) return Integer
   is
      pragma Unreferenced (Self, Field);
--        Res2     : Forward_Cursor;
   begin
      --  According to sqlite3 documentation, the last_rowid is also the
      --  primary key when the latter is a single integer primary key (which is
      --  the case here).
      --  ??? We assume here that Field is the primary key, but we cannot
      --  check that.

      return Integer (Last_Insert_Rowid
        (Sqlite_Connection_Record (Connection.all).DB));

      --  If we wanted to support multi-key primary keys, for instance, we
      --  would use:
--        Res2.Fetch
--          (Connection,
--           "SELECT " & Field.To_String (Connection.all, Long => True)
--           & " FROM " & Field.Table.all
--           & " WHERE ROWID="
--           & Long_Integer'Image (Self.Last_Rowid));
--        if Has_Row (Res2) then
--           return Integer_Value (Res2, 0);
--        end if;
--        return -1;
   end Last_Id;

   -----------------
   -- Field_Count --
   -----------------

   overriding function Field_Count
     (Self : Sqlite_Cursor) return GNATCOLL.SQL.Exec.Field_Index is
   begin
      return Field_Index (Column_Count (Self.Stmt));
   end Field_Count;

   ----------------
   -- Field_Name --
   ----------------

   overriding function Field_Name
     (Self  : Sqlite_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return String is
   begin
      return Column_Name (Self.Stmt, Natural (Field));
   end Field_Name;

   -------------------
   -- Foreach_Table --
   -------------------

   overriding procedure Foreach_Table
     (Connection : access Sqlite_Connection_Record;
      Callback   : access procedure
        (Name, Description : String; Kind : Relation_Kind))
   is
      R     : Forward_Cursor;
      Kind  : Relation_Kind;
   begin
      R.Fetch
        (Connection, "SELECT name, type FROM sqlite_master ORDER BY name");
      while Has_Row (R) loop
         if Value (R, 1) = "table" then
            Kind := Kind_Table;
         else
            Kind := Kind_View;
         end if;

         Callback (Name => Value (R, 0), Description => "", Kind => Kind);
         Next (R);
      end loop;
   end Foreach_Table;

   -------------------
   -- Is_Whitespace --
   -------------------

   function Is_Whitespace (C : Character) return Boolean is
   begin
      return C = ' '
        or else C = ASCII.HT
        or else C = ASCII.LF
        or else C = ASCII.CR;
   end Is_Whitespace;

   procedure Skip_Whitespace (S : String; Pos : in out Integer) is
   begin
      while Pos <= S'Last and then Is_Whitespace (S (Pos)) loop
         Pos := Pos + 1;
      end loop;
   end Skip_Whitespace;

   procedure Skip_To_Whitespace (S : String; Pos : in out Integer) is
   begin
      while Pos <= S'Last and then not Is_Whitespace (S (Pos)) loop
         Pos := Pos + 1;
      end loop;
   end Skip_To_Whitespace;

   -------------------
   -- Foreach_Field --
   -------------------

   overriding procedure Foreach_Field
     (Connection : access Sqlite_Connection_Record;
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
      R           : Forward_Cursor;
      Index       : Natural := 1;
      Paren_Count : Natural;
      Is_PK       : Boolean;
      Is_Not_Null : Boolean;
   begin
      R.Fetch
        (Connection,
         "SELECT sql FROM sqlite_master WHERE name='" & Table_Name & "'");

      while Has_Row (R) loop
         --  Crude parsing of the sql

         declare
            Sql : constant String := Value (R, 0);
            Pos, Pos2, Pos3, Pos4, Pos5 : Integer := Sql'First;
         begin
            while Pos <= Sql'Last and then Sql (Pos) /= '(' loop
               Pos := Pos + 1;
            end loop;

            Pos := Pos + 1;
            while Pos <= Sql'Last loop
               Skip_Whitespace (Sql, Pos);
               Pos2 := Pos;  --  First char of name

               Skip_To_Whitespace (Sql, Pos);
               Pos3 := Pos - 1;  -- Last char of name

               Skip_Whitespace (Sql, Pos);
               Pos4 := Pos; --  First char of type

               Paren_Count := 0;

               while Pos <= Sql'Last
                 and then not Is_Whitespace (Sql (Pos))
                 and then Sql (Pos) /= ','
                 and then (Sql (Pos) /= ')'
                           or else Paren_Count /= 0)
               loop
                  if Sql (Pos) = '(' then
                     Paren_Count := Paren_Count + 1;
                  elsif Sql (Pos) = ')' then
                     Paren_Count := Paren_Count - 1;
                  end if;

                  Pos := Pos + 1;
               end loop;

               Pos5 := Pos;

               while Pos <= Sql'Last
                 and then
                   (Paren_Count /= 0
                    or else (Sql (Pos) /= ',' and then Sql (Pos) /= ')'))
               loop
                  if Sql (Pos) = '(' then
                     Paren_Count := Paren_Count + 1;
                  elsif Sql (Pos) = ')' then
                     Paren_Count := Paren_Count - 1;
                  end if;
                  Pos := Pos + 1;
               end loop;

               Is_PK := Ada.Strings.Fixed.Index
                 (To_Lower (Sql (Pos2 .. Pos)), "primary key") >= 1;

               Is_Not_Null := Ada.Strings.Fixed.Index
                 (To_Lower (Sql (Pos2 .. Pos)), "not null") >= 1;

               --  Ignore constraints declarations

               if To_Lower (Sql (Pos2 .. Pos3)) /= "constraint" then
                  Callback
                    (Name           => Sql (Pos2 .. Pos3),
                     Typ            => Sql (Pos4 .. Pos5 - 1),
                     Index          => Index,
                     Description    => "",
                     Default_Value  => "",  --  ??? Should be specified
                     Not_Null       => Is_Not_Null,
                     Is_Primary_Key => Is_PK);
               end if;

               Pos := Pos + 1;
            end loop;
         end;

         Index := Index + 1;
         Next (R);
      end loop;
   end Foreach_Field;

   -------------------------
   -- Foreach_Foreign_Key --
   -------------------------

   procedure Foreach_Foreign_Key
     (Connection : access Sqlite_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Index             : Positive;
         Local_Attribute   : Integer;
         Foreign_Table     : String;
         Foreign_Attribute : Integer))
   is
      pragma Unreferenced (Connection, Table_Name, Callback);
   begin
      --  Unsupported for now (sqlite does not support them anyway)
      --  We could potentially parse the sql for "create table"
      null;
   end Foreach_Foreign_Key;

   -------------
   -- Has_Row --
   -------------

   overriding function Has_Row (Self : Sqlite_Cursor) return Boolean is
   begin
      return Self.Last_Status = Sqlite_Row;
   end Has_Row;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Sqlite_Cursor) is
   begin
      Step (Self.Stmt, Self.Last_Status);
      Self.Processed_Rows := Self.Processed_Rows + 1;
   end Next;

   -------------
   -- Current --
   -------------

   overriding function Current (Self : Sqlite_Cursor) return Positive is
   begin
      return Self.Processed_Rows + 1;
   end Current;

   ------------------------
   -- Has_Sqlite_Support --
   ------------------------

   function Has_Sqlite_Support return Boolean is
   begin
      return True;
   end Has_Sqlite_Support;

   ----------------------
   -- Build_Connection --
   ----------------------

   function Build_Connection
     (Descr : access Sqlite_Description'Class) return Database_Connection
   is
   begin
      return new Sqlite_Connection_Record
        (Descr,
         Always_Use_Transactions => Sqlite_Always_Use_Transactions);
   end Build_Connection;

   -----------------------
   -- Supports_Timezone --
   -----------------------

   overriding function Supports_Timezone
     (Self  : Sqlite_Connection_Record) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Supports_Timezone;

   ----------------------
   -- Parameter_String --
   ----------------------

   overriding function Parameter_String
     (Self  : Sqlite_Connection_Record;
      Index : Positive;
      Typ   : Parameter_Type) return String
   is
      pragma Unreferenced (Self, Typ);
   begin
      return '?' & Image (Index, 0);
   end Parameter_String;

   -------------------
   -- Boolean_Image --
   -------------------

   overriding function Boolean_Image
     (Self : Sqlite_Connection_Record; Value : Boolean) return String
   is
      pragma Unreferenced (Self);
   begin
      if Value then
         return "1";
      else
         return "0";
      end if;
   end Boolean_Image;

   -------------------
   -- Boolean_Value --
   -------------------

   overriding function Boolean_Value
     (Self : Sqlite_Cursor; Field : Field_Index) return Boolean
   is
   begin
      return Value (Sqlite_Cursor'Class (Self), Field) /= "0";
   end Boolean_Value;

   -----------------
   -- Money_Value --
   -----------------

   overriding function Money_Value
      (Self  : Sqlite_Cursor;
       Field : Field_Index) return T_Money is
   begin
      return T_Money'Value
         (Value (Sqlite_Cursor'Class (Self), Field)) * K_Delta;
   end Money_Value;

   ------------------------------
   -- Field_Type_Autoincrement --
   ------------------------------

   overriding function Field_Type_Autoincrement
     (Self : Sqlite_Connection_Record) return String
   is
      pragma Unreferenced (Self);
   begin
      return "INTEGER PRIMARY KEY AUTOINCREMENT";
   end Field_Type_Autoincrement;

   ----------------------
   -- Field_Type_Money --
   ----------------------

   overriding function Field_Type_Money
     (Self : Sqlite_Connection_Record) return String
   is
      pragma Unreferenced (Self);
   begin
      --  Note : As SQLite does not support fixed point real, Money type is
      --  represented as an integer that modelize cents.
      return "Integer";
   end Field_Type_Money;

   -----------------
   -- Money_Image --
   -----------------

   overriding function Money_Image
     (Self : Sqlite_Connection_Record; Value : T_Money) return String
   is
      pragma Unreferenced (Self);
      Long_Value : constant Long_Integer := Long_Integer (Value / K_Delta);
      Img : constant String := Long_Integer'Image (Long_Value);
   begin
      if Img (Img'First) = ' ' then
         return Img (Img'First + 1 .. Img'Last);
      else
         return Img;
      end if;
   end Money_Image;

   ---------------------------------
   -- Can_Alter_Table_Constraints --
   ---------------------------------

   overriding function Can_Alter_Table_Constraints
     (Self : access Sqlite_Connection_Record) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Can_Alter_Table_Constraints;

   -----------------
   -- Has_Pragmas --
   -----------------

   overriding function Has_Pragmas
     (Self : access Sqlite_Connection_Record) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return True;
   end Has_Pragmas;

   ------------
   -- Logger --
   ------------

   procedure Logger
     (Data       : System.Address;
      Error_Code : Result_Codes;
      Message    : Interfaces.C.Strings.chars_ptr)
   is
      pragma Unreferenced (Data);
   begin
      if Active (Me_Log) then
         Trace (Me_Log, Error_Code'Img & " " & Value (Message));
      end if;
   end Logger;

   -----------
   -- Setup --
   -----------

   procedure Setup is
   begin
      Set_Config_Memstatus (Collect_Stats => False);
      Set_Config_Log (Logger'Access);
   end Setup;

   ------------
   -- Backup --
   ------------

   function Backup
     (DB1 : access Database_Connection_Record'Class;
      DB2 : String;
      From_DB1_To_DB2 : Boolean := True) return Boolean
   is
      To     : Database_Connection;
      Result : Boolean := True;
   begin
      To := GNATCOLL.SQL.Sqlite.Setup (DB2).Build_Connection;
      To.Force_Connect;
      DB1.Force_Connect;

      if From_DB1_To_DB2 then
         Result := Backup (DB1, To);
      else
         Result := Backup (To, DB1);
      end if;

      Close (To);
      return Result;
   end Backup;

   ------------
   -- Backup --
   ------------

   function Backup
     (From : access Database_Connection_Record'Class;
      To   : access Database_Connection_Record'Class) return Boolean
   is
      Status  : Result_Codes;
      Bkp     : Sqlite3_Backup;
      Result  : Boolean := True;
   begin
      Bkp := Backup_Init
        (Pdest        => Sqlite_Connection_Record (To.all).DB,
         Pdest_Name   => "main",
         Psource      => Sqlite_Connection_Record (From.all).DB,
         Psource_Name => "main");

      if System.Address (Bkp) = System.Null_Address then
         Trace (Me_Log, "failed to create the backup object");
         return False;
      end if;

      Status := Backup_Step (Bkp, -1);
      if Status /= Sqlite_Done then
         Trace (Me_Log, Status'Img & " Error in Backup_Step "
                & Error (From));
         Result := False;
      end if;

      if Backup_Finish (Bkp) /= Sqlite_OK then
         Trace (Me_Log, "Error in Backup_Finish");
         Result := False;
      end if;

      return Result;
   end Backup;

end GNATCOLL.SQL.Sqlite.Builder;
