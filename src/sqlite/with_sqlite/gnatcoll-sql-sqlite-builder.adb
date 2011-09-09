-----------------------------------------------------------------------
--                               G N A T C O L L                     --
--                                                                   --
--                 Copyright (C) 2005-2011, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- As a special exception, if other files instantiate generics  from --
-- this unit, or you link this  unit with other files to produce  an --
-- executable, this unit does not by itself cause the resulting exe- --
-- cutable  to be covered by  the  GNU General  Public License. This --
-- exception does not however  invalidate any other reasons why  the --
-- executable  file  might  be  covered  by  the  GNU General Public --
-- License.                                                          --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Characters.Handling;      use Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNATCOLL.SQL.Sqlite.Gnade;    use GNATCOLL.SQL.Sqlite.Gnade;
with GNATCOLL.SQL.Exec_Private;    use GNATCOLL.SQL.Exec_Private;
with GNATCOLL.Traces;              use GNATCOLL.Traces;
with GNATCOLL.Utils;               use GNATCOLL.Utils;
with GNAT.Calendar;
with GNAT.OS_Lib;
with Interfaces.C.Strings;         use Interfaces.C.Strings;
with System;

package body GNATCOLL.SQL.Sqlite.Builder is
   Me : constant Trace_Handle := Create ("SQL.LITE");

   Empty_C_Str : aliased String := "" & ASCII.NUL;

   type Sqlite_Cursor is new DBMS_Forward_Cursor with record
      Stmt           : Statement;

      Free_Stmt      : Boolean := False;
      --  Whether the statement needs to be finalized; This will be false for
      --  a statement prepared explicitly by the user on the server.

      Processed_Rows : Natural := 0;
      Last_Status    : Result_Codes;  --  Last status of Step
      Last_Rowid     : Long_Integer := -1;
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

   package Direct_Cursors is new Generic_Direct_Cursors (Sqlite_Cursor);
   type Sqlite_Direct_Cursor is new Direct_Cursors.Direct with null record;
   type Sqlite_Direct_Cursor_Access is access all Sqlite_Direct_Cursor'Class;

   type Sqlite_Connection_Record is
     new GNATCOLL.SQL.Exec.Database_Connection_Record with
      record
         DB           : GNATCOLL.SQL.Sqlite.Gnade.Database;
         Connected_On : Ada.Calendar.Time := GNAT.Calendar.No_Time;
      end record;
   overriding function Boolean_Image
     (Self : Sqlite_Connection_Record; Value : Boolean) return String;
   overriding function Parameter_String
     (Self  : Sqlite_Connection_Record;
      Index : Positive;
      Typ   : Parameter_Type) return String;
   overriding procedure Close
     (Connection : access Sqlite_Connection_Record);
   overriding function Field_Type_Autoincrement
     (Self : Sqlite_Connection_Record) return String;
   overriding function Can_Alter_Table_Constraints
     (Self : access Sqlite_Connection_Record) return Boolean;
   overriding function Connect_And_Execute
     (Connection  : access Sqlite_Connection_Record;
      Query       : String;
      Is_Select   : Boolean;
      Direct      : Boolean;
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

   procedure Connect_If_Needed
     (Connection : access Sqlite_Connection_Record'Class);
   --  Connect to the database if needed

   function Is_Whitespace (C : Character) return Boolean;
   --  Whether C is a white space character

   procedure Skip_Whitespace (S : String; Pos : in out Integer);
   procedure Skip_To_Whitespace (S : String; Pos : in out Integer);
   --  Skip to or until the next whitespace character. Pos is left on the
   --  first whitespace character or on the first character after the spaces

   function On_Busy (Data : System.Address; Count : Integer) return Integer;
   pragma Convention (C, On_Busy);

   function Unchecked_Convert is new Ada.Unchecked_Conversion
     (Statement, DBMS_Stmt);
   function Unchecked_Convert is new Ada.Unchecked_Conversion
     (DBMS_Stmt, Statement);

   -------------
   -- On_Busy --
   -------------

   function On_Busy (Data : System.Address; Count : Integer) return Integer is
      pragma Unreferenced (Data);
   begin
      --  Retry commands up to 5 times automatically, in case we get a
      --  SQLITE_BUSY status.
      if Count < 300 then
         if Active (Me) then
            Trace (Me, "Received SQLITE_BUSY, trying again. Attempt="
                   & Count'Img);
         end if;
         delay 0.3;
         return 1;
      else
         return 0;
      end if;
   end On_Busy;

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
      if Self.Stmt /= No_Statement then
         if Self.Free_Stmt then
            Finalize (Self.Stmt);
         else
            --  We used to reset Self.Stmt here.
            --  But this is in fact dangerous: the statement is in fact the
            --  same DBMS_Stmt that is stored in
            --  gnatcoll.sql.exec.prepared_in_session_stmt. If the latter was
            --  finalized before Self is finalized, we end up with a storage
            --  error because we are referencing freed memory.
            --  Instead, Reset is called when we initialize the query. At worse
            --  this means we are keeping a bit of memory allocated in sqlite
            --  for the DBMS_Stmt for the prepared statements, even though we
            --  will not call Step() again. But this isn't a memory leak since
            --  the memory will be freed if the prepared statement is finalized

            Clear_Bindings (Self.Stmt);
            Status := Reset (Self.Stmt);
            if Status /= Sqlite_OK then
               Trace (Me, "Error when reseting cursor to free LOCKS: "
                      & Status'Img);
            end if;
         end if;
         Self.Stmt := No_Statement;
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
      Close (Connection.DB);
   end Close;

   -----------------------
   -- Connect_If_Needed --
   -----------------------

   procedure Connect_If_Needed
     (Connection : access Sqlite_Connection_Record'Class)
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

         --  Make sure the file exists, otherwise we'll get a storage_error.
         --  In some cases, the user will want to create a new database from
         --  scratch, so creating an empty file is valid.

         declare
            Name : constant String :=
              Sqlite_Description_Access
                (Get_Description (Connection)).Dbname.all;
            FD   : GNAT.OS_Lib.File_Descriptor;
            Tmp  : Integer;
            pragma Unreferenced (Tmp);
         begin
            Open
              (DB       => Connection.DB,
               Filename => Name,
               Flags    => Open_Readwrite,
               Status   => Status);
            Connection.Connected_On := Ada.Calendar.Clock;

            if Status = Sqlite_Cantopen then
               Trace (Me, "Create empty db file");

               FD  := GNAT.OS_Lib.Create_File (Name, GNAT.OS_Lib.Binary);
               Tmp := GNAT.OS_Lib.Write (FD, Empty_C_Str'Address, 0);
               GNAT.OS_Lib.Close (FD);

               Open
                 (DB       => Connection.DB,
                  Filename => Name,
                  Flags    => Open_Readwrite,
                  Status   => Status);
            end if;
         end;

         if Status /= Sqlite_OK then
            Print_Error
              (Connection,
               "Could not connect to database: " & Error_Msg (Connection.DB));
            Connection.DB := No_Database;
         else
            --  Add a busy handler, which will automatically attempt to
            --  re-attempt a command if SQLITE_BUSY was returned.

            Busy_Handler (Connection.DB, On_Busy'Access);

            --  Make sure that with appropriate versions of sqlite (>= 3.6.19)
            --  we do enforce foreign keys constraints

            Execute (Connection, "PRAGMA foreign_keys=ON");
         end if;
      end if;
   end Connect_If_Needed;

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

      Connect_If_Needed (Connection);

      if Connection.DB = No_Database then
         return No_DBMS_Stmt;
      end if;

      Prepare (Connection.DB, Query, Stmt, Status);

      if Active (Me) and then Name /= "" then
         --  The full query was already displayed in Compute_Statement
         Trace (Me, "PREPARE " & Name);
      end if;

      if Status /= Sqlite_OK then
         Trace (Me, "Connect_And_Execute failed to prepare statement for "
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
               declare
                  Str : aliased String :=
                    Time_To_SQL (Connection.all, Params (P).Time_Val,
                                 Quote => False);
               begin
                  Bind_Text (Stmt, P, Str'Address, Str'Length);
               end;

            when Parameter_Date =>
               declare
                  Str : aliased String :=
                    Date_To_SQL (Connection.all, Params (P).Date_Val,
                                 Quote => False);
               begin
                  Bind_Text (Stmt, P, Str'Address, Str'Length);
               end;
            end case;
         end if;
      end loop;

      Step (Stmt, Last_Status);

      --  Reset text parameters, since the string they point to will rapidly
      --  be out of scope

      for P in Params'Range loop
         case Params (P).Typ is
            when Parameter_Text =>
               Bind_Null (Stmt, P);
            when others =>
               null;
         end case;
      end loop;

      case Last_Status is
         when Sqlite_OK | Sqlite_Row | Sqlite_Done =>
            Res := new Sqlite_Cursor;
            Res.Stmt := Stmt;
            Res.Free_Stmt := False;
            Res.Last_Status := Last_Status;

            if Is_Select then
               Res.Processed_Rows := 0;
            else
               Res.Processed_Rows := Changes (Connection.DB);
               Res.Last_Rowid := Last_Insert_Rowid (Connection.DB);
            end if;

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
      Unchecked_Free (Res);

      return Abstract_Cursor_Access (Res2);
   end Execute;

   -------------------------
   -- Connect_And_Execute --
   -------------------------

   overriding function Connect_And_Execute
     (Connection  : access Sqlite_Connection_Record;
      Query       : String;
      Is_Select   : Boolean;
      Direct      : Boolean;
      Params      : SQL_Parameters := No_Parameters)
      return Abstract_Cursor_Access
   is
      Res    : Abstract_Cursor_Access;
      Stmt   : DBMS_Stmt;
   begin
      Stmt := Connect_And_Prepare (Connection, Query, "", Direct);

      if Stmt /= No_DBMS_Stmt then
         Res := Execute
           (Connection => Connection,
            Prepared   => Stmt,
            Is_Select  => Is_Select,
            Direct     => Direct,
            Params     => Params);

         if Res /= null then
            if Res.all in Sqlite_Direct_Cursor'Class then
               Get_Cursor (Sqlite_Direct_Cursor_Access (Res).all).Free_Stmt :=
                 True;
            else
               Sqlite_Cursor_Access (Res).Free_Stmt := True;
            end if;
         end if;
      else
         return null;
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
      pragma Unreferenced (Connection, Field);
--        Res2     : Forward_Cursor;
   begin
      --  According to sqlite3 documentation, the last_rowid is also the
      --  primary key when the latter is a single integer primary key (which is
      --  the case here).
      --  ??? We assume here that Field is the primary key, but we cannot
      --  check that.

      if Integer (Self.Last_Rowid) /= 0 then
         return Integer (Self.Last_Rowid);
      else
         return -1;
      end if;

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
      Index       : Natural := 0;
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

end GNATCOLL.SQL.Sqlite.Builder;
