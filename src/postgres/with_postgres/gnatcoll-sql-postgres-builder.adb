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

with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNATCOLL.SQL.Postgres.Gnade;  use GNATCOLL.SQL.Postgres.Gnade;
with GNATCOLL.SQL.Exec_Private;    use GNATCOLL.SQL.Exec_Private;
with GNATCOLL.Traces;              use GNATCOLL.Traces;
with GNATCOLL.Utils;               use GNATCOLL.Utils;
with GNAT.Strings;                 use GNAT.Strings;
with Interfaces.C.Strings;         use Interfaces.C.Strings;
with System.Storage_Elements;      use System.Storage_Elements;

package body GNATCOLL.SQL.Postgres.Builder is
   Me_Query  : constant Trace_Handle := Create ("SQL");

   Use_Cursors : constant Boolean := False;
   --  Whether to use "DECLARE name CURSOR ..." to use cursors for Forward
   --  cursors. Although this might save some memory since we do not have to
   --  have all results in memory, this is in fact *much* slower, so is
   --  disabled for now. Possible improvements would be to fetch several rows
   --  at once in the cursor, but even that does not seem to improve things too
   --  much.

   type Cursor_Id is new Natural;
   type Database_Access is access GNATCOLL.SQL.Postgres.Gnade.Database;

   type Postgresql_Connection_Record is
     new GNATCOLL.SQL.Exec.Database_Connection_Record with
      record
         Connection_String : GNAT.Strings.String_Access;
         Postgres          : Database_Access;
         Cursor            : Cursor_Id := 0;  --  Id for the current cursor
      end record;
   type Postgresql_Connection is access all Postgresql_Connection_Record'Class;
   overriding procedure Close
     (Connection : access Postgresql_Connection_Record);
   overriding function Connect_And_Execute
     (Connection  : access Postgresql_Connection_Record;
      Query       : String;
      Is_Select   : Boolean;
      Direct      : Boolean) return Abstract_Cursor_Access;
   overriding function Connect_And_Prepare
     (Connection : access Postgresql_Connection_Record;
      Query      : String;
      Id         : Stmt_Id;
      Direct     : Boolean)
      return DBMS_Stmt;
   overriding function Execute
     (Connection  : access Postgresql_Connection_Record;
      Prepared    : DBMS_Stmt;
      Is_Select   : Boolean;
      Direct      : Boolean) return Abstract_Cursor_Access;
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
      Stmt       : Stmt_Id := No_Stmt_Id;  --  Prepared statement, if any
      Cursor     : Cursor_Id := 0;  --  the associated DBMS cursor
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

   function Cursor_Name (Stmt : Stmt_Id; Cursor : Cursor_Id) return String;
   --  Name of cursor on DBMS for Self

   function Declare_Cursor
     (Query : String; Stmt : Stmt_Id; Cursor : Cursor_Id) return String;
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
      with procedure Perform (Res : out Result; Query : String);
   procedure Connect_And_Do
     (Connection : access Postgresql_Connection_Record;
      Query      : String;
      Res        : out Result;
      Success    : out Boolean);
   --  (Re)connect to the database if needed, and perform the action. If the
   --  result of the action is successfull (as per exec status in Res), Success
   --  is set to True and Res to the last result. Otherwise, Success is set to
   --  False.

   -------------------------------
   -- Build_Postgres_Connection --
   -------------------------------

   function Build_Postgres_Connection return Database_Connection is
   begin
      return new Postgresql_Connection_Record;
   end Build_Postgres_Connection;

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
      User   : constant String := Get_User (Description);
      Host   : constant String := Get_Host (Description);
      Passwd : constant String := Get_Password (Description);

      Str : Unbounded_String  := To_Unbounded_String
        ("dbname=" & Get_Database (Description));
   begin
      if User /= "" then
         Append (Str, " user=" & User);
      end if;

      if Host /= "" then
         Append (Str, " host=" & Host);
      end if;

      if With_Password and then Passwd /= "" then
         Append (Str, " password=" & Passwd);
      end if;

      Append (Str, " requiressl=0");

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

   --------------------
   -- Connect_And_Do --
   --------------------

   procedure Connect_And_Do
     (Connection : access Postgresql_Connection_Record;
      Query      : String;
      Res        : out Result;
      Success    : out Boolean)
   is
   begin
      if Connection.Postgres /= null then
         Clear (Res);

         if Query /= "" then
            Perform (Res, Query);
         end if;

         case ExecStatus'(Status (Res)) is
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
               else
                  Print_Warning
                    (Connection,
                     "DB status is " & Status (Connection.Postgres.all)'Img);
               end if;

            when others =>
               Success := True;
               return;
         end case;
      end if;

      Clear (Res);

      --  Attempt to reconnect, in case we lost the connection

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
      else
         Print_Warning
           (Connection,
            "Reconnecting to the database "
            & Get_Connection_String (Get_Description (Connection), False));
         Reset (Connection.Postgres.all);
      end if;

      --  Now that we have (re)connected, try to execute the query again

      if Query = "" then
         Success := Status (Connection.Postgres.all) = CONNECTION_OK;
         if not Success then
            Print_Error
              (Connection, "Cannot connect to PostgreSQL database "
               & " Connection String is """
               & Get_Connection_String
                 (Get_Description (Connection), False)
               & """");
            Close (Connection);
            Connection.Postgres := null;
         end if;
         return;

      else
         Perform (Res, Query);

         case ExecStatus'(Status (Res)) is
            when PGRES_NONFATAL_ERROR
               | PGRES_FATAL_ERROR
               | PGRES_EMPTY_QUERY =>

               Success := False;

               --  We do not check the connection status here. Ideally, we
               --  should check whether Res.Res (private) is a Null_Result,
               --  which postgreSQL uses to indicate fatal errors like
               --  connection issues.

            when others =>
               Success := True;
         end case;
      end if;
   end Connect_And_Do;

   -----------------
   -- Cursor_Name --
   -----------------

   function Cursor_Name (Stmt : Stmt_Id; Cursor : Cursor_Id) return String is
   begin
      if Stmt /= No_Stmt_Id then
         return "pcursor" & Image (Integer (Stmt), Min_Width => 0);
      else
         return "cursor" & Image (Integer (Cursor), Min_Width => 0);
      end if;
   end Cursor_Name;

   --------------------
   -- Declare_Cursor --
   --------------------

   function Declare_Cursor
     (Query : String; Stmt : Stmt_Id; Cursor : Cursor_Id) return String is
   begin
      return "DECLARE " & Cursor_Name (Stmt, Cursor)
        & " SCROLL CURSOR FOR " & Query;
   end Declare_Cursor;

   -------------------------
   -- Connect_And_Prepare --
   -------------------------

   overriding function Connect_And_Prepare
     (Connection : access Postgresql_Connection_Record;
      Query      : String;
      Id         : Stmt_Id;
      Direct     : Boolean)
      return DBMS_Stmt
   is
      procedure Perform (Res : out Result; Query : String);
      procedure Perform (Res : out Result; Query : String) is
         Name : constant String :=
           "stmt" & Image (Integer (Id), Min_Width => 0);
      begin
         if Active (Me_Query) then
            Trace (Me_Query, "Prepare " & Name & ": " & Query);
         end if;

         Prepare (Res, Connection.Postgres.all, Name, Query);
      end Perform;

      procedure Do_Perform is new Connect_And_Do (Perform);

      Res : Result;
      Success : Boolean;
      Was_Started : Boolean;
      pragma Unreferenced (Was_Started);
   begin
      if Direct or else not Use_Cursors then
         Do_Perform (Connection, Query, Res, Success);
      else
         Was_Started := Start_Transaction (Connection);
         Do_Perform
           (Connection,
            Declare_Cursor (Query, Id, Connection.Cursor), Res, Success);
      end if;

      if Success then
         Clear (Res);
         return DBMS_Stmt (To_Address (Integer_Address (Id)));
      else
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
      Direct      : Boolean) return Abstract_Cursor_Access
   is
      R   : Postgresql_Cursor_Access;
      DR  : Postgresql_Direct_Cursor_Access;
      Res : Result;
      Id  : constant Stmt_Id := Stmt_Id
        (To_Integer (System.Address (Prepared)));
      Name : constant String := "stmt" & Image (Integer (Id), Min_Width => 0);
   begin
      --  For a direct_cursor, this will execute the query. For a
      --  forward_cursor this will declare the cursor on the DBMS

      Exec_Prepared (Res, Connection.Postgres.all, Name);

      if Direct or not Use_Cursors then
         DR := new Postgresql_Direct_Cursor;
         DR.Res := Res;
         Post_Execute_And_Log (DR, Connection, "Exec prepared " & Name, False);

         if Is_Select then
            DR.Rows := Natural (Tuple_Count (Res));
         else
            DR.Rows := Natural'(Command_Tuples (Res));
         end if;

         return Abstract_Cursor_Access (DR);

      else
         R            := new Postgresql_Cursor;
         R.Connection := Postgresql_Connection (Connection);
         R.Res        := Res;
         R.Stmt       := Id;
         Post_Execute_And_Log (R, Connection, "Exec prepared " & Name, False);
         Next (R.all);  --  Read first row
         return Abstract_Cursor_Access (R);
      end if;
   end Execute;

   -------------------------
   -- Connect_And_Execute --
   -------------------------

   function Connect_And_Execute
     (Connection  : access Postgresql_Connection_Record;
      Query       : String;
      Is_Select   : Boolean;
      Direct      : Boolean) return Abstract_Cursor_Access
   is
      procedure Perform (Res : out Result; Query : String);
      procedure Perform (Res : out Result; Query : String) is
      begin
         Execute (Res, Connection.Postgres.all, Query);

--           if Is_Select then
--              Trace (Me_Select, Query & " => " & Status (Res));
--           else
--              Trace (Me_Query, Query & " => " & Status (Res));
--           end if;
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
        or else Query = "";
   begin
      if Create_Direct then
         DR := new Postgresql_Direct_Cursor;
      else
         R            := new Postgresql_Cursor;
         R.Connection := Postgresql_Connection (Connection);
      end if;

      if Create_Direct then
         Do_Perform (Connection, Query, Res, Success);
      else
         R.Nested_Transactions := Start_Transaction (Connection);
         R.Cursor   := Connection.Cursor;
         Connection.Cursor := Connection.Cursor + 1;  --  ??? Concurrency ?
         Do_Perform
           (Connection,
            Declare_Cursor (Query, No_Stmt_Id, R.Cursor),
            Res, Success);
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
        "FETCH" & Count'Img & " FROM " & Cursor_Name (Self.Stmt, Self.Cursor);
   begin
      Execute (Self.Res, Self.Connection.Postgres.all, Str);
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
      Close : constant String :=
        "CLOSE " & Cursor_Name (Self.Stmt, Self.Cursor);
   begin
      Execute (Self.Res, Self.Connection.Postgres.all, Close);
      Post_Execute_And_Log (Self'Access, Self.Connection, Close, False);

      --  Avoid having opened too many cursors
      if Self.Connection.Cursor = Self.Cursor + 1 then
         --  ??? Concurrency issues, we currently assume there is one
         --  connection per thread

         Self.Connection.Cursor := Self.Connection.Cursor - 1;
      end if;

      if Self.Nested_Transactions then
         --  ??? What if something has started a transaction in between ?
         Commit_Or_Rollback (Self.Connection);
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
      Id  : constant Stmt_Id := Stmt_Id
        (To_Integer (System.Address (Prepared)));
      Str : constant String :=
        "DEALLOCATE stmt" & Image (Integer (Id), Min_Width => 0);
      Res : Result;
   begin
      Execute (Res, Connection.Postgres.all, Str);
      if Active (Me_Query) then
         Trace (Me_Query, Str & " (" & Status (Res) & ")");
      end if;
      Clear (Res);
   end Finalize;

end GNATCOLL.SQL.Postgres.Builder;
