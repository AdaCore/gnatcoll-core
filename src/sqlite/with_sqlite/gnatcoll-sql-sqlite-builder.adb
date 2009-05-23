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
with GNATCOLL.SQL.Sqlite.Gnade;    use GNATCOLL.SQL.SQlite.Gnade;
with GNATCOLL.SQL.Exec_Private;    use GNATCOLL.SQL.Exec_Private;
with System;

package body GNATCOLL.SQL.Sqlite.Builder is

   type Sqlite_Cursor is new DBMS_Cursor with record
      Stmt           : Statement;
      Processed_Rows : Natural := 0;
      Last_Status    : Result_Codes;  --  Last status of Step
   end record;
   type Sqlite_Cursor_Access is access all Sqlite_Cursor'Class;

   overriding function Error_Msg
     (Self : Sqlite_Cursor) return String;
   overriding function Status
     (Self : Sqlite_Cursor) return String;
   overriding function Is_Success
     (Self : Sqlite_Cursor) return Boolean;
   overriding procedure Finalize (Result : in out Sqlite_Cursor);
   overriding function Rows_Count (Self : Sqlite_Cursor) return Natural;
   overriding function Processed_Rows (Self : Sqlite_Cursor) return Natural;
   overriding function Value
     (Self  : Sqlite_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return String;
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

   type Sqlite_Connection_Record is
     new GNATCOLL.SQL.Exec.Database_Connection_Record with
      record
         DB       : GNATCOLL.SQL.Sqlite.Gnade.Database;
      end record;
   overriding procedure Close
     (Connection : access Sqlite_Connection_Record);
   overriding function Connect_And_Execute
     (Connection  : access Sqlite_Connection_Record;
      Query       : String;
      Is_Select   : Boolean) return Abstract_Cursor_Access;
   overriding function Error
     (Connection : access Sqlite_Connection_Record) return String;
   overriding procedure Foreach_Table
     (Connection : access Sqlite_Connection_Record;
      Callback   : access procedure (Name, Description : String));
   overriding procedure Foreach_Field
     (Connection : access Sqlite_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Name        : String;
         Typ         : String;
         Index       : Natural;
         Description : String));
   overriding procedure Foreach_Foreign_Key
     (Connection : access Sqlite_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Index             : Positive;
         Local_Attribute   : Integer;
         Foreign_Table     : String;
         Foreign_Attribute : Integer));

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Sqlite_Cursor'Class, Sqlite_Cursor_Access);

   -------------------------------
   -- Build_Postgres_Connection --
   -------------------------------

   function Build_Postgres_Connection return Database_Connection is
   begin
      return new Sqlite_Connection_Record;
   end Build_Postgres_Connection;

   ---------------
   -- Error_Msg --
   ---------------

   function Error_Msg (Self : Sqlite_Cursor) return String is
   begin
      return Error_Msg (DB_Handle (Self.Stmt));
   end Error_Msg;

   ------------
   -- Status --
   ------------

   function Status (Self : Sqlite_Cursor) return String is
   begin
      return Result_Codes'Image (Self.Last_Status);
   exception
      when others =>
         return "ERROR";
   end Status;

   ----------------
   -- Is_Success --
   ----------------

   function Is_Success (Self : Sqlite_Cursor) return Boolean is
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

   procedure Finalize (Result : in out Sqlite_Cursor) is
      pragma Unreferenced (Result);
   begin
      null;
      --  PQclear (To_Addr (Object.Res));
   end Finalize;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (Connection : access Sqlite_Connection_Record) is
   begin
      Close (Connection.DB);
   end Close;

   -------------------------
   -- Connect_And_Execute --
   -------------------------

   function Connect_And_Execute
     (Connection  : access Sqlite_Connection_Record;
      Query       : String;
      Is_Select   : Boolean) return Abstract_Cursor_Access
   is
      Res    : Sqlite_Cursor_Access;
      Status : Result_Codes;
   begin
      --  With sqlite, we do not need to try and reconnect, since there is no
      --  no network involved. We either have a connection, or not

      --  Attempt to reconnect, in case we lost the connection

      if Connection.DB = No_Database then
         Print_Warning
           (Connection,
            "Connecting to sqlite database "
            & Get_Database (Get_Description (Connection)));
         Open
           (DB       => Connection.DB,
            Filename => Get_Database (Get_Description (Connection)),
            Flags    => Open_Readwrite,
            Status   => Status);

         if Status /= Sqlite_OK then
            Print_Error
              (Connection,
               "Could not connect to database: " & Error_Msg (Connection.DB));
            return null;
         end if;
      end if;

      Res := new Sqlite_Cursor;

      if Query /= "" then
         Prepare (Connection.DB, Query, Res.Stmt, Res.Last_Status);

         if Res.Last_Status /= Sqlite_OK then
            Finalize (Res.all);
            Unchecked_Free (Res);
            return null;
         end if;

         Step (Res.Stmt, Res.Last_Status);

         case Res.Last_Status is
            when Sqlite_OK =>
               if Is_Select then
                  Res.Processed_Rows := 0;
               else
                  Res.Processed_Rows := Changes (Connection.DB);
               end if;

            when others =>
               Print_Warning
                 (Connection,
                  "Error while executing query, status="
                  & Res.Last_Status'Img);
         end case;
      end if;
      return Abstract_Cursor_Access (Res);
   end Connect_And_Execute;

   ----------------
   -- Rows_Count --
   ----------------

   function Rows_Count (Self : Sqlite_Cursor) return Natural is
   begin
      return 0;  --  Unsupported for now
   end Rows_Count;

   --------------------
   -- Processed_Rows --
   --------------------

   function Processed_Rows (Self : Sqlite_Cursor) return Natural is
   begin
      return Self.Processed_Rows;
   end Processed_Rows;

   -----------
   -- Value --
   -----------

   function Value
     (Self  : Sqlite_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return String is
   begin
      return Column_Text (Self.Stmt, Natural (Field));
   end Value;

   -------------
   -- Is_Null --
   -------------

   function Is_Null
     (Self  : Sqlite_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return Boolean is
   begin
      return Column_Type (Self.Stmt, Natural (Field)) = Sqlite_Null;
   end Is_Null;

   -------------
   -- Last_Id --
   -------------

   function Last_Id
     (Self       : Sqlite_Cursor;
      Connection : access Database_Connection_Record'Class;
      Field      : SQL_Field_Integer) return Integer
   is
      pragma Unreferenced (Self);
      Q        : SQL_Query;
      Res2     : Cursor;
   begin
      --  Do not depend on OIDs, since the table might not have them (by
      --  default, recent versions of Sqlite disable them. Instead, we use
      --  the currval() function which returns the last value set for a
      --  sequence within the current connection.

      Q := SQL_Select
        (Fields => From_String ("currval('" & Field.Table.all
                                & "_" & Field.Name.all & "_seq')"));

      Execute (Connection, Res2, Q);
      if Rows_Count (Res2) = 1 then
         return Integer_Value (Res2, 0);
      end if;
      return -1;
   end Last_Id;

   -----------------
   -- Field_Count --
   -----------------

   function Field_Count
     (Self : Sqlite_Cursor) return GNATCOLL.SQL.Exec.Field_Index is
   begin
      return Field_Index (Column_Count (Self.Stmt));
   end Field_Count;

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name
     (Self  : Sqlite_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return String is
   begin
      return Column_Name (Self.Stmt, Natural (Field));
   end Field_Name;

   -------------------
   -- Foreach_Table --
   -------------------

   procedure Foreach_Table
     (Connection : access Sqlite_Connection_Record;
      Callback   : access procedure (Name, Description : String))
   is
      R     : Cursor;
   begin
      Execute (Connection, R, "SELECT name FROM sqlite_master ORDER BY name");
      while Has_Row (R) loop
         Callback (Name => Value (R, 0), Description => "");
         Next (R);
      end loop;
   end Foreach_Table;

   -------------------
   -- Is_Whitespace --
   -------------------

   function Is_Whitespace (C : Character) return Boolean is
   begin
      return C = ' ' or else C = ASCII.HT;
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

   procedure Foreach_Field
     (Connection : access Sqlite_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Name        : String;
         Typ         : String;
         Index       : Natural;
         Description : String))
   is
      R           : Cursor;
      Index       : Natural := 0;
      Paren_Count : Natural;
   begin
      Execute
        (Connection, R,
         "SELECT sql FROM sqlite_master WHERE name='" & Table_Name & "'");

      while Has_Row (R) loop
         --  Crude parsing of the sql

         declare
            Sql : constant String := Value (R, 0);
            Pos, Pos2, Pos3, Pos4 : Integer := Sql'First;
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

               while Pos <= Sql'Last
                 and then not Is_Whitespace (Sql (Pos))
                 and then Sql (Pos) /= ','
                 and then Sql (Pos) /= ')'
               loop
                  Pos := Pos + 1;
               end loop;

               Callback
                 (Name        => Sql (Pos2 .. Pos3),
                  Typ         => Sql (Pos4 .. Pos - 1),
                  Index       => Index,
                  Description => "");

               Paren_Count := 0;

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
   end Next;

   -----------------------------
   -- Build_Sqlite_Connection --
   -----------------------------

   function Build_Sqlite_Connection return Database_Connection is
   begin
      return new Sqlite_Connection_Record;
   end Build_Sqlite_Connection;

end GNATCOLL.SQL.Sqlite.Builder;
