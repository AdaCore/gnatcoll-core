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

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNATCOLL.SQL.Postgres_Low;  use GNATCOLL.SQL.Postgres_Low;

package body GNATCOLL.SQL.Postgres is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNATCOLL.SQL.Postgres_Low.Database, Database_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Postgresql_Result_Content'Class, Postgresql_Result_Content_Access);

   function Get_Connection_String
     (Description   : Database_Description;
      With_Password : Boolean) return String;
   --  Create a connection string from the database description

   ---------------
   -- Error_Msg --
   ---------------

   function Error_Msg (Result : Postgresql_Result_Content) return String is
   begin
      return Error (Result.Res);
   end Error_Msg;

   ------------
   -- Status --
   ------------

   function Status (Result : Postgresql_Result_Content) return String is
   begin
      return Status (Result.Res);
   end Status;

   ----------------
   -- Is_Success --
   ----------------

   function Is_Success (Result : Postgresql_Result_Content) return Boolean is
   begin
      return Status (Result.Res) = PGRES_TUPLES_OK
        or else Status (Result.Res) = PGRES_COMMAND_OK;
   end Is_Success;

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

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Result : in out Postgresql_Result_Content) is
   begin
      null;
      --  PQclear (To_Addr (Object.Res));
   end Finalize;

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

   -------------------------
   -- Connect_And_Execute --
   -------------------------

   procedure Connect_And_Execute
     (Connection  : access Postgresql_Connection_Record;
      Query       : String;
      R           : out Query_Result_Content_Access;
      Is_Select   : Boolean)
   is
      Res : Postgresql_Result_Content_Access;
   begin
      --  If we already have a connection, immediately try the query on it.

      if Connection.Postgres /= null then
         begin
            Res := new Postgresql_Result_Content;
            R := Query_Result_Content_Access (Res);

            if Query = "" then
               Execute (Res.Res, Connection.Postgres.all, "ROLLBACK");
            else
               Execute (Res.Res, Connection.Postgres.all, Query);
            end if;

            if Is_Select then
               Res.Rows := GNATCOLL.SQL.Exec.Tuple_Index
                 (Tuple_Count (Res.Res));
            else
               Res.Rows := GNATCOLL.SQL.Exec.Tuple_Index
                 (Natural'(Command_Tuples (Res.Res)));
            end if;

            case ExecStatus'(Status (Res.Res)) is
               when PGRES_NONFATAL_ERROR
                  | PGRES_FATAL_ERROR
                  | PGRES_EMPTY_QUERY =>
                  Print_Warning
                    (Connection, "Database warning: " & Error (Res.Res));
               when others =>
                  return;
            end case;
         exception
            when PostgreSQL_Error =>
               null;
            when others =>
               Print_Warning
                 (Connection,
                  "Exception raised when executing SQL query: " & Query);
         end;
      end if;

      --  If the connection is still good, that just means the request was
      --  invalid. Do not try to reconnect in this case, since that would
      --  kill any transaction BEGIN..COMMIT we are in the process of doing.

      if Connection.Postgres /= null then
         if Status (Connection.Postgres.all) = CONNECTION_OK then
            return;
         else
            Print_Warning
              (Connection,
               "DB status is " & Status (Connection.Postgres.all)'Img);
         end if;
      end if;

      --  Attempt to reconnect, in case we lost the connection

      Print_Warning
        (Connection,
         "Reconnecting to the database "
         & Get_Connection_String (Get_Description (Connection), False));

      if Connection.Postgres = null then
         if Connection.Connection_String = null then
            Connection.Connection_String := new String'
              (Get_Connection_String (Get_Description (Connection), True));
         end if;

         Connection.Postgres := new GNATCOLL.SQL.Postgres_Low.Database
           (Connection.Connection_String);
      else
         Reset (Connection.Postgres.all);
      end if;

      --  Output error message, including PostgreSQL connection string,
      --  but with password obscured.

      if Status (Connection.Postgres.all) /= CONNECTION_OK then
         Unchecked_Free (Connection.Postgres);
         Unchecked_Free (Res);
         Connection.Postgres := null;
         Print_Error
           (Connection,
            "Cannot connect to Postgres database."
            & " Connection string is """
            & Get_Connection_String (Get_Description (Connection), False)
            & """. Aborting...");
         return;
      end if;

      --  Now that we have (re)connected, try to execute the query again

      begin
         if Res = null then
            Res := new Postgresql_Result_Content;
            R := Query_Result_Content_Access (Res);
         end if;

         if Query /= "" then
            Execute (Res.Res, Connection.Postgres.all, Query);
            if Is_Select then
               Res.Rows := GNATCOLL.SQL.Exec.Tuple_Index
                 (Tuple_Count (Res.Res));
            else
               Res.Rows := GNATCOLL.SQL.Exec.Tuple_Index
                 (Natural'(Command_Tuples (Res.Res)));
            end if;

            case ExecStatus'(Status (Res.Res)) is
            when PGRES_NONFATAL_ERROR
               | PGRES_FATAL_ERROR
               | PGRES_EMPTY_QUERY =>
               Print_Error (Connection, "Database error: " & Error (Res.Res));
               Unchecked_Free (Res);
            when others =>
               null;
            end case;
         end if;

      exception
         when PostgreSQL_Error =>
            if Status (Connection.Postgres.all) /= CONNECTION_OK then
               Print_Error
                 (Connection, "Error with the connection to the database: "
                  & ConnStatus'Image (Status (Connection.Postgres.all)));
            else
               Print_Error
                 (Connection, ExecStatus'Image (Status (Res.Res))
                  & " " & Error (Res.Res) & "while executing: " & Query);
            end if;
            Unchecked_Free (Res);
      end;
   end Connect_And_Execute;

   -----------------
   -- Tuple_Count --
   -----------------

   function Tuple_Count
     (Res : Postgresql_Result_Content) return GNATCOLL.SQL.Exec.Tuple_Index is
   begin
      return Res.Rows;
   end Tuple_Count;

   -----------
   -- Value --
   -----------

   function Value
     (Res   : Postgresql_Result_Content;
      Tuple : GNATCOLL.SQL.Exec.Tuple_Index;
      Field : GNATCOLL.SQL.Exec.Field_Index) return String is
   begin
      return Value (Res.Res, GNATCOLL.SQL.Postgres_Low.Tuple_Index (Tuple),
                    GNATCOLL.SQL.Postgres_Low.Field_Index (Field));
   end Value;

   -------------------
   -- Boolean_Value --
   -------------------

   function Boolean_Value
     (Res   : Postgresql_Result_Content;
      Tuple : GNATCOLL.SQL.Exec.Tuple_Index;
      Field : GNATCOLL.SQL.Exec.Field_Index) return Boolean is
   begin
      return Boolean_Value
        (Res.Res,
         GNATCOLL.SQL.Postgres_Low.Tuple_Index (Tuple),
         GNATCOLL.SQL.Postgres_Low.Field_Index (Field));
   end Boolean_Value;

   -------------------
   -- Address_Value --
   -------------------

   function Address_Value
     (Res   : Postgresql_Result_Content;
      Tuple : GNATCOLL.SQL.Exec.Tuple_Index;
      Field : GNATCOLL.SQL.Exec.Field_Index) return System.Address
   is
      S : System.Address;
   begin
      Value
        (Res.Res,
         GNATCOLL.SQL.Postgres_Low.Tuple_Index (Tuple),
         GNATCOLL.SQL.Postgres_Low.Field_Index (Field),
         S);
      return S;
   end Address_Value;

   -------------
   -- Is_Null --
   -------------

   function Is_Null
     (Res   : Postgresql_Result_Content;
      Tuple : GNATCOLL.SQL.Exec.Tuple_Index;
      Field : GNATCOLL.SQL.Exec.Field_Index) return Boolean is
   begin
      return Is_Null
        (Res.Res,
         GNATCOLL.SQL.Postgres_Low.Tuple_Index (Tuple),
         GNATCOLL.SQL.Postgres_Low.Field_Index (Field));
   end Is_Null;

   ---------------
   -- OID_Field --
   ---------------

   function OID_Field (Table : SQL_Table'Class) return SQL_Field_Integer is
      D : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      D.Table := new SQL_Table'Class'(Table);
      D.Name  := new String'("OID");
      return SQL_Field_Integer'
        (SQL_Field_Or_List
         with Data => SQL_Field_Internal_Access (D));
   end OID_Field;

   -------------
   -- Last_Id --
   -------------

   function Last_Id
     (Connection : access Database_Connection_Record'Class;
      Res        : Postgresql_Result_Content;
      Field      : SQL_Field_Integer) return Integer
   is
      Last_OID : OID;
      Q        : SQL_Query;
      Res2     : Query_Result;
   begin
      Last_OID := OID_Value (Res.Res);
      if Last_OID /= InvalidOID then
         Q := SQL_Select
           (Fields => Field,
            Where  => From_Integer ("OID") = Integer (Last_OID));
         Auto_Complete (Q);

         Execute (Connection, Res2, Q);
         if Tuple_Count (Res2) = 1 then
            return Integer_Value (Res2, 0, 0);
         end if;
      end if;
      return -1;
   end Last_Id;

   -----------------
   -- Field_Count --
   -----------------

   function Field_Count
     (Res : Postgresql_Result_Content) return GNATCOLL.SQL.Exec.Field_Index is
   begin
      return GNATCOLL.SQL.Exec.Field_Index (Field_Count (Res.Res));
   end Field_Count;

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name
     (Res   : Postgresql_Result_Content;
      Field : GNATCOLL.SQL.Exec.Field_Index) return String is
   begin
      return Field_Name
         (Res.Res, GNATCOLL.SQL.Postgres_Low.Field_Index (Field));
   end Field_Name;

end GNATCOLL.SQL.Postgres;
