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

with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNATCOLL.SQL.Postgres.Gnade;  use GNATCOLL.SQL.Postgres.Gnade;
with System;

package body GNATCOLL.SQL.Postgres.Builder is

   type Postgresql_Result_Content is new Query_Result_Content with record
      Res  : GNATCOLL.SQL.Postgres.Gnade.Result;
      Rows : GNATCOLL.SQL.Exec.Tuple_Index := 0;
   end record;
   type Postgresql_Result_Content_Access
     is access all Postgresql_Result_Content'Class;
   overriding function Error_Msg
     (Result : Postgresql_Result_Content) return String;
   overriding function Status
     (Result : Postgresql_Result_Content) return String;
   overriding function Is_Success
     (Result : Postgresql_Result_Content) return Boolean;
   overriding procedure Finalize (Result : in out Postgresql_Result_Content);
   overriding function Tuple_Count
     (Res : Postgresql_Result_Content) return GNATCOLL.SQL.Exec.Tuple_Index;
   overriding function Value
     (Res   : Postgresql_Result_Content;
      Tuple : GNATCOLL.SQL.Exec.Tuple_Index;
      Field : GNATCOLL.SQL.Exec.Field_Index) return String;
   overriding function Address_Value
     (Res   : Postgresql_Result_Content;
      Tuple : GNATCOLL.SQL.Exec.Tuple_Index;
      Field : GNATCOLL.SQL.Exec.Field_Index) return System.Address;
   overriding function Boolean_Value
     (Res   : Postgresql_Result_Content;
      Tuple : GNATCOLL.SQL.Exec.Tuple_Index;
      Field : GNATCOLL.SQL.Exec.Field_Index) return Boolean;
   overriding function Is_Null
     (Res   : Postgresql_Result_Content;
      Tuple : GNATCOLL.SQL.Exec.Tuple_Index;
      Field : GNATCOLL.SQL.Exec.Field_Index) return Boolean;
   overriding function Last_Id
     (Connection : access Database_Connection_Record'Class;
      Res        : Postgresql_Result_Content;
      Field      : SQL_Field_Integer) return Integer;
   overriding function Field_Count
     (Res : Postgresql_Result_Content) return GNATCOLL.SQL.Exec.Field_Index;
   overriding function Field_Name
     (Res : Postgresql_Result_Content;
      Field : GNATCOLL.SQL.Exec.Field_Index) return String;

   type Database_Access is access GNATCOLL.SQL.Postgres.Gnade.Database;

   type Postgresql_Connection_Record is
     new GNATCOLL.SQL.Exec.Database_Connection_Record with
      record
         Connection_String : GNAT.Strings.String_Access;
         Postgres          : Database_Access;
      end record;
   overriding procedure Connect_And_Execute
     (Connection  : access Postgresql_Connection_Record;
      Query       : String;
      R           : out Query_Result_Content_Access;
      Is_Select   : Boolean);
   overriding function Error
     (Connection : access Postgresql_Connection_Record) return String;
   overriding procedure Foreach_Table
     (Connection : access Postgresql_Connection_Record;
      Callback   : access procedure (Name, Description : String));
   overriding procedure Foreach_Field
     (Connection : access Postgresql_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Name        : String;
         Typ         : String;
         Index       : Natural;
         Description : String));
   overriding procedure Foreach_Foreign_Key
     (Connection : access Postgresql_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Index             : Positive;
         Local_Attribute   : Integer;
         Foreign_Table     : String;
         Foreign_Attribute : Integer));

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNATCOLL.SQL.Postgres.Gnade.Database, Database_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Postgresql_Result_Content'Class, Postgresql_Result_Content_Access);

   function Get_Connection_String
     (Description   : Database_Description;
      With_Password : Boolean) return String;
   --  Create a connection string from the database description

   -------------------------------
   -- Build_Postgres_Connection --
   -------------------------------

   function Build_Postgres_Connection return Database_Connection is
   begin
      return new Postgresql_Connection_Record;
   end Build_Postgres_Connection;

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

         Connection.Postgres := new GNATCOLL.SQL.Postgres.Gnade.Database
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
      return Value (Res.Res, GNATCOLL.SQL.Postgres.Gnade.Tuple_Index (Tuple),
                    GNATCOLL.SQL.Postgres.Gnade.Field_Index (Field));
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
         GNATCOLL.SQL.Postgres.Gnade.Tuple_Index (Tuple),
         GNATCOLL.SQL.Postgres.Gnade.Field_Index (Field));
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
         GNATCOLL.SQL.Postgres.Gnade.Tuple_Index (Tuple),
         GNATCOLL.SQL.Postgres.Gnade.Field_Index (Field),
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
         GNATCOLL.SQL.Postgres.Gnade.Tuple_Index (Tuple),
         GNATCOLL.SQL.Postgres.Gnade.Field_Index (Field));
   end Is_Null;

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
         (Res.Res, GNATCOLL.SQL.Postgres.Gnade.Field_Index (Field));
   end Field_Name;

   -------------------
   -- Foreach_Table --
   -------------------

   procedure Foreach_Table
     (Connection : access Postgresql_Connection_Record;
      Callback   : access procedure (Name, Description : String))
   is
      R     : Query_Result;
   begin
      Execute
        (Connection, R,
         "SELECT pg_class.relname, pg_description.description"
         & " FROM (pg_class left join pg_description"
         & "         on  pg_description.objoid = pg_class.oid"
         & "         and pg_description.objsubid = 0),"
         & "      pg_namespace"
         & " WHERE relnamespace=pg_namespace.oid"
         & "   AND pg_namespace.nspname='public'"
         & "   AND pg_class.relkind ~ '[rv]'"
         & " ORDER BY pg_class.relname");

      for T in 0 .. Tuple_Count (R) - 1 loop
         Callback (Name        => Value (R, T, 0),
                   Description => Value (R, T, 1));
      end loop;
   end Foreach_Table;

   -------------------
   -- Foreach_Field --
   -------------------

   procedure Foreach_Field
     (Connection : access Postgresql_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Name        : String;
         Typ         : String;
         Index       : Natural;
         Description : String))
   is
      R : Query_Result;
   begin
      Execute
        (Connection, R,
         "SELECT pg_attribute.attname,"       --  0 att name
         & "     pg_catalog.format_type(atttypid, atttypmod),"  --  1 att type
         & "     pg_attribute.attnum,"        --  2 attribute index in table
         & "     pg_description.description"  --  3 field doc
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

      for T in 0 .. Tuple_Count (R) - 1 loop
         Callback
           (Name        => Value (R, T, 0),
            Typ         => Value (R, T, 1),
            Index       => Integer_Value (R, T, 2),
            Description => Value (R, T, 3));
      end loop;
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
      R : Query_Result;
   begin
      Execute
        (Connection, R,
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

      for T in 0 .. Tuple_Count (R) - 1 loop
         declare
            Attr_Array   : constant String := Value (R, T, 3);
            Foreign      : constant String := Value (R, T, 4);
            Foreign_Attr : constant String := Value (R, T, 5);
            Key1, Key2   : Integer;
            Field        : Positive := 1;
         begin
            loop
               Key1 := Integer'Value (Array_Field (Attr_Array, Field));
               Key2 := Integer'Value (Array_Field (Foreign_Attr, Field));

               Callback
                 (Index             => Integer (T) + 1,
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
      end loop;
   end Foreach_Foreign_Key;

end GNATCOLL.SQL.Postgres.Builder;
