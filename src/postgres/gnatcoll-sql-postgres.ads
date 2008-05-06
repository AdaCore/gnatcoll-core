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

--  This package instantiates the GNATCOLL.SQL hierarchy for the PostgreSQL
--  DBMS

with GNATCOLL.SQL.Postgres_Low;
with GNATCOLL.SQL.Exec;   use GNATCOLL.SQL.Exec;
with GNAT.Strings;
with System;

package GNATCOLL.SQL.Postgres is

   type Postgresql_Connection_Record is new Database_Connection_Record
      with private;

   type Postgresql_Result_Content is new Query_Result_Content with private;
   type Postgresql_Result_Content_Access
     is access all Postgresql_Result_Content'Class;

   -------------------------
   -- Postgres extensions --
   -------------------------
   --  Postgres-specific extensions for GNATCOLL.SQL

   function OID_Field (Table : SQL_Table'Class) return SQL_Field_Integer;
   --  The object identifier field, available in each table. This is postgres
   --  specific. It can be used for instance to retrieve the newly inserted
   --  row in a table, by retrieving the OID of the previous result.

private

   type Postgresql_Result_Content is new Query_Result_Content with record
      Res  : GNATCOLL.SQL.Postgres_Low.Result;
      Rows : GNATCOLL.SQL.Exec.Tuple_Index := 0;
   end record;
   overriding function Error_Msg
     (Result : Postgresql_Result_Content) return String;
   overriding function Status
     (Result : Postgresql_Result_Content) return String;
   overriding function Is_Success
     (Result : Postgresql_Result_Content) return Boolean;
   overriding procedure Finalize (Result : in out Postgresql_Result_Content);
   overriding function Tuple_Count
     (Res : Postgresql_Result_Content) return Tuple_Index;
   overriding function Value
     (Res   : Postgresql_Result_Content;
      Tuple : Tuple_Index;
      Field : Field_Index) return String;
   overriding function Address_Value
     (Res   : Postgresql_Result_Content;
      Tuple : Tuple_Index;
      Field : Field_Index) return System.Address;
   overriding function Boolean_Value
     (Res   : Postgresql_Result_Content;
      Tuple : Tuple_Index;
      Field : Field_Index) return Boolean;
   overriding function Is_Null
     (Res   : Postgresql_Result_Content;
      Tuple : Tuple_Index;
      Field : Field_Index) return Boolean;
   overriding function Last_Id
     (Connection : access Database_Connection_Record'Class;
      Res        : Postgresql_Result_Content;
      Field      : SQL_Field_Integer) return Integer;
   overriding function Field_Count
     (Res : Postgresql_Result_Content) return Field_Index;
   overriding function Field_Name
     (Res : Postgresql_Result_Content; Field : Field_Index) return String;

   type Database_Access is access GNATCOLL.SQL.Postgres_Low.Database;

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

end GNATCOLL.SQL.Postgres;
