------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

--  This package instantiates the GNATCOLL.SQL hierarchy for the PostgreSQL
--  DBMS

with Ada.Calendar;        use Ada.Calendar;
with Ada.Strings.Unbounded;
with GNATCOLL.SQL.Exec;   use GNATCOLL.SQL.Exec;
with GNATCOLL.Strings;    use GNATCOLL.Strings;
with GNATCOLL.SQL_Fields; use GNATCOLL.SQL_Fields;
with GNATCOLL.SQL.Ranges;

package GNATCOLL.SQL.Postgres is

   type Postgres_Description (<>)
     is new Database_Description_Record with private;
   type Postgres_Description_Access is access all Postgres_Description'Class;

   overriding function Build_Connection
     (Self : access Postgres_Description) return Database_Connection;

   type SSL_Mode is (Disable, Allow, Prefer, Require);
   --  Whether to use SSL to connect to the server. This might not be
   --  applicable to all backends (for instance it doesn't apply to sqlite),
   --  and even if the backend supports SSL, some of the modes might not exist.
   --    Disable  => require a non-SSL connection
   --    Allow    => first try a non-SSL connection, then SSL if failed
   --    Prefer   => first try a SSL connection, then non-SSL if failed
   --    Require  => require a SSL connection

   function Setup
     (Database      : String;
      User          : String := "";
      Host          : String := "";
      Password      : String := "";
      Port          : Integer := -1;
      SSL           : SSL_Mode := Allow;
      Cache_Support : Boolean := True;
      Errors        : access Error_Reporter'Class := null)
     return Database_Description;
   --  Return a database connection for PostgreSQL.
   --  If postgres was not detected at installation time, this function will
   --  return null.
   --  Errors (if specified) will be used to report errors and warnings to the
   --  application. Errors is never freed.

   function Get_Connection_String
     (Description   : Database_Description;
      With_Password : Boolean) return String;
   --  Create a connection string from the database description

   ----------------------------
   -- Postgres notifications --
   ----------------------------

   type Notification is record
      Channel_Name : XString;
      Notifier_PID : Integer;
      Payload      : XString;
   end record;

   procedure Notifies
     (DB      : Database_Connection;
      Message : out Notification;
      Done    : out Boolean);
   --  Returns the next notification from a list of unhandled notification
   --  messages received from the backend. Done is set to False if there are
   --  no pending notifications. In this case Message is not be set. If Done
   --  is set to False, Message contains a valid Notification. Once a
   --  notification is returned from Notifies, it is considered handled and
   --  will be removed from the list of notifications.

   procedure Consume_Input (DB : Database_Connection);
   --  If input is available from the backend, consume it.
   --  Note that the result does not say whether any input
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

   function Wait_For_Input
     (DB      : Database_Connection;
      Timeout : Duration := Duration'Last) return Boolean;
   --  Waiting for available input and return False on timeout or True on
   --  success. No need to call Consume_Input afterward, it is already called
   --  internally on wait success.

   -------------------------
   -- Postgres extensions --
   -------------------------

   --  Postgres-specific extensions for GNATCOLL.SQL

   function OID_Field (Table : SQL_Table'Class) return SQL_Field_Integer;
   --  The object identifier field, available in each table. This is postgres
   --  specific. It can be used for instance to retrieve the newly inserted
   --  row in a table, by retrieving the OID of the previous result.
   --  With recent versions of PostgreSQL, you must explicitly create the table
   --  with support for oids ("CREATE TABLE (...) WITH OIDS"), otherwise the
   --  oid will always be null. For this reason, and since oids slow things
   --  done a little, and take space, it is not recommended to depend on them.

   function Now is new Time_Fields.SQL_Function ("now()");
   --  Return the current timestamp, same as Current_Timestamp

   function Regexp
     (Self : Text_Fields.Field'Class;
      Str  : String) return SQL_Criteria;
   --  Check whether the field matches a regular expression. This is the "~*"
   --  operator specific to postgreSQL.

   --  Generic query extensions

   type SQL_PG_Extension is abstract tagged private;
   function To_String
      (Self : SQL_PG_Extension; Format : Formatter'Class)
      return Ada.Strings.Unbounded.Unbounded_String is abstract;

   function Returning (Fields : SQL_Field_List) return SQL_PG_Extension'Class;
   --  RETURNING clause for UPDATE query

   function For_Update
     (Tables  : SQL_Table_List := Empty_Table_List;
      No_Wait : Boolean := False) return SQL_PG_Extension'Class;
   --  FOR UPDATE clause for SELECT query

   function "&"
     (Query     : SQL_Query;
      Extension : SQL_PG_Extension'Class) return SQL_Query;
   --  Extends an existing query with postgres-specific additions. For
   --  instance:
   --      R.Fetch (DB, SQL_Select (...) & Returning (Field1));

   package Date_Ranges is new GNATCOLL.SQL.Ranges
      (Base_Fields    => GNATCOLL.SQL.Date_Fields,
       SQL_Type       => "daterange",
       Ada_Field_Type => "GNATCOLL.SQL.Postgres.SQL_Field_Date_Range");
   subtype Date_Range is Date_Ranges.Ada_Range;
   subtype SQL_Date_Range is Date_Ranges.SQL_Ada_Range;
   subtype SQL_Field_Date_Range is Date_Ranges.SQL_Field_Range;

   package Num_Ranges is new GNATCOLL.SQL.Ranges
      (Base_Fields    => GNATCOLL.SQL_Fields.Long_Float_Fields,
       SQL_Type       => "numrange",
       Ada_Field_Type => "GNATCOLL.SQL.Postgres.SQL_Field_Num_Range");
   subtype Num_Range is Num_Ranges.Ada_Range;
   subtype SQL_Num_Range is Num_Ranges.SQL_Ada_Range;
   subtype SQL_Field_Num_Range is Num_Ranges.SQL_Field_Range;

   package Integer_Ranges is new GNATCOLL.SQL.Ranges
      (Base_Fields    => GNATCOLL.SQL.Integer_Fields,
       SQL_Type       => "int4range",
       Ada_Field_Type => "GNATCOLL.SQL.Postgres.SQL_Field_Integer_Range");
   subtype Integer_Range is Integer_Ranges.Ada_Range;
   subtype SQL_Integer_Range is Integer_Ranges.SQL_Ada_Range;
   subtype SQL_Field_Integer_Range is Integer_Ranges.SQL_Field_Range;

   package Bigint_Ranges is new GNATCOLL.SQL.Ranges
      (Base_Fields    => GNATCOLL.SQL.Bigint_Fields,
       SQL_Type       => "int8range",
       Ada_Field_Type => "GNATCOLL.SQL.Postgres.SQL_Field_Bigint_Range");
   subtype Bigint_Range is Bigint_Ranges.Ada_Range;
   subtype SQL_Bigint_Range is Bigint_Ranges.SQL_Ada_Range;
   subtype SQL_Field_Bigint_Range is Bigint_Ranges.SQL_Field_Range;

private
   type Postgres_Description is new Database_Description_Record with record
      Host      : GNATCOLL.Strings.XString;
      Dbname    : GNATCOLL.Strings.XString;
      User      : GNATCOLL.Strings.XString;
      Password  : GNATCOLL.Strings.XString;
      SSL       : SSL_Mode := Prefer;
      Port      : Integer := -1;
   end record;

   type SQL_PG_Extension is abstract tagged null record;
   type SQL_PG_Extension_Access is access all SQL_PG_Extension'Class;

end GNATCOLL.SQL.Postgres;
