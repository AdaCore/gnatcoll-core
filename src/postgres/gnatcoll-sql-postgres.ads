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

--  This package instantiates the GNATCOLL.SQL hierarchy for the PostgreSQL
--  DBMS

with GNATCOLL.SQL.Exec;   use GNATCOLL.SQL.Exec;
with GNAT.Strings;        use GNAT.Strings;

package GNATCOLL.SQL.Postgres is

   type Postgres_Description (<>)
     is new Database_Description_Record with private;
   type Postgres_Description_Access is access all Postgres_Description'Class;

   overriding procedure Free (Description : in out Postgres_Description);
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
      SSL           : SSL_Mode := Prefer;
      Cache_Support : Boolean := True)
     return Database_Description;
   --  Return a database connection for PostgreSQL.
   --  If postgres was not detected at installation time, this function will
   --  return null.

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

private
   type Postgres_Description (Caching : Boolean)
     is new Database_Description_Record (Caching)
   with record
      Host     : GNAT.Strings.String_Access;
      Dbname   : GNAT.Strings.String_Access;
      User     : GNAT.Strings.String_Access;
      Password : GNAT.Strings.String_Access;
      SSL      : SSL_Mode := Prefer;
   end record;

end GNATCOLL.SQL.Postgres;
