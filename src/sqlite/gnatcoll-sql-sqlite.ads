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

--  This package instantiates the GNATCOLL.SQL hierarchy for the sqlite3
--  DBMS

with GNATCOLL.SQL.Exec;   use GNATCOLL.SQL.Exec;
with GNAT.Strings;        use GNAT.Strings;

package GNATCOLL.SQL.Sqlite is

   Sqlite_Always_Use_Transactions : Boolean := False;
   --  Sqlite is faster if we always use transactions, even for SELECT
   --  statements, according to
   --  http://web.utk.edu/~jplyon/sqlite/SQLite_optimization_FAQ.html
   --  You might still want to disable this settings if that doesn't match
   --  your expectation. It is recommended to set this variable before getting
   --  the first connection, and then no longer touching it.
   --  Changing the setting will not impact existing connections.
   --
   --  This setting is mostly experimental, and can be tricky in your
   --  application. It is recommend that you create the transactions yourself
   --  in a controlled fashion.
   --  Otherwise, it is easy to have a deadlock in your application if one
   --  thread is using more than one connection (the first connection does a
   --  BEGIN and a SELECT and gets a shared lock, then the second connection
   --  tries to write to the database but cannot obtain a write lock).

   type Sqlite_Description (<>)
     is new Database_Description_Record with private;
   type Sqlite_Description_Access is access all Sqlite_Description'Class;

   overriding procedure Free (Self : in out Sqlite_Description);
   overriding function Build_Connection
     (Self : access Sqlite_Description) return Database_Connection;

   function Setup
     (Database      : String;
      Cache_Support : Boolean := False)
     return Database_Description;
   --  Return a database connection for sqlite
   --  If sqlite was not detected at installation time, this function will
   --  return null.

private
   type Sqlite_Description (Caching : Boolean)
     is new Database_Description_Record (Caching)
   with record
      Dbname   : GNAT.Strings.String_Access;
   end record;
end GNATCOLL.SQL.Sqlite;
