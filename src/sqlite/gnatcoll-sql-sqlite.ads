------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2016, AdaCore                     --
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
   --  the first connection, and then no longer touch it.
   --  Changing the setting will not impact existing connections.
   --
   --  This setting is mostly experimental, and can be tricky in your
   --  application. It is recommend that you create the transactions yourself
   --  in a controlled fashion.
   --  Otherwise, it is easy to have a deadlock in your application if one
   --  thread is using more than one connection (the first connection does a
   --  BEGIN and a SELECT and gets a shared lock, then the second connection
   --  tries to write to the database but cannot obtain a write lock).

   Max_Ms_On_Busy : Natural := 0;
   --  Maximum number of milliseconds we are willing to wait when sqlite
   --  reports that it was enable to perform an action because it is already
   --  busy.
   --  Set this to 0 to disable retries altogether, although this means that
   --  some queries might fail as a result.

   type Sqlite_Description (<>)
     is new Database_Description_Record with private;
   type Sqlite_Description_Access is access all Sqlite_Description'Class;

   overriding procedure Free (Self : in out Sqlite_Description);
   overriding function Build_Connection
     (Self : access Sqlite_Description) return Database_Connection;

   function Setup
     (Database      : String;
      Cache_Support : Boolean := False;
      Errors        : access Error_Reporter'Class := null)
      return Database_Description;
   --  Return a database connection for sqlite
   --  If sqlite was not detected at installation time, this function will
   --  return null.
   --  Errors (if specified) will be used to report errors and warnings to the
   --  application. Errors is never freed.

   function Is_Sqlite
     (DB : access Database_Connection_Record'Class)
      return Boolean;
   --  Whether the connection is to a sqlite database

   function DB_Name
     (DB : access Database_Connection_Record'Class) return String;
   --  Return the name of the file DB is connecting to (or ":memory:" when
   --  in memory.

   function Backup
     (DB1             : access Database_Connection_Record'Class;
      DB2             : String;
      From_DB1_To_DB2 : Boolean := True) return Boolean;
   function Backup
     (From : access Database_Connection_Record'Class;
      To   : access Database_Connection_Record'Class) return Boolean;
   --  Backup the database DB1 to a new database with the given file name
   --  (or ":memory:"). It is possible to revert the direction of the backup
   --  by changing the value of From_DB1_To_DB2
   --  Returns False in case of error

private

   type Sqlite_Description is new Database_Description_Record with record
      Dbname   : GNAT.Strings.String_Access;
   end record;

end GNATCOLL.SQL.Sqlite;
