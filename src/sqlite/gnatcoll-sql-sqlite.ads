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

   type Sqlite_Description (<>)
     is new Database_Description_Record with private;
   type Sqlite_Description_Access is access all Sqlite_Description'Class;

   overriding procedure Free (Self : in out Sqlite_Description);
   overriding function Build_Connection
     (Self : Sqlite_Description) return Database_Connection;

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
