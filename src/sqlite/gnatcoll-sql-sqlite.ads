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

--  This package instantiates the GNATCOLL.SQL hierarchy for the sqlite3
--  DBMS

with GNATCOLL.SQL.Exec;   use GNATCOLL.SQL.Exec;

package GNATCOLL.SQL.Sqlite is

   function Build_Sqlite_Connection
     (Descr : GNATCOLL.SQL.Exec.Database_Description)
      return Database_Connection;
   --  Return a database connection for sqlite
   --  If sqlite was not detected at installation time, this function will
   --  return null. The type is hidden in the body so that the spec can always
   --  be imported in an application, even if sqlite is not installed on the
   --  machine. Combined with similar behavior for other DBMS, this allows you
   --  to have a connection factory in your application so that your
   --  application can potentially support multiple DBMS.
   --  This function is compatible with the factory expected for
   --  Get_Task_Connection, but will only work if Descr is for Sqlite

   -----------------------
   -- Sqlite extensions --
   -----------------------
   --  Sqlite-specific extensions for GNATCOLL.SQL

end GNATCOLL.SQL.Sqlite;
