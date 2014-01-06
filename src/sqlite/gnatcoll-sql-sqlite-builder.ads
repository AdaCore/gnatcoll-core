------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2014, AdaCore                     --
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

--  Implementation of gnatcoll-sql-exec_private for sqlite.
--  This isn't in GNATCOLL.SQL.Sqlite so that GNATCOLL can have the same API
--  no matter whether sqlite is installed on the machine or not

private package GNATCOLL.SQL.Sqlite.Builder is

   function Has_Sqlite_Support return Boolean;
   --  Whether Sqlite is supported.

   function Build_Connection
     (Descr : access Sqlite_Description'Class) return Database_Connection;
   --  See doc in GNATCOLL.SQL.Sqlite

   procedure Setup;
   --  Perform additional setup

   function Backup
     (DB1 : access Database_Connection_Record'Class;
      DB2 : String;
      From_DB1_To_DB2 : Boolean := True) return Boolean;
   --  Backup the database From to a new database with the given file name
   --  (or ":memory:")
   --  Returns False in case of error

   function Backup
     (From : access Database_Connection_Record'Class;
      To   : access Database_Connection_Record'Class) return Boolean;
   --  Copy all the contents from From to TO.
   --  Returns False in case of error

end GNATCOLL.SQL.Sqlite.Builder;
