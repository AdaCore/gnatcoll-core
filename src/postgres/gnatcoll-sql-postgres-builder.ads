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

--  Implementation of gnatcoll-sql-exec_private for Postgres.
--  This isn't in GNATCOLL.SQL.Postgres so that GNATCOLL can have the same API
--  no matter whether postgresql is installed on the machine or not

private package GNATCOLL.SQL.Postgres.Builder is

   function Has_Postgresql_Support return Boolean;
   --  Whether PostgreSQL is supported

   function Build_Connection
     (Descr : access Postgres_Description'Class) return Database_Connection;
   --  See doc in GNATCOLL.SQL.Postgres

end GNATCOLL.SQL.Postgres.Builder;
