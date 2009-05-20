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

--  Implementation of gnatcoll-sql-exec_private for Postgres.
--  This isn't in GNATCOLL.SQL.Postgres so that GNATCOLL can have the same API
--  no matter whether postgresql is installed on the machine or not

private package GNATCOLL.SQL.Postgres.Builder is

   function Build_Postgres_Connection return Database_Connection;
   --  See doc in GNATCOLL.SQL.Postgres

end GNATCOLL.SQL.Postgres.Builder;
