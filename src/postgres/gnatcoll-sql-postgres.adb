-----------------------------------------------------------------------
--                               G N A T C O L L                     --
--                                                                   --
--                 Copyright (C) 2005-2010, AdaCore                  --
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

with GNATCOLL.SQL.Postgres.Builder;

package body GNATCOLL.SQL.Postgres is

   N_OID : aliased constant String := "OID";

   -------------------------------
   -- Build_Postgres_Connection --
   -------------------------------

   function Build_Postgres_Connection
     (Descr : GNATCOLL.SQL.Exec.Database_Description)
      return Database_Connection is
   begin
      if Get_DBMS (Descr) = DBMS_Postgresql then
         return GNATCOLL.SQL.Postgres.Builder.Build_Postgres_Connection;
      else
         return null;
      end if;
   end Build_Postgres_Connection;

   ---------------
   -- OID_Field --
   ---------------

   function OID_Field (Table : SQL_Table'Class) return SQL_Field_Integer is
   begin
      return SQL_Field_Integer'
        (Table    => Table.Table_Name,
         Instance => Table.Instance,
         Instance_Index => Table.Instance_Index,
         Name     => N_OID'Access);
   end OID_Field;

end GNATCOLL.SQL.Postgres;
