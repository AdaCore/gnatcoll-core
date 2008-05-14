-----------------------------------------------------------------------
--                               G N A T C O L L                     --
--                                                                   --
--                 Copyright (C) 2005-2008, AdaCore                  --
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

   function Build_Postgres_Connection return Database_Connection is
   begin
      return GNATCOLL.SQL.Postgres.Builder.Build_Postgres_Connection;
   end Build_Postgres_Connection;

   ---------------
   -- OID_Field --
   ---------------

   function OID_Field (Table : SQL_Table'Class) return SQL_Field_Integer is
      D : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      D.Table := (Name => Table_Name (Table),
                  Instance => Table.Instance);
      D.Name  := N_OID'Access;
      return SQL_Field_Integer'
        (SQL_Field_Or_List
         with Data => SQL_Field_Internal_Access (D));
   end OID_Field;

end GNATCOLL.SQL.Postgres;
