-----------------------------------------------------------------------
--                               G N A T C O L L                     --
--                                                                   --
--                 Copyright (C) 2009-2011, AdaCore                  --
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

with GNATCOLL.SQL.Sqlite.Builder;

package body GNATCOLL.SQL.Sqlite is

   -----------
   -- Setup --
   -----------

   function Setup
     (Database      : String;
      Cache_Support : Boolean := False)
      return Database_Description
   is
      Result : Sqlite_Description_Access;
   begin
      if not GNATCOLL.SQL.Sqlite.Builder.Has_Sqlite_Support then
         return null;
      end if;

      Result := new Sqlite_Description (Caching => Cache_Support);
      Result.Dbname := new String'(Database);
      return Database_Description (Result);
   end Setup;

   ----------------------
   -- Build_Connection --
   ----------------------

   overriding function Build_Connection
     (Self : Sqlite_Description) return Database_Connection
   is
      pragma Unreferenced (Self);
   begin
      return GNATCOLL.SQL.Sqlite.Builder.Build_Connection;
   end Build_Connection;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Sqlite_Description) is
   begin
      Free (Self.Dbname);
   end Free;

end GNATCOLL.SQL.Sqlite;
