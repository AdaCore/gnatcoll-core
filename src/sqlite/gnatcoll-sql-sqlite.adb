------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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
     (Self : access Sqlite_Description) return Database_Connection
   is
      DB : Database_Connection;
   begin
      DB := GNATCOLL.SQL.Sqlite.Builder.Build_Connection (Self);
      Reset_Connection (DB);
      return DB;
   end Build_Connection;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Sqlite_Description) is
   begin
      Free (Self.Dbname);
   end Free;

end GNATCOLL.SQL.Sqlite;
