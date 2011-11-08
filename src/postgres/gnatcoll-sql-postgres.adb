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

with GNATCOLL.SQL.Postgres.Builder;

package body GNATCOLL.SQL.Postgres is

   N_OID : aliased constant String := "OID";

   -----------
   -- Setup --
   -----------

   function Setup
     (Database      : String;
      User          : String := "";
      Host          : String := "";
      Password      : String := "";
      SSL           : SSL_Mode := Allow;
      Cache_Support : Boolean := True)
      return Database_Description
   is
      Result : Postgres_Description_Access;
   begin
      if not GNATCOLL.SQL.Postgres.Builder.Has_Postgresql_Support then
         return null;
      end if;

      Result := new Postgres_Description (Caching => Cache_Support);
      Result.SSL      := SSL;
      Result.Dbname   := new String'(Database);
      Result.User     := new String'(User);
      Result.Password := new String'(Password);

      if Host /= ""
        and then Host /= "localhost"
      then
         Result.Host := new String'(Host);
      else
         Result.Host := new String'("");
      end if;

      return Database_Description (Result);
   end Setup;

   ----------------------
   -- Build_Connection --
   ----------------------

   overriding function Build_Connection
     (Self : access Postgres_Description) return Database_Connection
   is
      DB : Database_Connection;
   begin
      DB := GNATCOLL.SQL.Postgres.Builder.Build_Connection (Self);
      Reset_Connection (DB);
      return DB;
   end Build_Connection;

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

   ----------
   -- Free --
   ----------

   overriding procedure Free (Description : in out Postgres_Description) is
   begin
      GNAT.Strings.Free (Description.Host);
      GNAT.Strings.Free (Description.User);
      GNAT.Strings.Free (Description.Dbname);
      GNAT.Strings.Free (Description.Password);
   end Free;

end GNATCOLL.SQL.Postgres;
