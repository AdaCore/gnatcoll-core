------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

with GNATCOLL.SQL.Postgres.Builder;

package body GNATCOLL.SQL.Postgres is

   N_OID : aliased constant String := "OID";

   Comparison_Regexp : aliased constant String := " ~* ";

   -----------
   -- Setup --
   -----------

   function Setup
     (Database      : String;
      User          : String := "";
      Host          : String := "";
      Password      : String := "";
      Port          : Integer := -1;
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
      Result.Port     := Port;

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

   ------------
   -- Regexp --
   ------------

   function Regexp
     (Self : Text_Fields.Field'Class;
      Str  : String) return SQL_Criteria is
   begin
      return Compare (Self, Expression (Str), Comparison_Regexp'Access);
   end Regexp;

end GNATCOLL.SQL.Postgres;
