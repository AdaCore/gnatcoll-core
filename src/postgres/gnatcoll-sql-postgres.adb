------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2013, AdaCore                     --
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

   --  Support for extending standard query contents with
   --  Postgres-specific info.

   generic
      type Query_Std_Contents is new Query_Contents with private;
      type Query_PG_Contents  is new Query_Std_Contents with private;

   package Extended_Contents is
      type Query_PG_Contents_Access is access all Query_PG_Contents;
      function Extend
        (Std_Contents : SQL_Query_Contents_Access)
        return Query_PG_Contents_Access;
   end Extended_Contents;

   -----------------------
   -- Extended_Contents --
   -----------------------

   package body Extended_Contents is

      ------------
      -- Extend --
      ------------

      function Extend
        (Std_Contents : SQL_Query_Contents_Access)
        return Query_PG_Contents_Access
      is
      begin
         if Std_Contents.all in Query_PG_Contents then
            return new Query_PG_Contents'
                         (Query_PG_Contents (Std_Contents.all));

         elsif Std_Contents.all in Query_Std_Contents then
            return Result : constant Query_PG_Contents_Access :=
                              new Query_PG_Contents
            do
               Query_Std_Contents (Result.all) :=
                 Query_Std_Contents (Std_Contents.all);
            end return;

         else
            raise Constraint_Error with "unexpected query type";
         end if;
      end Extend;

   end Extended_Contents;

   --  SELECT extensions

   type SQL_PG_For_Update is new SQL_PG_Extension with record
      Tables : SQL_Table_List := Empty_Table_List;
      --  List of updated tables (empty means ALL tables in query)

      No_Wait : Boolean := False;
      --  Set True if NO WAIT
   end record;

   type Query_PG_Select_Contents is new Query_Select_Contents with record
      For_Update_Present : Boolean := False;
      For_Update         : SQL_PG_For_Update;
   end record;

   overriding function To_String
     (Self   : Query_PG_Select_Contents;
      Format : Formatter'Class) return Unbounded_String;

   package Select_Contents is new Extended_Contents
     (Query_Select_Contents, Query_PG_Select_Contents);
   subtype Query_PG_Select_Contents_Access is
     Select_Contents.Query_PG_Contents_Access;

   --  UPDATE extensions

   type SQL_PG_Returning is new SQL_PG_Extension with record
      Fields : SQL_Field_List;
   end record;

   type Query_PG_Update_Contents is new Query_Update_Contents with record
      Returning : SQL_PG_Returning;
   end record;

   overriding function To_String
     (Self   : Query_PG_Update_Contents;
      Format : Formatter'Class) return Unbounded_String;

   package Update_Contents is new Extended_Contents
     (Query_Update_Contents, Query_PG_Update_Contents);
   subtype Query_PG_Update_Contents_Access is
     Update_Contents.Query_PG_Contents_Access;

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

   ----------------
   -- For_Update --
   ----------------

   function For_Update
     (Tables  : SQL_Table_List := Empty_Table_List;
      No_Wait : Boolean := False) return SQL_PG_Extension'Class
   is
   begin
      return SQL_PG_For_Update'(Tables => Tables, No_Wait => No_Wait);
   end For_Update;

   ---------------
   -- Returning --
   ---------------

   function Returning
     (Fields : SQL_Field_List) return SQL_PG_Extension'Class
   is
   begin
      return SQL_PG_Returning'(Fields => Fields);
   end Returning;

   ---------
   -- "&" --
   ---------

   function "&"
     (Query     : SQL_Query;
      Extension : SQL_PG_Extension'Class) return SQL_Query
   is
      Q_Data : SQL_Query_Contents_Access;

   --  Start of processing for "&"

   begin
      if Extension in SQL_PG_Returning then
         declare
            use Update_Contents;

            Data : constant Query_PG_Update_Contents_Access :=
                     Extend (Query.Contents.Data);
         begin
            Data.Returning.Fields := Data.Returning.Fields
                                   & SQL_PG_Returning (Extension).Fields;

            Q_Data := SQL_Query_Contents_Access (Data);
         end;

      elsif Extension in SQL_PG_For_Update then
         declare
            use Select_Contents;

            Data : constant Query_PG_Select_Contents_Access :=
                     Extend (Query.Contents.Data);
         begin
            Data.For_Update_Present := True;
            Data.For_Update.Tables :=
              Data.For_Update.Tables & SQL_PG_For_Update (Extension).Tables;
            Data.For_Update.No_Wait :=
              Data.For_Update.No_Wait or SQL_PG_For_Update (Extension).No_Wait;

            Q_Data := SQL_Query_Contents_Access (Data);
         end;
      else
         raise Program_Error with "unexpected extension type";
      end if;

      return (Contents => (Ada.Finalization.Controlled with Q_Data));
   end "&";

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Self   : Query_PG_Select_Contents;
      Format : Formatter'Class) return Unbounded_String
   is
      Result : Unbounded_String :=
                 To_String (Query_Select_Contents (Self), Format);
   begin
      if Self.For_Update_Present then
         Append (Result, " FOR UPDATE");
         if Self.For_Update.Tables /= Empty_Table_List then
            Append (Result, " OF ");
            Append (Result, To_String (Self.For_Update.Tables, Format));
         end if;

         if Self.For_Update.No_Wait then
            Append (Result, " NO WAIT");
         end if;
      end if;
      return Result;
   end To_String;

   overriding function To_String
     (Self   : Query_PG_Update_Contents;
      Format : Formatter'Class) return Unbounded_String
   is
      Result : Unbounded_String :=
                 To_String (Query_Update_Contents (Self), Format);
   begin
      Append (Result, " RETURNING ");
      Append (Result, To_String (Self.Returning.Fields, Format, Long => True));
      return Result;
   end To_String;

end GNATCOLL.SQL.Postgres;
