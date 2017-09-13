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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces.C;
with GNAT.Sockets;
with GNATCOLL.SQL.Postgres.Builder;
with GNATCOLL.SQL.Postgres.Gnade;
with GNATCOLL.Strings;                use GNATCOLL.Strings;
with GNATCOLL.Utils;                  use GNATCOLL.Utils;

package body GNATCOLL.SQL.Postgres is

   N_OID : aliased constant String := "OID";

   Comparison_Regexp : aliased constant String := " ~* ";

   type Query_Postgres_Contents is new Query_Contents with record
      Base  : SQL_Query;
      Extra : SQL_PG_Extension_Access;
   end record;
   overriding procedure Free (Self : in out Query_Postgres_Contents);
   overriding procedure Append_To_String
     (Self       : Query_Postgres_Contents;
      Format     : Formatter'Class;
      Result     : in out XString;
      Show_Types : Boolean);
   overriding procedure Auto_Complete
     (Self                   : in out Query_Postgres_Contents;
      Auto_Complete_From     : Boolean := True;
      Auto_Complete_Group_By : Boolean := True);
   --  Supports adding a suffix string to the base_query

   type SQL_PG_For_Update is new SQL_PG_Extension with record
      Tables : SQL_Table_List := Empty_Table_List;
      --  List of updated tables (empty means ALL tables in query)

      No_Wait : Boolean := False;
      --  Set True if NO WAIT
   end record;
   overriding procedure Append_To_String
     (Self   : SQL_PG_For_Update;
      Format : Formatter'Class;
      Result : in out XString);
   --  Extensions for UPDATE

   type SQL_PG_Returning is new SQL_PG_Extension with record
      Returning : SQL_Field_List;
   end record;
   overriding procedure Append_To_String
     (Self   : SQL_PG_Returning;
      Format : Formatter'Class;
      Result : in out XString);
   --  Extensions for SELECT

   type SQL_PG_On_Conflict (Do_Nothing : Boolean)
      is new SQL_PG_Extension
   with record
      Column     : SQL_Field_Pointer;
      Constraint : XString;

      case Do_Nothing is
         when True =>
            null;
         when False =>
            Set        : SQL_Assignment;
            Where      : SQL_Criteria;
      end case;
   end record;
   overriding procedure Append_To_String
     (Self   : SQL_PG_On_Conflict;
      Format : Formatter'Class;
      Result : in out XString);
   --  Extensions for INSERT

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Query_Postgres_Contents) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (SQL_PG_Extension'Class, SQL_PG_Extension_Access);
   begin
      Unchecked_Free (Self.Extra);
      Free (Query_Contents (Self));
   end Free;

   ----------------------
   -- Append_To_String --
   ----------------------

   overriding procedure Append_To_String
     (Self       : Query_Postgres_Contents;
      Format     : Formatter'Class;
      Result     : in out XString;
      Show_Types : Boolean) is
   begin
      Append_To_String
         (Self.Base, Format, Result => Result, Show_Types => Show_Types);
      Append_To_String (Self.Extra.all, Format, Result);
   end Append_To_String;

   -------------------
   -- Auto_Complete --
   -------------------

   overriding procedure Auto_Complete
     (Self                   : in out Query_Postgres_Contents;
      Auto_Complete_From     : Boolean := True;
      Auto_Complete_Group_By : Boolean := True) is
   begin
      Auto_Complete (Self.Base, Auto_Complete_From, Auto_Complete_Group_By);
   end Auto_Complete;

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
      Cache_Support : Boolean := True;
      Errors        : access Error_Reporter'Class := null;
      Pgbouncer     : Pgbouncer_Config := No_Pgbouncer;
      Application_Name : String := "")
      return Database_Description
   is
      Result : Postgres_Description_Access;
   begin
      if not GNATCOLL.SQL.Postgres.Builder.Has_Postgresql_Support then
         return null;
      end if;

      Result := new Postgres_Description
        (Caching => Cache_Support, Errors => Errors);
      Result.SSL       := SSL;
      Result.Dbname    := To_XString (Database);
      Result.User      := To_XString (User);
      Result.Password  := To_XString (Password);
      Result.Port      := Port;
      Result.Host      := To_XString (Host);
      Result.Pgbouncer := Pgbouncer;
      Result.Appname   := To_XString (Application_Name);

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

   --------------
   -- Notifies --
   --------------

   procedure Notifies
     (DB      : Database_Connection;
      Message : out Notification;
      Done    : out Boolean) is
   begin
      Builder.To_Native (DB).Notifies (Message, Done);
   end Notifies;

   -------------------
   -- Consume_Input --
   -------------------

   procedure Consume_Input (DB : Database_Connection) is
      DBG : constant access Gnade.Database'Class := Builder.To_Native (DB);
   begin
      if not DBG.Consume_Input then
         DB.Set_Failure (DBG.Error);
      end if;
   end Consume_Input;

   --------------------
   -- Wait_For_Input --
   --------------------

   function Wait_For_Input
     (DB      : Database_Connection;
      Timeout : Duration := Duration'Last) return Boolean
   is
      use GNAT.Sockets;
      function To_Ada is new Ada.Unchecked_Conversion
        (Interfaces.C.int, Socket_Type);
      DBG : constant access Gnade.Database'Class := Builder.To_Native (DB);
      Sel : Selector_Type;
      Soc : constant Socket_Type := To_Ada (DBG.Socket);
      St  : Selector_Status;
      SS  : Socket_Set_Type;
      SE  : Socket_Set_Type;
      Rq  : Request_Type (N_Bytes_To_Read);
   begin
      if DBG.Is_Non_Blocking then
         raise Program_Error with "Non blocking connection is not supported";
      end if;

      Set (SS, Soc);
      Create_Selector (Sel);
      Check_Selector
        (Sel, R_Socket_Set => SS, W_Socket_Set => SE, Status => St,
         Timeout => Duration'Min (Forever, Timeout));
      Close_Selector (Sel);

      if St = Completed then
         Control_Socket (Soc, Rq);

         if Rq.Size = 0 then
            --  Socket ready to read but without data available mean socket
            --  closed by peer.

            DB.Set_Failure ("Connection closed on PostgreSQL server side");
            return False;
         end if;

         if not DBG.Consume_Input then
            DB.Set_Failure (DBG.Error);
            return False;
         end if;

         return True;
      end if;

      return False;
   end Wait_For_Input;

   ---------------
   -- OID_Field --
   ---------------

   function OID_Field (Table : SQL_Table'Class) return SQL_Field_Integer is
   begin
      return SQL_Field_Integer'
        (Table          => Table.Table_Name,
         Instance       => Table.Instance,
         Instance_Index => Table.Instance_Index,
         Constraints    => <>,
         Name           => N_OID'Access);
   end OID_Field;

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
      return SQL_PG_Returning'(Returning => Fields);
   end Returning;

   ----------------------------
   -- On_Conflict_Do_Nothing --
   ----------------------------

   function On_Conflict_Do_Nothing
      (Column  : SQL_Field'Class) return SQL_PG_Extension'Class is
   begin
      return SQL_PG_On_Conflict'
         (Do_Nothing => True,
          Column     => +Column,
          Constraint => Null_XString);
   end On_Conflict_Do_Nothing;

   ----------------------------
   -- On_Conflict_Do_Nothing --
   ----------------------------

   function On_Conflict_Do_Nothing
      (Constraint_Name : String := "") return SQL_PG_Extension'Class is
   begin
      return SQL_PG_On_Conflict'
         (Do_Nothing => True,
          Column     => No_Field_Pointer,
          Constraint => GNATCOLL.Strings.To_XString (Constraint_Name));
   end On_Conflict_Do_Nothing;

   ---------------------------
   -- On_Conflict_Do_Update --
   ---------------------------

   function On_Conflict_Do_Update
      (Column  : SQL_Field'Class;
       Set     : SQL_Assignment;
       Where   : SQL_Criteria := No_Criteria)
      return SQL_PG_Extension'Class is
   begin
      return SQL_PG_On_Conflict'
         (Do_Nothing => False,
          Column     => +Column,
          Constraint => Null_XString,
          Set        => Set,
          Where      => Where);
   end On_Conflict_Do_Update;

   ---------------------------
   -- On_Conflict_Do_Update --
   ---------------------------

   function On_Conflict_Do_Update
      (Constraint_Name : String;
       Set             : SQL_Assignment;
       Where           : SQL_Criteria := No_Criteria)
      return SQL_PG_Extension'Class is
   begin
      return SQL_PG_On_Conflict'
         (Do_Nothing => False,
          Column     => No_Field_Pointer,
          Constraint => GNATCOLL.Strings.To_XString (Constraint_Name),
          Set        => Set,
          Where      => Where);
   end On_Conflict_Do_Update;

   ---------
   -- "&" --
   ---------

   function "&"
     (Query     : SQL_Query;
      Extension : SQL_PG_Extension'Class) return SQL_Query
   is
      Data : Query_Postgres_Contents;
      Q    : SQL_Query;
   begin
      if Query.Get.all in Query_Postgres_Contents'Class then
         --  Merge the information with what has already been set.
         --  For now, assume that Extension is the same type as was
         --  already set, since we have a single extension for Update
         --  and a single extension for Select. Any other combination
         --  is invalid.

         if Extension in SQL_PG_For_Update'Class then
            declare
               Orig : SQL_PG_For_Update'Class renames
                  SQL_PG_For_Update'Class
                    (Query_Postgres_Contents'Class (Query.Get.all).Extra.all);
            begin
               Orig.Tables := Orig.Tables &
                  SQL_PG_For_Update'Class (Extension).Tables;
               Orig.No_Wait := Orig.No_Wait or else
                  SQL_PG_For_Update'Class (Extension).No_Wait;
            end;

         else
            declare
               Orig : SQL_PG_Returning'Class renames
                  SQL_PG_Returning'Class
                    (Query_Postgres_Contents'Class (Query.Get.all).Extra.all);
            begin
               Orig.Returning := Orig.Returning &
                   SQL_PG_Returning'Class (Extension).Returning;
            end;
         end if;

         return Query;

      else
         Data.Base := Query;
         Data.Extra := new SQL_PG_Extension'Class'(Extension);
         Q.Set (Data);
         return Q;
      end if;
   end "&";

   ----------------------
   -- Append_To_String --
   ----------------------

   overriding procedure Append_To_String
     (Self   : SQL_PG_For_Update;
      Format : Formatter'Class;
      Result : in out XString)
   is
   begin
      Result.Append (" FOR UPDATE");
      if Self.Tables /= Empty_Table_List then
         Result.Append (" OF ");
         Append_To_String (Self.Tables, Format, Result, Show_Types => False);
      end if;

      if Self.No_Wait then
         Result.Append (" NO WAIT");
      end if;
   end Append_To_String;

   ----------------------
   -- Append_To_String --
   ----------------------

   overriding procedure Append_To_String
     (Self   : SQL_PG_Returning;
      Format : Formatter'Class;
      Result : in out XString) is
   begin
      Append (Result, " RETURNING ");
      Append_To_String
         (Self.Returning, Format, Long => True, Result => Result,
          Show_Types => False);
   end Append_To_String;

   ----------------------
   -- Append_To_String --
   ----------------------

   overriding procedure Append_To_String
     (Self   : SQL_PG_On_Conflict;
      Format : Formatter'Class;
      Result : in out XString)
   is
   begin
      if Self.Constraint /= "" then
         Result.Append (" ON CONFLICT ON CONSTRAINT ");
         Result.Append (Self.Constraint);
      elsif Self.Column /= No_Field_Pointer then
         Result.Append (" ON CONFLICT (");
         Append_To_String
            (Self.Column, Format, Result => Result,
             Long => False, Show_Types => False);
         Result.Append (')');
      else
         Result.Append (" ON CONFLICT");
      end if;

      if Self.Do_Nothing then
         Result.Append (" DO NOTHING");
      else
         Result.Append (" DO UPDATE SET ");
         Append_To_String
            (Self.Set, Format, With_Field => True, Result => Result);

         if Self.Where /= No_Criteria then
            Result.Append (" WHERE ");
            Append_To_String
               (Self.Where, Format, Long => True, Result => Result);
         end if;
      end if;
   end Append_To_String;

   ---------------------------
   -- Get_Connection_String --
   ---------------------------

   function Get_Connection_String
     (Description   : Database_Description;
      With_Password : Boolean) return String
   is
      Descr : constant Postgres_Description_Access :=
        Postgres_Description_Access (Description);
      Str : XString;

      procedure Escape (Value : XString);
      procedure Escape (Value : XString) is
      begin
         for C of Value loop
            if C = ''' or else C = '\' then
               Str.Append ('\');
            end if;
            Str.Append (C);
         end loop;
      end Escape;

   begin
      Str.Append ("dbname='");
      Escape (Descr.Dbname);
      Str.Append (''');

      if Descr.User /= Null_XString then
         Str.Append (" user='");
         Escape (Descr.User);
         Str.Append (''');
      end if;

      if Descr.Host /= Null_XString then
         Str.Append (" host='");
         Escape (Descr.Host);
         Str.Append (''');
      end if;

      if Descr.Port /= -1 then
         Str.Append (" port=" & Image (Descr.Port, Min_Width => 1));
      end if;

      if With_Password and then Descr.Password /= Null_XString then
         Str.Append (" password='");
         Escape (Descr.Password);
         Str.Append (''');
      end if;

      case Descr.SSL is
         when Disable => Str.Append (" sslmode=disable");
         when Allow   => Str.Append (" sslmode=allow");
         when Prefer  => Str.Append (" sslmode=prefer");
         when Require => Str.Append (" sslmode=require");
      end case;

      if Descr.Appname /= Null_XString then
         Str.Append (" application_name='");
         Escape (Descr.Appname);
         Str.Append (''');
      end if;

      return Str.To_String;
   end Get_Connection_String;

   --------------------------
   -- Get_Application_Name --
   --------------------------

   overriding function Get_Application_Name
      (Description : not null access Postgres_Description) return String
   is
   begin
      return Description.Appname.To_String;
   end Get_Application_Name;

end GNATCOLL.SQL.Postgres;
