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

with Ada.Calendar;            use Ada.Calendar;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Task_Attributes;
with Ada.Unchecked_Deallocation;
with GNAT.Calendar.Time_IO;
with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.Utils;          use GNATCOLL.Utils;

package body GNATCOLL.SQL.Exec is

   Me_Error  : constant Trace_Handle := Create ("SQL.ERROR", On);
   Me_Select : constant Trace_Handle := Create ("SQL.SELECT");
   Me_Query  : constant Trace_Handle := Create ("SQL");

   Cache_Expiration_Delay : constant Duration := 3600.0;  --  1 hour
   --  Delay after which the SQL cache expires and must be reset

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Query_Result_Content'Class, Query_Result_Content_Access);

   package DB_Attributes is new Ada.Task_Attributes
     (Database_Connection, null);

   function Is_Select_Query (Query : String) return Boolean;
   --  Return true if Query is a select query

   procedure Execute_And_Log
     (Connection : access Database_Connection_Record'Class;
      R          : out Query_Result_Content_Access;
      Query      : String;
      Is_Select  : Boolean);
   --  Low-level call to perform a query on the database

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Query_Result,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   protected Query_Cache is
      procedure Get_Result
        (Query  : String;
         Cached : out Query_Result;
         Found  : out Boolean);
      --  Return null or the cached value for Query

      procedure Set_Cache (Query : String; Cached : Query_Result);
      --  Add a new value in the cache

      procedure Reset;
      --  Reset the cache

   private
      Cache     : String_Maps.Map;
      Timestamp : Ada.Calendar.Time := Ada.Calendar.Clock;
   end Query_Cache;

   -----------------
   -- Query_Cache --
   -----------------

   protected body Query_Cache is

      ----------------
      -- Get_Result --
      ----------------

      procedure Get_Result
        (Query  : String;
         Cached : out Query_Result;
         Found  : out Boolean)
      is
         use String_Maps;
         C : String_Maps.Cursor;
      begin
         if Clock - Timestamp > Cache_Expiration_Delay then
            Reset;
            Found := False;
         else
            C := Find (Cache, Query);
            if Has_Element (C) then
               Cached := Element (C);
               Found  := True;
            else
               Found  := False;
            end if;
         end if;
      end Get_Result;

      ---------------
      -- Set_Cache --
      ---------------

      procedure Set_Cache (Query : String; Cached : Query_Result) is
         use String_Maps;
      begin
         Include (Cache, Query, Cached);
      end Set_Cache;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         String_Maps.Clear (Cache);
         Timestamp := Clock;
      end Reset;

   end Query_Cache;

   -------------------
   -- Print_Warning --
   -------------------

   procedure Print_Warning
     (Connection : access Database_Connection_Record'Class; Str : String) is
   begin
      Trace (Me_Query, Str & " (" & Connection.Username.all & ")");
   end Print_Warning;

   -----------------
   -- Print_Error --
   -----------------

   procedure Print_Error
     (Connection : access Database_Connection_Record'Class; Str : String) is
   begin
      Trace (Me_Error, Str & " (" & Connection.Username.all & ")");
   end Print_Error;

   --------------------
   -- Setup_Database --
   --------------------

   procedure Setup_Database
     (Description   : out Database_Description;
      Database      : String;
      User          : String := "";
      Host          : String := "";
      Password      : String := "";
      DBMS          : String := DBMS_Postgresql;
      Cache_Support : Boolean := False)
   is
   begin
      if Description = null then
         Description := new Database_Description_Record;
      end if;

      GNAT.Strings.Free (Description.Host);
      GNAT.Strings.Free (Description.User);
      GNAT.Strings.Free (Description.Dbname);
      GNAT.Strings.Free (Description.Password);
      GNAT.Strings.Free (Description.DBMS);

      Description.Dbname   := new String'(Database);
      Description.User     := new String'(User);
      Description.Password := new String'(Password);
      Description.DBMS     := new String'(DBMS);

      if Host /= ""
        and then Host /= "localhost"
      then
         Description.Host := new String'(Host);
      else
         Description.Host := new String'("");
      end if;

      Description.Caching   := Cache_Support;
   end Setup_Database;

   --------------
   -- Get_Host --
   --------------

   function Get_Host     (Description : Database_Description) return String is
   begin
      return Description.Host.all;
   end Get_Host;

   --------------
   -- Get_User --
   --------------

   function Get_User     (Description : Database_Description) return String is
   begin
      return Description.User.all;
   end Get_User;

   ------------------
   -- Get_Database --
   ------------------

   function Get_Database (Description : Database_Description) return String is
   begin
      return Description.Dbname.all;
   end Get_Database;

   ------------------
   -- Get_Password --
   ------------------

   function Get_Password (Description : Database_Description) return String is
   begin
      return Description.Password.all;
   end Get_Password;

   --------------
   -- Get_DBMS --
   --------------

   function Get_DBMS (Description : Database_Description) return String is
   begin
      return Description.DBMS.all;
   end Get_DBMS;

   ----------------------
   -- Check_Connection --
   ----------------------

   function Check_Connection
     (Connection : access Database_Connection_Record'Class) return Boolean
   is
      Success    : Boolean;
      R          : Query_Result_Content_Access;
   begin
      Connect_And_Execute
        (Connection,
         Query     => "",
         R         => R,
         Is_Select => False);
      Success := R /= null;
      Unchecked_Free (R);

      if Success then
         Trace (Me_Query, "Init_Database: database successfuly initialized");
      else
         Trace
           (Me_Error,
            "Init_Database: database initialization FAILED: "
            & Error (Connection));
      end if;

      return Success;
   end Check_Connection;

   ---------------------
   -- Is_Select_Query --
   ---------------------

   function Is_Select_Query (Query : String) return Boolean is
      --  Allow both "SELECT" and "(SELECT" (the latter is used when we do a
      --  union between two selects
      Cst_Select      : constant String := "SELECT ";
   begin
      return Query'Length > Cst_Select'Length + 1
        and then
          (Query (Query'First .. Query'First + Cst_Select'Length - 1)
           = Cst_Select
           or else Query (Query'First + 1 .. Query'First + Cst_Select'Length) =
             Cst_Select);
   end Is_Select_Query;

   ---------------------
   -- Execute_And_Log --
   ---------------------

   procedure Execute_And_Log
     (Connection : access Database_Connection_Record'Class;
      R          : out Query_Result_Content_Access;
      Query      : String;
      Is_Select  : Boolean) is
   begin
      if Perform_Queries then
         Connect_And_Execute
           (Connection => Connection,
            Query      => Query,
            R          => R,
            Is_Select  => Is_Select);
      end if;

      if R = null then
         Set_Failure (Connection);

      elsif Is_Select then
         --  ??? Should use the local mirror database when doing a select,
         --  to speed up queries. Are we garanteed, with the mirror, that
         --  doing a INSERT on the master, and immediately a SELECT on the
         --  slave will return the newly inserted values ?
         Connection.Success := Is_Success (R.all);
         if not Connection.Success then
            Set_Failure (Connection);
            Trace
              (Me_Query,
               Query & " (" & Status (R.all) & " " & Error_Msg (R.all)
               & " (" & Connection.Username.all & ")");
         else
            Trace
              (Me_Select,
               Query & " ("
               & Image (Natural (Tuple_Count (R.all)), Min_Width => 1)
               & " tuples) "
               & Status (R.all) & " (" & Connection.Username.all & ")");
         end if;

      else
         Connection.Success := Is_Success (R.all);
         if not Connection.Success then
            Set_Failure (Connection);
            Trace
              (Me_Query,
               Query & " (" & Status (R.all) & " " & Error_Msg (R.all)
               & " (" & Connection.Username.all & ")");
         else
            Trace
              (Me_Query,
               Query & " ("
               & Image (Natural (Tuple_Count (R.all)), Min_Width => 1)
               & " tuples) " & Status (R.all)
               & " (" & Connection.Username.all & ")");
         end if;
      end if;
   end Execute_And_Log;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      R          : out Query_Result;
      Query      : String;
      Use_Cache  : Boolean := False)
   is
      Is_Select   : constant Boolean := Is_Select_Query (Query);
      Is_Begin    : constant Boolean := To_Lower (Query) = "begin";
      Is_Commit   : constant Boolean := To_Lower (Query) = "commit";
      Is_Rollback : constant Boolean := To_Lower (Query) = "rollback";
      Found       : Boolean;
   begin
      if Connection.In_Transaction
        and then not Connection.Success
      then
         Trace
           (Me_Error,
            "Ignored, since transaction failed: " & Query
            & " (" & Connection.Username.all & ")");
         return;

      elsif not Connection.In_Transaction
        and then Is_Begin
      then
         Connection.In_Transaction := True;

      elsif Connection.In_Transaction
        and then Is_Begin
      then
         --  ??? Could be ignored silently in fact, but this helps debugging
         raise Program_Error;

      elsif not Connection.In_Transaction
        and then not Is_Commit
        and then not Is_Rollback
        and then not Is_Select   --  INSERT, UPDATE, LOCK, DELETE,...
      then
         --  Start a transaction automatically
         Execute (Connection, "BEGIN", Use_Cache  => False);
         Connection.In_Transaction := True;
         if not Connection.Success then
            return;
         end if;
      end if;

      if Use_Cache and then Connection.DB.Caching then
         Query_Cache.Get_Result (Query, R, Found);
         if Found then
            if Is_Select then
               Trace (Me_Select, "Use cache for " & Query);
            else
               Trace (Me_Query, "Use cache for " & Query);
            end if;
            return;
         end if;
      end if;

      Execute_And_Log (Connection, R.Res, Query, Is_Select);

      if Use_Cache and then Connection.DB.Caching then
         Query_Cache.Set_Cache (Query, R);
      end if;

      if Connection.In_Transaction
        and then (Is_Commit or Is_Rollback)
      then
         Connection.In_Transaction := False;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      R          : out Query_Result;
      Query      : SQL_Query;
      Use_Cache  : Boolean := False) is
   begin
      Execute (Connection, R, To_String (To_String (Query)), Use_Cache);
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      Query      : SQL_Query;
      Use_Cache  : Boolean := False)
   is
      R : Query_Result;
      pragma Unreferenced (R);
   begin
      Execute (Connection, R, Query, Use_Cache);
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      Query      : String;
      Use_Cache  : Boolean := False)
   is
      R : Query_Result;
      pragma Unreferenced (R);
   begin
      Execute (Connection, R, Query, Use_Cache);
   end Execute;

   -------------
   -- Success --
   -------------

   function Success
     (Connection : access Database_Connection_Record) return Boolean is
   begin
      return Connection.Success;
   end Success;

   -----------------
   -- Set_Failure --
   -----------------

   procedure Set_Failure
     (Connection : access Database_Connection_Record'Class;
      Error_Msg  : String := "") is
   begin
      Connection.Success := False;
      if Connection.Error_Msg = null then
         if Error_Msg /= "" then
            Connection.Error_Msg := new String'(Error_Msg);
         else
            declare
               E : constant String := Error (Connection);
            begin
               if E /= "" then
                  Connection.Error_Msg := new String'(E);
               end if;
            end;
         end if;
      end if;
   end Set_Failure;

   --------------------
   -- In_Transaction --
   --------------------

   function In_Transaction
     (Connection : access Database_Connection_Record'Class) return Boolean is
   begin
      return Connection.In_Transaction;
   end In_Transaction;

   --------------
   -- Rollback --
   --------------

   procedure Rollback
     (Connection : access Database_Connection_Record'Class;
      Error_Msg  : String := "") is
   begin
      if Connection.In_Transaction then
         Connection.Success := True; --  we are allowed to perform this
         Execute (Connection, "ROLLBACK");
         if Connection.Error_Msg = null and then Error_Msg /= "" then
            Connection.Error_Msg := new String'(Error_Msg);
         end if;
      end if;
   end Rollback;

   ------------------------
   -- Commit_Or_Rollback --
   ------------------------

   procedure Commit_Or_Rollback
     (Connection : access Database_Connection_Record'Class) is
   begin
      if Connection.In_Transaction then
         if Connection.Success then
            Execute (Connection, "COMMIT");
         else
            Rollback (Connection);

            --  Still marked as failed, since the transaction was never
            --  performed.
            Connection.Success := False;
         end if;
      end if;
   end Commit_Or_Rollback;

   ----------------------
   -- Invalidate_Cache --
   ----------------------

   procedure Invalidate_Cache
     (Connection : access Database_Connection_Record'Class)
   is
      pragma Unreferenced (Connection);
   begin
      Trace (Me_Query, "Invalidate SQL cache");
      Query_Cache.Reset;
   end Invalidate_Cache;

   ----------------------
   -- Reset_Connection --
   ----------------------

   procedure Reset_Connection
     (Description : Database_Description;
      Connection  : access Database_Connection_Record'Class;
      Username    : String := "") is
   begin
      Connection.DB             := Description;
      Rollback (Connection); --  In case a previous thread had started on
      Connection.Success        := True;

      if Username /= "" or else Connection.Username = null then
         GNAT.Strings.Free (Connection.Username);
         Connection.Username := new String'(Username);
      end if;

      GNAT.Strings.Free (Connection.Error_Msg);
   end Reset_Connection;

   -------------------------
   -- Get_Task_Connection --
   -------------------------

   function Get_Task_Connection
     (Description : Database_Description;
      Factory     : access function
        (Desc : Database_Description) return Database_Connection;
      Username    : String := "")
      return Database_Connection
   is
      Connection : Database_Connection;
   begin
      Connection := DB_Attributes.Value;
      if Connection = null then
         Connection := Factory (Description);
         if Connection /= null then
            DB_Attributes.Set_Value (Connection);
         end if;
      end if;

      Reset_Connection (Description, Connection, Username);
      return Connection;
   end Get_Task_Connection;

   ------------------------
   -- Last_Error_Message --
   ------------------------

   function Last_Error_Message
     (Connection : access Database_Connection_Record'Class) return String
   is
   begin
      if Connection.Error_Msg = null then
         return "";
      else
         return Connection.Error_Msg.all;
      end if;
   end Last_Error_Message;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Query_Result) is
   begin
      if Self.Res /= null then
         Self.Res.Refcount := Self.Res.Refcount + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Query_Result) is
   begin
      if Self.Res /= null then
         Self.Res.Refcount := Self.Res.Refcount - 1;
         if Self.Res.Refcount = 0 then
            Finalize (Self.Res.all);
            Unchecked_Free (Self.Res);
         end if;
      end if;
   end Finalize;

   -----------
   -- Value --
   -----------

   function Boolean_Value
     (Res   : Query_Result_Content;
      Tuple : Tuple_Index;
      Field : Field_Index) return Boolean is
   begin
      return Boolean'Value
        (Value (Query_Result_Content'Class (Res), Tuple, Field));
   end Boolean_Value;

   -----------
   -- Value --
   -----------

   function Integer_Value
     (Res   : Query_Result_Content;
      Tuple : Tuple_Index;
      Field : Field_Index) return Integer is
   begin
      return Integer'Value
        (Value (Query_Result_Content'Class (Res), Tuple, Field));
   end Integer_Value;

   -----------
   -- Value --
   -----------

   function Float_Value
     (Res   : Query_Result_Content;
      Tuple : Tuple_Index;
      Field : Field_Index) return Float is
   begin
      return Float'Value
        (Value (Query_Result_Content'Class (Res), Tuple, Field));
   end Float_Value;

   -----------
   -- Value --
   -----------

   function Time_Value
     (Res   : Query_Result_Content;
      Tuple : Tuple_Index;
      Field : Field_Index) return Ada.Calendar.Time
   is
      Val : constant String :=
        Value (Query_Result_Content'Class (Res), Tuple, Field);
   begin
      if Val = "" then
         return No_Time;
      else
         --  Workaround bug(?) in GNAT.Calendar.Time_IO: if there is no time,
         --  set one to avoid daylight saving time issues

         if Ada.Strings.Fixed.Index (Val, ":") < Val'First then
            return GNAT.Calendar.Time_IO.Value (Val & " 12:00:00");
         else
            return GNAT.Calendar.Time_IO.Value (Val);
         end if;
      end if;
   end Time_Value;

   -----------------
   -- Tuple_Count --
   -----------------

   function Tuple_Count (Res : Query_Result) return Tuple_Index is
   begin
      if Res.Res = null then
         return 0;
      else
         return Tuple_Count (Res.Res.all);
      end if;
   end Tuple_Count;

   -----------
   -- Value --
   -----------

   function Value
     (Res   : Query_Result;
      Tuple : Tuple_Index;
      Field : Field_Index) return String is
   begin
      return Value (Res.Res.all, Tuple, Field);
   end Value;

   -------------------
   -- Address_Value --
   -------------------

   function Address_Value
     (Res   : Query_Result;
      Tuple : Tuple_Index;
      Field : Field_Index) return System.Address
   is
   begin
      return Address_Value (Res.Res.all, Tuple, Field);
   end Address_Value;

   -----------
   -- Value --
   -----------

   function Boolean_Value
     (Res   : Query_Result;
      Tuple : Tuple_Index;
      Field : Field_Index) return Boolean is
   begin
      return Boolean_Value (Res.Res.all, Tuple, Field);
   end Boolean_Value;

   -----------
   -- Value --
   -----------

   function Integer_Value
     (Res   : Query_Result;
      Tuple : Tuple_Index;
      Field : Field_Index;
      Default : Integer := Integer'First) return Integer
   is
   begin
      return Integer_Value (Res.Res.all, Tuple, Field);
   exception
      when Constraint_Error =>
         return Default;
   end Integer_Value;

   -----------
   -- Value --
   -----------

   function Float_Value
     (Res   : Query_Result;
      Tuple : Tuple_Index;
      Field : Field_Index) return Float is
   begin
      return Float_Value (Res.Res.all, Tuple, Field);
   end Float_Value;

   -----------
   -- Value --
   -----------

   function Time_Value
     (Res   : Query_Result;
      Tuple : Tuple_Index;
      Field : Field_Index) return Ada.Calendar.Time is
   begin
      return Time_Value (Res.Res.all, Tuple, Field);
   end Time_Value;

   -------------
   -- Is_Null --
   -------------

   function Is_Null
     (Res   : Query_Result;
      Tuple : Tuple_Index;
      Field : Field_Index) return Boolean is
   begin
      return Is_Null (Res.Res.all, Tuple, Field);
   end Is_Null;

   -------------
   -- Last_Id --
   -------------

   function Last_Id
     (Connection : access Database_Connection_Record'Class;
      Res        : Query_Result;
      Field      : SQL_Field_Integer) return Integer is
   begin
      return Last_Id (Connection, Res.Res.all, Field);
   end Last_Id;

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description
     (Connection : access Database_Connection_Record'Class)
      return Database_Description
   is
   begin
      return Connection.DB;
   end Get_Description;

   -----------------
   -- Field_Count --
   -----------------

   function Field_Count (Res : Query_Result) return Field_Index is
   begin
      return Field_Count (Res.Res.all);
   end Field_Count;

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name
     (Res : Query_Result; Field : Field_Index) return String is
   begin
      return Field_Name (Res.Res.all, Field);
   end Field_Name;

end GNATCOLL.SQL.Exec;
