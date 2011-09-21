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

with Ada.Calendar;               use Ada.Calendar;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Task_Attributes;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.SQL.Exec_Private; use GNATCOLL.SQL.Exec_Private;
with System.Address_Image;

package body GNATCOLL.SQL.Exec is

   Me_Error  : constant Trace_Handle := Create ("SQL.ERROR", On);
   Me_Select : constant Trace_Handle := Create ("SQL.SELECT");
   Me_Cache  : constant Trace_Handle := Create ("SQL.CACHE");
   Me_Query  : constant Trace_Handle := Create ("SQL");

   Cache_Expiration_Delay : constant Duration := 3600.0;  --  1 hour
   --  Delay after which the SQL cache expires and must be reset

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Abstract_DBMS_Forward_Cursor'Class, Abstract_Cursor_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Prepared_In_Session, Prepared_In_Session_List);

   package DB_Attributes is new Ada.Task_Attributes
     (Database_Connection, null);

   function Is_Select_Query (Query : String) return Boolean;
   --  Return true if Query is a select query

   procedure Prepare_If_Needed
     (Connection : access Database_Connection_Record'Class;
      Stmt       : Prepared_Statement'Class;
      Result     : out DBMS_Stmt);
   --  Prepare, if needed, the statement on the server

   procedure Execute_And_Log
     (Result     : in out Forward_Cursor'Class;
      Connection : access Database_Connection_Record'Class;
      Query      : String;
      Prepared   : Prepared_Statement'Class := No_Prepared;
      Is_Select  : Boolean;
      Direct     : Boolean;
      Params     : SQL_Parameters := No_Parameters);
   --  Low-level call to perform a query on the database and log results

   function Hash (Key : Cache_Id) return Ada.Containers.Hash_Type;

   package Cached_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Cache_Id,
      Element_Type    => Direct_Cursor,
      Hash            => Hash,
      Equivalent_Keys => "=");
   --  Cache the results of queries

   procedure Compute_Statement
     (Stmt : Prepared_Statement'Class; Format : Formatter'Class);
   --  Format the statement into a string, if not done yet

   function Hash (Key : Database_Connection) return Ada.Containers.Hash_Type;
   package Freed_DB_Maps is new Ada.Containers.Hashed_Sets
     (Element_Type        => Database_Connection,
      Hash                => Hash,
      Equivalent_Elements => "=");
   --  List of connections that were freed, so that we no longer try to use
   --  them

   -----------------
   -- Query_Cache --
   -----------------

   protected Query_Cache is
      procedure Get_Result
        (Stmt    : Prepared_Statement'Class;
         Cached  : out Direct_Cursor;
         Found   : out Boolean;
         Params  : SQL_Parameters := No_Parameters);
      --  Return null or the cached value for the statement

      procedure Set_Id (Stmt : Prepared_Statement'Class);
      --  Set the Cached_Result field of Stmt

      procedure Set_Cache
        (Stmt : Prepared_Statement'Class; Cached : Direct_Cursor);
      --  Add a new value in the cache

      procedure Unset_Cache (Stmt : Prepared_Statement_Data);
      --  Unset the cache entry for this particular element

      procedure Reset;
      --  Reset the cache

      procedure Mark_DB_As_Free (DB : Database_Connection);
      function Was_Freed (DB : Database_Connection) return Boolean;

   private
      Current_Cache_Id : Cache_Id := 1;
      --  First unassigned id for prepared statements

      Freed_DB  : Freed_DB_Maps.Set;

      Cache     : Cached_Maps.Map;
      Timestamp : Ada.Calendar.Time := Ada.Calendar.Clock;
   end Query_Cache;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Cache_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Database_Connection) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (System.Address_Image (Key.all'Address));
   end Hash;

   -----------------
   -- Query_Cache --
   -----------------

   protected body Query_Cache is

      ----------------
      -- Get_Result --
      ----------------

      procedure Get_Result
        (Stmt    : Prepared_Statement'Class;
         Cached  : out Direct_Cursor;
         Found   : out Boolean;
         Params  : SQL_Parameters := No_Parameters)
      is
         C : Cached_Maps.Cursor;
         S : constant Prepared_Statements.Encapsulated_Access := Stmt.Get;
      begin
         if Params /= No_Parameters then
            --  ??? The cache should take the parameters into account
            Found := False;
            return;
         end if;

         if Clock - Timestamp > Cache_Expiration_Delay then
            Reset;
            Found := False;
         else
            if S.Cached_Result = No_Cache_Id
              or else not S.Use_Cache
            then
               Found := False;
            else
               C := Cached_Maps.Find (Cache, S.Cached_Result);
               Found := Cached_Maps.Has_Element (C);
               if Found then
                  Cached := Cached_Maps.Element (C);
               end if;
            end if;
         end if;
      end Get_Result;

      ------------
      -- Set_Id --
      ------------

      procedure Set_Id (Stmt : Prepared_Statement'Class) is
         S : constant Prepared_Statements.Encapsulated_Access := Stmt.Get;
      begin
         if S.Cached_Result = No_Cache_Id then
            S.Cached_Result := Current_Cache_Id;
            Current_Cache_Id := Current_Cache_Id + 1;
         end if;
      end Set_Id;

      ---------------
      -- Set_Cache --
      ---------------

      procedure Set_Cache
        (Stmt : Prepared_Statement'Class; Cached : Direct_Cursor)
      is
         S : constant Prepared_Statements.Encapsulated_Access := Stmt.Get;
      begin
         --  Reserve capacity up to the current assigned id, since we are
         --  likely to need it anyway, and it is bound to be at least as big
         --  as Stmt.Cached.Id

         if S.Use_Cache then
            Set_Id (Stmt);
            Cache.Include (S.Cached_Result, Cached);
         end if;
      end Set_Cache;

      -----------------
      -- Unset_Cache --
      -----------------

      procedure Unset_Cache (Stmt : Prepared_Statement_Data) is
         C : Cached_Maps.Cursor;
      begin
         if Stmt.Cached_Result /= No_Cache_Id
           and then Stmt.Use_Cache
         then
            C := Cache.Find (Stmt.Cached_Result);
            if Cached_Maps.Has_Element (C) then
               Trace (Me_Query, "Unset cache for " & Stmt.Name.all);
               Cache.Delete (C);
            end if;
         end if;
      end Unset_Cache;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Cache.Clear;
         Timestamp := Clock;
      end Reset;

      ---------------------
      -- Mark_DB_As_Free --
      ---------------------

      procedure Mark_DB_As_Free (DB : Database_Connection) is
      begin
         Freed_DB.Include (DB);
      end Mark_DB_As_Free;

      ---------------
      -- Was_Freed --
      ---------------

      function Was_Freed (DB : Database_Connection) return Boolean is
      begin
         return Freed_DB.Contains (DB);
      end Was_Freed;

   end Query_Cache;

   -----------------------
   -- Compute_Statement --
   -----------------------

   procedure Compute_Statement
     (Stmt : Prepared_Statement'Class; Format : Formatter'Class)
   is
      S : constant Prepared_Statements.Encapsulated_Access := Stmt.Get;
   begin
      if S.Query_Str = null then
         S.Query_Str := new String'(To_String (To_String (S.Query, Format)));

         if Active (Me_Query) then
            Trace
              (Me_Query, "compute (" & S.Name.all & "): " & S.Query_Str.all);
         end if;

         S.Query := No_Query;   --  release memory
         S.Is_Select := Is_Select_Query (S.Query_Str.all);
      end if;
   end Compute_Statement;

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

   ----------
   -- Free --
   ----------

   procedure Free (Description : in out Database_Description) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Database_Description_Record'Class, Database_Description);
   begin
      if Description /= null then
         Free (Description.all);
         Unchecked_Free (Description);
      end if;
   end Free;

   ----------------------
   -- Check_Connection --
   ----------------------

   function Check_Connection
     (Connection : access Database_Connection_Record'Class) return Boolean
   is
      Success    : Boolean;
      R          : Abstract_Cursor_Access;
   begin
      if Connection = null then
         Trace (Me_Error, "DBMS backend not supported");
         return False;
      end if;

      R := Connect_And_Execute
        (Connection,
         Query     => "",
         Is_Select => False,
         Direct    => False);
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

   -----------
   -- Image --
   -----------

   function Image
     (Format : Formatter'Class; Param : SQL_Parameter) return String is
   begin
      if Param = Null_Parameter then
         return "<none>";
      else
         case Param.Typ is
         when Parameter_Text    =>
            return String_To_SQL (Format, Param.Str_Val.all, Quote => False);
         when Parameter_Integer =>
            return Integer_To_SQL (Format, Param.Int_Val, Quote => False);
         when Parameter_Float   =>
            return Float_To_SQL (Format, Param.Float_Val, Quote => False);
         when Parameter_Boolean =>
            return Boolean_To_SQL (Format, Param.Bool_Val, Quote => False);
         when Parameter_Time =>
            return Time_To_SQL (Format, Param.Time_Val, Quote => False);
         when Parameter_Date =>
            return Date_To_SQL (Format, Param.Date_Val, Quote => False);
         when Parameter_Character =>
            return String'(1 .. 1 => Param.Char_Val);
         end case;
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Format : Formatter'Class; Params : SQL_Parameters)
      return String
   is
      Result : Unbounded_String;
   begin
      for P in Params'Range loop
         Append (Result, ", ");
--           Append (Result, Image (P, 0));
--           Append (Result, "=>");
         Append (Result, Image (Format, Params (P)));
      end loop;

      return To_String (Result);
   end Image;

   --------------------------
   -- Post_Execute_And_Log --
   --------------------------

   procedure Post_Execute_And_Log
     (R          : access Abstract_DBMS_Forward_Cursor'Class;
      Connection : access Database_Connection_Record'Class;
      Query      : String;
      Prepared   : Prepared_Statement'Class := No_Prepared;
      Is_Select  : Boolean;
      Params     : SQL_Parameters := No_Parameters)
   is
      function Get_Rows return String;
      --  The number of rows downloaded. If we only have a forward cursor, we
      --  can't display them

      function Get_User return String;
      --  Return the user name

      function Get_Query return String;
      --  Return the query

      function Get_Query return String is
         use type Prepared_Statements.Encapsulated_Access;
         S : constant Prepared_Statements.Encapsulated_Access := Prepared.Get;
      begin
         if S /= null then
            return "(" & S.Name.all & ")";
         else
            return Query;
         end if;
      end Get_Query;

      function Get_User return String is
      begin
         if Connection.Username.all = "" then
            return "";
         else
            return " (" & Connection.Username.all & ")";
         end if;
      end Get_User;

      function Get_Rows return String is
      begin
         if R.all in DBMS_Direct_Cursor'Class then
            return " (" & Image
              (Processed_Rows (DBMS_Forward_Cursor'Class (R.all)),
               Min_Width => 1) & " tuples)";

         elsif Is_Select
           and then not Has_Row (DBMS_Forward_Cursor'Class (R.all))
         then
            return " (no tuples)";

         else
            --  We cannot count the number of rows, which would require getting
            --  all of them.
            return "";
         end if;
      end Get_Rows;

   begin
      if R = null then
         if Active (Me_Error) then
            Trace (Me_Error, "Transaction failed " & Query);
         end if;

         Set_Failure (Connection);

      elsif Is_Select then
         --  ??? Should use the local mirror database when doing a select,
         --  to speed up queries. Are we garanteed, with the mirror, that
         --  doing a INSERT on the master, and immediately a SELECT on the
         --  slave will return the newly inserted values ?
         Connection.Success := Is_Success (DBMS_Forward_Cursor'Class (R.all));

         if not Connection.Success then
            if Active (Me_Error) then
               Trace (Me_Error, "Transaction failed in select " & Query);
            end if;
            Set_Failure (Connection);

            if Active (Me_Query) then
               Trace
                 (Me_Query,
                  Get_Query & Image (Connection.all, Params)
                  & " " & Status (DBMS_Forward_Cursor'Class (R.all))
                  & " " & Error_Msg (DBMS_Forward_Cursor'Class (R.all))
                  & Get_User);
            end if;

         elsif Active (Me_Select) then
            Trace
              (Me_Select,
               Get_Query & Image (Connection.all, Params)
               & Get_Rows & " "
               & Status (DBMS_Forward_Cursor'Class (R.all)) & Get_User);
         end if;

      else
         Connection.Success := Is_Success (DBMS_Forward_Cursor'Class (R.all));
         if not Connection.Success then
            if Active (Me_Error) then
               --  This trace might duplicate information already available
               --  if both the SQL and SQL.ERRORS streams are active (since
               --  the result of the SQL has already shown the error message).
               --  However, it is useful when only SQL.ERRORS is active.
               Trace (Me_Error, "Transaction failed " & Query
                      & " => "
                      & Error_Msg (DBMS_Forward_Cursor'Class (R.all)));
            end if;

            Set_Failure
              (Connection, Error_Msg (DBMS_Forward_Cursor'Class (R.all)));

            if Active (Me_Query) then
               Trace
                 (Me_Query,
                  Get_Query & Image (Connection.all, Params)
                  & " " & Status (DBMS_Forward_Cursor'Class (R.all))
                  & " " & Error_Msg (DBMS_Forward_Cursor'Class (R.all))
                  & Get_User);
            end if;

         elsif Active (Me_Query) then
            Trace
              (Me_Query,
               Get_Query & Image (Connection.all, Params)
               & Get_Rows & " "
               & Status (DBMS_Forward_Cursor'Class (R.all)) & Get_User);
         end if;
      end if;
   end Post_Execute_And_Log;

   -----------------------
   -- Start_Transaction --
   -----------------------

   function Start_Transaction
     (Connection : access Database_Connection_Record'Class)
      return Boolean is
   begin
      if not Connection.In_Transaction then
         Execute (Connection, "BEGIN");
         Connection.In_Transaction := True;
         return True;
      end if;
      return False;
   end Start_Transaction;

   ---------------------
   -- Execute_And_Log --
   ---------------------

   procedure Execute_And_Log
     (Result     : in out Forward_Cursor'Class;
      Connection : access Database_Connection_Record'Class;
      Query      : String;
      Prepared   : Prepared_Statement'Class := No_Prepared;
      Is_Select  : Boolean;
      Direct     : Boolean;
      Params     : SQL_Parameters := No_Parameters)
   is
      Is_Commit_Or_Rollback   : constant Boolean :=
        Equal (Query, "commit", Case_Sensitive => False)
        or else Equal (Query, "rollback", Case_Sensitive => False);
      Stmt : DBMS_Stmt := No_DBMS_Stmt;
      R    : Abstract_Cursor_Access;
      Was_Started : Boolean;
      pragma Unreferenced (Was_Started);
   begin
      --  Transaction management: do we need to start a transaction ?

      if Connection.In_Transaction
        and then not Connection.Success
      then
         Trace
           (Me_Error,
            "Ignored, since transaction failed: " & Query
            & " (" & Connection.Username.all & ")");
         return;

      elsif Equal (Query, "begin", Case_Sensitive => False) then
         if not Connection.In_Transaction then
            Connection.In_Transaction := True;

         else
            --  Ignore silently: GNATCOLL might have started a transaction
            --  without the user knowing, for instance on the first SELECT
            --  statement if Always_Use_Transactions is true.
            null;
         end if;

      elsif not Connection.In_Transaction
        and then
          (Connection.Always_Use_Transactions
           or else
             (not Is_Commit_Or_Rollback
              and then not Is_Select))   --  INSERT, UPDATE, LOCK, DELETE,...
        and then
          (Query'Length <= 7   --  for sqlite
           or else Query (Query'First .. Query'First + 6) /= "PRAGMA ")
      then
         --  Start a transaction automatically
         Was_Started := Start_Transaction (Connection);
         if not Connection.Success then
            return;
         end if;
      end if;

      if Prepared /= Prepared_Statement'Class (No_Prepared) then
         if Prepared.Get.On_Server then
            Prepare_If_Needed (Connection, Prepared, Stmt);
         end if;
      end if;

      if Perform_Queries then
         if Stmt /= No_DBMS_Stmt then
            R := Execute
              (Connection => Connection,
               Prepared   => Stmt,
               Is_Select  => Is_Select,
               Direct     => Direct,
               Params     => Params);

         else
            R := Connect_And_Execute
              (Connection => Connection,
               Query      => Query,
               Is_Select  => Is_Select,
               Direct     => Direct,
               Params     => Params);
         end if;

         if R = null then
            if Active (Me_Error) then
               if Stmt /= No_DBMS_Stmt then
                  Trace (Me_Error, "Failed to execute prepared ("
                         & Prepared.Get.Name.all & ") " & Query
                         & " " & Image (Connection.all, Params));
               else
                  Trace (Me_Error, "Failed to execute " & Query
                         & " " & Image (Connection.all, Params));
               end if;
            end if;

            Set_Failure (Connection);
         else
            Post_Execute_And_Log
              (R, Connection, Query, Prepared, Is_Select, Params);
         end if;

         Result.Res := R;
      end if;

      if Connection.In_Transaction
        and then Is_Commit_Or_Rollback
      then
         Connection.In_Transaction := False;
      end if;
   end Execute_And_Log;

   -----------
   -- Fetch --
   -----------

   procedure Fetch
     (Result     : out Forward_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : String;
      Params     : SQL_Parameters := No_Parameters)
   is
      Is_Select   : constant Boolean := Is_Select_Query (Query);
   begin
      Result := No_Element;
      Execute_And_Log
        (Result, Connection, Query, No_Prepared, Is_Select, Direct => False,
         Params => Params);
   end Fetch;

   -----------
   -- Fetch --
   -----------

   procedure Fetch
     (Result     : out Forward_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : SQL_Query;
      Params     : SQL_Parameters := No_Parameters) is
   begin
      Fetch
        (Result, Connection, To_String (To_String (Query, Connection.all)),
         Params);
   end Fetch;

   -----------
   -- Fetch --
   -----------

   procedure Fetch
     (Result     : out Direct_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : String;
      Params     : SQL_Parameters := No_Parameters)
   is
      Is_Select   : constant Boolean := Is_Select_Query (Query);
   begin
      Result := No_Direct_Element;
      Execute_And_Log
        (Result, Connection, Query, No_Prepared, Is_Select, Direct => True,
         Params => Params);
   end Fetch;

   -----------
   -- Fetch --
   -----------

   overriding procedure Fetch
     (Result     : out Direct_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : GNATCOLL.SQL.SQL_Query;
      Params     : SQL_Parameters := No_Parameters) is
   begin
      Fetch
        (Result, Connection, To_String (To_String (Query, Connection.all)),
         Params => Params);
   end Fetch;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      Query      : SQL_Query;
      Params     : SQL_Parameters := No_Parameters)
   is
      R : Forward_Cursor;
      pragma Unreferenced (R);
   begin
      Fetch (R, Connection, Query, Params);
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      Query      : String;
      Params     : SQL_Parameters := No_Parameters)
   is
      R : Forward_Cursor;
      pragma Unreferenced (R);
   begin
      Fetch (R, Connection, Query, Params);
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
         Connection.In_Transaction := False;
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
         Connection.In_Transaction := False;
      end if;
   end Commit_Or_Rollback;

   ----------------------
   -- Invalidate_Cache --
   ----------------------

   procedure Invalidate_Cache is
   begin
      Trace (Me_Query, "Invalidate SQL cache");
      Query_Cache.Reset;
   end Invalidate_Cache;

   ----------------------
   -- Reset_Connection --
   ----------------------

   procedure Reset_Connection
     (Connection  : access Database_Connection_Record'Class;
      Username    : String := "") is
   begin
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
      Username    : String := "")
      return Database_Connection
   is
      Connection : Database_Connection;
   begin
      Connection := DB_Attributes.Value;
      if Connection = null then
         Connection := Description.Build_Connection;
         if Connection /= null then
            DB_Attributes.Set_Value (Connection);
         else
            Trace
              (Me_Error, "Could not create connection object for database");
         end if;

      else
         Reset_Connection (Connection, Username);
      end if;

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

   overriding procedure Adjust (Self : in out Forward_Cursor) is
   begin
      if Self.Res /= null then
         Self.Res.Refcount := Self.Res.Refcount + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Forward_Cursor) is
      Res : Abstract_Cursor_Access := Self.Res;
   begin
      Self.Res := null;  --  Make Finalize idempotent
      if Res /= null then
         Res.Refcount := Res.Refcount - 1;
         if Res.Refcount = 0 then
            Finalize (DBMS_Forward_Cursor'Class (Res.all));
            Unchecked_Free (Res);
         end if;
      end if;
   end Finalize;

   --------------------
   -- Processed_Rows --
   --------------------

   function Processed_Rows (Self : Forward_Cursor) return Natural is
   begin
      if Self.Res = null then
         return 0;
      else
         return Processed_Rows (DBMS_Forward_Cursor'Class (Self.Res.all));
      end if;
   end Processed_Rows;

   -------------
   -- Has_Row --
   -------------

   function Has_Row (Self : Forward_Cursor) return Boolean is
   begin
      if Self.Res = null then
         return False;
      else
         return Has_Row (DBMS_Forward_Cursor'Class (Self.Res.all));
      end if;
   end Has_Row;

   ----------
   -- Next --
   ----------

   procedure Next (Self : in out Forward_Cursor) is
   begin
      if Self.Res /= null then
         Next (DBMS_Forward_Cursor'Class (Self.Res.all));
      end if;
   end Next;

   -----------
   -- Value --
   -----------

   function Value
     (Self  : Forward_Cursor;
      Field : Field_Index) return String is
   begin
      return Value (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   end Value;

   -------------------
   -- Boolean_Value --
   -------------------

   function Boolean_Value
     (Self  : Forward_Cursor;
      Field : Field_Index) return Boolean is
   begin
      return Boolean_Value (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   end Boolean_Value;

   -------------------
   -- Integer_Value --
   -------------------

   function Integer_Value
     (Self   : Forward_Cursor;
      Field  : Field_Index) return Integer
   is
   begin
      return Integer_Value (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   end Integer_Value;

   -------------------
   -- Integer_Value --
   -------------------

   function Integer_Value
     (Self   : Forward_Cursor;
      Field  : Field_Index;
      Default : Integer) return Integer
   is
   begin
      return Integer_Value (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   exception
      when Constraint_Error =>
         return Default;
   end Integer_Value;

   -----------
   -- Value --
   -----------

   function Float_Value
     (Self  : Forward_Cursor;
      Field : Field_Index) return Float is
   begin
      return Float_Value (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   end Float_Value;

   -----------
   -- Value --
   -----------

   function Time_Value
     (Self  : Forward_Cursor;
      Field : Field_Index) return Ada.Calendar.Time is
   begin
      return Time_Value (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   end Time_Value;

   -------------
   -- Is_Null --
   -------------

   function Is_Null
     (Self  : Forward_Cursor;
      Field : Field_Index) return Boolean is
   begin
      return Is_Null (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   end Is_Null;

   -------------
   -- Last_Id --
   -------------

   function Last_Id
     (Self       : Forward_Cursor;
      Connection : access Database_Connection_Record'Class;
      Field      : SQL_Field_Integer) return Integer is
   begin
      if Perform_Queries then
         return Last_Id
           (DBMS_Forward_Cursor'Class (Self.Res.all), Connection, Field);
      else
         return 1;  --  Dummy
      end if;
   end Last_Id;

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description
     (Connection : access Database_Connection_Record'Class)
      return Database_Description
   is
   begin
      return Database_Description (Connection.Descr);
   end Get_Description;

   -----------------
   -- Field_Count --
   -----------------

   function Field_Count (Self : Forward_Cursor) return Field_Index is
   begin
      return Field_Count (DBMS_Forward_Cursor'Class (Self.Res.all));
   end Field_Count;

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name
     (Self : Forward_Cursor; Field : Field_Index) return String is
   begin
      return Field_Name (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   end Field_Name;

   -----------
   -- First --
   -----------

   procedure First (Self : in out Direct_Cursor) is
   begin
      First (DBMS_Direct_Cursor'Class (Self.Res.all));
   end First;

   -------------
   -- Current --
   -------------

   function Current (Self : Forward_Cursor) return Positive is
   begin
      return Current (DBMS_Forward_Cursor'Class (Self.Res.all));
   end Current;

   ----------
   -- Last --
   ----------

   procedure Last  (Self : in out Direct_Cursor) is
   begin
      Last (DBMS_Direct_Cursor'Class (Self.Res.all));
   end Last;

   --------------
   -- Absolute --
   --------------

   procedure Absolute (Self : in out Direct_Cursor; Row : Positive) is
   begin
      Absolute (DBMS_Direct_Cursor'Class (Self.Res.all), Row);
   end Absolute;

   --------------
   -- Relative --
   --------------

   procedure Relative (Self : in out Direct_Cursor; Step : Integer) is
   begin
      Relative (DBMS_Direct_Cursor'Class (Self.Res.all), Step);
   end Relative;

   ----------
   -- Free --
   ----------

   procedure Free (Connection : in out Database_Connection) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Database_Connection_Record'Class, Database_Connection);
   begin
      if Connection /= null then
         Close (Connection);
         Free (Connection.Username);
         Free (Connection.Error_Msg);

         Query_Cache.Mark_DB_As_Free (Connection);
         Unchecked_Free (Connection);
      end if;
   end Free;

   -------------
   -- Prepare --
   -------------

   function Prepare
     (Query         : SQL_Query;
      Auto_Complete : Boolean := False;
      Use_Cache     : Boolean := False;
      On_Server     : Boolean := False;
      Name          : String  := "")
      return Prepared_Statement
   is
      Stmt : Prepared_Statement;
      Data : Prepared_Statements.Encapsulated_Access;
   begin
      Data := new Prepared_Statement_Data'
        (GNATCOLL.Refcount.Refcounted with
         Query         => Query,
         Query_Str     => null,   --  Computed later
         Is_Select     => False,  --  Computed later
         Use_Cache     => Use_Cache,
         Cached_Result => No_Cache_Id,
         On_Server     => On_Server,
         Name          => null,
         Prepared      => null);

      Set (Stmt, Data);

      Query_Cache.Set_Id (Stmt);

      if Name = "" then
         Data.Name :=
           new String'("stmt" & Image (Integer (Data.Cached_Result), 0));
      else
         Data.Name := new String'(Name);
      end if;

      if Auto_Complete then
         GNATCOLL.SQL.Auto_Complete (Data.Query);
      end if;

      return Stmt;
   end Prepare;

   -------------
   -- Prepare --
   -------------

   function Prepare
     (Query      : String;
      Use_Cache  : Boolean := False;
      On_Server  : Boolean := False;
      Name       : String := "")
      return Prepared_Statement
   is
      Stmt : Prepared_Statement;
      Data : Prepared_Statements.Encapsulated_Access;
   begin
      Data := new Prepared_Statement_Data'
        (GNATCOLL.Refcount.Refcounted with
         Query         => No_Query,
         Query_Str     => new String'(Query),
         Is_Select     => Is_Select_Query (Query),
         Use_Cache     => Use_Cache,
         Cached_Result => No_Cache_Id,
         On_Server     => On_Server,
         Name          => null,
         Prepared      => null);

      if Active (Me_Query) then
         Trace (Me_Query, "compute (" & Name & "): " & Data.Query_Str.all);
      end if;

      Set (Stmt, Data);

      Query_Cache.Set_Id (Stmt);

      if Name = "" then
         Data.Name :=
           new String'("stmt" & Image (Integer (Data.Cached_Result), 0));
      else
         Data.Name := new String'(Name);
      end if;

      return Stmt;
   end Prepare;

   -----------------------
   -- Prepare_If_Needed --
   -----------------------

   procedure Prepare_If_Needed
     (Connection : access Database_Connection_Record'Class;
      Stmt       : Prepared_Statement'Class;
      Result     : out DBMS_Stmt)
   is
      S : constant Prepared_Statements.Encapsulated_Access := Stmt.Get;
      L : Prepared_In_Session_List := S.Prepared;
   begin
      while L /= null loop
         exit when L.DB = Database_Connection (Connection);
         L := L.Next;
      end loop;

      if L = null then
         S.Prepared := new Prepared_In_Session'
           (Stmt         => No_DBMS_Stmt,
            DB           => Database_Connection (Connection),
            DB_Timestamp => Connection.Connected_On,
            Next         => S.Prepared);
         L := S.Prepared;
      end if;

      if L.Stmt = No_DBMS_Stmt
         or else L.DB_Timestamp /= Connection.Connected_On
      then
         L.DB_Timestamp := Connection.Connected_On;
         L.Stmt := Connect_And_Prepare
           (Connection, S.Query_Str.all, S.Name.all, Direct => True);

         --  L.Stmt could still be No_DBMS_Stmt if the backend does not support
         --  preparation on the server.

      else
         Reset (Connection, L.Stmt);
      end if;

      Result := L.Stmt;
   end Prepare_If_Needed;

   -----------
   -- Fetch --
   -----------

   procedure Fetch
     (Result     : out Direct_Cursor;
      Connection : access Database_Connection_Record'Class;
      Stmt       : Prepared_Statement'Class;
      Params     : SQL_Parameters := No_Parameters)
   is
      use type Prepared_Statements.Encapsulated_Access;
      Found : Boolean;
      S : constant Prepared_Statements.Encapsulated_Access := Stmt.Get;
   begin
      Result := No_Direct_Element;   --  Free memory used by previous use

      if S = null then
         Trace (Me_Query, "Prepared statement was freed, can't execute");
         return;
      end if;

      if S.Use_Cache
        and then Connection.Descr.Caching
      then
         Query_Cache.Get_Result (Stmt, Result, Found, Params);
         if Found then
            Result.First; --  Move to first element
            if Active (Me_Cache) then
               Trace (Me_Cache, "Use cache for " & S.Name.all);
            end if;
            return;
         end if;
      end if;

      Compute_Statement (Stmt, Format => Connection.all);
      Execute_And_Log
        (Result, Connection,
         S.Query_Str.all, Stmt, S.Is_Select, Direct => True, Params => Params);

      if Success (Connection)
        and then S.Use_Cache
        and then Connection.Descr.Caching
      then
         Query_Cache.Set_Cache (Stmt, Result);
      end if;
   end Fetch;

   -----------
   -- Fetch --
   -----------

   procedure Fetch
     (Result     : out Forward_Cursor;
      Connection : access Database_Connection_Record'Class;
      Stmt       : Prepared_Statement'Class;
      Params     : SQL_Parameters := No_Parameters)
   is
      S : constant Prepared_Statements.Encapsulated_Access := Stmt.Get;
   begin
      Result := No_Element;

      if S.Use_Cache
        and then Connection.Descr.Caching
      then
         --  When using a cache, we have to use a Direct_Cursor for the cache
         --  to work
         declare
            R : Direct_Cursor;
         begin
            Fetch (R, Connection, Stmt, Params);
            Result := Forward_Cursor (R);
         end;

      else
         Compute_Statement (Stmt, Format => Connection.all);
         Execute_And_Log
           (Result, Connection, S.Query_Str.all, Stmt, S.Is_Select,
            Direct => False, Params => Params);
      end if;
   end Fetch;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      Stmt       : Prepared_Statement'Class;
      Params     : SQL_Parameters := No_Parameters)
   is
      R : Forward_Cursor;
      pragma Unreferenced (R);
   begin
      Fetch (R, Connection, Stmt, Params);
   end Execute;

   -------------------------
   -- Connect_And_Prepare --
   -------------------------

   function Connect_And_Prepare
     (Connection : access Database_Connection_Record;
      Query      : String;
      Name       : String;
      Direct     : Boolean)
      return DBMS_Stmt
   is
      pragma Unreferenced (Connection, Query, Direct, Name);
   begin
      return No_DBMS_Stmt;
   end Connect_And_Prepare;

   -------------
   -- Execute --
   -------------

   function Execute
     (Connection  : access Database_Connection_Record;
      Prepared    : DBMS_Stmt;
      Is_Select   : Boolean;
      Direct      : Boolean;
      Params      : SQL_Parameters := No_Parameters)
      return Abstract_Cursor_Access
   is
      pragma Unreferenced (Connection, Prepared, Is_Select, Direct, Params);
   begin
      return null;
   end Execute;

   ---------
   -- "+" --
   ---------

   function "+" (Value : access String) return SQL_Parameter is
   begin
      return SQL_Parameter'
        (Typ => Parameter_Text, Str_Val => Value.all'Unchecked_Access);
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Value : Integer) return SQL_Parameter is
   begin
      return SQL_Parameter'(Typ => Parameter_Integer, Int_Val => Value);
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Value : Boolean) return SQL_Parameter is
   begin
      return SQL_Parameter'(Typ => Parameter_Boolean, Bool_Val => Value);
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Value : Float) return SQL_Parameter is
   begin
      return SQL_Parameter'(Typ => Parameter_Float, Float_Val => Value);
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Value : Character) return SQL_Parameter is
   begin
      return SQL_Parameter'(Typ => Parameter_Character, Char_Val => Value);
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Time : Ada.Calendar.Time) return SQL_Parameter is
   begin
      return SQL_Parameter'(Typ => Parameter_Time, Time_Val => Time);
   end "+";

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Prepared_Statement_Data) is
      L, L2 : Prepared_In_Session_List;
      Count : Natural := 0;
   begin
      --  If there is a single DB, we are in one of two cases:
      --     - either the stmt was local to a procedure, and we are finalizing
      --       on exit of the procedure. It is thus safe to use the DB.
      --     - or we have a global variable that was only used from a single
      --       connection. Since the application is finalizing, we can use the
      --       session if it is still valid
      --  If there are more than one DB, we have a global variable and the
      --  application is finalizing. Don't do anything on the DBMS, just free
      --  memory.

      if Self.Prepared /= null
        and then Self.Prepared.Next = null
      then
         if Active (Me_Query) then
            Trace (Me_Query, "Finalize stmt on server: " & Self.Name.all);
         end if;

         --  ??? What if the connection was closed ?
         --  ??? Should not finalize if we haven't finalized all cursors built
         --  from that prepared statement.

         if not Query_Cache.Was_Freed (Self.Prepared.DB) then
            Finalize (Self.Prepared.DB, Self.Prepared.Stmt);
         end if;
         Unchecked_Free (Self.Prepared);

      elsif Self.Prepared /= null then
         L := Self.Prepared;
         while L /= null loop
            L2 := L.Next;
            Unchecked_Free (L);
            Count := Count + 1;
            L := L2;
         end loop;

         if Active (Me_Query) then
            Trace (Me_Query, "Finalize stmt on server: " & Self.Name.all
                   & " (for" & Count'Img & " connections)");
         end if;
      end if;

      Query_Cache.Unset_Cache (Self);
      Free (Self.Query_Str);
      Free (Self.Name);
   end Free;

end GNATCOLL.SQL.Exec;
