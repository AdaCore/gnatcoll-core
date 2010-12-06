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
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Task_Attributes;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with GNATCOLL.SQL.Exec_Private; use GNATCOLL.SQL.Exec_Private;

package body GNATCOLL.SQL.Exec is

   Me_Error  : constant Trace_Handle := Create ("SQL.ERROR", On);
   Me_Select : constant Trace_Handle := Create ("SQL.SELECT");
   Me_Cache  : constant Trace_Handle := Create ("SQL.CACHE");
   Me_Query  : constant Trace_Handle := Create ("SQL");

   Cache_Expiration_Delay : constant Duration := 3600.0;  --  1 hour
   --  Delay after which the SQL cache expires and must be reset

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Abstract_DBMS_Forward_Cursor'Class, Abstract_Cursor_Access);

   package DB_Attributes is new Ada.Task_Attributes
     (Database_Connection, null);

   function Is_Select_Query (Query : String) return Boolean;
   --  Return true if Query is a select query

   procedure Execute_And_Log
     (Result     : in out Forward_Cursor'Class;
      Connection : access Database_Connection_Record'Class;
      Query      : String;
      Prepared   : DBMS_Stmt;
      Is_Select  : Boolean;
      Direct     : Boolean);
   --  Low-level call to perform a query on the database and log results

   function Hash
     (Str : GNAT.Strings.String_Access) return Ada.Containers.Hash_Type;
   function Equal
     (Str1, Str2 : GNAT.Strings.String_Access) return Boolean;

   package String_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => GNAT.Strings.String_Access,
      Element_Type    => Cached_Statement_Access,
      Hash            => Hash,
      Equivalent_Keys => Equal);

   type Cached_Result is record
      Cursor : Direct_Cursor := No_Direct_Element;
      Found  : Boolean := False;
   end record;
   package Stmt_Vectors is new Ada.Containers.Vectors (Stmt_Id, Cached_Result);

   function Is_Pragma (Query : String) return Boolean;
   --  Return true if Query is a PRAGMA command (an sqlite extension).
   --  We do not need to start a transaction for this type of commands

   procedure Free (Cached : in out Cached_Statement_Access);
   --  Free memory occupied by Cached

   protected Query_Cache is
      procedure Prepare_Statement
        (Stmt   : in out Prepared_Statement;
         Format : Formatter'Class);
      --  Do the actual preparation of the statement. This needs to be done
      --  inside a locked region, since the prepared statement is shared

      procedure Finalize_Prepared_Statements;
      --  Free all memory related to prepared statements.

      procedure Get_Result
        (Stmt    : Prepared_Statement;
         Cached  : out Direct_Cursor;
         Found   : out Boolean);
      --  Return null or the cached value for the statement

      procedure Set_Cache (Stmt : Prepared_Statement; Cached : Direct_Cursor);
      --  Add a new value in the cache

      procedure Reset;
      --  Reset the cache

   private
      Current_Prepared_Id : Stmt_Id := 1;
      --  First unassigned id for prepared statements

      Query_To_Id  : String_Maps.Map;
      Cache        : Stmt_Vectors.Vector;

      Timestamp    : Ada.Calendar.Time := Ada.Calendar.Clock;
   end Query_Cache;

   ----------
   -- Hash --
   ----------

   function Hash
     (Str : GNAT.Strings.String_Access) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Str.all);
   end Hash;

   -----------
   -- Equal --
   -----------

   function Equal
     (Str1, Str2 : GNAT.Strings.String_Access) return Boolean is
   begin
      return Str1.all = Str2.all;
   end Equal;

   ----------
   -- Free --
   ----------

   procedure Free (Cached : in out Cached_Statement_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Cached_Statement, Cached_Statement_Access);
   begin
      Free (Cached.Str);
      Unchecked_Free (Cached);
   end Free;

   -----------------
   -- Query_Cache --
   -----------------

   protected body Query_Cache is

      -----------------------
      -- Prepare_Statement --
      -----------------------

      procedure Prepare_Statement
        (Stmt   : in out Prepared_Statement;
         Format : Formatter'Class)
      is
         Cached : Cached_Statement_Access renames Stmt.Cached;
      begin
         if Cached.Id = No_Stmt_Id then
            --  Check if we already have this statement in the cache. If yes,
            --  reuse the cache instead of the local version in the statement

            if Cached.Query /= No_Query then
               Cached.Str := new String'
                 (To_String (To_String (Cached.Query, Format)));
               Cached.Query := No_Query;   --  release memory
            end if;

            --  Parameters substitution depends on the DBMS
            --     sqlite:  ?  ?NNN :VVV  @VVV $VVV
            --     psql:    $1::bigint
            --     mysql:   ?

            declare
               C : String_Maps.Cursor;
            begin
               C := String_Maps.Find (Query_To_Id, Cached.Str);
               if String_Maps.Has_Element (C) then
                  Free (Stmt.Cached);  --  No longer needed
                  Stmt.Cached := String_Maps.Element (C);

               else
                  Cached.Id        := Current_Prepared_Id;
                  Cached.Is_Select := Is_Select_Query (Cached.Str.all);

                  Current_Prepared_Id := Current_Prepared_Id + 1;

                  String_Maps.Include (Query_To_Id, Cached.Str, Cached);
               end if;

               --  We can't use the cache for a statement other than SELECT,
               --  since there is nothing to cache in that case.
               if not Cached.Is_Select then
                  Stmt.Use_Cache := False;
               end if;
            end;
         end if;
      end Prepare_Statement;

      ----------------
      -- Get_Result --
      ----------------

      procedure Get_Result
        (Stmt    : Prepared_Statement;
         Cached  : out Direct_Cursor;
         Found   : out Boolean)
      is
         Tmp : Cached_Result;
      begin
         if Clock - Timestamp > Cache_Expiration_Delay then
            Reset;
            Found := False;
         else
            if Stmt.Cached.Id <= Cache.Last_Index then
               Tmp := Cache.Element (Stmt.Cached.Id);
               Found := Tmp.Found;
               if Found then
                  Cached := Tmp.Cursor;
               end if;

            else
               Found := False;
            end if;
         end if;
      end Get_Result;

      ---------------
      -- Set_Cache --
      ---------------

      procedure Set_Cache
        (Stmt : Prepared_Statement; Cached : Direct_Cursor) is
      begin
         --  Reserve capacity up to the current assigned id, since we are
         --  likely to need it anyway, and it is bound to be at least as big
         --  as Stmt.Cached.Id
         if Cache.Last_Index < Stmt.Cached.Id then
            Cache.Set_Length
              (Ada.Containers.Count_Type
                 (Current_Prepared_Id - Stmt_Id'First + 1));
         end if;
         Cache.Replace_Element (Stmt.Cached.Id, (Cached, True));
      end Set_Cache;

      ----------------------------------
      -- Finalize_Prepared_Statements --
      ----------------------------------

      procedure Finalize_Prepared_Statements is
         C    : String_Maps.Cursor := Query_To_Id.First;
         Stmt : Cached_Statement_Access;
      begin
         while String_Maps.Has_Element (C) loop
            Stmt := String_Maps.Element (C);
            String_Maps.Next (C);
            Free (Stmt);
         end loop;

         Query_To_Id.Clear;
         Cache.Clear;
      end Finalize_Prepared_Statements;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         --  Do not clear Query_To_Id though, since all already prepared
         --  statements would become invalid (and since they point to the
         --  string_access we would have invalid memory).

         Cache.Clear;
         Cache.Set_Length (0);
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

   ----------
   -- Free --
   ----------

   procedure Free (Description : in out Database_Description) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Database_Description_Record, Database_Description);
   begin
      if Description /= null then
         GNAT.Strings.Free (Description.Host);
         GNAT.Strings.Free (Description.User);
         GNAT.Strings.Free (Description.Dbname);
         GNAT.Strings.Free (Description.Password);
         GNAT.Strings.Free (Description.DBMS);
         Unchecked_Free (Description);
      end if;
   end Free;

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
      SSL           : SSL_Mode := Prefer;
      Cache_Support : Boolean := True)
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
      Description.SSL       := SSL;
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

   -------------
   -- Get_SSL --
   -------------

   function Get_SSL (Description : Database_Description) return SSL_Mode is
   begin
      return Description.SSL;
   end Get_SSL;

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

   --------------------------
   -- Post_Execute_And_Log --
   --------------------------

   procedure Post_Execute_And_Log
     (R          : access Abstract_DBMS_Forward_Cursor'Class;
      Connection : access Database_Connection_Record'Class;
      Query      : String;
      Is_Select  : Boolean)
   is
      function Get_Rows return String;
      --  The number of rows downloaded. If we only have a forward cursor, we
      --  can't display them

      function Get_User return String;
      --  Return the user name

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
         else
            return "";
         end if;
      end Get_Rows;

   begin
      if R = null then
         Set_Failure (Connection);

      elsif Is_Select then
         --  ??? Should use the local mirror database when doing a select,
         --  to speed up queries. Are we garanteed, with the mirror, that
         --  doing a INSERT on the master, and immediately a SELECT on the
         --  slave will return the newly inserted values ?
         Connection.Success := Is_Success (DBMS_Forward_Cursor'Class (R.all));

         if not Connection.Success then
            Set_Failure (Connection);

            if Active (Me_Query) then
               Trace
                 (Me_Query,
                  Query & " " & Status (DBMS_Forward_Cursor'Class (R.all))
                  & " " & Error_Msg (DBMS_Forward_Cursor'Class (R.all))
                  & Get_User);
            end if;

         elsif Active (Me_Select) then
            Trace
              (Me_Select,
               Query & Get_Rows & " "
               & Status (DBMS_Forward_Cursor'Class (R.all)) & Get_User);
         end if;

      else
         Connection.Success := Is_Success (DBMS_Forward_Cursor'Class (R.all));
         if not Connection.Success then
            Set_Failure
              (Connection, Error_Msg (DBMS_Forward_Cursor'Class (R.all)));

            if Active (Me_Query) then
               Trace
                 (Me_Query,
                  Query & " " & Status (DBMS_Forward_Cursor'Class (R.all))
                  & " " & Error_Msg (DBMS_Forward_Cursor'Class (R.all))
                  & Get_User);
            end if;

         elsif Active (Me_Query) then
            Trace
              (Me_Query,
               Query & Get_Rows & " "
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

   ---------------
   -- Is_Pragma --
   ---------------

   function Is_Pragma (Query : String) return Boolean is
   begin
      return Query'Length > 7
        and then Query (Query'First .. Query'First + 6) = "PRAGMA ";
   end Is_Pragma;

   ---------------------
   -- Execute_And_Log --
   ---------------------

   procedure Execute_And_Log
     (Result     : in out Forward_Cursor'Class;
      Connection : access Database_Connection_Record'Class;
      Query      : String;
      Prepared   : DBMS_Stmt;
      Is_Select  : Boolean;
      Direct     : Boolean)
   is
      Is_Begin    : constant Boolean := To_Lower (Query) = "begin";
      Is_Commit   : constant Boolean := To_Lower (Query) = "commit";
      Is_Rollback : constant Boolean := To_Lower (Query) = "rollback";
      R : Abstract_Cursor_Access;
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
        and then not Is_Pragma (Query) --  for sqlite
      then
         --  Start a transaction automatically
         Was_Started := Start_Transaction (Connection);
         if not Connection.Success then
            return;
         end if;
      end if;

      if Perform_Queries then
         if Prepared /= No_DBMS_Stmt then
            R := Execute
              (Connection => Connection,
               Prepared   => Prepared,
               Is_Select  => Is_Select,
               Direct     => Direct);

         else
            R := Connect_And_Execute
              (Connection => Connection,
               Query      => Query,
               Is_Select  => Is_Select,
               Direct     => Direct);
         end if;
      end if;

      if R = null then
         Trace (Me_Error, "Failed to execute " & Query
                & " prepared? "
                & Boolean'Image (Prepared /= No_DBMS_Stmt));
         Set_Failure (Connection);
      else
         Post_Execute_And_Log (R, Connection, Query, Is_Select);
      end if;

      Result.Res := R;

      if Connection.In_Transaction
        and then (Is_Commit or Is_Rollback)
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
      Query      : String)
   is
      Is_Select   : constant Boolean := Is_Select_Query (Query);
   begin
      Result := No_Element;
      Execute_And_Log
        (Result, Connection, Query, No_DBMS_Stmt, Is_Select, Direct => False);
   end Fetch;

   -----------
   -- Fetch --
   -----------

   procedure Fetch
     (Result     : out Forward_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : SQL_Query) is
   begin
      Fetch
        (Result, Connection, To_String (To_String (Query, Connection.all)));
   end Fetch;

   -----------
   -- Fetch --
   -----------

   procedure Fetch
     (Result     : out Direct_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : String)
   is
      Is_Select   : constant Boolean := Is_Select_Query (Query);
   begin
      Result := No_Direct_Element;
      Execute_And_Log
        (Result, Connection, Query, No_DBMS_Stmt, Is_Select, Direct => True);
   end Fetch;

   -----------
   -- Fetch --
   -----------

   overriding procedure Fetch
     (Result     : out Direct_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : GNATCOLL.SQL.SQL_Query) is
   begin
      Fetch
        (Result, Connection, To_String (To_String (Query, Connection.all)));
   end Fetch;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      Query      : SQL_Query)
   is
      R : Forward_Cursor;
      pragma Unreferenced (R);
   begin
      Fetch (R, Connection, Query);
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Connection : access Database_Connection_Record'Class;
      Query      : String)
   is
      R : Forward_Cursor;
      pragma Unreferenced (R);
   begin
      Fetch (R, Connection, Query);
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
         else
            Trace
              (Me_Error, "Could not create connection object for database");
         end if;
      end if;

      if Connection /= null then
         Reset_Connection (Description, Connection, Username);
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

   procedure Finalize (Self : in out Forward_Cursor) is
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
   --
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
      return Last_Id
         (DBMS_Forward_Cursor'Class (Self.Res.all), Connection, Field);
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

   -----------
   -- Close --
   -----------

   procedure Free (Connection : in out Database_Connection) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Database_Connection_Record'Class, Database_Connection);
   begin
      if Connection /= null then
         Close (Connection);
         Free (Connection.Username);
         Free (Connection.Error_Msg);
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
      On_Server     : Boolean := False)
      return Prepared_Statement
   is
      Stmt : Prepared_Statement;
   begin
      --  Memory will be freed by Query_Cache.Prepared_Statement when
      --  appropriate
      Stmt :=
        (Cached    => new Cached_Statement'
           (Id        => No_Stmt_Id,
            Str       => null,
            Is_Select => False,
            Query     => Query),
         Use_Cache => Use_Cache,
         On_Server => On_Server);

      if Auto_Complete then
         GNATCOLL.SQL.Auto_Complete (Stmt.Cached.Query);
      end if;

      return Stmt;
   end Prepare;

   -------------
   -- Prepare --
   -------------

   function Prepare
     (Query      : String;
      Use_Cache  : Boolean := False;
      On_Server  : Boolean := False)
      return Prepared_Statement is
   begin
      return
        (Cached    => new Cached_Statement'
           (Id        => No_Stmt_Id,
            Str       => new String'(Query),
            Is_Select => False,
            Query     => No_Query),
         Use_Cache => Use_Cache,
         On_Server => On_Server);
   end Prepare;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (Connection : access Database_Connection_Record'Class;
      Stmt       : in out Prepared_Statement)
   is
      DStmt : DBMS_Stmt;
   begin
      --  The intent is not to free Stmt.Cached itself, which might still be
      --  used by other connections. We just want to release memory on the
      --  DBMS server itself.

      if Stmt.Cached.Id <= Connection.Stmts.Last_Index then
         DStmt := Connection.Stmts.Element (Stmt.Cached.Id);

         if DStmt /= No_DBMS_Stmt then
            Finalize (Connection, DStmt);
            Connection.Stmts.Replace_Element (Stmt.Cached.Id, No_DBMS_Stmt);
         end if;
      end if;
   end Finalize;

   -----------
   -- Fetch --
   -----------

   procedure Fetch
     (Result     : out Direct_Cursor;
      Connection : access Database_Connection_Record'Class;
      Stmt       : in out Prepared_Statement)
   is
      Found : Boolean;
      DStmt  : DBMS_Stmt := No_DBMS_Stmt;
   begin
      Result := No_Direct_Element;   --  Free memory used by previous use

      --  Once set, the id is never reset, so it is safe to do the test outside
      --  of the protected area. This even saves some locking

      if Stmt.Cached.Id = 0 then
         Query_Cache.Prepare_Statement (Stmt, Connection.all);
      end if;

      if Stmt.Use_Cache
        and then Connection.DB.Caching
      then
         Query_Cache.Get_Result (Stmt, Result, Found);
         if Found then
            Result.First; --  Move to first element
            if Active (Me_Cache) then
               Trace (Me_Cache, "Use cache for " & Stmt.Cached.Str.all);
            end if;
            return;
         end if;
      end if;

      if Stmt.On_Server then
         if Stmt.Cached.Id <= Connection.Stmts.Last_Index then
            DStmt := Connection.Stmts.Element (Stmt.Cached.Id);
         end if;

         if DStmt = No_DBMS_Stmt then
            DStmt := Connect_And_Prepare
              (Connection, Stmt.Cached.Str.all,
               Stmt.Cached.Id, Direct => True);

            --  DBMS might not support prepared statements
            if DStmt /= No_DBMS_Stmt then
               if Connection.Stmts.Last_Index < Stmt.Cached.Id then
                  Connection.Stmts.Append
                    (New_Item => No_DBMS_Stmt,
                     Count    => Count_Type (Stmt.Cached.Id)
                        - Connection.Stmts.Length + 1);
               end if;
               Connection.Stmts.Replace_Element (Stmt.Cached.Id, DStmt);
            end if;
         else
            Reset (Connection, DStmt);
         end if;
      end if;

      --  If we successfully retrieved or created the prepared statement

      Execute_And_Log
        (Result, Connection,
         Stmt.Cached.Str.all, DStmt, Stmt.Cached.Is_Select, Direct => True);

      --  ??? Should only cache if the query was successful
      if Stmt.Use_Cache
        and then Connection.DB.Caching
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
      Stmt       : in out Prepared_Statement)
   is
      DStmt  : DBMS_Stmt := No_DBMS_Stmt;
   begin
      Result := No_Element;

      if Stmt.Cached.Id = 0 then
         Query_Cache.Prepare_Statement (Stmt, Connection.all);
      end if;

      if Stmt.Use_Cache then
         --  When using a cache, we have to use a Direct_Cursor for the cache
         --  to work
         declare
            R : Direct_Cursor;
         begin
            Fetch (R, Connection, Stmt);
            Result := Forward_Cursor (R);
         end;

      else
         if Stmt.On_Server then
            if Stmt.Cached.Id <= Connection.Stmts.Last_Index then
               DStmt := Connection.Stmts.Element (Stmt.Cached.Id);
            end if;

            if DStmt = No_DBMS_Stmt then
               DStmt := Connect_And_Prepare
                 (Connection, Stmt.Cached.Str.all,
                  Stmt.Cached.Id, Direct => False);

               --  DBMS might not support prepared statements
               if DStmt /= No_DBMS_Stmt then
                  if Connection.Stmts.Last_Index < Stmt.Cached.Id then
                     Connection.Stmts.Append
                       (New_Item => No_DBMS_Stmt,
                        Count    => Count_Type (Stmt.Cached.Id)
                        - Connection.Stmts.Length + 1);
                  end if;
                  Connection.Stmts.Replace_Element (Stmt.Cached.Id, DStmt);
               end if;
            else
               Reset (Connection, DStmt);
            end if;
         end if;

         --  If we successfully retrieved or created the prepared statement

         Execute_And_Log
           (Result, Connection,
            Stmt.Cached.Str.all, DStmt, Stmt.Cached.Is_Select,
            Direct => False);
      end if;
   end Fetch;

   ----------------------------------
   -- Finalize_Prepared_Statements --
   ----------------------------------

   procedure Finalize_Prepared_Statements is
   begin
      Query_Cache.Finalize_Prepared_Statements;
   end Finalize_Prepared_Statements;

   -------------------------
   -- Connect_And_Prepare --
   -------------------------

   function Connect_And_Prepare
     (Connection : access Database_Connection_Record;
      Query      : String;
      Id         : Stmt_Id;
      Direct     : Boolean)
      return DBMS_Stmt
   is
      pragma Unreferenced (Connection, Query, Direct, Id);
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
      Direct      : Boolean) return Abstract_Cursor_Access
   is
      pragma Unreferenced (Connection, Prepared, Is_Select, Direct);
   begin
      return null;
   end Execute;

   -----------
   -- Fetch --
   -----------

   procedure Fetch
     (Result     : out Direct_Cursor;
      Connection : access Database_Connection_Record'Class;
      Stmt       : SQL_Query;
      Use_Cache  : Boolean)
   is
      P : Prepared_Statement := Prepare (Stmt, Use_Cache => Use_Cache);
   begin
      Result.Fetch (Connection, P);
   end Fetch;

end GNATCOLL.SQL.Exec;
