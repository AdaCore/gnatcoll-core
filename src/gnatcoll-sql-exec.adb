------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2016, AdaCore                     --
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

pragma Ada_2012;
with Ada.Calendar;               use Ada.Calendar;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Fixed;          use Ada.Strings;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.SQL.Exec_Private; use GNATCOLL.SQL.Exec_Private;
with GNATCOLL.SQL.Exec.Tasking; use GNATCOLL.SQL.Exec.Tasking;
with Interfaces.C.Strings;
with System.Address_Image;

package body GNATCOLL.SQL.Exec is

   Me_Error  : constant Trace_Handle := Create ("SQL.ERROR", On);
   Me_Select : constant Trace_Handle := Create ("SQL.SELECT", Off);
   Me_Cache  : constant Trace_Handle := Create ("SQL.CACHE");
   Me_Perf   : constant Trace_Handle := Create ("SQL.PERF", Off);
   Me_Query  : constant Trace_Handle := Create ("SQL", Off);
   --  Disable by default those streams that tend to output a lot of data in
   --  standard applications.

   Cache_Expiration_Delay : constant Duration := 3600.0;  --  1 hour
   --  Delay after which the SQL cache expires and must be reset

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Abstract_DBMS_Forward_Cursor'Class, Abstract_Cursor_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Prepared_In_Session, Prepared_In_Session_List);

   function Is_Select_Query (Query : String) return Boolean;
   --  Return true if Query is a select query

   function Display_Query
     (Query      : String;
      Prepared   : Prepared_Statement'Class := No_Prepared) return String;
   --  Return the display for Query (or Prepared, if specified).
   --  This is for debug purposes only.

   procedure Execute_And_Log
     (Result     : in out Forward_Cursor'Class;
      Connection : access Database_Connection_Record'Class;
      Query      : String;
      Prepared   : Prepared_Statement'Class := No_Prepared;
      Direct     : Boolean;
      Params     : SQL_Parameters := No_Parameters);
   --  Low-level call to perform a query on the database and log results.
   --  The Query parameter is ignored if Prepared is provided.

   procedure Fetch_Internal
     (Result     : out Forward_Cursor'Class;
      Connection : access Database_Connection_Record'Class;
      Stmt       : Prepared_Statement'Class;
      Params     : SQL_Parameters);

   function Hash (Key : Cache_Id) return Ada.Containers.Hash_Type;

   package Cached_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Cache_Id,
      Element_Type    => Direct_Cursor,
      Hash            => Hash,
      Equivalent_Keys => "=");
   --  Cache the results of queries

   procedure Compute_And_Prepare_Statement
     (Prepared   : Prepared_Statement'Class;
      Connection : access Database_Connection_Record'Class;
      Stmt       : out DBMS_Stmt);
   --  Format the statement into a string, if not done yet.

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
         Found   : out Boolean);
      --  Return null or the cached value for the statement

      procedure Set_Id (Stmt : Prepared_Statement'Class);
      --  Set the Cached_Result field of Stmt

      procedure Set_Cache
        (Stmt : Prepared_Statement'Class; Cached : Forward_Cursor'Class);
      --  Add a new value in the cache

      procedure Unset_Cache (Stmt : Prepared_Statement_Data);
      --  Unset the cache entry for this particular element

      procedure Reset;
      --  Reset the cache

      procedure Mark_DB_As_Free (DB : Database_Connection; Closed : Boolean);
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
         Found   : out Boolean) is
      begin
         if Clock - Timestamp > Cache_Expiration_Delay then
            Reset;
            Found := False;
         else
            declare
               C : Cached_Maps.Cursor;
            begin
               if Stmt.Get.Cached_Result = No_Cache_Id
                 or else not Stmt.Get.Use_Cache
               then
                  Found := False;
               else
                  C := Cached_Maps.Find (Cache, Stmt.Get.Cached_Result);
                  Found := Cached_Maps.Has_Element (C);

                  if Found then
                     Cached := Task_Safe_Clone (Cached_Maps.Element (C));
                  end if;
               end if;
            end;
         end if;

      exception
         when E : others =>
            Trace (Me_Cache, E, "Get_Result ");
      end Get_Result;

      ------------
      -- Set_Id --
      ------------

      procedure Set_Id (Stmt : Prepared_Statement'Class) is
      begin
         if not Stmt.Is_Null
            and then Stmt.Get.Cached_Result = No_Cache_Id
         then
            Stmt.Get.Cached_Result := Current_Cache_Id;
            Current_Cache_Id := Current_Cache_Id + 1;
         end if;

      exception
         when E : others =>
            Trace (Me_Cache, E, "Set_Id ");
      end Set_Id;

      ---------------
      -- Set_Cache --
      ---------------

      procedure Set_Cache
        (Stmt : Prepared_Statement'Class; Cached : Forward_Cursor'Class)
      is
      begin
         --  Reserve capacity up to the current assigned id, since we are
         --  likely to need it anyway, and it is bound to be at least as big
         --  as Stmt.Cached.Id

         if Stmt.Get.Use_Cache then
            Set_Id (Stmt);
            Cache.Include
              (Stmt.Get.Cached_Result,
               Task_Safe_Instance (Cached, Index_By => Stmt.Get.Index_By));
         end if;

      exception
         when E : others =>
            Trace (Me_Cache, E, "Set_Cache ");
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

      exception
         when E : others =>
            Trace (Me_Cache, E, "Unset_Cache ");
      end Unset_Cache;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Cache.Clear;
         Timestamp := Clock;
      exception
         when E : others =>
            Trace (Me_Cache, E, "Reset ");
      end Reset;

      ---------------------
      -- Mark_DB_As_Free --
      ---------------------

      procedure Mark_DB_As_Free (DB : Database_Connection; Closed : Boolean) is
      begin
         if Closed then
            Freed_DB.Include (DB);
         else
            Freed_DB.Exclude (DB);
         end if;

      exception
         when E : others =>
            Trace (Me_Cache, E, "Mark_DB_As_Free ");
      end Mark_DB_As_Free;

      ---------------
      -- Was_Freed --
      ---------------

      function Was_Freed (DB : Database_Connection) return Boolean is
      begin
         return Freed_DB.Contains (DB);

      exception
         when E : others =>
            Trace (Me_Cache, E, "Was_Freed ");

            return False;
      end Was_Freed;

   end Query_Cache;

   ---------------
   -- To_String --
   ---------------

   function To_String
      (Connection : access Database_Connection_Record;
       Stmt       : Prepared_Statement'Class)
      return String
   is
      S : constant access Prepared_Statement_Data := Stmt.Unchecked_Get;
   begin
      if S.Query_Str = null then
         S.Query_Str := new String'
            (To_String (To_String (S.Query, Connection.all)));

         if Active (Me_Query) then
            Trace
              (Me_Query, "compute (" & S.Name.all & "): "
               & S.Query_Str.all);
         end if;

         S.Query := No_Query;   --  release memory
         S.Is_Select := Is_Select_Query (S.Query_Str.all);
      end if;

      return S.Query_Str.all;
   end To_String;

   -----------------------------------
   -- Compute_And_Prepare_Statement --
   -----------------------------------

   procedure Compute_And_Prepare_Statement
     (Prepared   : Prepared_Statement'Class;
      Connection : access Database_Connection_Record'Class;
      Stmt       : out DBMS_Stmt)
   is
      L : Prepared_In_Session_List;

      --  The side effect is to set S.Query_Str
      Str : constant String := To_String (Connection, Prepared);
   begin
      if Prepared.Get.On_Server
         and Is_Prepared_On_Server_Supported (Connection)
      then
         --  Reuse a prepared statement if one exists for this connection.

         L := Prepared.Get.Prepared;
         while L /= null loop
            exit when L.DB = Database_Connection (Connection);
            L := L.Next;
         end loop;

         if L = null then
            L := new Prepared_In_Session'
              (Stmt         => No_DBMS_Stmt,
               DB           => Database_Connection (Connection),
               DB_Timestamp => Connection.Connected_On,
               Next         => Prepared.Get.Prepared);
            Prepared.Get.Prepared := L;
         end if;

         --  Else prepare the statement

         if L.Stmt = No_DBMS_Stmt
            or else L.DB_Timestamp /= Connection.Connected_On
         then
            L.Stmt := Connect_And_Prepare
              (Connection, Str, Prepared.Get.Name.all, Direct => True);

            --  Set the timestamp *after* we have created the connection, in
            --  case it did not exist before (if prepare is the first command
            --  done on this connection).

            L.DB_Timestamp := Connection.Connected_On;

            --  L.Stmt could still be No_DBMS_Stmt if the backend does not
            --  support preparation on the server.  ??? This means we'll try
            --  again next time. For now, all supported DBMS have prepared
            --  statement, so that's not an issue.

         else
            Reset (Connection, L.Stmt);
         end if;

         Stmt := L.Stmt;
      else
         Stmt := No_DBMS_Stmt;
      end if;
   end Compute_And_Prepare_Statement;

   -------------------
   -- Print_Warning --
   -------------------

   procedure Print_Warning
     (Connection : access Database_Connection_Record'Class; Str : String) is
   begin
      Trace (Me_Query, Str & " (" & Connection.Username.all & ")");

      if Connection.Descr.Errors /= null then
         Connection.Descr.Errors.On_Warning (Connection, Str);
      end if;
   end Print_Warning;

   -----------------
   -- Print_Error --
   -----------------

   procedure Print_Error
     (Connection : access Database_Connection_Record'Class; Str : String) is
   begin
      Trace (Me_Error, Str & " (" & Connection.Username.all & ")");

      if Connection.Descr.Errors /= null then
         Connection.Descr.Errors.On_Error (Connection, Str);
      end if;
   end Print_Error;

   -------------------------------
   -- Report_Database_Corrupted --
   -------------------------------

   procedure Report_Database_Corrupted
     (Connection : access Database_Connection_Record'Class)
   is
   begin
      if Connection.Descr.Errors /= null then
         Connection.Descr.Errors.On_Database_Corrupted (Connection);
      end if;

      Print_Error (Connection, "Error, database is corrupted");
   end Report_Database_Corrupted;

   ----------
   -- Free --
   ----------

   procedure Free (Description : in out Database_Description) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Database_Description_Record'Class, Database_Description);
   begin
      if Description /= null then
         --  as documented (in gnatcoll.sql.sqlite.setup), do not free the
         --  memory for Errors

         Free (Description.all);
         Unchecked_Free (Description);
      end if;
   end Free;

   ----------------------
   -- Check_Connection --
   ----------------------

   function Check_Connection
     (Connection : access Database_Connection_Record) return Boolean
   is
      Success    : Boolean;
      R          : Abstract_Cursor_Access;
   begin
      if Connection = null then
         Trace (Me_Error, "DBMS backend not supported");
         return False;
      end if;

      R := Connect_And_Execute
        (Database_Connection (Connection),
         Query     => "",
         Is_Select => False,
         Direct    => False);
      Success := R /= null;
      Unchecked_Free (R);

      if Success then
         Trace (Me_Query, "Init_Database: database successfully initialized");
      else
         Trace
           (Me_Error,
            "Init_Database: check_connection FAILED: "
            & Error (Database_Connection (Connection)));
      end if;

      return Success;
   end Check_Connection;

   ---------------------
   -- Is_Select_Query --
   ---------------------

   function Is_Select_Query (Query : String) return Boolean is
      --  Allow both "SELECT" and "(SELECT" (the latter is used when we do a
      --  union between two selects
   begin
      return Fixed.Index
               (Query, "SELECT ", Mapping => Maps.Constants.Upper_Case_Map)
             in 1 .. 2;
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
            return String_To_SQL (Format, To_String (Param), Quote => False);
         when Parameter_Integer =>
            return Integer_To_SQL (Format, Param.Int_Val, Quote => False);
         when Parameter_Bigint =>
            return Bigint_To_SQL (Format, Param.Bigint_Val, Quote => False);
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
         when Parameter_Money =>
            return Money_To_SQL (Format, Param.Money_Val, Quote => False);
         when Parameter_Json =>
            return Json_To_SQL (Format, To_String (Param), Quote => False);
         when Parameter_XML =>
            return XML_To_SQL (Format, To_String (Param), Quote => False);
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
         Append (Result, Image (Format, Params (P)));
      end loop;

      return To_String (Result);
   end Image;

   -------------------
   -- Display_Query --
   -------------------

   function Display_Query
     (Query      : String;
      Prepared   : Prepared_Statement'Class := No_Prepared) return String is
   begin
      if not Prepared.Is_Null then
         return "(" & Prepared.Get.Name.all & ")";
      else
         return Query;
      end if;
   end Display_Query;

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
            Trace (Me_Error, "Transaction failed (null result): "
                   & Display_Query (Query, Prepared)
                   & Image (Connection.all, Params));
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
               Trace (Me_Error, "select failed: "
                      & Display_Query (Query, Prepared)
                      & Image (Connection.all, Params)
                      & " " & Status (DBMS_Forward_Cursor'Class (R.all))
                      & " " & Error_Msg (DBMS_Forward_Cursor'Class (R.all))
                      & Get_User);
            end if;

            Set_Failure (Connection);

         elsif Active (Me_Select) then
            Trace
              (Me_Select,
               Display_Query (Query, Prepared)
               & Image (Connection.all, Params)
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
               Trace (Me_Error, "Transaction failed: "
                      & Display_Query (Query, Prepared)
                      & Image (Connection.all, Params)
                      & " " & Status (DBMS_Forward_Cursor'Class (R.all))
                      & " " & Error_Msg (DBMS_Forward_Cursor'Class (R.all))
                      & Get_User);
            end if;

            Set_Failure
              (Connection, Error_Msg (DBMS_Forward_Cursor'Class (R.all)));

         elsif Active (Me_Query) then
            declare
               Q : constant String := Display_Query (Query, Prepared);
            begin
               if Q = "BEGIN" then
                  Increase_Indent
                    (Me_Query,
                     Q & Image (Connection.all, Params)
                     & Get_Rows & " "
                     & Status (DBMS_Forward_Cursor'Class (R.all)) & Get_User);
               else
                  Trace
                    (Me_Query,
                     Q & Image (Connection.all, Params)
                     & Get_Rows & " "
                     & Status (DBMS_Forward_Cursor'Class (R.all)) & Get_User);
               end if;
            end;
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
      if not Connection.In_Transaction
        or else not Connection.Automatic_Transactions
      then
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
      Direct     : Boolean;
      Params     : SQL_Parameters := No_Parameters)
   is
      Is_Select : Boolean;
      Is_Commit_Or_Rollback : Boolean := False;
      Stmt : DBMS_Stmt := No_DBMS_Stmt;
      R    : Abstract_Cursor_Access;
      Was_Started : Boolean;
      pragma Unreferenced (Was_Started);

      Is_Prepared : constant Boolean :=
        Prepared /= Prepared_Statement'Class (No_Prepared);
      Index_By : Field_Index'Base;

      Start : Time;

      Q : access String := Query'Unrestricted_Access;
      --  Should be safe here, we do not intend to free anything

   begin
      if Active (Me_Perf) then
         Start := Clock;
      end if;

      if Is_Prepared then
         --  Compute the query. We cannot reference the query before
         --  that, since it might not have been computed yet.

         Compute_And_Prepare_Statement (Prepared, Connection, Stmt);
         Is_Select := Prepared.Get.Is_Select;
         Q := Prepared.Get.Query_Str;

      else
         Is_Select := Is_Select_Query (Query);
      end if;

      if Active (Me_Query) then
         Is_Commit_Or_Rollback :=
           Equal (Q.all, "commit", Case_Sensitive => False)
           or else Equal (Q.all, "rollback", Case_Sensitive => False);
         if Is_Commit_Or_Rollback and then Active (Me_Query) then
            Decrease_Indent (Me_Query);
         end if;
      end if;

      --  Transaction management: do we need to start a transaction ?

      if Connection.Automatic_Transactions then
         if not Is_Select then
            Is_Commit_Or_Rollback :=
              Equal (Q.all, "commit", Case_Sensitive => False)
              or else Equal (Q.all, "rollback", Case_Sensitive => False);
         end if;

         if Connection.In_Transaction
           and then not Connection.Success
         then
            Trace
              (Me_Error,
               "Ignored, since transaction in failure: "
               & Display_Query (Q.all, Prepared)
               & " (" & Connection.Username.all & ")");
            return;

         elsif Equal (Q.all, "begin", Case_Sensitive => False) then
            if not Connection.In_Transaction then
               Connection.In_Transaction := True;
            else
               --  Ignore silently: GNATCOLL might have started a transaction
               --  without the user knowing, for instance on the first SELECT
               --  statement if Always_Use_Transactions is true.
               return;
            end if;

         elsif not Connection.In_Transaction
           and then
             (Connection.Always_Use_Transactions
              or else
                (not Is_Commit_Or_Rollback
                 and then not Is_Select))  --  INSERT, UPDATE, LOCK, DELETE,...
           and then
             (Q'Length <= 7   --  for sqlite
              or else Q (Q'First .. Q'First + 6) /= "PRAGMA ")
           and then
             (Q'Length <= 7   --  for sqlite
              or else Q (Q'First .. Q'First + 6) /= "ANALYZE")
         then
            --  Start a transaction automatically
            Was_Started := Start_Transaction (Connection);
            if not Connection.Success then
               return;
            end if;
         end if;
      end if;

      if Perform_Queries then
         R := Connect_And_Execute
           (Connection => Connection,
            Query      => Q.all,
            Stmt       => Stmt,
            Is_Select  => Is_Select,
            Direct     => Direct,
            Params     => Params);

         if R = null then
            if Active (Me_Error) then
               if Stmt /= No_DBMS_Stmt then
                  Trace (Me_Error, "Failed to execute prepared ("
                         & Prepared.Get.Name.all & ") " & Q.all
                         & " " & Image (Connection.all, Params)
                         & " error=" & Error (Connection));
               else
                  Trace (Me_Error, "Failed to execute " & Q.all
                         & " " & Image (Connection.all, Params)
                         & " error=" & Error (Connection));
               end if;
            end if;

            Set_Failure (Connection);

         else
            Index_By := (if Is_Prepared then Prepared.Get.Index_By
                         else No_Field_Index);

            if Direct
              and then (R.all not in DBMS_Direct_Cursor'Class
                        or Index_By /= No_Field_Index)
            then
               --  DBMS does not support Direct_Cursor or indexed cursor
               --  requested. We now need to read all the results and store
               --  them into GNATCOLL implemented Direct_Cursor.

               declare
                  R2 : constant Abstract_Cursor_Access :=
                    Task_Safe_Instance (R, Index_By);
               begin
                  Unchecked_Free (R);

                  R := R2;
               end;
            end if;

            Post_Execute_And_Log
              (R, Connection, Q.all, Prepared, Is_Select, Params);
         end if;

         Result.Res := R;
      end if;

      if Connection.Automatic_Transactions
        and then Connection.In_Transaction
        and then Is_Commit_Or_Rollback
      then
         Connection.In_Transaction := False;
      end if;

      if Active (Me_Perf) then
         Trace (Me_Perf, "Finished executing query:"
                & Duration'Image ((Clock - Start) * 1000.0) & " ms");
      end if;
   end Execute_And_Log;

   -----------------------
   -- Insert_And_Get_PK --
   -----------------------

   function Insert_And_Get_PK
     (Connection : access Database_Connection_Record'Class;
      Query      : GNATCOLL.SQL.SQL_Query;
      Params     : SQL_Parameters := No_Parameters;
      PK         : SQL_Field_Integer) return Integer
   is
   begin
      return Insert_And_Get_PK
         (Connection, To_String (To_String (Query, Connection.all)),
          Params, PK);
   end Insert_And_Get_PK;

   -----------------------
   -- Insert_And_Get_PK --
   -----------------------

   function Insert_And_Get_PK
     (Connection : access Database_Connection_Record;
      Query      : String;
      Params     : SQL_Parameters := No_Parameters;
      PK         : SQL_Field_Integer) return Integer
   is
      R : Forward_Cursor;
      Id : Integer;
   begin
      Fetch (R, Connection, Query, Params);
      Id := Last_Id (R, Connection, PK);

      if Active (Me_Query) then
         Trace (Me_Query, "  => id=" & Id'Img);
      end if;

      return Id;
   end Insert_And_Get_PK;

   -----------
   -- Fetch --
   -----------

   procedure Fetch
     (Result     : out Forward_Cursor;
      Connection : access Database_Connection_Record'Class;
      Query      : String;
      Params     : SQL_Parameters := No_Parameters) is
   begin
      Result := No_Element;
      Execute_And_Log
        (Result, Connection, Query, No_Prepared, Direct => False,
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
      Params     : SQL_Parameters := No_Parameters) is
   begin
      Result := No_Direct_Element;
      Execute_And_Log
        (Result, Connection, Query, No_Prepared, Direct => True,
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
      if Connection.In_Transaction
        or else not Connection.Automatic_Transactions
      then
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
      if Connection.In_Transaction
        or else not Connection.Automatic_Transactions
      then
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
      if Connection.In_Transaction then
         Rollback (Connection); --  In case a previous thread had started on
      end if;
      Connection.Success := True;
      Connection.Automatic_Transactions := True;

      if Username /= "" or else Connection.Username = null then
         GNAT.Strings.Free (Connection.Username);
         Connection.Username := new String'(Username);
      end if;

      GNAT.Strings.Free (Connection.Error_Msg);
   end Reset_Connection;

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

   ---------------------
   -- Unbounded_Value --
   ---------------------

   function Unbounded_Value
     (Self : Forward_Cursor; Field : Field_Index) return Unbounded_String is
   begin
      return Unbounded_Value (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   end Unbounded_Value;

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
      when Constraint_Error | Interfaces.C.Strings.Dereference_Error  =>
         return Default;
   end Integer_Value;

   ------------------
   -- Bigint_Value --
   ------------------

   function Bigint_Value
     (Self   : Forward_Cursor;
      Field  : Field_Index) return Long_Long_Integer
   is
   begin
      return Bigint_Value (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   end Bigint_Value;

   ------------------
   -- Bigint_Value --
   ------------------

   function Bigint_Value
     (Self   : Forward_Cursor;
      Field  : Field_Index;
      Default : Long_Long_Integer) return Long_Long_Integer
   is
   begin
      return Bigint_Value (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   exception
      when Constraint_Error | Interfaces.C.Strings.Dereference_Error  =>
         return Default;
   end Bigint_Value;

   -----------------
   -- Float_Value --
   -----------------

   function Float_Value
     (Self  : Forward_Cursor;
      Field : Field_Index) return Float is
   begin
      return Float_Value (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   end Float_Value;

   function Float_Value
     (Self    : Forward_Cursor;
      Field   : Field_Index;
      Default : Float) return Float is
   begin
      return Float_Value (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   exception
      when Constraint_Error | Interfaces.C.Strings.Dereference_Error =>
         return Default;
   end Float_Value;

   -----------
   -- Value --
   -----------

   function Money_Value
     (Self : Forward_Cursor; Field : Field_Index)
     return T_Money is
   begin
      return Money_Value (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   end Money_Value;

   ----------------
   -- Time_Value --
   ----------------

   function Time_Value
     (Self  : Forward_Cursor;
      Field : Field_Index) return Ada.Calendar.Time is
   begin
      return Time_Value (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   end Time_Value;

   ---------------------
   -- Json_Text_Value --
   ---------------------

   function Json_Text_Value
     (Self  : Forward_Cursor;
      Field : Field_Index) return String is
   begin
      return Value (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   end Json_Text_Value;

   --------------------
   -- XML_Text_Value --
   --------------------

   function XML_Text_Value
     (Self  : Forward_Cursor;
      Field : Field_Index) return String is
   begin
      return Value (DBMS_Forward_Cursor'Class (Self.Res.all), Field);
   end XML_Text_Value;

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
         if Self.Res = null then
            return -1;
         else
            return Last_Id
              (DBMS_Forward_Cursor'Class (Self.Res.all), Connection, Field);
         end if;
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
   -- Find --
   ----------

   procedure Find (Self : in out Direct_Cursor; Value : Integer) is
   begin
      Find (Self, Image (Value, Min_Width => 0));
   end Find;

   procedure Find (Self : in out Direct_Cursor; Value : String) is
   begin
      Tasking.Find (Self.Res, Value);
   end Find;

   --------------------
   -- Mark_As_Closed --
   --------------------

   procedure Mark_As_Closed
     (Connection : access Database_Connection_Record'Class;
      Closed     : Boolean) is
   begin
      Query_Cache.Mark_DB_As_Free (Database_Connection (Connection), Closed);
   end Mark_As_Closed;

   ----------------
   -- Was_Closed --
   ----------------

   function Was_Closed
     (Connection : access Database_Connection_Record'Class) return Boolean is
   begin
      return Query_Cache.Was_Freed (Database_Connection (Connection));
   end Was_Closed;

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

         Mark_As_Closed (Connection, Closed => True);
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
      Index_By      : Field_Index'Base := No_Field_Index;
      Name          : String := "") return Prepared_Statement
   is
      Stmt : Prepared_Statement;
      Data : Prepared_Statement_Data;
      Ptr  : access Prepared_Statement_Data;
   begin
      Data := Prepared_Statement_Data'
        (Query         => Query,
         Query_Str     => null,   --  Computed later
         Is_Select     => False,  --  Computed later
         Use_Cache     => Use_Cache,
         Cached_Result => No_Cache_Id,
         Index_By      => Index_By,
         On_Server     => On_Server,
         Name          => null,
         Prepared      => null);

      if Auto_Complete then
         GNATCOLL.SQL.Auto_Complete (Data.Query);
      end if;

      Stmt.Set (Data);

      Query_Cache.Set_Id (Stmt);

      Ptr := Stmt.Unchecked_Get;

      if Name = "" then
         Ptr.Name :=
           new String'("stmt" & Image (Integer (Ptr.Cached_Result), 0));
      else
         Ptr.Name := new String'(Name);
      end if;

      return Stmt;
   end Prepare;

   -------------
   -- Prepare --
   -------------

   function Prepare
     (Query     : String;
      Use_Cache : Boolean := False;
      On_Server : Boolean := False;
      Index_By  : Field_Index'Base := No_Field_Index;
      Name      : String := "") return Prepared_Statement
   is
      Stmt : Prepared_Statement;
      Data : Prepared_Statement_Data :=
        (Query         => No_Query,
         Query_Str     => new String'(Query),
         Is_Select     => Is_Select_Query (Query),
         Use_Cache     => Use_Cache,
         Cached_Result => No_Cache_Id,
         Index_By      => Index_By,
         On_Server     => On_Server,
         Name          => null,
         Prepared      => null);
   begin
      if Active (Me_Query) then
         Trace (Me_Query, "compute (" & Name & "): " & Query);
      end if;

      Query_Cache.Set_Id (Stmt);

      if Name = "" then
         Data.Name :=
           new String'("stmt" & Image (Integer (Data.Cached_Result), 0));
      else
         Data.Name := new String'(Name);
      end if;

      Stmt.Set (Data);
      return Stmt;
   end Prepare;

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache (Stmt : Prepared_Statement) is
   begin
      Query_Cache.Unset_Cache (Stmt.Get);
   end Clear_Cache;

   --------------------
   -- Fetch_Internal --
   --------------------

   procedure Fetch_Internal
     (Result     : out Forward_Cursor'Class;
      Connection : access Database_Connection_Record'Class;
      Stmt       : Prepared_Statement'Class;
      Params     : SQL_Parameters)
   is
      Direct : constant Boolean := Result in Direct_Cursor'Class;
      Found : Boolean;

      procedure Put_Result (Item : Forward_Cursor'Class);

      procedure Put_Result (Item : Forward_Cursor'Class) is
      begin
         if Direct then
            Direct_Cursor (Result) := Direct_Cursor (Item);
         else
            Forward_Cursor (Result) := Forward_Cursor (Item);
         end if;
      end Put_Result;

   begin
      Put_Result (No_Direct_Element);

      if Stmt.Is_Null then
         Trace (Me_Error, "Prepared statement was freed, can't execute");
         return;
      end if;

      if Stmt.Get.Use_Cache
        and then Connection.Descr.Caching
        and then Params = No_Parameters --  Parameters not supported for now
      then
         declare
            RC : Direct_Cursor;
         begin
            Query_Cache.Get_Result (Stmt, RC, Found);

            if Found then
               RC.First; --  Move to first element
               if Active (Me_Cache) then
                  Trace (Me_Cache, "(" & Stmt.Get.Name.all & "): from cache");
               end if;

               Put_Result (RC);

               return;
            end if;
         end;

         declare
            RE : Forward_Cursor;
         begin
            --  for the caching do not need the Direct cursor, because it have
            --  to be translated into cache task safe anyway.

            Execute_And_Log
              (RE, Connection, "", Stmt, Direct => False, Params => Params);

            if Success (Connection) then
               Query_Cache.Set_Cache (Stmt, RE);

               --  Recursive reuse get from cache code of this routine

               Fetch_Internal (Result, Connection, Stmt, Params);

            elsif Direct then
               --  Just to return error in direct cursor type

               Put_Result (Task_Safe_Instance (RE, Stmt.Get.Index_By));

            else
               --  Just to return error

               Put_Result (RE);
            end if;

            return;
         end;
      end if; -- Cache processing

      Execute_And_Log
        (Result, Connection, "", Stmt, Direct => Direct, Params => Params);
   end Fetch_Internal;

   -----------
   -- Fetch --
   -----------

   procedure Fetch
     (Result     : out Direct_Cursor;
      Connection : access Database_Connection_Record'Class;
      Stmt       : Prepared_Statement'Class;
      Params     : SQL_Parameters := No_Parameters) is
   begin
      Fetch_Internal (Result, Connection, Stmt, Params);
   end Fetch;

   -----------
   -- Fetch --
   -----------

   procedure Fetch
     (Result     : out Forward_Cursor;
      Connection : access Database_Connection_Record'Class;
      Stmt       : Prepared_Statement'Class;
      Params     : SQL_Parameters := No_Parameters) is
   begin
      Fetch_Internal (Result, Connection, Stmt, Params);
   end Fetch;

   -----------------------
   -- Insert_And_Get_PK --
   -----------------------

   function Insert_And_Get_PK
     (Connection : access Database_Connection_Record;
      Stmt       : Prepared_Statement'Class;
      Params     : SQL_Parameters := No_Parameters;
      PK         : SQL_Field_Integer) return Integer
   is
      Result : Forward_Cursor;
      Id : Integer;
   begin
      Execute_And_Log
        (Result, Connection, "", Stmt, Direct => False, Params => Params);
      Id := Last_Id (Result, Connection, PK);

      if Active (Me_Query) then
         Trace (Me_Query, "  => id=" & Id'Img);
      end if;

      return Id;
   end Insert_And_Get_PK;

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

   function "+" (Value : access constant String) return SQL_Parameter is
   begin
      return SQL_Parameter'
        (Typ => Parameter_Text, Str_Ptr => Value.all'Unchecked_Access,
         Str_Val => <>);
   end "+";

   function "+" (Value : String) return SQL_Parameter is
   begin
      return SQL_Parameter'
        (Typ => Parameter_Text, Str_Val => To_Unbounded_String (Value),
         Str_Ptr => <>);
   end "+";

   function "+" (Value : Unbounded_String) return SQL_Parameter is
   begin
      return SQL_Parameter'
         (Typ => Parameter_Text, Str_Val => Value, Str_Ptr => <>);
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Value : Integer) return SQL_Parameter is
   begin
      return SQL_Parameter'(Typ => Parameter_Integer, Int_Val => Value);
   end "+";

   ---------------
   -- As_Bigint --
   ---------------

   function As_Bigint (Value : Long_Long_Integer) return SQL_Parameter is
   begin
      return SQL_Parameter'(Typ => Parameter_Bigint, Bigint_Val => Value);
   end As_Bigint;

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

   ---------
   -- "+" --
   ---------

   function "+" (Value : T_Money) return SQL_Parameter is
   begin
      return SQL_Parameter'(Typ => Parameter_Money, Money_Val => Value);
   end "+";

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Prepared_Statement_Data) is
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

   ----------------------------
   -- Automatic_Transactions --
   ----------------------------

   procedure Automatic_Transactions
     (Connection : access Database_Connection_Record'Class;
      Active     : Boolean := True) is
   begin
      Connection.Automatic_Transactions := Active;
   end Automatic_Transactions;

   ----------------------------
   -- Automatic_Transactions --
   ----------------------------

   function Automatic_Transactions
     (Connection : access Database_Connection_Record'Class) return Boolean
   is
   begin
      return Connection.Automatic_Transactions;
   end Automatic_Transactions;

   -------------------------------------
   -- Is_Prepared_On_Server_Supported --
   -------------------------------------

   function Is_Prepared_On_Server_Supported
     (Connection : access Database_Connection_Record) return Boolean
   is
      pragma Unreferenced (Connection);
   begin
      return True;
   end Is_Prepared_On_Server_Supported;

end GNATCOLL.SQL.Exec;
