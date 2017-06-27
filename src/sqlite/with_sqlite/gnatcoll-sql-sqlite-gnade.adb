------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2017, AdaCore                     --
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
with Interfaces.C;             use Interfaces.C;
with Interfaces.C.Strings;     use Interfaces.C.Strings;
with System.Address_Image;
with GNATCOLL.Traces;          use GNATCOLL.Traces;

package body GNATCOLL.SQL.Sqlite.Gnade is
   Me : constant Trace_Handle := Create ("SQL.SQLITE.GNADE", Off);
   Debug : constant Boolean := False;
   --  Low-level debugging enabled. This traces all C functions that are
   --  called, and is used to check whether we use sqlite correctly.

   type Char_Ptr_Array is array (Natural) of chars_ptr;
   type Char_Ptr_Array_Access is access Char_Ptr_Array;
   pragma Convention (C, Char_Ptr_Array_Access);

   function To_Array is new Ada.Unchecked_Conversion
      (System.Address, Char_Ptr_Array_Access);

   type Sqlite3_Config_Option is
     (
      --  Disable all mutexes
      Config_Single_Thread,  --  nil

      --  Cannot share connections among threads
      Config_Multi_Thread,   --  nil

      --  Full serialization done by sqlite3, connections can be used
      --  from multithreads concurrently.
      Config_Serialized,     --  nil

      --  Specifies alternative memory allocation method
      Config_Malloc,         --  sqlite3_mem_methods
      Config_Get_Malloc,     --  sqlite3_mem_methods

      --  Specifies static buffers to use in various cases
      Config_Scratch,        --  void*, int sz, int N
      Config_Page_Cache,     --  void*, int sz, int N
      Config_Heap,           --  void*, int nByte, int min

      --  Should we collect memory allocation statistics (default is True)
      Config_Mem_Status,     --  boolean

      --  Alternative low-level mutexes
      Config_Mutex,          --  sqlite3_mutex_methods
      Config_Get_Mutex,      --  sqlite3_mutex_methods

      Config_Look_Aside,     --  int int
      Config_Pcache,         --  sqlite3_pcache_methods
      Config_Get_Pcache,     --  sqlite3_pcache_methods
      Config_Log,            --  xFunc, void*
      Config_URI);           --  int

   for Sqlite3_Config_Option use
     (Config_Single_Thread => 1,
      Config_Multi_Thread  => 2,
      Config_Serialized    => 3,
      Config_Malloc        => 4,
      Config_Get_Malloc    => 5,
      Config_Scratch       => 6,
      Config_Page_Cache    => 7,
      Config_Heap          => 8,
      Config_Mem_Status    => 9,
      Config_Mutex         => 10,
      Config_Get_Mutex     => 11,
      Config_Look_Aside    => 13,
      Config_Pcache        => 14,
      Config_Get_Pcache    => 15,
      Config_Log           => 16,
      Config_URI           => 17);

   function Sqlite3_Config
      (Option : Sqlite3_Config_Option) return Result_Codes;
   pragma Import (C, Sqlite3_Config, "sqlite3_config");

   ----------
   -- Open --
   ----------

   procedure Open
     (DB       : out Database;
      Filename : String := Open_In_Memory;
      Flags    : Open_Flags := Open_Readwrite or Open_Create;
      Status   : out Result_Codes)
   is
      function Internal
        (Name  : String;
         Db    : access Database;
         Flags : Integer;
         Vfs   : System.Address := System.Null_Address) return Result_Codes;
      pragma Import (C, Internal, "sqlite3_open_v2");

      DB2    : aliased Database;
      Ignored : Result_Codes;
      pragma Unreferenced (Ignored);
   begin
      if Debug then
         Trace (Me, "sqlite3_open_v2");
      end if;

      Status := Internal
        (Filename & ASCII.NUL, DB2'Unchecked_Access, Integer (Flags));

      if Status = Sqlite_OK then
         DB := DB2;
      else
         Close (DB2);
         DB := No_Database;
      end if;
   end Open;

   ---------------
   -- Error_Msg --
   ---------------

   function Error_Msg (DB : Database) return String is
      function Internal (DB : Database) return chars_ptr;
      pragma Import (C, Internal, "sqlite3_errmsg");
   begin
      if Debug then
         Trace (Me, "sqlite3_errmsg");
      end if;

      --  No need to free the result
      return Value (Internal (DB));
   end Error_Msg;

   -----------
   -- Close --
   -----------

   procedure Close
      (DB : Database;
       Finalize_Prepared_Statements : Boolean := True)
   is
      function Internal_Close (DB : Database) return Result_Codes;
      pragma Import (C, Internal_Close, "sqlite3_close_v2");
      --  If there are still unfinalized prepared statement, actual
      --  deallocation will be deferred. This is intended for use with garbage
      --  collected programmation languages, which is the case for the
      --  GNATCOLL.SQL package.

      function Next_Stmt
        (DB : Database; After : Statement := No_Statement)
         return Statement;
      pragma Import (C, Next_Stmt, "sqlite3_next_stmt");

      Stmt    : Statement;
      Ignored : Result_Codes;
      pragma Unreferenced (Ignored);
   begin
      if DB /= null then

         if Finalize_Prepared_Statements then
            if Debug then
               Trace (Me, "Finalize prepared statements");
            end if;

            loop
               if Debug then
                  Trace (Me, "sqlite3_next_stmt");
               end if;

               Stmt := Next_Stmt (DB);
               exit when Stmt = No_Statement;
               Finalize (Stmt);
            end loop;
         end if;

         if Debug then
            Trace (Me, "sqlite3_close_v2");
         end if;

         Ignored := Internal_Close (DB);
      end if;
   end Close;

   -------------
   -- Prepare --
   -------------

   procedure Prepare
     (DB     : Database;
      SQL    : String;  --  UTF-8 encoded
      Stmt   : out Statement;
      Status : out Result_Codes)
   is
      function Internal
        (DB    : Database;
         SQL   : String;
         NByte : Integer;
         Stmt  : access Statement;
         Tail  : access System.Address) return Result_Codes;
      pragma Import (C, Internal, "sqlite3_prepare_v2");

      Stmt2 : aliased Statement;
      Tail  : aliased System.Address;
   begin
      if Debug then
         Trace (Me, "sqlite3_prepare_v2");
      end if;

      Status := Internal (DB, SQL, SQL'Length, Stmt2'Access, Tail'Access);
      Stmt := Stmt2;
   end Prepare;

   ----------
   -- Step --
   ----------

   procedure Step
     (Stmt   : in out Statement;
      Status : out Result_Codes)
   is
      function Internal (Stmt : Statement) return Result_Codes;
      pragma Import (C, Internal, "sqlite3_step");
   begin
      if Debug then
         Trace (Me, "sqlite3_step");
      end if;

      Status := Internal (Stmt);
   end Step;

   -----------------
   -- Column_Text --
   -----------------

   function Column_Text (Stmt : Statement; Col : Natural) return String is
      Val : constant chars_ptr := Column_C_Text (Stmt, Col);
   begin
      return Value (Val);
   end Column_Text;

   -----------------
   -- Column_Name --
   -----------------

   function Column_Name (Stmt : Statement; Col : Natural) return String is
      function Internal (Stmt : Statement; Col : Natural) return chars_ptr;
      pragma Import (C, Internal, "sqlite3_column_name");
   begin
      if Debug then
         Trace (Me, "sqlite3_column_name");
      end if;

      return Value (Internal (Stmt, Col));
   end Column_Name;

   ---------------
   -- Get_Table --
   ---------------

   procedure Get_Table
     (DB     : Database;
      SQL    : String;
      Result : out Result_Table;
      Status : out Result_Codes;
      Error  : out chars_ptr)
   is
      function Internal
        (DB      : Database;
         SQL     : String;
         Result  : access System.Address;
         Rows    : access Natural;
         Columns : access Natural;
         Error   : access Interfaces.C.Strings.chars_ptr) return Result_Codes;
      pragma Import (C, Internal, "sqlite3_get_table");

   begin
      if Debug then
         Trace (Me, "sqlite3_get_table");
      end if;

      Status := Internal
        (DB, SQL & ASCII.NUL, Result.Values'Unrestricted_Access,
         Result.Rows'Unrestricted_Access,
         Result.Columns'Unrestricted_Access,
         Error'Unrestricted_Access);
   end Get_Table;

   ----------------
   -- Free_Table --
   ----------------

   procedure Free_Table (Result : in out Result_Table) is
      procedure Internal (Result : System.Address);
      pragma Import (C, Internal, "sqlite3_free_table");
   begin
      if Debug then
         Trace (Me, "sqlite3_free_table");
      end if;

      Internal (Result.Values);
   end Free_Table;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Result : Result_Table;
      Row, Column : Natural) return Interfaces.C.Strings.chars_ptr
   is
      Val : constant Char_Ptr_Array_Access := To_Array (Result.Values);
   begin
      --  First row is for column names, so skip it
      return Val ((Row * 1) * Result.Columns + Column);
   end Get_Value;

   --------------
   -- Get_Rows --
   --------------

   function Get_Rows (Result : Result_Table) return Natural is
   begin
      return Result.Rows;
   end Get_Rows;

   -----------------
   -- Get_Columns --
   -----------------

   function Get_Columns (Result : Result_Table) return Natural is
   begin
      return Result.Columns;
   end Get_Columns;

   ---------------------
   -- Get_Column_Name --
   ---------------------

   function Get_Column_Name
     (Result : Result_Table; Column : Natural) return String
   is
      Val : constant Char_Ptr_Array_Access := To_Array (Result.Values);
   begin
      return Interfaces.C.Strings.Value (Val (Column));
   end Get_Column_Name;

   -----------
   -- Image --
   -----------

   function Image (DB : Database) return String is
      function Convert is new Ada.Unchecked_Conversion
        (Database, System.Address);
   begin
      return "sqlite=" & System.Address_Image (Convert (DB));
   end Image;

   --------------------
   -- Set_Config_Log --
   --------------------

   procedure Set_Config_Log
     (Func : Logger;
      Data : System.Address := System.Null_Address)
   is
      procedure Sqlite3_Config
        (Option : Sqlite3_Config_Option;
         Func  : Logger;
         Data  : System.Address);
      pragma Import (C, Sqlite3_Config, "sqlite3_config");
   begin
      if Debug then
         Trace (Me, "sqlite3_config");
      end if;

      Sqlite3_Config (Config_Log, Func, Data);
   end Set_Config_Log;

   --------------------------
   -- Set_Config_Memstatus --
   --------------------------

   procedure Set_Config_Memstatus (Collect_Stats : Boolean) is
      procedure Sqlite3_Config
        (Option : Sqlite3_Config_Option;
         Status : Integer);
      pragma Import (C, Sqlite3_Config, "sqlite3_config");
   begin
      if Debug then
         Trace (Me, "sqlite3_config");
      end if;

      Sqlite3_Config (Config_Mem_Status, Boolean'Pos (Collect_Stats));
   end Set_Config_Memstatus;

   -----------------------------
   -- Set_Config_Multi_Thread --
   -----------------------------

   function Set_Config_Multi_Thread return Result_Codes is
   begin
      if Debug then
         Trace (Me, "sqlite3_config(MULTI_THREAD)");
      end if;

      return Sqlite3_Config (Config_Multi_Thread);
   end Set_Config_Multi_Thread;

   ---------------------------
   -- Set_Config_Serialized --
   ---------------------------

   procedure Set_Config_Serialized is
      procedure Sqlite3_Config (Option : Sqlite3_Config_Option);
      pragma Import (C, Sqlite3_Config, "sqlite3_config");
   begin
      if Debug then
         Trace (Me, "sqlite3_config");
      end if;

      Sqlite3_Config (Config_Serialized);
   end Set_Config_Serialized;

   ------------------------------
   -- Set_Config_Single_Thread --
   ------------------------------

   function Set_Config_Single_Thread return Result_Codes is
   begin
      if Debug then
         Trace (Me, "sqlite3_config (SINGLE_THREAD)");
      end if;

      return Sqlite3_Config (Config_Single_Thread);
   end Set_Config_Single_Thread;

   -----------------------
   -- Last_Insert_Rowid --
   -----------------------

   function Last_Insert_Rowid (DB : Database) return Long_Long_Integer is
      function Internal (DB : Database) return Long_Long_Integer;
      pragma Import (C, Internal, "sqlite3_last_insert_rowid");
   begin
      if Debug then
         Trace (Me, "sqlite3_last_insert_rowid");
      end if;
      return Internal (DB);
   end Last_Insert_Rowid;

   -----------
   -- Reset --
   -----------

   function Reset (Stmt : Statement) return Result_Codes is
      function Internal (Stmt : Statement) return Result_Codes;
      pragma Import (C, Internal, "sqlite3_reset");
   begin
      if Debug then
         Trace (Me, "sqlite3_reset");
      end if;
      return Internal (Stmt);
   end Reset;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Stmt : Statement) is
      procedure Internal (Stmt : Statement);
      pragma Import (C, Internal, "sqlite3_finalize");
   begin
      if Debug then
         Trace (Me, "sqlite3_finalize");
      end if;
      Internal (Stmt);
   end Finalize;

   --------------------
   -- Clear_Bindings --
   --------------------

   procedure Clear_Bindings (Stmt : Statement) is
      procedure Internal (Stmt : Statement);
      pragma Import (C, Internal, "sqlite3_clear_bindings");
   begin
      if Debug then
         Trace (Me, "sqlite3_clear_bindings");
      end if;
      Internal (Stmt);
   end Clear_Bindings;

   -----------------
   -- Bind_Double --
   -----------------

   procedure Bind_Double
     (Stmt : Statement; Index : Integer; Value : Interfaces.C.double)
   is
      procedure Internal
        (Stmt : Statement; Index : Integer; Value : Interfaces.C.double);
      pragma Import (C, Internal, "sqlite3_bind_double");
   begin
      if Debug then
         Trace (Me, "sqlite3_bind_double");
      end if;

      Internal (Stmt, Index, Value);
   end Bind_Double;

   --------------
   -- Bind_Int --
   --------------

   procedure Bind_Int
     (Stmt : Statement; Index : Integer; Value : Interfaces.C.int)
   is
      procedure Internal
        (Stmt : Statement; Index : Integer; Value : Interfaces.C.int);
      pragma Import (C, Internal, "sqlite3_bind_int");
   begin
      if Debug then
         Trace (Me, "sqlite3_bind_int");
      end if;

      Internal (Stmt, Index, Value);
   end Bind_Int;

   ----------------
   -- Bind_Int64 --
   ----------------

   procedure Bind_Int64
     (Stmt : Statement; Index : Integer; Value : Interfaces.C.long)
   is
      procedure Internal
        (Stmt : Statement; Index : Integer; Value : Interfaces.C.long);
      pragma Import (C, Internal, "sqlite3_bind_int64");
   begin
      if Debug then
         Trace (Me, "sqlite3_bind_int64");
      end if;

      Internal (Stmt, Index, Value);
   end Bind_Int64;

   ---------------
   -- Bind_Null --
   ---------------

   procedure Bind_Null (Stmt : Statement; Index : Integer) is
      procedure Internal (Stmt : Statement; Index : Integer);
      pragma Import (C, Internal, "sqlite3_bind_null");
   begin
      if Debug then
         Trace (Me, "sqlite3_bind_null");
      end if;

      Internal (Stmt, Index);
   end Bind_Null;

   ---------------
   -- Bind_Text --
   ---------------

   procedure Bind_Text
     (Stmt : Statement;
      Index : Integer;
      Str : System.Address;
      N_Bytes : Natural;
      Destructor : Text_Destructor := null)
   is
      procedure Internal
        (Stmt : Statement; Index : Integer;
         Str : System.Address; N_Bytes : Natural;
         Destructor : Text_Destructor := null);
      pragma Import (C, Internal, "sqlite3_bind_text");
   begin
      if Debug then
         Trace (Me, "sqlite3_bind_text");
      end if;

      Internal (Stmt, Index, Str, N_Bytes, Destructor);
   end Bind_Text;

   -----------------
   -- Backup_Init --
   -----------------

   function Backup_Init
     (Pdest : Database;      --  destination database handle
      Pdest_Name : String;   --  destination database name
      Psource : Database;    --  source database handle
      Psource_Name : String) --  source database name
      return Sqlite3_Backup
   is
      function Internal
        (Pdest : Database;
         PdestName : String;
         Psource : Database;
         PsourceName : String) return Sqlite3_Backup;
      pragma Import (C, Internal, "sqlite3_backup_init");
   begin
      if Debug then
         Trace (Me, "sqlite3_backup_init");
      end if;

      return Internal (Pdest, Pdest_Name & ASCII.NUL,
                       Psource, Psource_Name & ASCII.NUL);
   end Backup_Init;

end GNATCOLL.SQL.Sqlite.Gnade;
