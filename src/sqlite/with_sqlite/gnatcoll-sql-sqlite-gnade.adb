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

with Ada.Unchecked_Conversion;
with Interfaces.C;             use Interfaces.C;
with Interfaces.C.Strings;     use Interfaces.C.Strings;
with System.Address_Image;

package body GNATCOLL.SQL.Sqlite.Gnade is
   type Address_Array is array (Natural) of System.Address;
   pragma Convention (C, Address_Array);

   function To_Chars_Ptr is new Ada.Unchecked_Conversion
     (System.Address, chars_ptr);

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
      --  No need to free the result
      return Value (Internal (DB));
   end Error_Msg;

   -----------
   -- Close --
   -----------

   procedure Close (DB : Database) is
      function Internal_Close (DB : Database) return Result_Codes;
      pragma Import (C, Internal_Close, "sqlite3_close");

      function Next_Stmt
        (DB : Database; After : Statement := No_Statement)
         return Statement;
      pragma Import (C, Next_Stmt, "sqlite3_next_stmt");

      Stmt    : Statement;
      Ignored : Result_Codes;
      pragma Unreferenced (Ignored);
   begin
      if DB /= null then
         --  Finalize prepared statements
         loop
            Stmt := Next_Stmt (DB);
            exit when Stmt = No_Statement;
            Finalize (Stmt);
         end loop;

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
      Internal (Result.Values);
   end Free_Table;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Result : Result_Table;
      Row, Column : Natural) return Interfaces.C.Strings.chars_ptr
   is
      Val : Address_Array;
      for Val'Address use Result.Values;
      Index : constant Natural :=
        (Row + 1) * Result.Columns + Column;
   begin
      --  First row is for column names, so skip it
      return To_Chars_Ptr (Val (Index));
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
      Val : Address_Array;
      for Val'Address use Result.Values;
   begin
      return Interfaces.C.Strings.Value (To_Chars_Ptr (Val (Column)));
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

end GNATCOLL.SQL.Sqlite.Gnade;
