-------------------------------------------------------------------------------
--                               G N A T C O L L                     --
--                                                                   --
--                 Copyright (C) 2009, AdaCore                       --
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
-------------------------------------------------------------------------------

with System.Address_To_Access_Conversions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with System;

package body GNATCOLL.SQL.Sqlite.Gnade is
   pragma Linker_Options ("-lsqlite3");

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
   begin
      Status := Internal
        (Filename & ASCII.NUL, DB2'Unchecked_Access, Integer (Flags));

      if Status = SQLITE_OK then
         DB := DB2;
      else
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
      function Internal (DB : Database) return Result_Codes;
      pragma Import (C, Internal, "sqlite3_close");
      Ignored : Result_Codes;
      pragma Unreferenced (Ignored);
   begin
      --  Finalize prepared statements
--            sqlite3_stmt *pStmt;
--      while( (pStmt = sqlite3_next_stmt(db, 0))!=0 ){
--          sqlite3_finalize(pStmt);
--      }

      Ignored := Internal (DB);
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

end GNATCOLL.SQL.Sqlite.Gnade;
