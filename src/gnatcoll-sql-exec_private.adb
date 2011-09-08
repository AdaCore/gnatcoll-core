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

--  This package declares various types and subprograms that must be overridden
--  by anyone wishing to add new backends to GNATCOLL.SQL.Exec.
--  Most users can ignore the contents of this package altogether, since none
--  of these types is intended to be visible in the user's code. They are
--  wrapped up in other types in GNATCOLL.SQL.Exec, which is the actual user
--  API.

with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Utils;        use GNATCOLL.Utils;

package body GNATCOLL.SQL.Exec_Private is

   -----------
   -- Value --
   -----------

   function Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return String is
   begin
      return Value (C_Value (DBMS_Forward_Cursor'Class (Self), Field));
   end Value;

   -------------------
   -- Boolean_Value --
   -------------------

   function Boolean_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Boolean is
   begin
      return Boolean'Value (Value (DBMS_Forward_Cursor'Class (Self), Field));
   end Boolean_Value;

   -------------------
   -- Integer_Value --
   -------------------

   function Integer_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Integer is
   begin
      return Integer'Value (Value (DBMS_Forward_Cursor'Class (Self), Field));
   end Integer_Value;

   -----------------
   -- Float_Value --
   -----------------

   function Float_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Float is
   begin
      return Float'Value (Value (DBMS_Forward_Cursor'Class (Self), Field));
   end Float_Value;

   ----------------
   -- Time_Value --
   ----------------

   function Time_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Ada.Calendar.Time
   is
      Val : constant String := Value (DBMS_Forward_Cursor'Class (Self), Field);
   begin
      if Val = "" then
         return No_Time;
      else
         --  Workaround bug(?) in GNAT.Calendar.Time_IO: if there is no time,
         --  set one to avoid daylight saving time issues

         if Ada.Strings.Fixed.Index (Val, ":") < Val'First then
            return GNATCOLL.Utils.Time_Value (Val & " 12:00:00");
         else
            return GNATCOLL.Utils.Time_Value (Val);
         end if;
      end if;
   end Time_Value;

   ----------------------------
   -- Generic_Direct_Cursors --
   ----------------------------

   package body Generic_Direct_Cursors is

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Result_Table, Result_Table_Access);

      ---------------
      -- Error_Msg --
      ---------------

      overriding function Error_Msg (Self : Direct) return String is
      begin
         return Error_Msg (Self.Cursor.all);
      end Error_Msg;

      ------------
      -- Status --
      ------------

      overriding function Status (Self : Direct) return String is
      begin
         return Status (Self.Cursor.all);
      end Status;

      ----------------
      -- Is_Success --
      ----------------

      overriding function Is_Success (Self : Direct) return Boolean is
      begin
         return Is_Success (Self.Cursor.all);
      end Is_Success;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Result : in out Local_Forward) is
      begin
         if Result.Table /= null then
            for R in Result.Table'First
              .. Result.Processed_Rows * Result.Columns
            loop
               Free (Result.Table (R));
            end loop;

            Unchecked_Free (Result.Table);
            Result.Table := null;
         end if;
      end Finalize;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Result : in out Direct) is
         procedure Unchecked_Free is new Ada.Unchecked_Deallocation
           (Local_Forward'Class, Local_Forward_Access);
      begin
         if Result.Cursor /= null then
            Finalize (Result.Cursor.all);
            Unchecked_Free (Result.Cursor);
         end if;
      end Finalize;

      --------------------
      -- Processed_Rows --
      --------------------

      overriding function Processed_Rows (Self : Direct) return Natural is
      begin
         return Self.Cursor.Processed_Rows;
      end Processed_Rows;

      -----------
      -- Value --
      -----------

      overriding function Value
        (Self  : Direct; Field : Field_Index) return String
      is
      begin
         return Value (Self.Cursor.all, Field);
      end Value;

      -----------
      -- Value --
      -----------

      overriding function Value
        (Self  : Local_Forward; Field : Field_Index) return String
      is
         Str : constant GNAT.Strings.String_Access := Self.Table
           (Self.Table'First + Self.Current * Self.Columns + Natural (Field));
      begin
         if Str = null then
            return "";
         else
            return Str.all;
         end if;
      end Value;

      -------------
      -- C_Value --
      -------------

      overriding function C_Value
        (Self  : Direct; Field : Field_Index) return chars_ptr
      is
      begin
         return C_Value (Self.Cursor.all, Field);
      end C_Value;

      -------------------
      -- Boolean_Value --
      -------------------

      overriding function Boolean_Value
         (Self : Direct; Field : Field_Index) return Boolean
      is
      begin
         return Boolean_Value (Self.Cursor.all, Field);
      end Boolean_Value;

      -------------
      -- Is_Null --
      -------------

      overriding function Is_Null
        (Self  : Local_Forward; Field : Field_Index) return Boolean is
      begin
         return Self.Table
           (Self.Table'First + Self.Columns * Self.Current + Natural (Field)) =
            null;
      end Is_Null;

      -------------
      -- Is_Null --
      -------------

      overriding function Is_Null
        (Self  : Direct; Field : Field_Index) return Boolean is
      begin
         return Is_Null (Self.Cursor.all, Field);
      end Is_Null;

      -------------
      -- Last_Id --
      -------------

      overriding function Last_Id
        (Self       : Direct;
         Connection : access Database_Connection_Record'Class;
         Field      : SQL_Field_Integer) return Integer is
      begin
         return Last_Id (Self.Cursor.all, Connection, Field);
      end Last_Id;

      -----------------
      -- Field_Count --
      -----------------

      overriding function Field_Count (Self : Direct) return Field_Index is
      begin
         return Field_Index (Self.Cursor.Columns);
      end Field_Count;

      ----------------
      -- Field_Name --
      ----------------

      overriding function Field_Name
        (Self  : Direct; Field : Field_Index) return String is
      begin
         return Field_Name (Self.Cursor.all, Field);
      end Field_Name;

      -------------
      -- Has_Row --
      -------------

      overriding function Has_Row (Self : Direct) return Boolean is
      begin
         return Self.Cursor.Current < Self.Cursor.Processed_Rows;
      end Has_Row;

      ----------
      -- Next --
      ----------

      overriding procedure Next (Self : in out Direct) is
      begin
         Self.Cursor.Current := Self.Cursor.Current + 1;
      end Next;

      -----------
      -- First --
      -----------

      overriding procedure First (Self : in out Direct) is
      begin
         Self.Cursor.Current := 0;
      end First;

      ----------
      -- Last --
      ----------

      overriding procedure Last (Self : in out Direct) is
      begin
         Self.Cursor.Current := Self.Cursor.Processed_Rows - 1;
      end Last;

      --------------
      -- Absolute --
      --------------

      overriding procedure Absolute (Self : in out Direct; Row : Positive) is
      begin
         Self.Cursor.Current := Row - 1;
      end Absolute;

      -------------
      -- Current --
      -------------

      overriding function Current (Self : Direct) return Positive is
      begin
         return Self.Cursor.Current + 1;
      end Current;

      --------------
      -- Relative --
      --------------

      overriding procedure Relative (Self : in out Direct; Step : Integer) is
      begin
         Self.Cursor.Current := Integer'Min
           (Integer'Max (0, Self.Cursor.Current + Step),
            Self.Cursor.Processed_Rows - 1);
      end Relative;

      ----------------
      -- Get_Cursor --
      ----------------

      function Get_Cursor (Self : Direct) return Forward_Access is
      begin
         return Forward_Access (Self.Cursor);
      end Get_Cursor;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Self : access Direct; From : access Forward)
      is
         Cols : Natural;
      begin
         --  Initialize size is 20 rows (this is a random value for now,
         --  choice between wasting memory and doing too many allocs).

         Cols := Natural (Field_Count (Forward'Class (From.all)));

         Self.Cursor := new Local_Forward'
           (From.all with
            Table   => new Result_Table (1 .. 20 * Cols),
            Columns => Cols,
            Current => 0);

         declare
            pragma Suppress (All_Checks);
            Tmp    : Result_Table_Access;
            Index  : Natural := Self.Cursor.Table'First;
            Cval   : chars_ptr;
         begin
            while Has_Row (Self.Cursor.all) loop
               if Index + Self.Cursor.Columns > Self.Cursor.Table'Last then
                  Tmp := Self.Cursor.Table;

                  --  Multiply size by 2, and make sure there is at least
                  --  enough space for the number of columns
                  --  ??? Can we use some sort of Realloc here, would be more
                  --  efficient since it might save some copies
                  Self.Cursor.Table := new Result_Table
                    (1 .. 2 * Tmp'Length + Self.Cursor.Columns);
                  Self.Cursor.Table (Tmp'Range) := Tmp.all;
                  Unchecked_Free (Tmp);
               end if;

               for C in 0 .. Self.Cursor.Columns - 1 loop
                  Cval := C_Value (Self.Cursor.all, Field_Index (C));
                  if Cval /= Null_Ptr then
                     --  For efficiency reasons, we do not use
                     --  Interfaces.C.Strings.Value (and new String), since
                     --  that would first copy the string on the secondary
                     --  stack, then to our final string.
                     --  This function is still slightly slower than
                     --  sqlite3_get_table

                     declare
                        Length : constant Natural := Integer (Strlen (Cval));
                        subtype Arr is String (1 .. Length);
                        type Arr_Access is access all Arr;
                        function Unchecked_Convert is new
                          Ada.Unchecked_Conversion (chars_ptr, Arr_Access);
                        Tmp2   : constant GNAT.Strings.String_Access :=
                          new String (1 .. Length);
                     begin
                        Tmp2.all := Unchecked_Convert (Cval).all;
                        Self.Cursor.Table (Index) := Tmp2;
                     end;
                  end if;

                  Index := Index + 1;
               end loop;

               Next (Self.Cursor.all);
            end loop;
         end;
      end Initialize;

   end Generic_Direct_Cursors;

end GNATCOLL.SQL.Exec_Private;
