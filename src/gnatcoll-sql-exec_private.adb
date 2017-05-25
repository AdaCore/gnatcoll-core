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

--  This package declares various types and subprograms that must be overridden
--  by anyone wishing to add new backends to GNATCOLL.SQL.Exec.
--  Most users can ignore the contents of this package altogether, since none
--  of these types is intended to be visible in the user's code. They are
--  wrapped up in other types in GNATCOLL.SQL.Exec, which is the actual user
--  API.

with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with GNATCOLL.Utils;        use GNATCOLL.Utils;

package body GNATCOLL.SQL.Exec_Private is

   function Class_Value
     (Self : DBMS_Forward_Cursor'Class; Field : Field_Index) return String
   is (Value (Self, Field)) with Inline_Always;

   generic
      type Base_Type is digits <>;
   function Any_Float_Value (S : String) return Base_Type;

   -----------
   -- Value --
   -----------

   function Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return String is
   begin
      return Value (C_Value (DBMS_Forward_Cursor'Class (Self), Field));
   end Value;

   ---------------------
   -- Unbounded_Value --
   ---------------------

   function Unbounded_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Unbounded_String is
   begin
      return To_Unbounded_String (Self.Class_Value (Field));
   end Unbounded_Value;

   -------------------
   -- XString_Value --
   -------------------

   function XString_Value
     (Self : DBMS_Forward_Cursor; Field : Field_Index) return XString is
   begin
      return To_XString (Self.Class_Value (Field));
   end XString_Value;

   -------------------
   -- Boolean_Value --
   -------------------

   function Boolean_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Boolean is
   begin
      return Boolean'Value (Self.Class_Value (Field));
   end Boolean_Value;

   -------------------
   -- Integer_Value --
   -------------------

   function Integer_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Integer is
   begin
      return Integer'Value (Self.Class_Value (Field));
   end Integer_Value;

   ------------------
   -- Bigint_Value --
   ------------------

   function Bigint_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Long_Long_Integer is
   begin
      return Long_Long_Integer'Value (Self.Class_Value (Field));
   end Bigint_Value;

   ---------------------
   -- Any_Float_Value --
   ---------------------

   function Any_Float_Value (S : String) return Base_Type is
      pragma Warnings (Off, "*is not modified, could be declared constant");
      Zero : Base_Type := 0.0;
      pragma Warnings (On, "*is not modified, could be declared constant");
   begin
      if S = "NaN" then
         return 0.0 / Zero;
      elsif S = "-Infinity" then
         return -1.0 / Zero;
      elsif S = "Infinity" then
         return 1.0 / Zero;
      else
         return Base_Type'Value (S);
      end if;
   end Any_Float_Value;

   -----------------
   -- Float_Value --
   -----------------

   function Float_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Float
   is
      function To_Float is new Any_Float_Value (Float);
   begin
      return To_Float (Self.Class_Value (Field));
   end Float_Value;

   ----------------------
   -- Long_Float_Value --
   ----------------------

   function Long_Float_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Long_Float
   is
      function To_Float is new Any_Float_Value (Long_Float);
   begin
      return To_Float (Self.Class_Value (Field));
   end Long_Float_Value;

   -----------------
   -- Money_Value --
   -----------------

   function Money_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return T_Money
   is
   begin
      return T_Money'Value (Self.Class_Value (Field));
   end Money_Value;

   ----------------
   -- Time_Value --
   ----------------

   function Time_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Ada.Calendar.Time
   is
      Val : constant String := Self.Class_Value (Field);
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

end GNATCOLL.SQL.Exec_Private;
