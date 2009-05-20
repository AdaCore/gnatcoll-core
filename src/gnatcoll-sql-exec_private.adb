-----------------------------------------------------------------------
--                               G N A T C O L L                     --
--                                                                   --
--                 Copyright (C) 2005-2009, AdaCore                  --
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

--  This package declares various types and subprograms that must be overridden
--  by anyone wishing to add new backends to GNATCOLL.SQL.Exec.
--  Most users can ignore the contents of this package altogether, since none
--  of these types is intended to be visible in the user's code. They are
--  wrapped up in other types in GNATCOLL.SQL.Exec, which is the actual user
--  API.

with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with GNATCOLL.SQL.Exec;     use GNATCOLL.SQL.Exec;
with GNAT.Calendar.Time_IO; use GNAT.Calendar, GNAT.Calendar.Time_IO;
with System;

package body GNATCOLL.SQL.Exec_Private is

   -------------------
   -- Boolean_Value --
   -------------------

   function Boolean_Value
     (Self  : DBMS_Cursor;
      Field : Field_Index) return Boolean is
   begin
      return Boolean'Value (Value (DBMS_Cursor'Class (Self), Field));
   end Boolean_Value;

   -------------------
   -- Integer_Value --
   -------------------

   function Integer_Value
     (Self  : DBMS_Cursor;
      Field : Field_Index) return Integer is
   begin
      return Integer'Value (Value (DBMS_Cursor'Class (Self), Field));
   end Integer_Value;

   -----------------
   -- Float_Value --
   -----------------

   function Float_Value
     (Self  : DBMS_Cursor;
      Field : Field_Index) return Float is
   begin
      return Float'Value (Value (DBMS_Cursor'Class (Self), Field));
   end Float_Value;

   ----------------
   -- Time_Value --
   ----------------

   function Time_Value
     (Self  : DBMS_Cursor;
      Field : Field_Index) return Ada.Calendar.Time
   is
      Val : constant String := Value (DBMS_Cursor'Class (Self), Field);
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

end GNATCOLL.SQL.Exec_Private;
