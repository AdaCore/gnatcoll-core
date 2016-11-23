------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
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

with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

package body GNATCOLL.SQL_Fields is

   -----------------
   -- Json_To_SQL --
   -----------------

   function Json_To_SQL
     (Self : Formatter'Class; Value : String; Quote : Boolean) return String
   is
      pragma Unreferenced (Self, Quote);
   begin
      if Trim (Value, Ada.Strings.Both) = "" then
         return "null";
         --  Json null, not to be confused with SQL NULL.
      else
         return Value;
      end if;
   end Json_To_SQL;

   -----------------
   -- XML_To_SQL --
   -----------------

   function XML_To_SQL
     (Self : Formatter'Class; Value : String; Quote : Boolean) return String
   is
      pragma Unreferenced (Self, Quote);
   begin
      if Trim (Value, Ada.Strings.Both) = "" then
         return "<null/>";
         --  XML null, not to be confused with SQL NULL.
      else
         return Value;
      end if;
   end XML_To_SQL;

begin
   All_Field_Types.Append (Field_Type_Json'(null record));
   All_Field_Types.Append (Field_Type_XML'(null record));
end GNATCOLL.SQL_Fields;
