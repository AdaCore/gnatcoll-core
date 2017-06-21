------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

with Ada.Strings;

package body GNATCOLL.SQL_Fields is

   --------------
   -- Identity --
   --------------

   function Identity (Val : Long_Long_Float) return Long_Long_Float is
      --  Support for Nan and Inf
      pragma Validity_Checks (Off);
   begin
      return Val;
   end Identity;

   ---------------------
   -- Maps_Long_Float --
   ---------------------

   function Maps_Long_Float
      (Schema : String; Value : out Null_Record) return Boolean
   is
      pragma Unreferenced (Value);
   begin
      if Schema = "double precision"
         or else Schema = "float"
         or else Schema = "numeric"
      then
         return True;

      elsif Schema'Length >= 7
         and then Schema (Schema'First .. Schema'First + 6) = "numeric"
      then
         --  Check the scale
         for Comma in reverse Schema'Range loop
            if Schema (Comma) = ',' then
               return Schema (Comma + 1 .. Schema'Last - 1) /= "0";
            elsif Schema (Comma) = '(' then
               --  No scale specified, but has a precision. This is
               --  an integer.
               return False;
            end if;
         end loop;

         --  With no argument, this means maximum precision and
         --  scale. It should be mapped to a long float.
         return True;
      end if;

      return False;
   end Maps_Long_Float;

   -----------------
   -- Json_To_SQL --
   -----------------

   function Json_To_SQL
     (Self : Formatter'Class; Value : XString; Quote : Boolean) return String
   is
   begin
      if Value.Trim (Ada.Strings.Both) = "" then
         return "null";
         --  Json null, not to be confused with SQL NULL.
      else
         return String_Image (Self, Value.To_String, Quote);
      end if;
   end Json_To_SQL;

   -----------------
   -- XML_To_SQL --
   -----------------

   function XML_To_SQL
     (Self : Formatter'Class; Value : XString; Quote : Boolean) return String
   is
      pragma Unreferenced (Self, Quote);
   begin
      if Value.Trim (Ada.Strings.Both) = "" then
         return "<null/>";
         --  XML null, not to be confused with SQL NULL.
      else
         return Value.To_String;
      end if;
   end XML_To_SQL;

end GNATCOLL.SQL_Fields;
