------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2008-2014, AdaCore                     --
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

package body GNATCOLL.Tribooleans is

   And_Truth_Table1 : constant array (Triboolean, Triboolean) of Triboolean :=
     (True          =>
        (True          => True,
         False         => False,
         Indeterminate => Indeterminate),
      False         =>
        (True          => False,
         False         => False,
         Indeterminate => False),
      Indeterminate         =>
        (True          => Indeterminate,
         False         => False,
         Indeterminate => Indeterminate));

   And_Truth_Table2 : constant array (Triboolean, Boolean) of Triboolean :=
     (True          => (True => True,          False => False),
      False         => (True => False,         False => False),
      Indeterminate => (True => Indeterminate, False => False));

   Or_Truth_Table1 : constant array (Triboolean, Triboolean) of Triboolean :=
     (True          =>
        (True          => True,
         False         => True,
         Indeterminate => True),
      False         =>
        (True          => True,
         False         => False,
         Indeterminate => Indeterminate),
      Indeterminate         =>
        (True          => True,
         False         => Indeterminate,
         Indeterminate => Indeterminate));

   Or_Truth_Table2 : constant array (Triboolean, Boolean) of Triboolean :=
     (True          => (True => True,          False => True),
      False         => (True => True,          False => False),
      Indeterminate => (True => True,          False => Indeterminate));

   Xor_Truth_Table1 : constant array (Triboolean, Triboolean) of Triboolean :=
     (True          =>
        (True          => False,
         False         => True,
         Indeterminate => Indeterminate),
      False         =>
        (True          => True,
         False         => False,
         Indeterminate => Indeterminate),
      Indeterminate         =>
        (True          => Indeterminate,
         False         => Indeterminate,
         Indeterminate => Indeterminate));

   Xor_Truth_Table2 : constant array (Triboolean, Boolean) of Triboolean :=
     (True          => (True => False,         False => True),
      False         => (True => True,          False => False),
      Indeterminate => (True => Indeterminate, False => Indeterminate));

   Eq_Truth_Table1 : constant array (Triboolean, Triboolean) of Triboolean :=
     (True          =>
        (True          => True,
         False         => False,
         Indeterminate => Indeterminate),
      False         =>
        (True          => False,
         False         => True,
         Indeterminate => Indeterminate),
      Indeterminate         =>
        (True          => Indeterminate,
         False         => Indeterminate,
         Indeterminate => Indeterminate));

   Eq_Truth_Table2 : constant array (Triboolean, Boolean) of Triboolean :=
     (True          => (True => True,          False => False),
      False         => (True => False,         False => True),
      Indeterminate => (True => Indeterminate, False => Indeterminate));

   -------------------
   -- To_TriBoolean --
   -------------------

   function To_TriBoolean (Value : Boolean) return Triboolean is
   begin
      if Value then
         return Triboolean'(True);
      else
         return Triboolean'(False);
      end if;
   end To_TriBoolean;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (Value : Triboolean) return Boolean is
   begin
      return Value = Triboolean'(True);
   end To_Boolean;

   -----------
   -- "not" --
   -----------

   function "not" (Value : Triboolean) return Triboolean is
   begin
      case Value is
         when Triboolean'(True)          => return Triboolean'(False);
         when Triboolean'(False)         => return Triboolean'(True);
         when Triboolean'(Indeterminate) => return Triboolean'(Indeterminate);
      end case;
   end "not";

   -----------
   -- "and" --
   -----------

   function "and" (Value1, Value2 : Triboolean) return Triboolean is
   begin
      return And_Truth_Table1 (Value1, Value2);
   end "and";

   function "and" (Value1 : Triboolean; Value2 : Boolean) return Triboolean is
   begin
      return And_Truth_Table2 (Value1, Value2);
   end "and";

   function "and" (Value1 : Boolean; Value2 : Triboolean) return Triboolean is
   begin
      return And_Truth_Table2 (Value2, Value1);
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Value1, Value2 : Triboolean) return Triboolean is
   begin
      return Or_Truth_Table1 (Value1, Value2);
   end "or";

   function "or" (Value1 : Triboolean; Value2 : Boolean) return Triboolean is
   begin
      return Or_Truth_Table2 (Value1, Value2);
   end "or";

   function "or" (Value1 : Boolean; Value2 : Triboolean) return Triboolean is
   begin
      return Or_Truth_Table2 (Value2, Value1);
   end "or";

   -----------
   -- "xor" --
   -----------

   function "xor" (Value1, Value2 : Triboolean) return Triboolean is
   begin
      return Xor_Truth_Table1 (Value1, Value2);
   end "xor";

   function "xor" (Value1 : Triboolean; Value2 : Boolean) return Triboolean is
   begin
      return Xor_Truth_Table2 (Value1, Value2);
   end "xor";

   function "xor" (Value1 : Boolean; Value2 : Triboolean) return Triboolean is
   begin
      return Xor_Truth_Table2 (Value2, Value1);
   end "xor";

   ---------
   -- "=" --
   ---------

   function "=" (Value1 : Boolean; Value2 : Triboolean) return Boolean is
   begin
      if Value1 then
         return Value2 = Triboolean'(True);
      else
         return Value2 = Triboolean'(False);
      end if;
   end "=";

   function "=" (Value1 : Triboolean; Value2 : Boolean) return Boolean is
   begin
      if Value2 then
         return Value1 = Triboolean'(True);
      else
         return Value1 = Triboolean'(False);
      end if;
   end "=";

   -----------
   -- Equal --
   -----------

   function Equal (Value1 : Triboolean; Value2 : Boolean) return Triboolean is
   begin
      return Eq_Truth_Table2 (Value1, Value2);
   end Equal;

   -----------
   -- Equal --
   -----------

   function Equal (Value1 : Boolean; Value2 : Triboolean) return Triboolean is
   begin
      return Eq_Truth_Table2 (Value2, Value1);
   end Equal;

   function Equal
     (Value1 : Triboolean; Value2 : Triboolean) return Triboolean is
   begin
      return Eq_Truth_Table1 (Value1, Value2);
   end Equal;

   -----------
   -- Image --
   -----------

   function Image (Value : Triboolean) return String is
   begin
      case Value is
         when True          => return "TRUE";
         when False         => return "FALSE";
         when Indeterminate => return "INDETERMINATE";
      end case;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Str : String) return Triboolean is
   begin
      if Boolean'Value (Str) then
         return True;
      else
         return False;
      end if;
   exception
      when Constraint_Error =>
         return Indeterminate;
   end Value;

end GNATCOLL.Tribooleans;
