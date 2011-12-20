------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

package GNATCOLL.Tribooleans is

   type Triboolean is (True, False, Indeterminate);
   --  This type is an extension to the basic boolean type.
   --  It provides a 3-state boolean logic, where the first two states are
   --  equivalent to the standard Boolean values.
   --  You can easily provide a renaming for the Indeterminate state by
   --  declaring a constant:
   --        Maybe : constant Triboolean := Indeterminate;

   function To_TriBoolean (Value : Boolean) return Triboolean;
   --  Convert a boolean into a TriBoolean

   function To_Boolean (Value : Triboolean) return Boolean;
   --  Convert to a boolean, with the following rules:
   --  if Value is True, the resulting boolean is true, otherwise the result
   --  is false.

   function "not" (Value : Triboolean) return Triboolean;
   --  Returns the negative of a triboolean:
   --     True          => False
   --     False         => True,
   --     Indeterminate => Indeterminate

   function "and" (Value1, Value2 : Triboolean) return Triboolean;
   function "and" (Value1 : Triboolean; Value2 : Boolean) return Triboolean;
   function "and" (Value1 : Boolean; Value2 : Triboolean) return Triboolean;
   --  Logical "and" between two tribooleans, with the following truth table:
   --             T | F | I
   --            ----------
   --         T | T | F | I
   --         F | F | F | F
   --         I | I | F | I

   function "or" (Value1, Value2 : Triboolean) return Triboolean;
   function "or" (Value1 : Triboolean; Value2 : Boolean) return Triboolean;
   function "or" (Value1 : Boolean; Value2 : Triboolean) return Triboolean;
   --  Logical "or" between two tribooleans, with the following truth table:
   --             T | F | I
   --            ----------
   --         T | T | T | T
   --         F | T | F | I
   --         I | T | I | I

   function "xor" (Value1, Value2 : Triboolean) return Triboolean;
   function "xor" (Value1 : Triboolean; Value2 : Boolean) return Triboolean;
   function "xor" (Value1 : Boolean; Value2 : Triboolean) return Triboolean;
   --  Logical "xor" between two tribooleans, with the following truth table:
   --             T | F | I
   --            ----------
   --         T | F | T | I
   --         F | T | F | I
   --         I | I | I | I

   function "=" (Value1 : Boolean; Value2 : Triboolean) return Boolean;
   function "=" (Value1 : Triboolean; Value2 : Boolean) return Boolean;
   --  Compare a triboolean and a boolean. If the triboolean is Indeterminate,
   --  the result is always False.

   function Equal (Value1 : Triboolean; Value2 : Boolean) return Triboolean;
   function Equal (Value1 : Boolean; Value2 : Triboolean) return Triboolean;
   function Equal (Value1 : Triboolean; Value2 : Triboolean) return Triboolean;
   --  Compare two tribooleans, with the following truth table:
   --             T | F | I
   --            ----------
   --         T | T | F | I
   --         F | F | T | I
   --         I | I | I | I
   --  Note that comparing two indeterminate values also returns indeterminate,
   --  as opposed to what "=" would return!

   function Image (Value : Triboolean) return String;
   function Value (Str : String) return Triboolean;
   --  Convert to and from a string. Any value that does not match the value
   --  that would be returned by Boolean'Image or Boolean'Value is declared as
   --  indeterminate.

   pragma Inline (To_TriBoolean);
   pragma Inline (To_Boolean);
   pragma Inline ("and");
   pragma Inline ("or");
   pragma Inline ("xor");
   pragma Inline ("=");
   pragma Inline (Equal);
end GNATCOLL.Tribooleans;
