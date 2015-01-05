------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2015, AdaCore                     --
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

package GNATCOLL.GMP.Integers.Misc is

   pragma Preelaborate;

   function As_Signed_Long (This : Big_Integer) return Long;
   --  If This fits into a signed long integer, returns the value of This.
   --  Otherwise returns the least significant part of This, with the same sign
   --  as This.

   pragma Inline (As_Signed_Long);

   function Fits_Signed_Long (This : Big_Integer) return Boolean;

   pragma Inline (Fits_Signed_Long);

   function Odd  (This : Big_Integer) return Boolean;
   function Even (This : Big_Integer) return Boolean;

   pragma Inline (Odd);
   pragma Inline (Even);

   procedure Swap (This, That : in out Big_Integer);

   pragma Inline (Swap);

end GNATCOLL.GMP.Integers.Misc;
