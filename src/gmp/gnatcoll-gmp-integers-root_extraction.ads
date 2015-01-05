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

package GNATCOLL.GMP.Integers.Root_Extraction is

   pragma Preelaborate;

   function SQRT (This : Big_Integer) return Big_Integer;
   --  Returns the truncated integer part of the square root of This.

   pragma Inline (SQRT);

   procedure Get_SQRT (This : Big_Integer;  Into : out Big_Integer);
   --  Set Into to the truncated integer part of the square root of This

   pragma Inline (Get_SQRT);

   procedure Get_SQRT_Remainder
     (This      : Big_Integer;
      Root      : out Big_Integer;
      Remainder : out Big_Integer);
   --  Set Root to the truncated integer part of the square root of This. Set
   --  Remainder to the remainder This-Root*Root, which will be zero if This is
   --  a perfect square. If Root and Remainder are the same variable, the
   --  results are undefined.

   pragma Inline (Get_SQRT_Remainder);

   function Nth_Root (This : Big_Integer;  N : Unsigned_Long)
     return Big_Integer;
   --  Returns the truncated integer part of the Nth root of This.

   pragma Inline (Nth_Root);

   procedure Get_Nth_Root
     (This  : Big_Integer;
      N     : Unsigned_Long;
      Into  : out Big_Integer;
      Exact : out Boolean);
   --  Set Into to the truncated integer part of the Nth root of This. On
   --  return, Exact will be True if the computation was exact, i.e., if Into
   --  is This to the Nth power, and will be False otherwise.

   pragma Inline (Get_Nth_Root);

   procedure Get_Nth_Root_Remainder
     (This      : Big_Integer;
      N         : Unsigned_Long;
      Root      : out Big_Integer;
      Remainder : out Big_Integer);
   --  Set Root to the truncated integer part of the Nth root of This.  Set
   --  Remainder to the remainder, This-Root**N.

   pragma Inline (Get_Nth_Root_Remainder);

end GNATCOLL.GMP.Integers.Root_Extraction;
