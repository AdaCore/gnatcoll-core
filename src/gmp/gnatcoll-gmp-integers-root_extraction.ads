-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                      Copyright (C) 2009, AdaCore                  --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

package GNATCOLL.GMP.Integers.Root_Extraction is

   pragma Preelaborate;

   function SQRT (This : Big_Integer) return Big_Integer;
   --  Returns the truncated integer part of the square root of This.

   pragma Inline (SQRT);

   procedure Get_SQRT (This : in Big_Integer;  Into : out Big_Integer);
   --  Set Into to the truncated integer part of the square root of This

   pragma Inline (Get_SQRT);

   procedure Get_SQRT_Remainder
     (This      : in Big_Integer;
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
     (This  : in Big_Integer;
      N     : in Unsigned_Long;
      Into  : out Big_Integer;
      Exact : out Boolean);
   --  Set Into to the truncated integer part of the Nth root of This. On
   --  return, Exact will be True if the computation was exact, i.e., if Into
   --  is This to the Nth power, and will be False otherwise.

   pragma Inline (Get_Nth_Root);

   procedure Get_Nth_Root_Remainder
     (This      : in Big_Integer;
      N         : in Unsigned_Long;
      Root      : out Big_Integer;
      Remainder : out Big_Integer);
   --  Set Root to the truncated integer part of the Nth root of This.  Set
   --  Remainder to the remainder, This-Root**N.

   pragma Inline (Get_Nth_Root_Remainder);

end GNATCOLL.GMP.Integers.Root_Extraction;
