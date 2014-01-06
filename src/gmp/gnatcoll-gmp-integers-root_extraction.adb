------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2014, AdaCore                     --
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

with GNATCOLL.GMP.Lib;  use GNATCOLL.GMP.Lib;

package body GNATCOLL.GMP.Integers.Root_Extraction is

   ----------
   -- SQRT --
   ----------

   function SQRT (This : Big_Integer) return Big_Integer is
   begin
      return Result : Big_Integer do
         mpz_sqrt (Result.Value'Access, This.Value'Access);
      end return;
   end SQRT;

   --------------
   -- Get_SQRT --
   --------------

   procedure Get_SQRT (This : Big_Integer; Into : out Big_Integer) is
   begin
      mpz_sqrt (Into.Value'Access, This.Value'Access);
   end Get_SQRT;

   ------------------------
   -- Get_SQRT_Remainder --
   ------------------------

   procedure Get_SQRT_Remainder
     (This      : Big_Integer;
      Root      : out Big_Integer;
      Remainder : out Big_Integer)
   is
   begin
      mpz_sqrtrem (Root.Value'Access,
                   Remainder.Value'Access,
                   This.Value'Access);
   end Get_SQRT_Remainder;

   --------------
   -- Nth_Root --
   --------------

   function Nth_Root (This : Big_Integer;  N : Unsigned_Long)
      return Big_Integer
   is
      Dummy : Int;
      pragma Unreferenced (Dummy);
   begin
      return Result : Big_Integer do
         Dummy := mpz_root (Result.Value'Access, This.Value'Access, N);
      end return;
   end Nth_Root;

   ------------------
   -- Get_Nth_Root --
   ------------------

   procedure Get_Nth_Root
     (This  : Big_Integer;
      N     : Unsigned_Long;
      Into  : out Big_Integer;
      Exact : out Boolean)
   is
      Was_Exact : Int;
   begin
      Was_Exact := mpz_root (Into.Value'Access, This.Value'Access, N);
      Exact := Was_Exact /= 0;
   end Get_Nth_Root;

   ----------------------------
   -- Get_Nth_Root_Remainder --
   ----------------------------

   procedure Get_Nth_Root_Remainder
     (This      : Big_Integer;
      N         : Unsigned_Long;
      Root      : out Big_Integer;
      Remainder : out Big_Integer)
   is
   begin
      mpz_rootrem
         (Root.Value'Access, Remainder.Value'Access, This.Value'Access, N);
   end Get_Nth_Root_Remainder;

end GNATCOLL.GMP.Integers.Root_Extraction;
