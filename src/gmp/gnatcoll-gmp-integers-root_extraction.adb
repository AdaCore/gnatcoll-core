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
-- As a special exception, if other files instantiate generics  from --
-- this unit, or you link this  unit with other files to produce  an --
-- executable, this unit does not by itself cause the resulting exe- --
-- cutable  to be covered by  the  GNU General  Public License. This --
-- exception does not however  invalidate any other reasons why  the --
-- executable  file  might  be  covered  by  the  GNU General Public --
-- License.                                                          --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

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
