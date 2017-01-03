------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2017, AdaCore                     --
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

package body GNATCOLL.GMP.Integers.Misc is

   --------------------
   -- As_Signed_Long --
   --------------------

   function As_Signed_Long (This : Big_Integer) return Long is
   begin
      return mpz_get_si (This.Value'Access);
   end As_Signed_Long;

   ----------------------
   -- Fits_Signed_Long --
   ----------------------

   function Fits_Signed_Long (This : Big_Integer) return Boolean is
   begin
      return mpz_fits_slong_p (This.Value'Access) /= 0;
   end Fits_Signed_Long;

   ---------
   -- Odd --
   ---------

   function Odd (This : Big_Integer) return Boolean is
   begin
      return mpz_odd_p (This.Value'Access) /= 0;
   end Odd;

   ----------
   -- Even --
   ----------

   function Even (This : Big_Integer) return Boolean is
   begin
      return mpz_even_p (This.Value'Access) /= 0;
   end Even;

   ----------
   -- Swap --
   ----------

   procedure Swap (This, That : in out Big_Integer) is
   begin
      mpz_swap (This.Value'Access, That.Value'Access);
   end Swap;

end GNATCOLL.GMP.Integers.Misc;
