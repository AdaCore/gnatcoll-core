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

package body GNATCOLL.GMP.Integers.Random is

   ------------
   -- Number --
   ------------

   function Number (State : Generator; N : Big_Integer) return Big_Integer is
   begin
      return Result : Big_Integer do
         mpz_urandomm (Result.Value'Access,
                       As_gmp_randstate_t (State),
                       N.Value'Access);
      end return;
   end Number;

   ---------------------
   -- Generate_Number --
   ---------------------

   procedure Generate_Number
     (State : in out Generator;
      Into  : out Big_Integer;
      N     : Big_Integer)
   is
   begin
      mpz_urandomm (Into.Value'Access,
                    As_gmp_randstate_t (State),
                    N.Value'Access);
   end Generate_Number;

   -----------------
   -- Number_Bits --
   -----------------

   function Number_Bits (State : Generator;  N : Unsigned_Long)
      return Big_Integer
   is
   begin
      return Result : Big_Integer do
         mpz_urandomb (Result.Value'Access, As_gmp_randstate_t (State), N);
      end return;
   end Number_Bits;

   --------------------------
   -- Generate_Number_Bits --
   --------------------------

   procedure Generate_Number_Bits
     (State : in out Generator;
      Into  : out Big_Integer;
      N     : Unsigned_Long)
   is
   begin
      mpz_urandomb (Into.Value'Access,
                    As_gmp_randstate_t (State),
                    N);
   end Generate_Number_Bits;

end GNATCOLL.GMP.Integers.Random;
