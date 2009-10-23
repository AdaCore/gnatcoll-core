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
