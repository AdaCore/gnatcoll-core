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

package body GNATCOLL.GMP.Random_State is

   use GNATCOLL.GMP.Lib;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : out Generator) is
   begin
      gmp_randinit_default (This.G'Access);
   end Initialize;

   ---------------------------------
   -- Initialize_Mersenne_Twister --
   ---------------------------------

   procedure Initialize_Mersenne_Twister (This : out Generator) is
   begin
      gmp_randinit_mt (This.G'Access);
   end Initialize_Mersenne_Twister;

   ---------
   -- Set --
   ---------

   procedure Set
     (This : out Generator;
      To   : Generator)
   is
   begin
      gmp_randinit_set (This.G'Access, To.G'Access);
   end Set;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Generator) is
   begin
      gmp_randclear (This.G'Access);
   end Clear;

   --------------
   -- Set_Seed --
   --------------

   procedure Set_Seed
     (This : in out Generator;
      Seed : Big_Integer)
   is
   begin
      gmp_randseed (This.G'Access, As_mpz_t (Seed));
   end Set_Seed;

   --------------
   -- Set_Seed --
   --------------

   procedure Set_Seed
     (This : in out Generator;
      Seed : Unsigned_Long)
   is
   begin
      gmp_randseed_ui (This.G'Access, Seed);
   end Set_Seed;

   -------------------------
   -- Uniform_Number_Bits --
   -------------------------

   function Number_Bits
     (This : Generator;
      N    : Unsigned_Long)
      return Long
   is
      That : Generator renames This'Unrestricted_Access.all;
   begin
      return gmp_urandomb_ui (That.G'Access, N);
   end Number_Bits;

   --------------------
   -- Uniform_Number --
   --------------------

   function Number
     (This : Generator;
      N    : Unsigned_Long)
      return Long
   is
      That : Generator renames This'Unrestricted_Access.all;
   begin
      return gmp_urandomm_ui (That.G'Access, N);
   end Number;

   ------------------------
   -- As_gmp_randstate_t --
   ------------------------

   function As_gmp_randstate_t (This : Generator)
      return access constant GNATCOLL.GMP.Lib.gmp_randstate_t
   is
   begin
      return This.G'Unchecked_Access;
   end As_gmp_randstate_t;

end GNATCOLL.GMP.Random_State;
