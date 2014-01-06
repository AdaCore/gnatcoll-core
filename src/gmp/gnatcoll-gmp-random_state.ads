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

with GNATCOLL.GMP.Lib;
with GNATCOLL.GMP.Integers;  use GNATCOLL.GMP.Integers;

package GNATCOLL.GMP.Random_State is

   pragma Preelaborate;

   type Generator is tagged limited private;

   --  State Initialization

   procedure Initialize (This : out Generator);
   --  Initialize This with a default algorithm. The choice will be a
   --  compromise between speed and randomness, and is recommended for
   --  applications with no special requirements. Currently the choice is
   --  Initialize_Mersenne_Twister.

   procedure Initialize_Mersenne_Twister (This : out Generator);
   --  Initialize This for a Mersenne Twister algorithm.  This algorithm
   --  is fast and has good randomness properties.

   procedure Set
     (This : out Generator;
      To   : Generator);
   --  Initialize This with a copy of the algorithm and state from To.

   procedure Clear (This : in out Generator);
   --  Free all memory occupied by This.

   --  State Seeding

   procedure Set_Seed
     (This : in out Generator;
      Seed : Big_Integer);
   --  Set an initial seed value into This.

   procedure Set_Seed
     (This : in out Generator;
      Seed : Unsigned_Long);
   --  Set an initial seed value into This.

   --  Misc

   function Number_Bits (This : Generator; N : Unsigned_Long) return Long;
   --  Return a uniformly distributed random number of N bits, ie. in the
   --  range 0 to 2^N-1 inclusive.  N must be less than or equal to the
   --  number of bits in an `unsigned long'.

   function Number (This : Generator; N : Unsigned_Long) return Long;
   --  Return a uniformly distributed random number in the range 0 to
   --  N-1, inclusive.

   function As_gmp_randstate_t (This : Generator)
      return access constant GNATCOLL.GMP.Lib.gmp_randstate_t;
   --  This function is useful for passing Generator values to routines from
   --  gmplib that do not have an Ada binding defined by this package. In that
   --  case the user will define the binding but will not be able to pass
   --  Generator objects as parameters to their routine. This function provides
   --  the required visibility to the internal gmp_randstate_t component of a
   --  Generator object. The type should really be access-to-variable since the
   --  routines will be modifying the state of the referenced Generator object,
   --  but that will not support use within functions. However it doesn't
   --  really matter since the underlying C routines just use it as an address
   --  anyway. Alternatively we could have two of these functions, one with a
   --  read-write view.

private

   type Generator is tagged limited
      record
         G : aliased GNATCOLL.GMP.Lib.gmp_randstate_t;
      end record;

end GNATCOLL.GMP.Random_State;
