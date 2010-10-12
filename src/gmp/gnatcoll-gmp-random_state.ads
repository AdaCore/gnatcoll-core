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
