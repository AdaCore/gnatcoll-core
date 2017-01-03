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

with GNATCOLL.GMP.Random_State;  use GNATCOLL.GMP.Random_State;

package GNATCOLL.GMP.Integers.Random is

   pragma Preelaborate;

   function Number (State : Generator; N : Big_Integer) return Big_Integer;
   --  Generate a uniform random integer in the range 0 to N-1, inclusive

   procedure Generate_Number
     (State : in out Generator;
      Into  : out Big_Integer;
      N     : Big_Integer);
   --  Generate a uniform random integer in the range 0 to N-1, inclusive

   function Number_Bits (State : Generator; N : Unsigned_Long)
      return Big_Integer;
   --  Generate a uniformly distributed random integer in the range 0 to
   --  2^N-1, inclusive

   procedure Generate_Number_Bits
     (State : in out Generator;
      Into  : out Big_Integer;
      N     : Unsigned_Long);
   --  Generate a uniformly distributed random integer in the range 0 to
   --  2^N-1, inclusive

end GNATCOLL.GMP.Integers.Random;
