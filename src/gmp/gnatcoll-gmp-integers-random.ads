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
