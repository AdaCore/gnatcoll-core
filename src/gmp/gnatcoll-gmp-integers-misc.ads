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

package GNATCOLL.GMP.Integers.Misc is

   pragma Preelaborate;

   function As_Signed_Long (This : Big_Integer) return Long;
   --  If This fits into a signed long integer, returns the value of This.
   --  Otherwise returns the least significant part of This, with the same sign
   --  as This.

   pragma Inline (As_Signed_Long);

   function Fits_Signed_Long (This : Big_Integer) return Boolean;

   pragma Inline (Fits_Signed_Long);

   function Odd  (This : Big_Integer) return Boolean;
   function Even (This : Big_Integer) return Boolean;

   pragma Inline (Odd);
   pragma Inline (Even);

   procedure Swap (This, That : in out Big_Integer);

   pragma Inline (Swap);

end GNATCOLL.GMP.Integers.Misc;
