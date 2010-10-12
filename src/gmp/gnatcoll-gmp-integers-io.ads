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

with Interfaces.C_Streams;

package GNATCOLL.GMP.Integers.IO is

   pragma Preelaborate;

   procedure Put
     (This   : Big_Integer;
      Base   : Positive := 10;
      Stream : Interfaces.C_Streams.FILEs := Interfaces.C_Streams.stdout);
   --  Output This on Stream, as a string of digits in base Base. The base
   --  argument may vary from 2 to 62 or from -2 to -36.
   --
   --  For Base in the range 2..36, digits and lower-case letters are used; for
   --  -2..-36, digits and upper-case letters are used; for 37..62, digits,
   --  upper-case letters, and lower-case letters (in that significance order)
   --  are used.
   --
   --  Raises Failure if the entire sequence of digits is not written.

end GNATCOLL.GMP.Integers.IO;
