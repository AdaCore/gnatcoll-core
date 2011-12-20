------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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
