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

--  This program prints the first 50 "square triangular numbers", i.e., those
--  that are both perfect squares and also are the sum of consecutive integers
--  starting at one. The time required to calculate these numbers is also
--  displayed.

with GNAT.IO;                   use GNAT.IO;
with GNATCOLL.GMP.Integers;     use GNATCOLL.GMP.Integers;
with GNATCOLL.GMP.Integers.IO;  use GNATCOLL.GMP.Integers.IO;
with Ada.Calendar;              use Ada.Calendar;

procedure Square_Triangular_Numbers is

   Values : array (1 .. 50) of Big_Integer;

   Start   : Time;
   Elapsed : Duration;

begin
   Start := Clock;

   Set (Values (1), To => 1);   --  the first square triangular number
   Set (Values (2), To => 36);  --  the second square triangular number

   for N in 3 .. Values'Last loop
      Set (Values (N), To => (34 * Values (N - 1)) - Values (N - 2) + 2);
   end loop;

   Elapsed := Clock - Start;

   for K in Values'Range loop
      Put (Values (K));
      New_Line;
   end loop;

   Put_Line ("Computed" & Values'Last'Img &
             " values in" & Elapsed'Img &
             " seconds");
end Square_Triangular_Numbers;

