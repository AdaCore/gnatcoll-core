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

