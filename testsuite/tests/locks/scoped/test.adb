------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with Test_Assert;

with GNATCOLL.Locks; use GNATCOLL.Locks;

function Test return Integer is
   package A renames Test_Assert;

   M : aliased Mutual_Exclusion;
   Counter : Natural := 0;

   procedure Increment;

   procedure Increment is
      Dummy : Scoped_Lock (M'Access);
   begin
      Counter := Counter + 1;
   end Increment;

   task type Worker is
      entry Stop;
   end Worker;

   task body Worker is
   begin
      for I in 1 .. 1_000 loop
         Increment;
      end loop;

      accept Stop;
   end Worker;

   Pool : array (1 .. 10) of Worker;
begin
   for T of Pool loop
      T.Stop;
   end loop;

   A.Assert (Counter = 10_000, "Atomic increment");

   return A.Report;
end Test;
