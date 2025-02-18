------------------------------------------------------------------------------
--                                                                          --
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2025, AdaCore                          --
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

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;

   Cur_Dir : constant Virtual_File := Get_Current_Dir;
   File    : Virtual_File;

begin
   File := Create_From_Dir (Dir => Cur_Dir, Base_Name => "foo.txt");

   declare
      task type T;

      task body T is
      begin
         --  Starts multiple of ajustments and finalization simultaneously
         --  on all threads to check for race conditions.
         for Iter in 1 .. 1000000 loop
            pragma Warnings (Off);
            declare
               F : constant Virtual_File := File;
               --  Copying the file increments the virtual file reference
               --  counter (in the Adjust subprogram).
            begin
               null;
            end;
            pragma Warnings (On);
         end loop;
      end T;

      Tasks : array (1 .. 32) of T;
   begin
      null;
   end;

   A.Assert (True);
   return A.Report;

end Test;
