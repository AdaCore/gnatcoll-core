------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
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
--  Test the aggregated libraries project iteration

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Test_Assert;       use Test_Assert;

function Test return Integer is
   PT : Project_Tree;
   procedure Silent_Report (S : String) is null;

   procedure Internal (Include_Aggregate_Libraries : Boolean);

   procedure Internal (Include_Aggregate_Libraries : Boolean) is

      Files      : File_Array (1 .. 4);
      File_Index : Integer := 1;
      It         : Project_Iterator :=
        Start
          (PT.Root_Project,
           Include_Aggregate_Libraries => Include_Aggregate_Libraries);

   begin
      while Current (It) /= No_Project loop
         Files (File_Index) := Current (It).Project_Path;
         Next (It);
         File_Index := File_Index + 1;
      end loop;

      if Include_Aggregate_Libraries then
         Assert (File_Index = 5, "Check that we iterate over 4 projects");
         Sort (Files);

         Assert (Files (1).Display_Base_Name, "agg.gpr", "check 1st project");
         Assert (Files (2).Display_Base_Name, "lib1.gpr", "check 2nd project");
         Assert (Files (3).Display_Base_Name, "lib2.gpr", "check 3rd project");
         Assert (Files (4).Display_Base_Name, "root.gpr", "check 4th project");
      else
         Assert (File_Index = 4, "Check that we iterate over 3 projects");
         Sort (Files (1 .. 3));

         Assert (Files (1).Display_Base_Name, "lib1.gpr", "check 1st project");
         Assert (Files (2).Display_Base_Name, "lib2.gpr", "check 2rd project");
         Assert (Files (3).Display_Base_Name, "root.gpr", "check 3rd project");
      end if;
   end Internal;

begin
   PT.Load
     (Create ("tree/root.gpr"), Errors => Silent_Report'Unrestricted_Access);

   Internal (True);
   Internal (False);

   PT.Unload;

   return Test_Assert.Report;

end Test;
