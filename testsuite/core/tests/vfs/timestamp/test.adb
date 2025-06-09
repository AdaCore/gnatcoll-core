------------------------------------------------------------------------------
--                                                                          --
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2025, AdaCore                     --
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

with Ada.Directories;
with Ada.Calendar; use Ada.Calendar;

with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;
   F : Virtual_File;
   W : Writable_File;

   Cur_Dir : constant Virtual_File := Get_Current_Dir;

   Filestamp, Filestamp_VFS : Ada.Calendar.Time;
begin

   F := Create_From_Dir (Dir => Cur_Dir, Base_Name => "foo.txt");
   W := Write_File (F);
   Write (W, "first word ");
   Close (W);

   Filestamp_VFS := File_Time_Stamp (Create_From_Base (+"foo.txt"));
   Filestamp := Ada.Directories.Modification_Time ("foo.txt");

   A.Assert
     (Ada.Calendar.Year (Filestamp_VFS) = Ada.Calendar.Year (Filestamp),
      "Both filestamp should have the same year");
   A.Assert
     (Ada.Calendar.Month (Filestamp_VFS) = Ada.Calendar.Month (Filestamp),
      "Both filestamp should have the same month");
   A.Assert
     (Ada.Calendar.Day (Filestamp_VFS) = Ada.Calendar.Day (Filestamp),
      "Both filestamp should have the same day");

   --  ??? The duration may have one second of difference between the two calls
   A.Assert
     (abs (Long_Long_Integer (Ada.Calendar.Seconds (Filestamp_VFS))
           - Long_Long_Integer (Ada.Calendar.Seconds (Filestamp)))
      < 2,
      "Both filestamp should have the same second");

   return A.Report;
end Test;
