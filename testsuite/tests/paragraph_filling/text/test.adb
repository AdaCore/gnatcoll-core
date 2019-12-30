------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Strings.Unbounded;
with GNAT.Strings;

with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNATCOLL.Paragraph_Filling; use GNATCOLL.Paragraph_Filling;

with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;
   package ASU renames Ada.Strings.Unbounded;

   subtype Acc is GNAT.Strings.String_Access;

   S_In : constant Acc := Read_File (Create_From_Base ("in.txt"));
   S_Gr : constant Acc := Read_File (Create_From_Base ("greedy.txt"));
   S_Pr : constant Acc := Read_File (Create_From_Base ("pretty.txt"));
   S_Kn : constant Acc := Read_File (Create_From_Base ("knuth.txt"));

begin

   --  Test no-fill option
   A.Assert (S_In.all, ASU.To_String (No_Fill (S_In.all, 60)),
             "no fill");

   --  Test greedy option
   A.Assert (S_Gr.all, ASU.To_String (Greedy_Fill (S_In.all, 60)),
             "greedy fill");

   --  Test pretty option
   A.Assert (S_Pr.all, ASU.To_String (Pretty_Fill (S_In.all, 60)),
             "pretty fill");

   --  Test Knuth option
   A.Assert (S_Kn.all, ASU.To_String (Knuth_Fill (S_In.all, 60)),
             "Knuth fill");

   return A.Report;
end Test;
