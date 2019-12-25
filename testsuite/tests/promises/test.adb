------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

with Ada.Text_IO;            use Ada.Text_IO;
with GNATCOLL.Promises;      use GNATCOLL.Promises;
with Test_Promises_Support;  use Test_Promises_Support;

with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;

   use Int_To_Float, Float_To_Str, Str_Promises;

   function Get_Promise return Int_Promises.Promise;
   --  Dummy function

   function Get_Promise return Int_Promises.Promise is
      P : constant Int_Promises.Promise := Int_Promises.Create;
   begin
      --  ??? Could resolve in a task for instance
      return P;
   end Get_Promise;

   P : Int_Promises.Promise;

begin
   pragma Warnings (Off, "use of an anonymous access type allocator");
   Put_Line ("=== Create chain");
   P := Get_Promise;
   Subscribe (P and (new Convert_Int & new Display_Int)
                and new Convert_Float
                and (new Display_String & new Display_String));

   Put_Line ("Resolving...");
   Baseline := 2;
   P.Set_Value (2);
   A.Assert (A.Assert_Count, 5, "expected number of asserts #1");

   Put_Line ("=== Create chain, will Fail");
   P := Get_Promise;
   Subscribe (P and new Convert_Int
                and new Convert_Float
                and new Display_String);
   Put_Line ("Failing...");
   Message := new String'("Explicit failure");
   P.Set_Error ("Explicit failure");
   A.Assert (A.Assert_Count, 7, "expected number of asserts #2");

   Put_Line ("=== Create chain, will Fail in middle");
   P := Get_Promise;
   Subscribe (P and new Convert_Int
                and new Fail_On_Float
                and (new Display_String & new Display_String));
   Baseline := 3;
   Message := new String'("explicit");
   P.Set_Value (3);
   A.Assert (A.Assert_Count, 11, "expected number of asserts #3");

   return A.Report;

end Test;
