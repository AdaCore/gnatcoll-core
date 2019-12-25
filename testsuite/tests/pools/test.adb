------------------------------------------------------------------------------
--                                                                          --
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

with GNATCOLL.Pools;
with GNATCOLL.Traces;

with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;

   function Factory (Param : Integer) return Integer;
   --  Simple integer factory

   type Resource_Set is new Integer range 1 .. 1;

   package Int_Pools is new GNATCOLL.Pools
      (Element_Type  => Integer,
       Factory_Param => Integer,
       Resource_Set  => Resource_Set,
       Factory       => Factory);

   Val2, Val3  : Int_Pools.Resource;

   -------------
   -- Factory --
   -------------

   Last_Id : Natural := 0;

   function Factory (Param : Integer) return Integer is
      pragma Unreferenced (Param);
   begin
      Last_Id := Last_Id + 1;
      return Last_Id;
   end Factory;

begin
   GNATCOLL.Traces.Parse_Config_File;

   Int_Pools.Set_Factory (1, Max_Elements => 2);

   declare
      Val1 : Int_Pools.Resource;
   begin
      Int_Pools.Get (Val1);
      A.Assert (Val1.Element.all, 1, "element from pool");
      Int_Pools.Get (Val2);
      A.Assert (Val2.Element.all, 2, "element from pool");

      --  Now Val1 goes out of scope and we release the resource
   end;

   Int_Pools.Get (Val3);
   A.Assert (Val3.Element.all, 1, "element from pool");

   --  The following would Deadlock
   --  Pool.Get (Val2);

   Int_Pools.Free;
   GNATCOLL.Traces.Finalize;

   return A.Report;

end Test;
