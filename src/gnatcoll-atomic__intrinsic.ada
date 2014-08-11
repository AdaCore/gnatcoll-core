------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2014, AdaCore                     --
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

package body GNATCOLL.Atomic is

   ------------------------
   -- Sync_Add_And_Fetch --
   ------------------------

   function Sync_Add_And_Fetch
     (Ptr   : access Interfaces.Integer_32;
      Value : Interfaces.Integer_32) return Interfaces.Integer_32
   is
      function Intrinsic_Sync_Add_And_Fetch
        (Ptr   : access Interfaces.Integer_32;
         Value : Interfaces.Integer_32) return Interfaces.Integer_32;
      pragma Import
        (Intrinsic, Intrinsic_Sync_Add_And_Fetch, "__sync_add_and_fetch_4");
   begin
      return Intrinsic_Sync_Add_And_Fetch (Ptr, Value);
   end Sync_Add_And_Fetch;

end GNATCOLL.Atomic;
