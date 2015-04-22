------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2015, AdaCore                     --
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

with Interfaces;    use Interfaces;

package body GNATCOLL.Atomic is

   function Intrinsic_Sync_Add_And_Fetch
     (Ptr   : access Atomic_Counter;
      Value : Atomic_Counter) return Atomic_Counter;
   pragma Import (Intrinsic, Intrinsic_Sync_Add_And_Fetch,
                  External_Name => "__sync_add_and_fetch_4");

   ------------------------
   -- Sync_Add_And_Fetch --
   ------------------------

   function Sync_Add_And_Fetch
     (Ptr   : access Atomic_Counter;
      Value : Atomic_Counter) return Atomic_Counter is
   begin
      return Intrinsic_Sync_Add_And_Fetch (Ptr, Value);
   end Sync_Add_And_Fetch;

   procedure Sync_Add_And_Fetch
     (Ptr : access Atomic_Counter; Value : Atomic_Counter)
   is
      Dummy : Atomic_Counter;
      pragma Unreferenced (Dummy);
   begin
      Dummy := Intrinsic_Sync_Add_And_Fetch (Ptr, Value);
   end Sync_Add_And_Fetch;

   --------------------------------
   -- Sync_Bool_Compare_And_Swap --
   --------------------------------

   function Sync_Bool_Compare_And_Swap
      (Ptr    : access Element_Access;
       Oldval : Element_Access;
       Newval : Element_Access) return Boolean
   is
      function Intrinsic_Sync_Bool_And_Swap_Access
         (Ptr   : access Element_Access;
          Oldval, Newval : Element_Access) return Interfaces.Integer_8;
      pragma Import
         (Intrinsic, Intrinsic_Sync_Bool_And_Swap_Access,
          External_Name => "gnatcoll_sync_bool_compare_and_swap_access");
   begin
      return Intrinsic_Sync_Bool_And_Swap_Access (Ptr, Oldval, Newval) /= 0;
   end Sync_Bool_Compare_And_Swap;

end GNATCOLL.Atomic;
