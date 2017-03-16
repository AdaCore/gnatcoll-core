------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2017, AdaCore                     --
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

   function Intrinsic_Sync_Bool_Compare_And_Swap
     (Ptr    : access Atomic_Counter;
      Oldval : Atomic_Counter;
      Newval : Atomic_Counter) return Boolean;
   pragma Import (Intrinsic, Intrinsic_Sync_Bool_Compare_And_Swap,
                  External_Name => "__sync_bool_compare_and_swap_4");

   function Intrinsic_Sync_Val_Compare_And_Swap
     (Ptr    : access Atomic_Counter;
      Oldval : Atomic_Counter;
      Newval : Atomic_Counter) return Atomic_Counter;
   pragma Import (Intrinsic, Intrinsic_Sync_Val_Compare_And_Swap,
                  External_Name => "__sync_val_compare_and_swap_4");

   function Intrinsic_Sync_Add_And_Fetch
     (Ptr   : access Atomic_Counter;
      Value : Atomic_Counter) return Atomic_Counter;
   pragma Import (Intrinsic, Intrinsic_Sync_Add_And_Fetch,
                  External_Name => "__sync_add_and_fetch_4");

   function Intrinsic_Sync_Sub_And_Fetch
     (Ptr   : access Atomic_Counter;
      Value : Atomic_Counter) return Atomic_Counter;
   pragma Import (Intrinsic, Intrinsic_Sync_Sub_And_Fetch,
                  External_Name => "__sync_sub_and_fetch_4");

   ------------------
   -- Is_Lock_Free --
   ------------------

   function Is_Lock_Free return Boolean is (True);

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
      Dummy : Atomic_Counter with Unreferenced;
   begin
      Dummy := Intrinsic_Sync_Add_And_Fetch (Ptr, Value);
   end Sync_Add_And_Fetch;

   ------------------------
   -- Sync_Sub_And_Fetch --
   ------------------------

   function Sync_Sub_And_Fetch
     (Ptr   : access Atomic_Counter;
      Value : Atomic_Counter) return Atomic_Counter is
   begin
      return Intrinsic_Sync_Sub_And_Fetch (Ptr, Value);
   end Sync_Sub_And_Fetch;

   procedure Sync_Sub_And_Fetch
     (Ptr : access Atomic_Counter; Value : Atomic_Counter)
   is
      Dummy : Atomic_Counter with Unreferenced;
   begin
      Dummy := Intrinsic_Sync_Sub_And_Fetch (Ptr, Value);
   end Sync_Sub_And_Fetch;

   ---------------
   -- Increment --
   ---------------

   procedure Increment (Value : aliased in out Atomic_Counter) is
   begin
      System.Atomic_Counters.Increment (Value);
   end Increment;

   ---------------
   -- Decrement --
   ---------------

   procedure Decrement (Value : aliased in out Atomic_Counter) is
   begin
      System.Atomic_Counters.Decrement (Value);
   end Decrement;

   function Decrement (Value : aliased in out Atomic_Counter) return Boolean is
   begin
      return System.Atomic_Counters.Decrement (Value);
   end Decrement;

   ----------------------
   -- Unsafe_Increment --
   ----------------------

   procedure Unsafe_Increment (Value : in out Atomic_Counter) is
   begin
      Value := Atomic_Counter'Succ (Value);
   end Unsafe_Increment;

   ----------------------
   -- Unsafe_Decrement --
   ----------------------

   function Unsafe_Decrement (Value : in out Atomic_Counter) return Boolean is
   begin
      Value := Atomic_Counter'Pred (Value);
      return Value = 0;
   end Unsafe_Decrement;

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

   ----------------------------------------
   -- Sync_Bool_Compare_And_Swap_Counter --
   ----------------------------------------

   function Sync_Bool_Compare_And_Swap_Counter
      (Ptr    : access Atomic_Counter;
       Oldval : Atomic_Counter;
       Newval : Atomic_Counter) return Boolean is
   begin
      return Intrinsic_Sync_Bool_Compare_And_Swap (Ptr, Oldval, Newval);
   end Sync_Bool_Compare_And_Swap_Counter;

   ---------------------------------------
   -- Sync_Val_Compare_And_Swap_Counter --
   ---------------------------------------

   function Sync_Val_Compare_And_Swap_Counter
      (Ptr    : access Atomic_Counter;
       Oldval : Atomic_Counter;
       Newval : Atomic_Counter) return Atomic_Counter is
   begin
      return Intrinsic_Sync_Val_Compare_And_Swap (Ptr, Oldval, Newval);
   end Sync_Val_Compare_And_Swap_Counter;

end GNATCOLL.Atomic;
