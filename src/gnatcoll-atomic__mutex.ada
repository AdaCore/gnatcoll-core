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

pragma Ada_2012;

with GNAT.Task_Lock;
with Interfaces;

package body GNATCOLL.Atomic is

   ------------------
   -- Is_Lock_Free --
   ------------------

   function Is_Lock_Free return Boolean is (False);

   ------------------------
   -- Sync_Add_And_Fetch --
   ------------------------

   function Sync_Add_And_Fetch
     (Ptr   : access Atomic_Counter;
      Value : Atomic_Counter) return Atomic_Counter
   is
      Result : Atomic_Counter;
   begin
      GNAT.Task_Lock.Lock;
      Ptr.all := Unsafe_Add (Ptr.all, Value);
      Result := Ptr.all;
      --   ??? Should use a memory barriere here
      GNAT.Task_Lock.Unlock;
      return Result;
   end Sync_Add_And_Fetch;

   procedure Sync_Add_And_Fetch
     (Ptr : access Atomic_Counter; Value : Atomic_Counter)
   is
      Dummy : Atomic_Counter;
      pragma Unreferenced (Dummy);
   begin
      Dummy := Sync_Add_And_Fetch (Ptr, Value);
   end Sync_Add_And_Fetch;

   ------------------------
   -- Sync_Sub_And_Fetch --
   ------------------------

   function Sync_Sub_And_Fetch
     (Ptr   : access Atomic_Counter;
      Value : Atomic_Counter) return Atomic_Counter
   is
      Result : Atomic_Counter;
   begin
      GNAT.Task_Lock.Lock;
      Ptr.all := Unsafe_Sub (Ptr.all, Value);
      Result := Ptr.all;
      --   ??? Should use a memory barriere here
      GNAT.Task_Lock.Unlock;
      return Result;
   end Sync_Sub_And_Fetch;

   procedure Sync_Sub_And_Fetch
     (Ptr : access Atomic_Counter; Value : Atomic_Counter)
   is
      Dummy : Atomic_Counter;
      pragma Unreferenced (Dummy);
   begin
      Dummy := Sync_Sub_And_Fetch (Ptr, Value);
   end Sync_Sub_And_Fetch;

   ---------------
   -- Increment --
   ---------------

   procedure Increment (Value : aliased in out Atomic_Counter) is
   begin
      Sync_Add_And_Fetch (Value'Unrestricted_Access, 1);
   end Increment;

   ---------------
   -- Decrement --
   ---------------

   procedure Decrement (Value : aliased in out Atomic_Counter) is
   begin
      Sync_Sub_And_Fetch (Value'Unrestricted_Access, 1);
   end Decrement;

   function Decrement (Value : aliased in out Atomic_Counter) return Boolean is
   begin
      return Sync_Sub_And_Fetch (Value'Unrestricted_Access, 1) = 0;
   end Decrement;

   ----------------------
   -- Unsafe_Increment --
   ----------------------

   procedure Unsafe_Increment (Value : in out Atomic_Counter) is
   begin
      Value := Unsafe_Add (Value, 1);
   end Unsafe_Increment;

   --------------------------------
   -- Sync_Bool_Compare_And_Swap --
   --------------------------------

   function Sync_Bool_Compare_And_Swap
      (Ptr    : access Element_Access;
       Oldval : Element_Access;
       Newval : Element_Access) return Boolean is
   begin
      GNAT.Task_Lock.Lock;
      if Ptr.all = Oldval then
         Ptr.all := Newval;
         GNAT.Task_Lock.Unlock;
         return True;
      else
         GNAT.Task_Lock.Unlock;
         return False;
      end if;
   end Sync_Bool_Compare_And_Swap;

   --------------------------------
   -- Sync_Bool_Compare_And_Swap --
   --------------------------------

   function Sync_Bool_Compare_And_Swap_Counter
      (Ptr    : access Atomic_Counter;
       Oldval : Atomic_Counter;
       Newval : Atomic_Counter) return Boolean
   is
   begin
      GNAT.Task_Lock.Lock;
      if Ptr.all = Oldval then
         Ptr.all := Newval;
         GNAT.Task_Lock.Unlock;
         return True;
      else
         GNAT.Task_Lock.Unlock;
         return False;
      end if;
   end Sync_Bool_Compare_And_Swap_Counter;

   -------------------------------
   -- Sync_Val_Compare_And_Swap --
   -------------------------------

   function Sync_Val_Compare_And_Swap_Counter
      (Ptr    : access Atomic_Counter;
       Oldval : Atomic_Counter;
       Newval : Atomic_Counter) return Atomic_Counter
   is
      Initial : Atomic_Counter;
   begin
      GNAT.Task_Lock.Lock;
      Initial := Ptr.all;
      if Ptr.all = Oldval then
         Ptr.all := Newval;
         GNAT.Task_Lock.Unlock;
         return Initial;
      else
         GNAT.Task_Lock.Unlock;
         return Initial;
      end if;
   end Sync_Val_Compare_And_Swap_Counter;

end GNATCOLL.Atomic;
