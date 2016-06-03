------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2016, AdaCore                     --
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

package body GNATCOLL.Atomic is

   ------------------------
   -- Sync_Add_And_Fetch --
   ------------------------

   function Sync_Add_And_Fetch
     (Ptr   : access Atomic_Counter;
      Value : Atomic_Counter) return Atomic_Counter
   is
      use type Interfaces.Integer_32;
      Result : Interfaces.Integer_32;
   begin
      GNAT.Task_Lock.Lock;
      Ptr.all := Ptr.all + Value;
      Result := Ptr.all;
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
