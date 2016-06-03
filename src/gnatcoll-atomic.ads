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

--  This package provides a number of low-level primitives to execute
--  task-safe operations.
--  When possible, these operations are executed via one of the intrinsic
--  atomic operations of the compiler (generally implemented with special
--  support from the CPU).

with Interfaces;

package GNATCOLL.Atomic is

   subtype Atomic_Counter is Interfaces.Integer_32;

   function Sync_Add_And_Fetch
     (Ptr   : access Atomic_Counter;
      Value : Atomic_Counter) return Atomic_Counter;
   --  Increment Ptr by Value. This is task safe (either using a lock or
   --  intrinsic atomic operations). Returns the new value (as set, it
   --  might already have been changed by another task by the time this
   --  function returns.

   procedure Sync_Add_And_Fetch
     (Ptr : access Atomic_Counter; Value : Atomic_Counter);
   pragma Inline (Sync_Add_And_Fetch);

   generic
      type Element_Type (<>) is private;
      type Element_Access is access Element_Type;
   function Sync_Bool_Compare_And_Swap
      (Ptr    : access Element_Access;
       Oldval : Element_Access;
       Newval : Element_Access) return Boolean;
   --  If Ptr is equal to Oldval, set it to Newval and return True.
   --  Otherwise, return False and do not modify the current value.
   --  This operation is task safe and atomic.

   function Sync_Bool_Compare_And_Swap_Counter
      (Ptr    : access Atomic_Counter;
       Oldval : Atomic_Counter;
       Newval : Atomic_Counter) return Boolean;
   function Sync_Val_Compare_And_Swap_Counter
      (Ptr    : access Atomic_Counter;
       Oldval : Atomic_Counter;
       Newval : Atomic_Counter) return Atomic_Counter;
   --  A version that works with Atomic_Counter.
   --  Ptr.all is set to Newval if and only if it is currently set to Oldval.
   --  Returns True if the value was changed.
   --  The second version returns the initial value of Ptr.all

end GNATCOLL.Atomic;
