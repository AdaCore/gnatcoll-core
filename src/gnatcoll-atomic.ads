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

--  This package provides a number of low-level primitives to execute
--  task-safe operations.
--  When possible, these operations are executed via one of the intrinsic
--  atomic operations of the compiler (generally implemented with special
--  support from the CPU).

with System.Atomic_Counters;

package GNATCOLL.Atomic is

   subtype Atomic_Counter is System.Atomic_Counters.Atomic_Unsigned;

   Minus_One : constant Atomic_Counter :=
      System.Atomic_Counters."-" (0, 1);

   function Is_Lock_Free return Boolean;
   --  Whether the implementation uses the processor's atomic operations
   --  or falls back on using locks

   function Sync_Add_And_Fetch
     (Ptr   : access Atomic_Counter;
      Value : Atomic_Counter) return Atomic_Counter
     with Inline_Always;
   --  Increment Ptr by Value. This is task safe (either using a lock or
   --  intrinsic atomic operations). Returns the new value (as set, it
   --  might already have been changed by another task by the time this
   --  function returns.

   function Sync_Sub_And_Fetch
     (Ptr   : access Atomic_Counter;
      Value : Atomic_Counter) return Atomic_Counter
     with Inline_Always;
   --  Decrement Ptr by Value.

   procedure Sync_Add_And_Fetch
     (Ptr : access Atomic_Counter; Value : Atomic_Counter)
     with Inline_Always;
   procedure Sync_Sub_And_Fetch
     (Ptr : access Atomic_Counter; Value : Atomic_Counter)
     with Inline_Always;
   --  Same as above, but ignore the return value.

   procedure Increment
      (Value : aliased in out Atomic_Counter) with Inline_Always;
   procedure Decrement
      (Value : aliased in out Atomic_Counter) with Inline_Always;
   function Decrement
      (Value : aliased in out Atomic_Counter) return Boolean
      with Inline_Always;
   --  Similar to the Sync_Add_And_Fetch and Sync_Sub_And_And, but
   --  always increment or decrement by one.
   --  On some systems (x86) this uses faster assembly instructions.
   --  Decrement returns True if the value reaches 0.

   function "+"
      (Left, Right : Atomic_Counter) return Atomic_Counter is abstract;
   function "-"
      (Left, Right : Atomic_Counter) return Atomic_Counter is abstract;
   --  Prevent standard operations on these counters

   function Unsafe_Add
      (Left, Right : Atomic_Counter) return Atomic_Counter
      is (Atomic_Counter (Natural (Left) + Natural (Right)));
   procedure Unsafe_Increment (Value : in out Atomic_Counter)
      with Inline_Always;
   function Unsafe_Sub
      (Left, Right : Atomic_Counter) return Atomic_Counter
      is (Atomic_Counter (Natural (Left) - Natural (Right)));
   --  These are unsafe operations. If you have two threads, and they all try
   --  to do "Unsafe_Add (A, 2)" at the same time, when A was initially 0,
   --  you could end up with the following values in A:
   --      2 (both threads have read 0, then added 2)
   --      4 (thread 1 has read and incremented, then thread 2)
   --  If you use the other operations above, you always end up with 4.

   function "="
      (Left, Right : Atomic_Counter) return Boolean
      renames System.Atomic_Counters."=";
   --  Make the operator visible

   generic
      type Element_Type (<>) is limited private;
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
