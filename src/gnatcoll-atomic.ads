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

--  This package provides a number of low-level primitives to execute
--  task-safe operations.
--  When possible, these operations are executed via one of the intrinsic
--  atomic operations of the compiler (generally implemented with special
--  support from the CPU).

pragma Ada_2012;

with Interfaces;

package GNATCOLL.Atomic is

   type Atomic_Counter is new Interfaces.Integer_32;

   procedure Increment
     (Item : aliased in out Atomic_Counter; Value : Atomic_Counter := 1)
     with Inline_Always;
   --  Increments value of atomic counter

   procedure Decrement
     (Item : aliased in out Atomic_Counter) with Inline_Always;
   --  Decrements value of atomic counter

   function Decrement
     (Item : aliased in out Atomic_Counter) return Atomic_Counter
     with Inline_Always;

   function Sync_Add_And_Fetch
      (Ptr   : access Interfaces.Integer_32;
       Value : Interfaces.Integer_32) return Interfaces.Integer_32;
    --  Increment Ptr by Value. This is task safe (either using a lock or
    --  intrinsic atomic operations). Returns the new value (as set, it
    --  might already have been changed by another task by the time this
    --  function returns.

end GNATCOLL.Atomic;
