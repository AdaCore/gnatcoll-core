------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2018-2018, AdaCore                     --
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

--  A package that provides either atomic counters, or non-atomic counters.

with GNATCOLL.Atomic;

pragma Warnings (Off, "* is an internal GNAT unit");
with System.Soft_Links;               use System.Soft_Links;
pragma Warnings (On, "* is an internal GNAT unit");

package GNATCOLL.Counters is

   generic
      type Counter is limited private;

      with procedure Increment (Val : aliased in out Counter);
      --  Increment Val by 1

      with function Decrement (Val : aliased in out Counter) return Boolean;
      --  Decrement Val by 1.
      --  Returns True if the new value is 0.

      with procedure Set_To_One (Val : out Counter) is <>;
      with function Greater_Than_One (Val : Counter) return Boolean is <>;
      --  Set the counter to 1

      with procedure Set_To_Zero (Val : out Counter) is <>;
      with function Is_Zero (Val : Counter) return Boolean is <>;
      --  A value that will not be reached in the normal flow of things. This
      --  is used to indicate that a counter is uninitialized, or as a special
      --  markup for instance.  This must be return False with
      --  Greater_Than_One.

      Task_Safe : Boolean;
      --  Whether these counters are thread safe

      Is_Lock_Free : Boolean;
      --  Whether the counter is implement with lock free operations
      --  (compare-and-swap,...), or via controlled types (with locking)

   package Signature is
      Is_Task_Safe : constant Boolean := Task_Safe;
      --  Make available to users of the package

   end Signature;

   ---------------------
   -- Atomic counters --
   ---------------------

   Atomic_Zero : constant GNATCOLL.Atomic.Atomic_Counter;
   procedure Set_To_One
      (Val : out GNATCOLL.Atomic.Atomic_Counter) with Inline;
   procedure Set_To_Zero
      (Val : out GNATCOLL.Atomic.Atomic_Counter) with Inline;
   function  Is_Zero (Val : GNATCOLL.Atomic.Atomic_Counter) return Boolean
      is (GNATCOLL.Atomic."=" (Val, Atomic_Zero));
   function Greater_Than_One (Val : GNATCOLL.Atomic.Atomic_Counter)
      return Boolean
      is (GNATCOLL.Atomic.">" (Val, 1));

   package Atomic_Counters is new Signature
      (Counter      => GNATCOLL.Atomic.Atomic_Counter,
       Increment    => GNATCOLL.Atomic.Increment,
       Decrement    => GNATCOLL.Atomic.Decrement,
       Task_Safe    => True,
       Is_Lock_Free => GNATCOLL.Atomic.Is_Lock_Free);

   -------------------------
   -- Non atomic counters --
   -------------------------

   type Non_Atomic_Counter is new Natural;
   Non_Atomic_Zero : constant Non_Atomic_Counter;
   procedure Increment (Val : aliased in out Non_Atomic_Counter)
      with Inline;
   function Decrement (Val : aliased in out Non_Atomic_Counter) return Boolean
      with Inline;
   procedure Set_To_One (Val : out Non_Atomic_Counter) with Inline;
   procedure Set_To_Zero (Val : out Non_Atomic_Counter) with Inline;
   function  Is_Zero (Val : Non_Atomic_Counter) return Boolean
      is (Val = Non_Atomic_Zero);
   function Greater_Than_One (Val : Non_Atomic_Counter) return Boolean
      is (Val > 1);

   package Non_Atomic_Counters is new Signature
      (Counter      => Non_Atomic_Counter,
       Increment    => Increment,
       Decrement    => Decrement,
       Task_Safe    => False,
       Is_Lock_Free => True);

   ------------------------
   -- Automatic counters --
   ------------------------
   --  At the cost of a slightly bigger size, these counters will either be
   --  atomic (if the tasking runtime was initialized) or not.

   Application_Uses_Tasks : constant Boolean :=
      System.Soft_Links.Lock_Task /= System.Soft_Links.Task_Lock_NT'Access;
   --  Whether the tasking run time has been initialized.

   type Automatic_Counter (Atomic : Boolean := True) is record
      case Atomic is
         when True  => A_Value : aliased GNATCOLL.Atomic.Atomic_Counter;
         when False => N_Value : aliased Non_Atomic_Counter;
      end case;
   end record
   with Unchecked_Union;

   Automatic_Zero : constant Automatic_Counter;

   procedure Set_To_One (Val : out Automatic_Counter) with Inline;
   procedure Set_To_Zero (Val : out Automatic_Counter) with Inline;
   function  Is_Zero (Val : Automatic_Counter) return Boolean
      is (case Application_Uses_Tasks is
          when True  => GNATCOLL.Atomic."=" (Val.A_Value, 0),
          when False => Val.N_Value = 0);
   procedure Increment (Val : aliased in out Automatic_Counter)
      with Inline;
   function Decrement (Val : aliased in out Automatic_Counter) return Boolean
      is (case Application_Uses_Tasks is
          when True  => GNATCOLL.Atomic.Decrement (Val.A_Value),
          when False => Decrement (Val.N_Value));
   function Greater_Than_One (Val : Automatic_Counter) return Boolean
      is (case Application_Uses_Tasks is
          when True  => GNATCOLL.Atomic.">" (Val.A_Value, 1),
          when False => Val.N_Value > 1);

   package Automatic_Counters is new Signature
      (Counter      => Automatic_Counter,
       Increment    => Increment,
       Decrement    => Decrement,
       Task_Safe    => Application_Uses_Tasks,
       Is_Lock_Free => not Application_Uses_Tasks
          or else GNATCOLL.Atomic.Is_Lock_Free);

private

   Non_Atomic_Zero : constant Non_Atomic_Counter := 0;
   Atomic_Zero : constant GNATCOLL.Atomic.Atomic_Counter := 0;
   Automatic_Zero : constant Automatic_Counter :=
      (if Application_Uses_Tasks
       then ((Atomic => True,  A_Value => 0))
       else ((Atomic => False, N_Value => 0)));

end GNATCOLL.Counters;
