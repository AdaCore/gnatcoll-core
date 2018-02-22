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

package body GNATCOLL.Counters is

   One_Automatic : constant Automatic_Counter :=
      (if Application_Uses_Tasks
       then (Atomic => True,  A_Value => 1)
       else (Atomic => False, N_Value => 1));

   ----------------
   -- Set_To_One --
   ----------------

   procedure Set_To_One (Val : out GNATCOLL.Atomic.Atomic_Counter) is
   begin
      Val := 1;
   end Set_To_One;

   procedure Set_To_One (Val : out Non_Atomic_Counter) is
   begin
      Val := 1;
   end Set_To_One;

   procedure Set_To_One (Val : out Automatic_Counter) is
   begin
      Val := One_Automatic;
   end Set_To_One;

   -----------------
   -- Set_To_Zero --
   -----------------

   procedure Set_To_Zero (Val : out GNATCOLL.Atomic.Atomic_Counter) is
   begin
      Val := Atomic_Zero;
   end Set_To_Zero;

   procedure Set_To_Zero (Val : out Non_Atomic_Counter) is
   begin
      Val := Non_Atomic_Zero;
   end Set_To_Zero;

   procedure Set_To_Zero (Val : out Automatic_Counter) is
   begin
      Val := Automatic_Zero;
   end Set_To_Zero;

   ---------------
   -- Increment --
   ---------------

   procedure Increment (Val : aliased in out Non_Atomic_Counter) is
      pragma Suppress (Overflow_Check);
   begin
      Val := Val + 1;
   end Increment;

   procedure Increment (Val : aliased in out Automatic_Counter) is
      pragma Suppress (Overflow_Check);
   begin
      case Application_Uses_Tasks is
         when True  => GNATCOLL.Atomic.Increment (Val.A_Value);
         when False => Val.N_Value := Val.N_Value + 1;
      end case;
   end Increment;

   ---------------
   -- Decrement --
   ---------------

   function Decrement
      (Val : aliased in out Non_Atomic_Counter) return Boolean
   is
      pragma Suppress (Overflow_Check);
   begin
      Val := Val - 1;
      return Val = 0;
   end Decrement;

end GNATCOLL.Counters;
