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

with GNAT.Semaphores; use GNAT.Semaphores;

private with Ada.Finalization;

--  This package provides simple locking primitives on top of GNAT.Semaphores.

package GNATCOLL.Locks is

   subtype Mutual_Exclusion is Binary_Semaphore
     (Initially_Available => True,
      Ceiling             => Default_Ceiling);
  --  This subtype represents a Mutual exclusion primitive, which is a common
  --  use case for Semaphores.

   type Scoped_Lock (Lock : access Mutual_Exclusion)
   is limited private;
   --  This type represents a scoped lock for the Mutual_Exclusion object
   --  passed as discriminant. It will hold the Mutex as long as the object is
   --  alive, and release it when it is finalized.
   --
   --  It provides a useful idiom to protect a subprogram or a part of a
   --  subprogram from data races via a critical section. Here is code without
   --  a scoped lock::
   --
   --     A : Integer := 0;
   --     M : Mutual_Exclusion;
   --
   --     procedure Modify_State is
   --     begin
   --        M.Seize;
   --        A := A + 1;
   --        if A > 12 then
   --           M.Release;
   --           return;
   --        end if;
   --
   --        A := A + 2;
   --        M.Release;
   --     exception  -- Addition can overflow!
   --        when others =>
   --        M.Release;
   --        raise;
   --     end Modify_State;
   --
   --  And here is the code with the scoped lock idiom::
   --
   --     A : Integer := 0;
   --     M : Mutual_Exclusion;
   --
   --     procedure Modify_State is
   --        Lock : Scoped_Lock (M);
   --     begin
   --        A := A + 1;
   --        if A > 12 then
   --           return;
   --        end if;
   --
   --        A := A + 2;
   --     end Modify_State;

private
   type Scoped_Lock (Lock : access Mutual_Exclusion)
   is new Ada.Finalization.Limited_Controlled with null record;

   overriding procedure Initialize (This : in out Scoped_Lock);
   overriding procedure Finalize   (This : in out Scoped_Lock);
end GNATCOLL.Locks;
