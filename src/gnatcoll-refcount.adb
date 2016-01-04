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

--  Notes on the implementation of weak pointers:
--  There are several ways in which a weak pointer can be implemented:
--    - Using two counters (one for full references, one for weak). When both
--      reach 0, the memory blocks is freed; when only the first reaches 0,
--      the element is released, and the block can be resized.
--      This is hard to make task safe without using critical section though.
--    - store a doubly-linked list of weak pointers along with the counter.
--      When the counter reaches 0, change each of the weak pointers to null.
--      This requires more memory.
--    - (our choice) make the weak pointer a smart pointer pointing to the
--      same data:
--           smart_ptr ---> chunk1: counter + element + pointer to chunk2
--           weak_ptr  ---> chunk2: weak_counter + pointer to chunk1

pragma Ada_2012;
with Ada.Finalization; use Ada.Finalization;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Atomic;  use GNATCOLL.Atomic;
with Interfaces;       use Interfaces;
with System.Memory;    use System, System.Memory;

package body GNATCOLL.Refcount is

   procedure Inc_Ref (R : access Counters; Atomic : Boolean)
      with Inline => True;
   procedure Inc_Ref (R : access Weak_Data; Atomic : Boolean)
      with Inline => True;
   --  Increase/Decrease the refcount, and return the new value

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      (Weak_Data, Weak_Data_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Refcounted'Class, Refcounted_Access);

   procedure Finalize (Data : in out Weak_Data_Access; Atomic : Boolean);
   --  Decrease refcount, and free memory if needed

   function Sync_Bool_Compare_And_Swap
      is new GNATCOLL.Atomic.Sync_Bool_Compare_And_Swap
      (Weak_Data, Weak_Data_Access);

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (R : access Counters; Atomic : Boolean) is
   begin
      if Atomic then
         Sync_Add_And_Fetch (R.Refcount'Access, 1);
      else
         R.Refcount := R.Refcount + 1;
      end if;
   end Inc_Ref;

   procedure Inc_Ref (R : access Weak_Data; Atomic : Boolean) is
   begin
      if Atomic then
         Sync_Add_And_Fetch (R.Refcount'Access, 1);
      else
         R.Refcount := R.Refcount + 1;
      end if;
   end Inc_Ref;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Data : in out Weak_Data_Access; Atomic : Boolean) is
      Tmp  : Atomic_Counter;
   begin
      if Atomic then
         Tmp := Sync_Add_And_Fetch (Data.Refcount'Access, -1);
      else
         Data.Refcount := Data.Refcount - 1;
         Tmp := Data.Refcount;
      end if;

      if Tmp = 0 then
         Unchecked_Free (Data);
      end if;
   end Finalize;

   ---------------------
   -- Shared_Pointers --
   ---------------------

   package body Shared_Pointers is
      use type Pools.Element_Access;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Element_Type, Pools.Element_Access);

      pragma Warnings (Off, "*possible aliasing problem*");
      function Convert is new Ada.Unchecked_Conversion
         (Pools.Element_Access, System.Address);
      function Convert is new Ada.Unchecked_Conversion
         (System.Address, Pools.Element_Access);
      pragma Warnings (On, "*possible aliasing problem*");

      ---------
      -- Set --
      ---------

      procedure Set (Self : in out Ref'Class; Data : Element_Type) is
         R : access Counters;
      begin
         Finalize (Self);
         Self.Data := new Element_Type'(Data);  --  uses storage pool
         R := Pools.Header_Of (Self.Data);
         R.Refcount := 1;
         R.Weak_Data := null;
      end Set;

      -------------------
      -- Unchecked_Get --
      -------------------

      function Unchecked_Get (Self : Ref'Class) return Element_Access is
      begin
         return Self.Data;
      end Unchecked_Get;

      -------------
      -- Is_Null --
      -------------

      function Is_Null (Self : Ref'Class) return Boolean is
      begin
         return Self.Data = null;
      end Is_Null;

      ----------
      -- Weak --
      ----------

      function Weak (Self : Ref'Class) return Weak_Ref is
         R : access Counters;
         V : Weak_Data_Access;
      begin
         if Self.Data = null then
            return Null_Weak_Ref;
         end if;

         R := Pools.Header_Of (Self.Data);

         if R.Weak_Data = null then
            V := new Weak_Data'
               (Refcount => 2,   --  hold by Self and the result
                Element  => Convert (Self.Data));
            if not Sync_Bool_Compare_And_Swap
               (R.Weak_Data'Access, Oldval => null, Newval => V)
            then
               --  Was set by another thread concurrently
               Unchecked_Free (V);

               --  Need to increase refcount for the old weak ref
               Inc_Ref (R.Weak_Data, Atomic_Counters);
            end if;

         else
            Inc_Ref (R.Weak_Data, Atomic_Counters);
         end if;

         return (Controlled with Data => R.Weak_Data);
      end Weak;

      ---------
      -- Set --
      ---------

      procedure Set (Self : in out Ref'Class; Weak : Weak_Ref'Class) is
      begin
         Finalize (Self);

         if not Weak.Was_Freed then
            Self.Data := Convert (Weak.Data.Element);
            Inc_Ref (Pools.Header_Of (Self.Data), Atomic_Counters);
         end if;
      end Set;

      ---------------
      -- Was_Freed --
      ---------------

      function Was_Freed (Self : Weak_Ref'Class) return Boolean is
      begin
         return Self.Data = null
            or else Self.Data.Element = System.Null_Address;
      end Was_Freed;

      ---------
      -- "=" --
      ---------

      overriding function "=" (P1, P2 : Ref) return Boolean is
      begin
         return P1.Data = P2.Data;
      end "=";

      ------------
      -- Adjust --
      ------------

      overriding procedure Adjust (Self : in out Ref) is
      begin
         if Self.Data /= null then
            Inc_Ref (Pools.Header_Of (Self.Data), Atomic_Counters);
         end if;
      end Adjust;

      ------------
      -- Adjust --
      ------------

      overriding procedure Adjust (Self : in out Weak_Ref) is
      begin
         if Self.Data /= null then
            Inc_Ref (Self.Data, Atomic_Counters);
         end if;
      end Adjust;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Self : in out Weak_Ref) is
         Data : Weak_Data_Access := Self.Data;
      begin
         if Data /= null then
            Self.Data := null;
            Finalize (Data, Atomic_Counters);
         end if;
      end Finalize;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Self : in out Ref) is
         R    : access Counters;
         Data : Pools.Element_Access := Self.Data;
         Tmp  : Atomic_Counter;
      begin
         if Data /= null then
            Self.Data := null;

            R := Pools.Header_Of (Data);
            if Atomic_Counters then
               Tmp := Sync_Add_And_Fetch (R.Refcount'Access, -1);
            else
               R.Refcount := R.Refcount - 1;
               Tmp := R.Refcount;
            end if;

            if Tmp = 0 then
               if R.Weak_Data /= null then
                  R.Weak_Data.Element := System.Null_Address;
                  Finalize (R.Weak_Data, Atomic_Counters);
               end if;

               Release (Data.all);
               Unchecked_Free (Data);  --  using storage_pool
            end if;
         end if;
      end Finalize;

      ------------------
      -- Get_Refcount --
      ------------------

      function Get_Refcount (Self : Ref'Class) return Natural is
      begin
         if Self.Data = null then
            return 0;
         else
            return Natural (Pools.Header_Of (Self.Data).Refcount);
         end if;
      end Get_Refcount;

      ------------------
      -- From_Element --
      ------------------

      procedure From_Element
         (Self : out Ref'Class; Element : Element_Access) is
      begin
         if Self.Data /= Element then
            Finalize (Self);
            Self.Data := Element;
            Adjust (Self);
         end if;
      end From_Element;
   end Shared_Pointers;

   --------------------
   -- Smart_Pointers --
   --------------------

   package body Smart_Pointers is

      ---------
      -- Set --
      ---------

      procedure Set (Self : in out Ref; Data : access Encapsulated'Class) is
      begin
         if Self.Data = Refcounted_Access (Data) then
            --  Avoid finalizing Self.Data if we are going to reuse it
            return;
         end if;

         Finalize (Self);  -- decrement reference count
         Self.Data := Refcounted_Access (Data);
         Adjust (Self);    -- increment reference count if needed
      end Set;

      ---------
      -- Set --
      ---------

      procedure Set (Self : in out Ref; Data : Encapsulated'Class) is
         Tmp : constant Encapsulated_Access := new Encapsulated'Class'(Data);
      begin
         Set (Self, Tmp);
      end Set;

      ---------
      -- Get --
      ---------

      function Get (P : Ref) return Encapsulated_Access is
      begin
         return Encapsulated_Access (P.Data);
      end Get;

      ---------
      -- "=" --
      ---------

      overriding function "=" (P1, P2 : Ref) return Boolean is
      begin
         return P1.Data = P2.Data;
      end "=";

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (P : in out Ref) is
         Data : Refcounted_Access := P.Data;
      begin
         --  Make Finalize idempotent, since it could be called several
         --  times for the same instance (RM 7.6.1(24)).

         P.Data := null;

         --  Test if refcount is > 0, in case we are already freeing this
         --  element.

         if Data /= null then
            if Sync_Add_And_Fetch (Data.Refcount'Access, -1) = 0 then
               Free (Data.all);
               Unchecked_Free (Data);
            end if;
         end if;
      end Finalize;

      ------------
      -- Adjust --
      ------------

      overriding procedure Adjust (P : in out Ref) is
      begin
         if P.Data /= null then
            Sync_Add_And_Fetch (P.Data.Refcount'Access, 1);
         end if;
      end Adjust;

      ------------------
      -- Get_Refcount --
      ------------------

      function Get_Refcount (Self : Ref) return Natural is
      begin
         if Self.Data = null then
            return 0;
         else
            return Natural (Self.Data.Refcount);
         end if;
      end Get_Refcount;
   end Smart_Pointers;

end GNATCOLL.Refcount;
