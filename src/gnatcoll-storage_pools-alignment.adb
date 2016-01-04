------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2016, AdaCore                     --
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

with System.Storage_Pools;       use System, System.Storage_Pools;
with System.Storage_Elements;    use System.Storage_Elements;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body GNATCOLL.Storage_Pools.Alignment is

   type Storage_Element_Access is access Storage_Element;
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Storage_Element_Access);

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Pool         : in out Unbounded_No_Reclaim_Align_Pool;
      Address      : out System.Address;
      Storage_Size : Storage_Count;
      Alignment    : Storage_Count)
   is
      pragma Unreferenced (Alignment);

      --   We need to allocate more memory than actually requested, so that
      --   even if "new" returns an incorrect alignment, we have enough spare
      --   memory to return the correct alignment. We also always need a buffer
      --   of at least two Storage_Element to store the offset between the
      --   address from "new" and the one returned by the use, so that
      --   Deallocates works appropriately.
      --   Worst case is when "new" returned a correctly aligned chunk, and we
      --   then need to offset by Pool.Alignment bytes.

      Bytes_For_Offset : constant := 3;

      Align : constant Storage_Count  := Pool.Alignment;
      Size  : constant Storage_Offset :=
        Storage_Size + Align + Bytes_For_Offset - 1;

      subtype Local_Storage_Array is Storage_Array (1 .. Size);
      type Ptr is access Local_Storage_Array;

      Allocated : constant Ptr := new Local_Storage_Array;
      Offset    : constant Storage_Count :=
        Align - Allocated.all'Address mod Align;

   begin
      Allocated (Offset - 2) := Storage_Element (Offset / 65_536);
      Allocated (Offset - 1) := Storage_Element ((Offset mod 65_536) / 256);
      Allocated (Offset) := Storage_Element (Offset mod 256);
      Address := Allocated.all'Address + Offset;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Pool         : in out Unbounded_No_Reclaim_Align_Pool;
      Address      : System.Address;
      Storage_Size : Storage_Count;
      Alignment    : Storage_Count)
   is
      pragma Unreferenced (Alignment);

      Size : constant Storage_Offset :=
        Storage_Size + Pool.Alignment;
      subtype Local_Storage_Array is Storage_Array (1 .. Size);
      type Ptr is access Local_Storage_Array;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Local_Storage_Array, Ptr);
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Ptr);

      Offset_High2 : constant Storage_Element := Convert (Address - 3).all;
      Offset_High  : constant Storage_Element := Convert (Address - 2).all;
      Offset_Low   : constant Storage_Element := Convert (Address - 1).all;
      Offset : constant Storage_Count :=
         Storage_Count (Offset_High2) * 65_536
         + Storage_Count (Offset_High) * 256
         + Storage_Count (Offset_Low);

      Real_Address : constant System.Address := Address - Offset;
      Var : Ptr := Convert (Real_Address);

   begin
      Unchecked_Free (Var);
   end Deallocate;

   ------------------
   -- Storage_Size --
   ------------------

   function Storage_Size
     (Pool  : Unbounded_No_Reclaim_Align_Pool) return Storage_Count
   is
      pragma Unreferenced (Pool);
   begin
      --  Intuitively, should return System.Memory_Size. But on Sun/Alsys,
      --  System.Memory_Size > System.Max_Int, which means all you can do with
      --  it is raise CONSTRAINT_ERROR...
      return Storage_Count'Last;
   end Storage_Size;
end GNATCOLL.Storage_Pools.Alignment;
