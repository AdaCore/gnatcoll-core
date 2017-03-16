------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
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

with System.Memory;           use System, System.Memory;

package body GNATCOLL.Storage_Pools.Headers is

   Default_Align : constant Storage_Count :=
      Standard'System_Allocator_Alignment;

   ------------------
   -- Header_Pools --
   ------------------

   package body Header_Pools is
      type Header is record
         Extra : Extra_Header;
      end record;
      type Extra_Header_Access is access all Extra_Header;

      Extra_Bytes  : constant Storage_Offset :=
         (Header'Max_Size_In_Storage_Elements
          - Header'Object_Size / Storage_Unit);
      --  If the header is a controlled type, we need to allocate extra size
      --  for its Previous and Next pointers. This constant computes how
      --  much such extra size is needed.

      Header_Size_Bytes : constant Storage_Count :=
         Header'Size / Storage_Unit;

      Extra_Allocation_Bytes : constant Storage_Count :=
         ((Header_Size_Bytes + Extra_Bytes + Default_Align - 1)
          / Default_Align) * Default_Align;
      --   Allocate a multiple of Default_Align bytes, so that the
      --   alignment of the Element_Type is suitable.

      function Convert is new Ada.Unchecked_Conversion
         (System.Address, Extra_Header_Access);

      function Address_Header_Of
        (Addr : System.Address) return System.Address
      is (Addr - Extra_Allocation_Bytes);
      --  Compute the address of the header.
      --  Do not call with a null pointer.

      --------------
      -- Allocate --
      --------------

      overriding procedure Allocate
         (Self      : in out Header_Pool;
          Addr      : out System.Address;
          Size      : System.Storage_Elements.Storage_Count;
          Alignment : System.Storage_Elements.Storage_Count)
      is
         --  The compiler requests a size that include the object size
         --  plus any extra header like bounds or next/previous for
         --  controlled types. This size also includes a padding to
         --  ensure that the element will be properly aligned.
         --  The computation is done in s-stposu.adb, in
         --  Header_Size_With_Padding.

         pragma Unreferenced (Self, Alignment);
         Aligned_Size : constant Storage_Count :=   --  bytes
            Size + Extra_Allocation_Bytes;
         Allocated : constant System.Address :=
            Alloc (size_t (Aligned_Size));
      begin
         Addr := Allocated + Extra_Allocation_Bytes;
      end Allocate;

      ----------------
      -- Deallocate --
      ----------------

      overriding procedure Deallocate
         (Self      : in out Header_Pool;
          Addr      : System.Address;
          Size      : System.Storage_Elements.Storage_Count;
          Alignment : System.Storage_Elements.Storage_Count)
      is
         pragma Unreferenced (Self, Alignment, Size);
         Header : constant System.Address := Address_Header_Of (Addr);
      begin
         System.Memory.Free (Header);
      end Deallocate;

      -----------
      -- Typed --
      -----------

      package body Typed is

         function Header_Of
            (Element : Element_Access) return access Extra_Header
         is
            F : constant Integer := Element.all'Finalization_Size;
            --  If the element_type is a controlled type, this constant will
            --  be the number of extra bytes requested by the compiler in
            --  calls to Allocate and Deallocate (see the memory layout
            --  description in the specs).
            --
            --  These extra bytes are automatically added and substracted by
            --  the compiler when calling Deallocate, but not when calling
            --  Header_Of so we need to take them into account when looking
            --  for the our own header.

            H : constant access Extra_Header :=
               (if Element = null
                then null
                else Convert
                   (Address_Header_Of
                      (Element.all'Address
                       - Storage_Offset (F)
                       - Element_Type'Descriptor_Size)));
         begin
            return H;
         end Header_Of;

      end Typed;

   end Header_Pools;

end GNATCOLL.Storage_Pools.Headers;
