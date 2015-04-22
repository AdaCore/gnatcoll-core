------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2015, AdaCore                          --
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

with Ada.Unchecked_Conversion;
with System.Memory;           use System, System.Memory;

package body GNATCOLL.Storage_Pools.Headers is

   ------------------
   -- Header_Pools --
   ------------------

   package body Header_Pools is

      type Header is record
         Extra : Extra_Header;
      end record;
      type Extra_Header_Access is access all Extra_Header;

      Header_Size_Bytes : constant Storage_Count :=
         Header'Size / Storage_Unit;
      Pointer_Size_Bytes : constant Storage_Count :=
         System.Address'Size / Storage_Unit;

      Extra_Allocation_Bytes : constant Storage_Count :=
         ((Header_Size_Bytes + Pointer_Size_Bytes - 1) / Pointer_Size_Bytes)
         * Pointer_Size_Bytes;
      --   Allocate a multiple of Pointer_Size bytes, so that the
      --   alignment of the Element_Type is suitable.

      function Convert is new Ada.Unchecked_Conversion
         (System.Address, Extra_Header_Access);
      function Convert is new Ada.Unchecked_Conversion
         (Extra_Header_Access, System.Address);

      ---------------
      -- Header_Of --
      ---------------

      function Header_Of
         (Self : Header_Pool; Addr : System.Address)
         return access Extra_Header is
      begin
         return Convert (Addr - Extra_Allocation_Bytes - Self.Descriptor_Size);
      end Header_Of;

      --------------
      -- Allocate --
      --------------

      overriding procedure Allocate
         (Self      : in out Header_Pool;
          Addr      : out System.Address;
          Size      : System.Storage_Elements.Storage_Count;
          Alignment : System.Storage_Elements.Storage_Count)
      is
         pragma Unreferenced (Alignment);
         Aligned_Size : constant Storage_Count :=   --  bytes
            Size + Extra_Allocation_Bytes + Self.Descriptor_Size;
         Allocated : constant System.Address :=
            Alloc (size_t (Aligned_Size));
      begin
         Addr := To_Address
            (To_Integer (Allocated) +
            Integer_Address (Extra_Allocation_Bytes + Self.Descriptor_Size));
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
         pragma Unreferenced (Alignment, Size);
         Header : constant Extra_Header_Access :=
            Extra_Header_Access (Header_Of (Self, Addr));
      begin
         System.Memory.Free (Convert (Header));
      end Deallocate;
   end Header_Pools;

end GNATCOLL.Storage_Pools.Headers;
