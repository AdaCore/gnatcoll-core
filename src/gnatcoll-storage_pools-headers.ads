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

--  This package provides storage pools that allocate enough memory for their
--  element and an extra header.
--  This header can be used to store extra information. For instance, it has
--  been used to store a reference counter, or pointers to the next element
--  in a container.
--  The goal is to reduce the number of memory allocations: instead of doing
--  one allocation for the element, and a second one for the counter, we can
--  do a single allocation, which is much faster.
--
--  Usage example:
--     type Header is record
--        Refcount : Natural;
--     end record;
--     package Pools is new Header_Pools (Header);
--     package String_Pools is new Pools.Typed (String);
--     package Integer_Pools is new Pools.Typed (Integer);
--
--     Str : String_Pools.Element_Access;
--     Str := new String'("foo");    --  uses the storage pool
--
--     String_Pools.Header_Of (Str).Refcount := 1;

pragma Ada_2012;
with System.Storage_Pools;    use System.Storage_Pools;
with System.Storage_Elements; use System.Storage_Elements;

package GNATCOLL.Storage_Pools.Headers is

   ------------------
   -- Header_Pools --
   ------------------

   generic
      type Extra_Header is private;
      --  The header to allocate for each element. The pool will make sure
      --  to pad its size so that the element's data is properly aligned.

   package Header_Pools is
      type Header_Pool (Descriptor_Size : Storage_Count)
         is new Root_Storage_Pool
         with null record;
      --  The descriptor size is based on the element_type 'Descriptor_Size
      --  attribute (see also Typed_Header_Pools package below).
      --  This is a size in bytes.
      --  It will be 0 for most types, but for unconstrained arrays it will
      --  provide the size needed to store the bounds of the array.

      function Header_Of
        (Self : Header_Pool; Addr : System.Address) return access Extra_Header;
      pragma Inline (Header_Of);
      --  Points to the begining of the header, given an element allocated by
      --  the pool.

      overriding procedure Allocate
         (Self      : in out Header_Pool;
          Addr      : out System.Address;
          Size      : Storage_Count;
          Alignment : Storage_Count);
      overriding procedure Deallocate
         (Self      : in out Header_Pool;
          Addr      : System.Address;
          Size      : Storage_Count;
          Alignment : Storage_Count);
      overriding function Storage_Size
         (Self      : Header_Pool) return Storage_Count
         is (Storage_Count'Last);
      pragma Inline (Storage_Size);

      -----------
      -- Typed --
      -----------

      generic
         type Element_Type (<>) is limited private;
         --  A pool is specific to an element type, so that we can properly
         --  handle unconstrained arrays (for which we need a specific Size
         --  representation clause and we need to take into account the bounds
         --  of the array, which are stored next to the array).

         Potentially_Controlled : Boolean := True;
         --  See comment for Finalization_Master_Size below.
         --  If the element_type cannot be controlled or contain controlled
         --  elements, you should set this parameter to False to allocate
         --  less memory.
         --  ??? This parameter will be removed when the compiler can provide
         --  this information automatically.

      package Typed is
         Finalization_Master_Size : constant Storage_Count :=
            (if Potentially_Controlled then 2 * System.Address'Size else 0);
         --  Extra memory that needs to be allocated for the handling of
         --  potentially controlled types (see System.Finalization_Masters).
         --  Ideally, the compiler will be able to tell us whether these
         --  pointers are needed for Element_Type. For now, we simply allocate
         --  more memory than necessary.

         Extra_Offset : constant Storage_Count :=
            (Element_Type'Descriptor_Size + Finalization_Master_Size)
            / System.Storage_Unit;
         --  The actual memory layout that we need to allocate is:
         --
         --  +--------+-------+------+----------+------+---------+
         --  | Header | First | Last | Previous | Next | Element |
         --  +--------+-------+------+----------+------+---------+
         --
         --  Where:
         --    * Header is the formal parameter given above
         --    * (First, Last) are optional bounds if the element is an
         --      unconstraint array (otherwise these are not allocated).
         --      Their combined size is given by Element_Type'Descriptor_Size.
         --    * (Previous, Next) are optional pointers needed for potentially
         --      controlled types (or 'Class, or arrays of controlled types).
         --      See comment for Finalization_Master_Size.

         Pool : Header_Pool (Extra_Offset);

         type Element_Access is access all Element_Type;
         for Element_Access'Size use Standard'Address_Size;
         --  Force array bounds to be stored before the array's data, rather
         --  than as a separate dope vector.

         for Element_Access'Storage_Pool use Pool;
         --  All memory allocation and deallocation for Element_Access will go
         --  through the pool.

         function Header_Of
            (Element : Element_Access) return access Extra_Header
            is (if Element = null
                then null else Header_Of (Pool, Element.all'Address));
         pragma Inline (Header_Of);
      end Typed;

   end Header_Pools;

end GNATCOLL.Storage_Pools.Headers;
