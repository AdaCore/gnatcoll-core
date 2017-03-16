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
--     String_Pools.Free (Str);   --  reclaim memory

pragma Ada_2012;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Storage_Pools;     use System.Storage_Pools;
with System.Storage_Elements;  use System.Storage_Elements;

package GNATCOLL.Storage_Pools.Headers is

   ------------------
   -- Header_Pools --
   ------------------

   --  The actual memory layout that we need to allocate is described below. In
   --  all cases, we had a "Pad" (padding) which is used to obey the requested
   --  alignment for the object.

   --  Currently, this pool doesn't support alignment clauses (and the generic
   --  Typed package below doesn't declare any), so the padding is always 0
   --  bytes.

   --  * For a scalar, record, tagged record or constrained array:

   --      +--------+------+-----------------------+
   --      | Header | Pad  | Element               |
   --      +--------+------+-----------------------+

   --  * For an unconstrained array, whether we use a standard access
   --    type or a flattened access type (a representation clause gives
   --    it a size of a standard pointer)
   --
   --      +--------+------+-----------------------+
   --      | Header | Pad  | First+Last+Element    |
   --      +--------+------+-----------------------+
   --      First and Last are the bounds of the array.
   --      Our pool should return the address of First, and the compiler
   --      automatically deduces the address of Element to return to the
   --      user code.

   --  * For a controlled type:
   --
   --      1        2                      3
   --      +--------+------+-----------------------+
   --      | Header | Pad  | Previous+Next+Element |
   --      +--------+------+-----------------------+
   --      Previous and Next are pointers to other controlled types.
   --      In code like:
   --              A := new ...;
   --      the header pool allocates memory at 1 via malloc, but
   --         returns 2 to the compiler
   --      then the compiler stores 3 in A.
   --      Conversely, when calling Free, the compiler converts A back to
   --         2, and our pool converts this back to 1 before calling free()
   --      The trouble is that when we call "Header_Of" on A, we receive
   --      the address 3, so it is harder to find 1.
   --
   --      See System.Storage_Pools.Subpools.Allocate_Any_Controlled.

   generic
      type Extra_Header is private;
      --  The header to allocate for each element. The pool will make sure
      --  to pad its size so that the element's data is properly aligned.

   package Header_Pools is

      type Header_Pool is new Root_Storage_Pool with null record;

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
         is (Storage_Count'Last)
         with Inline;

      Pool : Header_Pool;

      -----------
      -- Typed --
      -----------

      generic
         type Element_Type (<>) is limited private;
      package Typed is
         type Element_Access is access all Element_Type;
         for Element_Access'Size use Standard'Address_Size;
         for Element_Access'Storage_Pool use Pool;
         --  Force array bounds to be stored before the array's data, rather
         --  than as a separate dope vector.

         function Header_Of
            (Element : Element_Access) return access Extra_Header
            with Inline;
         --  Points to the beginning of the header for Element.
         --  Returns null if Element is null

         procedure Free is new Ada.Unchecked_Deallocation
            (Element_Type, Element_Access);
         --  Free the memory used by Element

      end Typed;

   end Header_Pools;

end GNATCOLL.Storage_Pools.Headers;
