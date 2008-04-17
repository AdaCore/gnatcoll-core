-----------------------------------------------------------------------
--                               G N A T C O L L                     --
--                                                                   --
--                 Copyright (C) 2005-2008, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides a storage pool that allows you to select any
--  possible alignment for your data.
--  The alignment itself is chosen through discriminant of the pool
--  itself.
--
--      My_Pool : Unbounded_No_Reclaim_Align_Pool (Alignment => 64);
--      type My_Data is ...;
--      for My_Data'Storage_Pool use My_Pool;

with System.Storage_Pools;
with System.Storage_Elements;

package GNATCOLL.Storage_Pools.Alignment is
   pragma Elaborate_Body;
   --  Needed to ensure that library routines can execute allocators

   type Unbounded_No_Reclaim_Align_Pool
     (Alignment : System.Storage_Elements.Storage_Count)
     is new System.Storage_Pools.Root_Storage_Pool with private;
   --  A storage pool that uses malloc() internally, but always returns
   --  addresses aligned on Alignment bytes.

private
   type Unbounded_No_Reclaim_Align_Pool
     (Alignment : System.Storage_Elements.Storage_Count)
     is new System.Storage_Pools.Root_Storage_Pool with null record;

   function Storage_Size
     (Pool : Unbounded_No_Reclaim_Align_Pool)
      return System.Storage_Elements.Storage_Count;

   procedure Allocate
     (Pool         : in out Unbounded_No_Reclaim_Align_Pool;
      Address      : out System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count);

   procedure Deallocate
     (Pool         : in out Unbounded_No_Reclaim_Align_Pool;
      Address      : System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count);

end GNATCOLL.Storage_Pools.Alignment;
