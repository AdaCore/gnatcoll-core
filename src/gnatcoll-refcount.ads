------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

--  This package provides support for reference counting.
--  A Smart_Pointer plays the role of an access type (although it is not an
--  access type), and keeps a reference to the designated entity. When a smart
--  pointer goes out of scope, the designated entity's reference count is
--  automatically decremented.
--  When the reference count reaches 0, the corresponding entity is freed.

pragma Ada_05;

with Interfaces;
private with Ada.Finalization;

package GNATCOLL.Refcount is

   type Refcounted is abstract tagged private;
   type Refcounted_Access is access all Refcounted'Class;
   --  The common ancestor for all refcounted types.
   --  This ancestor adds a refcount field, which keeps track of how many
   --  references exist to a particular instance of Refcounted.
   --
   --  The refcounting is type safe (that is you can use the smart pointer from
   --  multiple tasks concurrently, and the refcounting will always be
   --  accurate). But the task-safety of Refcounted itself depends on your
   --  application.

   procedure Free (Self : in out Refcounted) is null;
   --  Free the memory associated with Self, when Self is no longer referenced.

   package Sync_Counters is
      function Sync_Add_And_Fetch
        (Ptr   : access Interfaces.Integer_32;
         Value : Interfaces.Integer_32) return Interfaces.Integer_32;
      --  Increment Ptr by Value. This is task safe (either using a lock or
      --  intrinsic atomic operations). Returns the new value (as set, it
      --  might already have been changed by another by the time this function
      --  returns.
   end Sync_Counters;

   --------------------
   -- Smart_Pointers --
   --------------------

   generic
      type Encapsulated is abstract new Refcounted with private;
   package Smart_Pointers is
      type Encapsulated_Access is access all Encapsulated'Class;

      type Ref is tagged private;
      Null_Ref : constant Ref;

      procedure Set (Self : in out Ref; Data : Encapsulated'Class);
      procedure Set (Self : in out Ref; Data : access Encapsulated'Class);
      --  Replace the current contents of Self.
      --  Data is adopted by the smart pointer, and should no longer be
      --  referenced directly elsewhere. The reference count of Data is
      --  incremented by 1.
      --  Typical code looks like:
      --      Tmp := new Encapsulated;
      --      Set (Ptr, Tmp);
      --  (You can't do
      --      Set (Ptr, new Encapsulated);
      --   for visibility reasons)

      function Get (P : Ref) return Encapsulated_Access;
      pragma Inline (Get);
      --  Return a pointer the data pointed to by P.
      --  We return an access type for efficiency reasons. However, the
      --  returned value must not be freed by the caller.

      overriding function "=" (P1, P2 : Ref) return Boolean;
      --  Whether the two pointers point to the same data

      function Get_Refcount (Self : Ref) return Natural;
      --  Return the current reference count.
      --  This is mostly intended for debug purposes.

   private
      type Ref is new Ada.Finalization.Controlled with record
         Data : Refcounted_Access;
      end record;

      overriding procedure Finalize (P : in out Ref);
      overriding procedure Adjust   (P : in out Ref);
      --  Take care of reference counting

      Null_Ref : constant Ref :=
                   (Ada.Finalization.Controlled with Data => null);
   end Smart_Pointers;

private

   type Refcounted is abstract tagged record
      Refcount : aliased Interfaces.Integer_32 := 0;
   end record;
   --  This requires, as a result, that all refcounted types also be tagged
   --  types (thus adding the size of a tag and the size of an integer to each
   --  instance). This approach was chosen over storing the refcounting
   --  independently of the refcounted type. The chosen approach provides a
   --  tighter integration between the two.

end GNATCOLL.Refcount;
