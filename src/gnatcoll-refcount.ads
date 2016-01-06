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

--  This package provides support for reference counting.  A Smart_Pointer
--  plays the role of an access type (although it is not an access type), and
--  keeps a reference to the designated entity. When a smart pointer goes out
--  of scope, the designated entity's reference count is automatically
--  decremented.
--  When the reference count reaches 0, the corresponding entity is freed.
--
--  This package also provides support for weak pointers. These do not prevent
--  the freeing of the object they point to. However, when that object is
--  freed, the weak pointer is safely reset to null.
--
--  Cycles of references will prevent the freeing of the memory, since the
--  objects' refcounts will never reach 0. For instance, if you consider a
--  tree, the parent could hold a reference (via a smart_pointer) to each of
--  its children. Thus the children will exist for at least as long as their
--  parents. However, if the children also point to their parents with a
--  smart_pointer, the parent can never be freed in the first place.  The
--  solution is that the children should point to their parents through a weak
--  pointer instead.
--
--  This package provides two versions of such pointers:
--      * Smart_Pointers is the older version (obsolescent). It is less
--        efficient, and the element_type must derive from the Refcounted
--        type.
--      * Shared_Pointers is a more flexible API, where the element_type
--        can be any unconstrained type. The implementation is also faster.

pragma Ada_2012;

private with Ada.Finalization;
with System;
with GNATCOLL.Atomic;
with GNATCOLL.Storage_Pools.Headers;  use GNATCOLL.Storage_Pools.Headers;
with Interfaces;

package GNATCOLL.Refcount is

   -------------------
   -- Internal data --
   -------------------
   --  This section provides several types that are used in the implementation
   --  of this package. They are not useful for applications.

   type Weak_Data is record
      Element  : System.Address := System.Null_Address;
      Refcount : aliased GNATCOLL.Atomic.Atomic_Counter;
   end record;
   type Weak_Data_Access is access all Weak_Data;

   type Counters is record
      Weak_Data     : aliased Weak_Data_Access := null;
      --  A pointer to the weak pointers'data. This data is created the
      --  first time we create a weak pointer. We hold a reference to that
      --  data, so that it can never be freed while at least one reference
      --  exists.

      Refcount      : aliased GNATCOLL.Atomic.Atomic_Counter := 1;
   end record;

   package Headers is new Header_Pools (Counters);

   ---------------------
   -- Shared_Pointers --
   ---------------------

   generic
      type Element_Type (<>) is private;
      --  The element that will be encapsulated within a smart pointer.
      --  We need to be able to copy it as part of Set.

      with procedure Release (Self : in out Element_Type) is null;
      --  This procedure should be used if you need to perform actions when
      --  the last reference to an element is removed. Typically, this is
      --  used to free element_type and its contents, when it is not a
      --  controlled type.

      Atomic_Counters : Boolean := True;
      --  Whether to use atomic (and thus thread-safe) counters. If set to
      --  True, the smart pointer is task safe. Of course, that does not
      --  mean that the Element_Type itself is task safe.

      Potentially_Controlled : Boolean := True;
      --  See the comment for GNATCOLL.Storage_Pools.Headers.
      --  Set this to False if you know that Element_Type cannot be
      --  controlled or contain controlled element. This parameter will be
      --  removed when the compiler can provide this information automatically

   package Shared_Pointers is
      pragma Suppress (All_Checks);

      type Ref is tagged private;
      Null_Ref : constant Ref;
      --  This type acts like a pointer, but holds a reference to the object,
      --  which will thus never be freed while there exists at least one
      --  reference to it.

      type Weak_Ref is tagged private;
      Null_Weak_Ref : constant Weak_Ref;
      --  A weak reference to an object. The value returned by Get will be
      --  reset to null when the object is freed (because its last reference
      --  expired). Holding a weak reference does not prevent the deallocation
      --  of the object.

      package Pools is new Headers.Typed
         (Element_Type, Potentially_Controlled => Potentially_Controlled);
      subtype Element_Access is Pools.Element_Access;

      procedure Set (Self : in out Ref'Class; Data : Element_Type);
      pragma Inline (Set);
      --  A copy of Data will be put under control of Self, and freed when
      --  the last reference to it is removed.

      procedure From_Element (Self : out Ref'Class; Element : Element_Access);
      pragma Inline (From_Element);
      --  Given an element that is already under control of a
      --  shared pointer, returns the corresponding shared pointer.
      --  This is especially useful when the element_type is a tagged
      --  type. This element might be used for dynamic dispatching, but
      --  it might be necessary to retrieve the smart pointer:
      --
      --      type Object is tagged private;
      --      package Pointers is new Shared_Pointers (Object'Class);
      --      use Pointers;
      --
      --      procedure Method (Self : Object'Class) is
      --         R : Ref;
      --      begin
      --         From_Element (R, Self);
      --      end Method;
      --
      --      R : Ref;
      --      R.Set (Obj);
      --      Method (R.Get);
      --
      --  Warning: this must only be called when Element comes from a
      --  shared pointer, otherwise an invalid memory access will result.

      type Reference_Type (Element : access Element_Type)
         is limited null record
         with Implicit_Dereference => Element;
      --  A reference to an element_type.
      --  This type is used as the return value for Get, instead of an
      --  Element_Access, because it is safer:
      --     * applications cannot free the returned value (and
      --       they should never do it !)
      --     * the Element discriminant cannot be stored in a variable,
      --       so that prevents keeping a reference when it could be freed at
      --       any time.
      --     * since the type is limited, it is in general difficult to
      --       store it in records. This is intended, since the shared
      --       pointer itself should be stored instead (at the access type
      --       might be freed at any time).
      --  This type is often mostly transparent for the application. Assuming
      --  the Element_Type is defined as:
      --
      --       type Element_Type is tagged record
      --          Field : Integer;
      --       end record;
      --       procedure Primitive (Self : Element_Type);
      --       procedure Primitive2 (Self : access Element_Type);
      --
      --  then a shared pointer SP can be used as:
      --
      --       SP.Get.Field := 1;
      --       SP.Get.Primitive1;
      --       SP.Get.Element.Primitive2;
      --
      --  WARNING:
      --  The use of a reference_type ensures that Get can return an access to
      --  the object (more efficient than a copy when the objects are large),
      --  while preventing users from freeing the returned value. But this
      --  does not prevent all invalid cases. Using 'renames', for instance,
      --  can lead to invalid code, as in:
      --
      --     package IP is new Shared_Pointers (Integer);
      --     use IP;
      --     R : Ref;
      --     R.Set (99);
      --     declare
      --        Int : Integer renames R.Get.Element.all;
      --     begin
      --        R := Null_Ref;     --  Frees Int !
      --        Put_Line (I'Img);  --  Invalid memory access
      --     end;

      function Unchecked_Get (Self : Ref'Class) return Element_Access;
      pragma Inline_Always (Unchecked_Get);
      function Get (Self : Ref'Class) return Reference_Type
         is ((Element => Unchecked_Get (Self)));
      pragma Inline_Always (Get);
      --  The resulting access must not be deallocated. Passing it to
      --  Set might also be dangerous if the Element_Type contains data
      --  that might be freed when other smart pointers are freed.
      --  It also must not be stored in a record (store Self instead).

      function Is_Null (Self : Ref'Class) return Boolean;
      pragma Inline (Is_Null);
      --  Whether the data is unset. Using this function might avoid the
      --  need for a "use type Element_Access" in your code.

      overriding function "=" (P1, P2 : Ref) return Boolean;
      pragma Inline ("=");
      --  This operator checks whether P1 and P2 share the same pointer.
      --  When the pointers differ, this operator returns False even if the
      --  two pointed elements are equal.

      function Weak (Self : Ref'Class) return Weak_Ref;
      procedure Set (Self : in out Ref'Class; Weak : Weak_Ref'Class);
      --  Set returns a reference to the object. Otherwise, it would be
      --  possible for a procedure to retrieve a pointer from the weak
      --  reference, and then reference it throughout the procedure, even
      --  though the pointer might be freed in between.
      --
      --  If Weak is Null_Weak_Ref, then the element pointed by Self simply
      --  loses a reference, and Self points to nothing on exit.

      function Was_Freed (Self : Weak_Ref'Class) return Boolean;
      --  True if the object referenced by Self was freed.

      function Get_Refcount (Self : Ref'Class) return Natural;
      --  Return the current reference count.
      --  This is mostly intended for debug purposes.

   private
      type Ref is new Ada.Finalization.Controlled with record
         Data : Element_Access;
      end record;
      pragma Finalize_Storage_Only (Ref);
      overriding procedure Adjust (Self : in out Ref);
      pragma Inline (Adjust);
      overriding procedure Finalize (Self : in out Ref);

      type Weak_Ref is new Ada.Finalization.Controlled with record
         Data : Weak_Data_Access;
      end record;
      pragma Finalize_Storage_Only (Weak_Ref);
      overriding procedure Adjust (Self : in out Weak_Ref);
      pragma Inline (Adjust);
      overriding procedure Finalize (Self : in out Weak_Ref);

      Null_Ref : constant Ref :=
         (Ada.Finalization.Controlled with Data => null);
      Null_Weak_Ref : constant Weak_Ref :=
         (Ada.Finalization.Controlled with Data => null);
   end Shared_Pointers;

   --------------------
   -- Smart_Pointers --
   --------------------
   --  For backward compatibility only. The above package is more flexible
   --  and more efficient.

   type Refcounted is abstract tagged private;
   type Refcounted_Access is access all Refcounted'Class;
   --  The common ancestor for all refcounted types.
   --  This ancestor adds a refcount field, which keeps track of how many
   --  references exist to a particular instance of Refcounted.
   --
   --  The refcounting is task safe (that is you can use the smart pointer from
   --  multiple tasks concurrently, and the refcounting will always be
   --  accurate). But the task-safety of Refcounted itself depends on your
   --  application.

   procedure Free (Self : in out Refcounted) is null;
   --  Free the memory associated with Self, when Self is no longer referenced.

   generic
      type Encapsulated is abstract new Refcounted with private;
   package Smart_Pointers is
      pragma Obsolescent (Smart_Pointers, "Use Shared_Pointers instead");

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
