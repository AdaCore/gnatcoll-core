------------------------------------------------------------------------------
--                             M O D E L I N G                              --
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

--  This package implements resource pools.
--  The resources are created once (the first time they are needed). The
--  application can then get a temporary exclusive handle on a resource (ie if
--  another part of the application is also requesting a resource, it will in
--  fact retrieve another instance). When the resource is no longer used by the
--  application, it is automatically released into the pool, and will be reused
--  the next time the application requests a resource.
--
--  A typical usage is when the resource creation is expensive, such as a pool
--  of database connections.
--
--  Each instantiation of this package provides one task-safe global pool.
--  However, this pool can contains multiple sets of elements. Each sets has
--  its own factory parameter. The subprograms will by default apply to the
--  first such subset, since in most cases you will only need one such subset
--  in your application. An example where multiple subsets might be needed is
--  for instance when we have a pool of database connections. We would for
--  instance one subset for each DBMS we want to connect to it. Each subset
--  then contains a number of connections to that specific DBMS.
--  Of course, this pattern could be implemented by having multiple
--  instantiations of GNATCOLL.Pools, but this makes the API more complex and
--  forces the duplication of the whole GNATCOLL.SQL.Session API.

pragma Ada_05;

private with GNATCOLL.Refcount.Weakref;

generic
   type Element_Type is private;
   --  The elements that are pooled

   type Factory_Param is private;
   with function Factory (Param : Factory_Param) return Element_Type;
   --  Information needed to create new elements as needed. This is passed as
   --  is to the Factory function.

   type Resource_Set is (<>);
   --  Represents a set of elements within the pool.
   --  There can be multiple such sets in one pool. Each set is associated with
   --  its own factory parameter, but all other elements are compatible between
   --  the various sets (in particular, the resources are the same, so the rest
   --  of the application doesn't need to know which set this resource is
   --  from).
   --  Most times, a pool will need only one such subset, which is created by
   --  default. All subprograms below apply to this default set, unless
   --  otherwise specified.

   with procedure Free (Self : in out Element_Type) is null;
   --  Called when the [Self] is finally removed from the pool

   with procedure On_Release (Self : in out Element_Type) is null;
   --  Called when Self is released into the pool.
   --  The application has no more reference to that element, apart from the
   --  one in the pool.
   --  The result of Element.Element should not be freed yet, since it is
   --  returned to the pool (instead, override the formal [Free] parameter).
   --  But any other custom field from Element should be reset at that time.

   with procedure Free_Param (Data : in out Factory_Param) is null;
   --  Free Factory_Param.
   --  Called when the pool itself is freed.

package GNATCOLL.Pools is

   Default_Set : constant Resource_Set;

   type Resource is tagged private;
   No_Resource : constant Resource;
   --  A resource retrieved from the pool.
   --  This is a smart pointer to an Element_Type. When your application has no
   --  more references to it, the Element_Type is released into the pool (not
   --  destroyed).
   --  The resource itself does its refcounting in a task-safe manner.

   function Element (Self : Resource) return access Element_Type;
   --  Get a copy of the element stored in the wrapper. The result should
   --  really only be used while you have a handle on Self, so that you are
   --  sure it has not been released into the pool, and thus reset.

   type Weak_Resource is private;
   Null_Weak_Resource : constant Weak_Resource;
   function Get_Weak (Self : Resource'Class) return Weak_Resource;
   procedure Get (Self : Weak_Resource; Res : out Resource'Class);
   --  A resource with a weak-reference.
   --  Such a resource does not prevent the release into the pool when no other
   --  Resource exists. While the resource has not been released, you can get
   --  access to it through this Weak_Resource. One it has been released, the
   --  Weak_Resource will return No_Resource.
   --  This datatype can thus be stored in some long-lived data structure, if
   --  you do not want to prevent the release. For instance if you have a
   --  cache of some sort.

   procedure Set_Factory
     (Param        : Factory_Param;
      Max_Elements : Positive;
      Set          : Resource_Set := Default_Set);
   --  Configure the internal resource pool. This must be called before
   --  calling Get, and only once.

   procedure Get
     (Self : out Resource'Class;
      Set  : Resource_Set := Default_Set);
   --  Return an available resource (or create a new one if the pool is not
   --  full yet and none is available).
   --  In a multitasking context, this blocks until a resource is actually
   --  available.
   --  The resource is automatically released when you no longer have a
   --  reference to the wrapper.

   procedure Free;
   --  Detach all resources from the pool.
   --  Any resource that is not in use elsewhere (ie retrieved by Get) will
   --  get freed (and the corresponding [Free] formal subprogram will be
   --  called).

   function Get_Refcount (Self : Resource) return Natural;
   --  Return the reference counting for self

   function Get_Factory_Param
     (Set : Resource_Set := Default_Set) return access Factory_Param;
   --  Returns a the factory param used for the set.
   --  Remember that the factory will not be called again for resources that
   --  have already been created, even if they have been released to the pool
   --  since then.
   --  This must only be called when you have called Set_Factory for the
   --  corresponding set.
   --  The returned value is shared among multiple threads, so you should only
   --  modify it from a protected region or before tasks are created.

private

   type Pool_Resource is record
      Element           : aliased Element_Type;
      Available         : Boolean;  --  Is the resource available ?
   end record;
   type Pool_Resource_Access is access all Pool_Resource;
   --  The data stored in the pool.
   --  These are not smart pointers, which are created on demand in Get.

   Default_Set : constant Resource_Set := Resource_Set'First;

   type Resource_Data is new GNATCOLL.Refcount.Weakref.Weak_Refcounted with
      record
         In_Set : Pool_Resource_Access;
         Set    : Resource_Set;
      end record;
   overriding procedure Free (Self : in out Resource_Data);
   package Pointers is new GNATCOLL.Refcount.Weakref.Weakref_Pointers
     (Resource_Data);
   --  The smart pointers returned to the application. When no longer
   --  referenced, the resource is released back into the pool.

   type Resource is new Pointers.Ref with null record;
   No_Resource : constant Resource :=
     Resource'(Pointers.Null_Ref with null record);

   type Weak_Resource is record
      Ref : Pointers.Weak_Ref;
   end record;

   Null_Weak_Resource : constant Weak_Resource :=
     (Ref => Pointers.Null_Weak_Ref);

end GNATCOLL.Pools;
