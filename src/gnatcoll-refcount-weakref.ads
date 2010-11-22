-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2010, AdaCore                       --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- As a special exception, if other files instantiate generics  from --
-- this unit, or you link this  unit with other files to produce  an --
-- executable, this unit does not by itself cause the resulting exe- --
-- cutable  to be covered by  the  GNU General  Public License. This --
-- exception does not however  invalidate any other reasons why  the --
-- executable  file  might  be  covered  by  the  GNU General Public --
-- License.                                                          --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  By definition, an object can never be freed while there are references
--  to it.
--  However, this simple scheme fails in some cases. For instance, imagine
--  you want to cache some refcounted type into a map. The map would then
--  own a reference to the object, which is thus never freed while the map
--  exists (presumably for the life of your application).
--  A solution to this problem is the notion of "weak reference": these act
--  as containers that point to the element, without owning a reference to
--  them. When the element is destroyed (because its refcount can now reach
--  0), the container is set to a special state that indicates the element
--  no longer exists.
--  With this scheme, the cache will still contain entries for the elements,
--  but those entries will return a Null_Ref when accessed.

pragma Ada_05;

package GNATCOLL.Refcount.Weakref is

   type Weak_Refcounted
     is abstract new GNATCOLL.Refcount.Refcounted with private;
   --  A special refcounted type, which can manipulate weak references.

   overriding procedure Free (Self : in out Weak_Refcounted);
   --  If you need to override this procedure in your own code, you need to
   --  make sure you correctly call this inherited procedure.

   type Proxy is new GNATCOLL.Refcount.Refcounted with record
      Proxied : Refcounted_Access;
   end record;
   package Proxy_Pointers is new Smart_Pointers (Proxy);
   --  An internal, implementation type.
   --
   --  A weak ref acts as a smart pointed with two level of indirection:
   --      type My_Type is new GNATCOLL.Refcount.Weakref.Refcounted with ...;
   --      package P is new Weakref_Pointers (My_Type);
   --      R  : P.Ref;
   --      WR : P.Weak_Ref;
   --  R now takes care of the reference counting for R.Data.
   --  R.Data is an access to My_Type, freed automatically.
   --
   --  WR now takes care of the reference counting for a Proxy, whose Proxied
   --  is set to R.Data. This does not hold a reference to R.Data. However,
   --  R.Data holds a reference to the proxy.
   --  As a result, the proxy is never freed while R.Data exists. But the
   --  latter can be destroyed even when the proxy exists.

   generic
      type Encapsulated is abstract new Weak_Refcounted with private;
   package Weakref_Pointers is
      package Pointers is new Smart_Pointers (Encapsulated);
      subtype Encapsulated_Access is Pointers.Encapsulated_Access;

      subtype Ref is Pointers.Ref;
      Null_Ref : constant Ref := Pointers.Null_Ref;

      function Allocate (Data : Encapsulated'Class) return Ref
        renames Pointers.Allocate;
      function Allocate (Data : access Encapsulated'Class) return Ref
        renames Pointers.Allocate;
      function Get (P : Ref) return Encapsulated_Access
        renames Pointers.Get;
      function "=" (P1, P2 : Ref) return Boolean
        renames Pointers."=";
      --  The manipulation of the smart pointers

      type Weak_Ref is private;
      Null_Weak_Ref : constant Weak_Ref;

      function Get_Weak_Ref (Self : Ref) return Weak_Ref;
      --  Return a weak reference to Self.
      --  It does not hold a reference to Self, which means that Self could be
      --  destroyed while the weak reference exists. However, this will not
      --  result
      --  in a Storage_Error when you access the reference.

      function Get (Self : Weak_Ref) return Ref;
      --  Return the weakly referenced object. This will return Null_Ref
      --  if the object has already been destroyed.
      --  The code should look like:
      --
      --      --  Create the smart pointer
      --      Tmp : Refcounted_Access := new My_Refcounted_Type;
      --      R   : Ref := Allocate (Tmp);  --  Hold a ref to Tmp
      --
      --      WRef := Get_Weak_Ref (R);  -- Does not hold a ref to Tmp
      --
      --      R := Null_Ref;   --  Releases ref to Tmp, and free Tmp
      --      we now have Get (WRef) = null
      --
      --  In the case of a multitasking application, you must write your code
      --  so that the referenced type is not freed while you are using it. For
      --  instance:
      --      declare
      --         R : constant Ref := Get (WRef);  --  hold a ref to Tmp
      --      begin
      --         if R /= Null_Ref then
      --             ... manipulate R
      --             Tmp cannot be freed while in the declare block, since we
      --             own a reference to it
      --         end if;
      --      end;

   private
      type Weak_Ref is new Proxy_Pointers.Ref;
      Null_Weak_Ref : constant Weak_Ref := Weak_Ref (Proxy_Pointers.Null_Ref);
   end Weakref_Pointers;

private
   type Weak_Refcounted
     is abstract new GNATCOLL.Refcount.Refcounted with record
      Proxy : Proxy_Pointers.Ref;  --  Hold a reference to a proxy
   end record;
end GNATCOLL.Refcount.Weakref;
