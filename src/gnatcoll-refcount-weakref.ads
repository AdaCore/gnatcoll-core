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
   pragma Obsolescent (Weakref, "Use GNATCOLL.Refcount.Shared_Pointers");

   type Weak_Refcounted
     is abstract new GNATCOLL.Refcount.Refcounted with private;
   --  A special refcounted type, which can manipulate weak references

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

      procedure Set (Self : in out Ref; Data : Encapsulated'Class)
        renames Pointers.Set;
      procedure Set (Self : in out Ref; Data : access Encapsulated'Class)
        renames Pointers.Set;
      function Get (P : Ref) return Encapsulated_Access
         renames Pointers.Get;
      function "=" (P1, P2 : Ref) return Boolean
        renames Pointers."=";
      function "=" (P1, P2 : Pointers.Encapsulated_Access) return Boolean
        renames Pointers."=";
      --  The manipulation of the smart pointers

      subtype Weak_Ref is Proxy_Pointers.Ref;
      Null_Weak_Ref : constant Weak_Ref := Weak_Ref (Proxy_Pointers.Null_Ref);
      function "=" (P1, P2 : Weak_Ref) return Boolean
        renames Proxy_Pointers."=";

      function Get_Weak_Ref (Self : Ref'Class) return Weak_Ref;
      --  Return a weak reference to Self.
      --  It does not hold a reference to Self, which means that Self could be
      --  destroyed while the weak reference exists. However, this will not
      --  result
      --  in a Storage_Error when you access the reference.

      function Was_Freed (Self : Weak_Ref'Class) return Boolean;
      --  True if the weakly referenced element was freed (thus Get would
      --  return Null_Ref). It is more efficient to use this function than
      --  compare the result of Get with Null_Ref, since the latter will need
      --  to play with refcounting.

      function Get (Self : Weak_Ref'Class) return Ref;
      procedure Get (Self : Weak_Ref'Class; R : out Ref'Class);
      --  Return the weakly referenced object. This will return Null_Ref
      --  if the object has already been destroyed.
      --  The procedure version can be used if you have subclassed Ref.
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
   end Weakref_Pointers;

private

   type Weak_Refcounted
     is abstract new GNATCOLL.Refcount.Refcounted
   with record
      Proxy : Proxy_Pointers.Ref;  --  Hold a reference to a proxy
   end record;

end GNATCOLL.Refcount.Weakref;
