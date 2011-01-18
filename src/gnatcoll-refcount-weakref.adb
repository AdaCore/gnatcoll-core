-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                    Copyright (C) 2010-2011, AdaCore               --
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

pragma Ada_05;
with Interfaces;  use Interfaces;

package body GNATCOLL.Refcount.Weakref is
   use Proxy_Pointers;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Weak_Refcounted) is
   begin
      if Self.Proxy /= Proxy_Pointers.Null_Ref then
         Proxy (Get (Self.Proxy).all).Proxied := null;
         Self.Proxy := Proxy_Pointers.Null_Ref;
      end if;
      Free (Refcounted (Self));   --  ??? static call to a "null" procedure
   end Free;

   ----------------------
   -- Weakref_Pointers --
   ----------------------

   package body Weakref_Pointers is
      use Pointers;

      ------------------
      -- Get_Weak_Ref --
      ------------------

      function Get_Weak_Ref (Self : Ref'Class) return Weak_Ref is
         Data : constant Encapsulated_Access := Self.Get;
         P    : Proxy_Pointers.Ref;
         D    : Proxy_Pointers.Encapsulated_Access;
      begin
         if Data = null then
            return Null_Weak_Ref;
         end if;

         P := GNATCOLL.Refcount.Weakref.Weak_Refcounted'Class (Data.all).Proxy;
         if P = Proxy_Pointers.Null_Ref then
            D := new Proxy'(GNATCOLL.Refcount.Refcounted
                            with Proxied => Refcounted_Access (Data));
            Set (P, D);  --  now owns a reference to D
            Weak_Refcounted'Class (Data.all).Proxy := P;
         end if;

         return Weak_Ref (P);
      end Get_Weak_Ref;

      ---------------
      -- Was_Freed --
      ---------------

      function Was_Freed (Self : Weak_Ref'Class) return Boolean is
         P : constant access Proxy :=
           Proxy_Pointers.Get (Proxy_Pointers.Ref (Self));
      begin
         return P = null or else P.Proxied = null;
      end Was_Freed;

      ---------
      -- Get --
      ---------

      procedure Get (Self : Weak_Ref'Class; R : out Ref'Class) is
         P : constant access Proxy :=
           Proxy_Pointers.Get (Proxy_Pointers.Ref (Self));
      begin
         if Was_Freed (Self) then
            R.Set (null);
         else
            --  A subtetly here: it is possible that the element is actually
            --  being freed, and Free() is calling Get on one of the weakref.
            --  In such a case, we do not want to resuscitate the element

            if P.Proxied.Refcount = 0 then
               R.Set (null);
            else
               --  Adds a reference to P.Proxied
               R.Set (Encapsulated_Access (P.Proxied));
            end if;
         end if;
      end Get;

      ---------
      -- Get --
      ---------

      function Get (Self : Weak_Ref'Class) return Ref is
         Result : Ref;
      begin
         Get (Self, Result);
         return Result;
      end Get;

   end Weakref_Pointers;

end GNATCOLL.Refcount.Weakref;
