------------------------------------------------------------------------------
--                             M O D E L I N G                              --
--                                                                          --
--                     Copyright (C) 2010-2017, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with GNATCOLL.Refcount;           use GNATCOLL.Refcount;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with Interfaces;                  use Interfaces;

package body GNATCOLL.Pools is
   use Pointers;

   Me : constant Trace_Handle := Create ("Pools");

   type Pool_Array is array (Positive range <>) of Pool_Resource_Access;
   type Pool_Array_Access is access all Pool_Array;

   type Resource_Set_Data is record
      Elements  : Pool_Array_Access;
      Param     : aliased Factory_Param;
      Available : aliased Integer_32 := 0;
   end record;

   type Sets is array (Resource_Set range <>) of Resource_Set_Data;
   type Sets_Access is access all Sets;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Pool_Resource, Pool_Resource_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Pool_Array, Pool_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Sets, Sets_Access);

   protected type Pool is
      entry Get (Resource_Set) (Element : out Resource'Class);
      --  Get one resource
      --  You must have called Set_Factory before.
      --  The resource must be released explicitly by calling Release, or
      --  there will be starvation

      procedure Release
        (In_Pool : in out Pool_Resource_Access; Set : Resource_Set);
      --  Release the resource, and make it available to others.
      --  In_Pool might have been freed on exit

      procedure Set_Factory
        (Descr        : Factory_Param;
         Max_Elements : Positive;
         Set          : Resource_Set);
      --  Describe how to connect to the database. This can be called only
      --  once ie before getting the first connection

      procedure Free;
      --  Detach all resources from the pool.
      --  If they are in use elsewhere they will not be freed immediately, only
      --  when they are no longer in use.

      function Get_Factory_Param
        (Set : Resource_Set) return access Factory_Param;

   private
      Elements  : Sets_Access;
   end Pool;

   protected body Pool is

      -----------------
      -- Set_Factory --
      -----------------

      procedure Set_Factory
        (Descr        : Factory_Param;
         Max_Elements : Positive;
         Set          : Resource_Set) is
      begin
         if Elements = null then
            Elements := new Sets (Resource_Set'Range);
         end if;

         if Elements (Set).Elements = null then
            Elements (Set) :=
              (Elements => new Pool_Array'(1 .. Max_Elements => null),
               Available => Integer_32 (Max_Elements),
               Param     => Descr);
         else
            raise Program_Error with
              "Set_Factory can be called only once per resource_set";
         end if;
      end Set_Factory;

      -----------------------
      -- Get_Factory_Param --
      -----------------------

      function Get_Factory_Param
        (Set : Resource_Set) return access Factory_Param is
      begin
         return Elements (Set).Param'Access;
      end Get_Factory_Param;

      ---------
      -- Get --
      ---------

      entry Get (for Set in Resource_Set) (Element : out Resource'Class)
        when Elements (Set).Available > 0
      is
         In_Pool : Resource_Data;
      begin
         Elements (Set).Available := Elements (Set).Available - 1;

         --  Get the first available resource. Since they are allocated
         --  sequentially, this ensures that we preferably reuse an existing
         --  connection rather than create a new one.

         for E in Elements (Set).Elements'Range loop
            if Elements (Set).Elements (E) = null then
               --  ??? Issue: the factory might take a long time (for
               --  instance establishing a database connection). During
               --  that time, all threads waiting on Get are blocked.
               --  We should mark the slot as no longer available, and
               --  initialize the resource once returned to the user.

               Trace (Me, "Get: creating resource, at index" & E'Img);

               --  We have to cheat with the refcounting temporarily: the
               --  above call, if initialized at refcount=1, would call
               --  adjust once, and then finalize, thus try to call Release,
               --  resulting in a deadlock. Instead, we start with an
               --  off-by-one refcount, and put things back straight afterward.

               Elements (Set).Elements (E) := new Pool_Resource'
                 (Element           => Factory (Elements (Set).Param),
                  Available         => False);

               In_Pool := Resource_Data'
                 (Set    => Set,
                  In_Set => Elements (Set).Elements (E));
               Element.Set (In_Pool);
               return;

            elsif Elements (Set).Elements (E).Available then
               if Active (Me) then
                  Trace (Me, "Get: pool " & Set'Img
                         & " returning resources at index" & E'Img);
               end if;
               Elements (Set).Elements (E).Available         := False;

               In_Pool := Resource_Data'
                 (Set    => Set,
                  In_Set => Elements (Set).Elements (E));
               Element.Set (In_Pool);
               return;
            end if;
         end loop;

         --  The entry guard said we had an available resource
         raise Program_Error with "A resource should have been available";
      end Get;

      -------------
      -- Release --
      -------------

      procedure Release
        (In_Pool : in out Pool_Resource_Access; Set : Resource_Set)
      is
      begin
         --  Nothing to do after the pool itself has been freed.
         --  Normal reference counting will take place

         if Elements /= null then
            Trace (Me, "Released one resource");
            In_Pool.Available := True;
            Elements (Set).Available := Elements (Set).Available + 1;
         else
            --  The pool has been destroyed and the resource is no longer used.
            --  Simply free it.

            Free (In_Pool.Element);
            Unchecked_Free (In_Pool);
         end if;
      end Release;

      ----------
      -- Free --
      ----------

      procedure Free is
         R : Pool_Resource_Access;
      begin
         Increase_Indent (Me, "Global_Pool.Free");

         if Elements /= null then
            for Set in Elements'Range loop
               if Elements (Set).Elements /= null then
                  for E in Elements (Set).Elements'Range loop
                     R := Elements (Set).Elements (E);

                     if R /= null
                       and then R.Available
                     then
                        Trace (Me, "Freeing a resource");
                        Free (R.Element);
                        Unchecked_Free (R);
                     elsif R /= null then
                        Trace
                          (Me, "One ressource still in use, can't be freed");
                     end if;
                  end loop;

                  Free_Param (Elements (Set).Param);
                  Unchecked_Free (Elements (Set).Elements);
               end if;
            end loop;

            Unchecked_Free (Elements);
         end if;

         Decrease_Indent (Me, "Done Global_Pool.Free");
      end Free;
   end Pool;

   Global_Pool : Pool;
   --  a global pool
   --  This is task safe.

   -------------
   -- Element --
   -------------

   function Element (Self : Resource) return access Element_Type is
      Enc : access Resource_Data := Get (Self).Element;
   begin
      Assert (Me, Enc /= null,
              "A wrapper should not exist without an element");
      return Enc.In_Set.Element'Access;
   end Element;

   ---------
   -- Get --
   ---------

   procedure Get
     (Self : out Resource'Class; Set : Resource_Set := Default_Set) is
   begin
      Global_Pool.Get (Set) (Self);
   end Get;

   --------------
   -- Get_Weak --
   --------------

   function Get_Weak (Self : Resource'Class) return Weak_Resource is
   begin
      return Weak_Resource'(Ref => Self.Weak);
   end Get_Weak;

   ---------
   -- Get --
   ---------

   procedure Get (Self : Weak_Resource; Res : out Resource) is
   begin
      Res.Set (Self.Ref);
   end Get;

   ---------------
   -- Was_Freed --
   ---------------

   function Was_Freed (Self : Weak_Resource) return Boolean is
   begin
      return Pointers.Was_Freed (Self.Ref);
   end Was_Freed;

   ----------
   -- Free --
   ----------

   procedure Free is
   begin
      Global_Pool.Free;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Resource_Data) is
   begin
      --  Call the user's callback before releasing into the pool, so that the
      --  resource doesn't get reused in the meantime.

      On_Release (Self.In_Set.Element);

      begin
         Global_Pool.Release (Self.In_Set, Self.Set);
      exception
         when E : Program_Error =>
            Trace (Me, "Global pool was already finalized");
            Trace (Me, E);
      end;
   end Free;

   -----------------
   -- Set_Factory --
   -----------------

   procedure Set_Factory
     (Param        : Factory_Param;
      Max_Elements : Positive;
      Set          : Resource_Set := Default_Set) is
   begin
      Global_Pool.Set_Factory (Param, Max_Elements, Set);
   end Set_Factory;

   -----------------------
   -- Get_Factory_Param --
   -----------------------

   function Get_Factory_Param
     (Set : Resource_Set := Default_Set) return access Factory_Param is
   begin
      return Global_Pool.Get_Factory_Param (Set);
   end Get_Factory_Param;

   ------------------
   -- Get_Refcount --
   ------------------

   function Get_Refcount (Self : Resource) return Natural is
   begin
      return Pointers.Get_Refcount (Self);
   end Get_Refcount;

end GNATCOLL.Pools;
