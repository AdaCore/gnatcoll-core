-----------------------------------------------------------------------
--                          M O D E L I N G                          --
--                                                                   --
--                 Copyright (C) 2010-2011, AdaCore                  --
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

with Ada.Unchecked_Deallocation;
with GNATCOLL.Refcount;           use GNATCOLL.Refcount;
with GNATCOLL.Refcount.Weakref;   use GNATCOLL.Refcount.Weakref;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with Interfaces;                  use Interfaces;

package body GNATCOLL.Pools is
   use Pointers;

   Me : constant Trace_Handle := Create ("Pools");

   type Pool_Array is array (Positive range <>) of Pool_Resource_Access;
   type Pool_Array_Access is access Pool_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Pool_Resource, Pool_Resource_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Pool_Array, Pool_Array_Access);

   protected type Pool is
      entry Get (Element : out Resource'Class);
      --  Get one resource
      --  You must have called Set_Factory before.
      --  The resource must be released explicitly by calling Release, or
      --  there will be starvation

      procedure Release (In_Pool : in out Pool_Resource_Access);
      --  Release the resource, and make it available to others.
      --  In_Pool might have been freed on exit

      procedure Set_Factory
        (Descr        : Factory_Param;
         Max_Elements : Positive);
      --  Describe how to connect to the database. This can be called only
      --  once ie before getting the first connection

      procedure Free;
      --  Detach all resources from the pool.
      --  If they are in use elsewhere they will not be freed immediately, only
      --  when they are no longer in use.

   private
      Available : aliased Integer_32 := 0;
      Elements  : Pool_Array_Access;
      Param     : Factory_Param;
   end Pool;

   protected body Pool is

      -----------------
      -- Set_Factory --
      -----------------

      procedure Set_Factory
        (Descr        : Factory_Param;
         Max_Elements : Positive)
      is
      begin
         Assert (Me, Elements = null, "Set_Factory can be called only once");
         Available := Integer_32 (Max_Elements);
         Elements  := new Pool_Array'(1 .. Max_Elements => null);
         Param     := Descr;
      end Set_Factory;

      ---------
      -- Get --
      ---------

      entry Get (Element : out Resource'Class)
        when Available > 0
      is
         In_Pool : Pointers.Encapsulated_Access;
      begin
         Available := Available - 1;

         --  Get the first available resource. Since they are allocated
         --  sequentially, this ensures that we preferably reuse an existing
         --  connection rather than create a new one.

         for E in Elements'Range loop
            if Elements (E) = null then
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

               Elements (E) := new Pool_Resource'
                 (Element           => Factory (Param),
                  Available         => False);

               In_Pool := new Resource_Data'
                 (Weak_Refcounted with In_Pool => Elements (E));
               Set (Element, In_Pool);
               return;

            elsif Elements (E).Available then
               Trace (Me, "Get: returning resources at index" & E'Img);
               Elements (E).Available         := False;

               In_Pool := new Resource_Data'
                 (Weak_Refcounted with In_Pool => Elements (E));
               Set (Element, In_Pool);
               return;
            end if;
         end loop;

         --  The entry guard said we had an available resource
         raise Program_Error with "A resource should have been available";
      end Get;

      -------------
      -- Release --
      -------------

      procedure Release (In_Pool : in out Pool_Resource_Access) is
      begin
         --  Nothing to do after the pool itself has been freed.
         --  Normal reference counting will take place

         if Elements /= null then
            Trace (Me, "Released one resource");
            In_Pool.Available := True;
            Available := Available + 1;
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
      begin
         Increase_Indent (Me, "Global_Pool.Free");

         if Elements /= null then
            for E in Elements'Range loop
               if Elements (E) /= null
                 and then Elements (E).Available
               then
                  Trace (Me, "Freeing a resource");
                  Free (Elements (E).Element);
                  Unchecked_Free (Elements (E));
               elsif Elements (E) /= null then
                  Trace (Me, "One ressource still in use, can't be freed");
               end if;
            end loop;

            Free_Param (Param);
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
      Enc : constant Encapsulated_Access := Get (Self);
   begin
      Assert (Me, Enc /= null,
              "A wrapper should not exist without an element");
      return Enc.In_Pool.Element'Access;
   end Element;

   ---------
   -- Get --
   ---------

   procedure Get (Self : out Resource'Class) is
   begin
      Global_Pool.Get (Self);
   end Get;

   --------------
   -- Get_Weak --
   --------------

   function Get_Weak (Self : Resource'Class) return Weak_Resource is
      W : Weak_Ref;
   begin
      W := Get_Weak_Ref (Self);
      return Weak_Resource'(Ref => W);
   end Get_Weak;

   ---------
   -- Get --
   ---------

   procedure Get (Self : Weak_Resource; Res : out Resource'Class) is
   begin
      Get (Self.Ref, Res);
   end Get;

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

   overriding procedure Free (Self : in out Resource_Data) is
   begin
      Free (Weak_Refcounted (Self));

      --  Call the user's callback before releasing into the pool, so that the
      --  resource doesn't get reused in the meantime.

      On_Release (Self.In_Pool.Element);

      begin
         Global_Pool.Release (Self.In_Pool);
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
     (Descr        : Factory_Param;
      Max_Elements : Positive) is
   begin
      Global_Pool.Set_Factory (Descr, Max_Elements);
   end Set_Factory;

   ------------------
   -- Get_Refcount --
   ------------------

   overriding function Get_Refcount (Self : Resource) return Natural is
   begin
      return Pointers.Pointers.Get_Refcount (Pointers.Pointers.Ref (Self));
   end Get_Refcount;

end GNATCOLL.Pools;
