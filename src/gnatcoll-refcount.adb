-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2010-2011, AdaCore                  --
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

with Ada.Unchecked_Deallocation;
with Interfaces;  use Interfaces;
with Ada.Tags;    use Ada.Tags;
with GNATCOLL.Traces; use GNATCOLL.Traces;

package body GNATCOLL.Refcount is
   Me : constant Trace_Handle := Create ("REFCOUNT", Off);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Refcounted'Class, Refcounted_Access);

   package body Sync_Counters is separate;

   --------------------
   -- Smart_Pointers --
   --------------------

   package body Smart_Pointers is

      ---------
      -- Set --
      ---------

      procedure Set (Self : in out Ref; Data : access Encapsulated'Class) is
      begin
         if Self.Data = Refcounted_Access (Data) then
            --  Avoid finalizing Self.Data if we are going to reuse it
            return;
         end if;

         if Self.Data /= null then
            Finalize (Self);  -- decrement reference count
         end if;

         if Data /= null then
            Self.Data := Refcounted_Access (Data);
            Adjust (Self);    -- increment reference count
         end if;
      end Set;

      ---------
      -- Set --
      ---------

      procedure Set (Self : in out Ref; Data : Encapsulated'Class) is
         Tmp : constant Encapsulated_Access := new Encapsulated'Class'(Data);
      begin
         Set (Self, Tmp);
      end Set;

      ---------
      -- Get --
      ---------

      function Get (P : Ref) return Encapsulated_Access is
      begin
         return Encapsulated_Access (P.Data);
      end Get;

      ---------
      -- "=" --
      ---------

      overriding function "=" (P1, P2 : Ref) return Boolean is
      begin
         return P1.Data = P2.Data;
      end "=";

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (P : in out Ref) is
         Data : Refcounted_Access := P.Data;
      begin
         --  Make Finalize idempotent, since it could be called several
         --  times for the same instance (RM 7.6.1(24)).

         P.Data := null;

         --  Test if refcount is > 0, in case we are already freeing this
         --  element.

         if Data /= null then
            if Sync_Counters.Sync_Add_And_Fetch (Data.Refcount'Access, -1) =
              0
            then
               Trace (Me, "Freeing memory for "
                      & External_Tag (Ref'Class (P)'Tag));
               Free (Data.all);
               Unchecked_Free (Data);
            end if;
         end if;
      end Finalize;

      ------------
      -- Adjust --
      ------------

      overriding procedure Adjust (P : in out Ref) is
         Dummy : Integer_32;
         pragma Unreferenced (Dummy);
      begin
         if P.Data /= null then
            Dummy := Sync_Counters.Sync_Add_And_Fetch
              (P.Data.Refcount'Access, 1);
         end if;
      end Adjust;

      ------------------
      -- Get_Refcount --
      ------------------

      function Get_Refcount (Self : Ref) return Natural is
      begin
         if Self.Data = null then
            return 0;
         else
            return Natural (Self.Data.Refcount);
         end if;
      end Get_Refcount;

   end Smart_Pointers;

end GNATCOLL.Refcount;
