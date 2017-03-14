------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2017, AdaCore                          --
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

with Ada.Containers.Vectors;
with Ada.Exceptions;     use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;       use GNAT.Strings;
with GNATCOLL.Atomic;    use GNATCOLL.Atomic;

package body GNATCOLL.Promises is

   use type Impl.Promise_Callback_Access;

   package Cb_Vectors is new Ada.Containers.Vectors
      (Positive, Impl.Promise_Callback_Access, Impl."=");

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Freeable_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (IFreeable'Class, Freeable_Access);
   begin
      if Self /= null then
         Free (Self.all);
         Unchecked_Free (Self);
      end if;
   end Free;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe (Self : Promise_Chain) is
   begin
      null;
   end Subscribe;

   ----------
   -- Impl --
   ----------

   package body Impl is

      -------------------
      -- Dispatch_Free --
      -------------------

      procedure Dispatch_Free (Self : in out IPromise_Data'Class) is
      begin
         Free (Self);
      end Dispatch_Free;

   end Impl;

   --------------
   -- Promises --
   --------------

   package body Promises is

      type T_Access is access all T;

      type Promise_Data is new Impl.IPromise_Data with record
         State     : aliased Promise_State := Pending;

         Callbacks : Cb_Vectors.Vector;
         --  Need a vector here, but should try to limit memory allocs.
         --  A bounded vector might be more efficient, and sufficient in
         --  practice.

         Value     : T_Access;
         --  ??? Using the ada-traits-containers approach, we could avoid
         --  some memory allocation here.

         Reason    : GNAT.Strings.String_Access;
      end record;
      type Promise_Data_Access is access all Promise_Data'Class;

      overriding procedure Free (Self : in out Promise_Data);

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (T, T_Access);

      ---------------
      -- Get_State --
      ---------------

      function Get_State (Self : Promise'Class) return Actual_Promise_State is
         D : constant not null access Promise_Data'Class :=
            Promise_Data_Access (Impl.Promise_Pointers.Unchecked_Get (Self));
      begin
         return Actual_Promise_State (D.State);
      end Get_State;

      ------------
      -- Create --
      ------------

      function Create return Promise is
      begin
         return P : Promise do
            P.Set
               (Data => Promise_Data'
                  (Callbacks => Cb_Vectors.Empty_Vector,
                   State     => Pending,
                   Value     => null,
                   Reason    => null));
         end return;
      end Create;

      ----------
      -- Free --
      ----------

      overriding procedure Free (Self : in out Promise_Data) is
      begin
         Unchecked_Free (Self.Value);
         Free (Self.Reason);
      end Free;

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value (Self : in out Promise; R : T) is
         D : constant not null access Promise_Data'Class :=
            Promise_Data_Access (Impl.Promise_Pointers.Unchecked_Get (Self));
         Old : Actual_Promise_State;
      begin
         loop
            Old := Actual_Promise_State
               (Sync_Val_Compare_And_Swap_Counter
                  (Ptr    => D.State'Access,
                   Oldval => Pending,
                   Newval => Resolving));
            case Old is
               when Resolved | Failed =>
                  --  Promise has already been completed, this is an error
                  return;

               when Resolving | Failing | Subscribing =>
                  --  Try again
                  null;

               when Pending =>
                  --  OK, we can change the state
                  for Cb of D.Callbacks loop
                     Callback_Access (Cb).On_Next (R);
                     Free (Freeable_Access (Cb));
                  end loop;

                  D.Callbacks.Clear;   --  No longer needed, release them
                  D.Value := new T'(R);
                  D.State := Resolved;  --  Fully resolved now
                  exit;
            end case;
         end loop;
      end Set_Value;

      ---------------
      -- Set_Error --
      ---------------

      procedure Set_Error (Self : in out Promise; Reason : String) is
         D : constant not null access Promise_Data'Class :=
            Promise_Data_Access (Impl.Promise_Pointers.Unchecked_Get (Self));
         Old : Actual_Promise_State;
      begin
         loop
            Old := Actual_Promise_State
               (Sync_Val_Compare_And_Swap_Counter
                  (Ptr    => D.State'Access,
                   Oldval => Pending,
                   Newval => Failing));
            case Old is
               when Resolved | Failed =>
                  --  Promise has already been completed, this is an error
                  return;

               when Resolving | Failing | Subscribing =>
                  --  Try again
                  null;

               when Pending =>
                  --  OK, we can change the state
                  for Cb of D.Callbacks loop
                     Callback_Access (Cb).On_Error (Reason);
                     Free (Freeable_Access (Cb));
                  end loop;

                  D.Callbacks.Clear;   --  No longer needed, release them
                  D.Reason := new String'(Reason);
                  D.State := Failed;  --  Fully failed now
                  exit;
            end case;
         end loop;
      end Set_Error;

      ---------------
      -- Subscribe --
      ---------------

      procedure Subscribe
        (Self : Promise; Cb : not null access Callback'Class)
      is
         D : constant not null access Promise_Data'Class :=
            Promise_Data_Access (Impl.Promise_Pointers.Unchecked_Get (Self));

         --  ??? Unrestricted_Access is temporary, so that user can
         --  use "new Cb" directly in the call to Subscribe.
         C : Callback_Access := Cb.all'Unrestricted_Access;
         Old : Actual_Promise_State;
      begin
         loop
            Old := Actual_Promise_State
               (Sync_Val_Compare_And_Swap_Counter
                  (Ptr    => D.State'Access,
                   Oldval => Pending,
                   Newval => Subscribing));
            case Old is
               when Resolving | Failing | Subscribing =>
                  --  Try again
                  null;

               when Resolved =>
                  --  We don't need to change D, so we leave the state to
                  --  Pending
                  C.On_Next (D.Value.all);
                  Free (Freeable_Access (C));
                  return;

               when Failed =>
                  C.On_Error (D.Reason.all);
                  Free (Freeable_Access (C));
                  return;

               when Pending =>
                  D.Callbacks.Append (Impl.Promise_Callback_Access (C));
                  D.State := Pending;
                  exit;
            end case;
         end loop;
      end Subscribe;

      -----------
      -- "and" --
      -----------

      function "and"
         (Self : Promise; Cb : Callback_List)
         return Promise_Chain is
      begin
         for C of Cb loop
            Self.Subscribe (C);
         end loop;
         return Promise_Chain'(null record);
      end "and";

      -----------
      -- "and" --
      -----------

      function "and"
         (Self  : Promise;
          Cb    : not null access Callback'Class)
         return Promise_Chain is
      begin
         Self.Subscribe (Cb);
         return Promise_Chain'(null record);
      end "and";

      ---------
      -- "&" --
      ---------

      function "&"
        (Cb    : not null access Callback'Class;
         Cb2   : not null access Callback'Class)
        return Callback_List is
      begin
         return (Cb.all'Unrestricted_Access,
                 Cb2.all'Unrestricted_Access);
      end "&";

      ---------
      -- "&" --
      ---------

      function "&"
        (List  : Callback_List;
         Cb2   : not null access Callback'Class)
        return Callback_List is
      begin
         return List & (1 => Cb2.all'Unrestricted_Access);
      end "&";

   end Promises;

   ------------
   -- Chains --
   ------------

   package body Chains is

      -----------
      -- "and" --
      -----------

      function "and"
         (Input : Input_Promises.Promise;
          Cb    : not null access Callback'Class)
         return Output_Promises.Promise is
      begin
         Cb.Promise := Output_Promises.Create;
         Input_Promises.Subscribe (Input, Cb.all'Unrestricted_Access);
         return Cb.Promise;
      end "and";

      -----------
      -- "and" --
      -----------

      function "and"
        (Input : Input_Promises.Promise;
         Cb    : Callback_List)
        return Output_Promises.Promise
      is
         P : constant Output_Promises.Promise := Input and Cb.Cb;
      begin
         for C of Cb.Cb2 loop
            Input_Promises.Subscribe (Input, C);
         end loop;
         return P;
      end "and";

      -------------------
      -- Is_Registered --
      -------------------

      function Is_Registered
         (Self : not null access Callback'Class) return Boolean is
      begin
         return Self.Promise.Is_Created;
      end Is_Registered;

      -------------------
      -- Is_Registered --
      -------------------

      function Is_Registered
         (Self : Callback_List) return Boolean is
      begin
         return Self.Cb.Promise.Is_Created;
      end Is_Registered;

      -------------
      -- On_Next --
      -------------

      overriding procedure On_Next
         (Self : in out Callback; P : Input_Promises.Result_Type) is
      begin
         On_Next (Callback'Class (Self), P, Self.Promise);
      exception
         when E : others =>
            Self.Promise.Set_Error (Exception_Message (E));
      end On_Next;

      --------------
      -- On_Error --
      --------------

      overriding procedure On_Error
         (Self : in out Callback; Reason : String) is
      begin
         --  Propagate the failure
         Self.Promise.Set_Error (Reason);
      end On_Error;

      -----------
      -- "&" --
      -----------

      function "&"
         (Cb   : not null access Callback'Class;
          Cb2  : not null access Input_Promises.Callback'Class)
         return Callback_List is
      begin
         return Callback_List'
            (N   => 1,
             Cb  => Cb.all'Unrestricted_Access,
             Cb2 => (1 => Cb2.all'Unrestricted_Access));
      end "&";

      -----------
      -- "&" --
      -----------

      function "&"
         (List : Callback_List;
          Cb2  : not null access Input_Promises.Callback'Class)
         return Callback_List is
      begin
         return Callback_List'
            (N   => List.N + 1,
             Cb  => List.Cb,
             Cb2 => List.Cb2 & (1 => Cb2.all'Unrestricted_Access));
      end "&";

   end Chains;

end GNATCOLL.Promises;
