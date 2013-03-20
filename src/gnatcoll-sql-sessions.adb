------------------------------------------------------------------------------
--                             M O D E L I N G                              --
--                                                                          --
--                     Copyright (C) 2010-2013, AdaCore                     --
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

--  ??? Other implementations: no pooling (connection/close each time)
--  ??? From sqlalchemy, a parameter use_threadlocal to always return the same
--      connection when one has been checked out in the thread already.
--  ??? sqlalchemy allows overflows in pools
--  ??? sqlalchemy provides a SingletonThreadPool where a connection is only
--      returned in the thread that was used to create it (for sqlite)

with Ada.Containers.Doubly_Linked_Lists;
use Ada.Containers;
with Ada.Unchecked_Deallocation;
with GNATCOLL.SQL;        use GNATCOLL.SQL;
with GNATCOLL.SQL.Orm;
with GNATCOLL.Traces;     use GNATCOLL.Traces;
with GNATCOLL.Utils;      use GNATCOLL.Utils;

package body GNATCOLL.SQL.Sessions is
   Me : constant Trace_Handle := Create ("Session", Off);
   Me_Info : constant Trace_Handle := Create ("Session.Info");

   use Element_Maps, Weak_Element_Maps, Element_Lists, Pointers;

   Default_Fact              : Element_Factory := Null_Factory'Access;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Detached_Element'Class, Detached_Element_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (User_Data'Class, User_Data_Access);

   function Is_Dirty (Data : Detached_Data_Access) return Boolean;
   pragma Inline (Is_Dirty);
   --  Return True if Data has been modified in memory, and not synchronized
   --  yet with the database

   type Null_Element is new Detached_Element with null record;
   overriding procedure Insert_Or_Update
     (Self        : in out Null_Element;
      PK_Modified : in out Boolean;
      Dirty       : Dirty_Mask) is null;
   overriding procedure Internal_Delete (Self : Null_Element) is null;
   --  A special, always uninitialized element

   procedure Clear_Cache (Data : in out Session_Data);
   --  Clear the cache, releasing memory as appropriate

   function Image (K : Element_Key) return String;
   procedure Trace_Debug
     (Me  : GNATCOLL.Traces.Trace_Handle;
      K   : Weak_Cache;
      Msg : String := "");
   procedure Trace_Debug
     (Me  : GNATCOLL.Traces.Trace_Handle;
      E   : Detached_Element_Access;
      Msg : String := "");
   --  Print information on C

   package Hash_Lists is new Ada.Containers.Doubly_Linked_Lists
      (Ada.Containers.Hash_Type);
   use Hash_Lists;

   procedure Add_To_Cache (Self : Session_Type; E : Detached_Element'Class);
   --  Add the element into the cache. We want the element to outlive the
   --  session (so that we can find out all changes when committing the
   --  session), so we store a real reference.

   -----------------
   -- Ref_Or_Weak --
   -----------------

   function Get_Data (Self : Weak_Cache) return Detached_Data_Access;
   function Get_Data
     (Self : Detached_Element_Access) return Detached_Data_Access;
   pragma Inline (Get_Data);
   --  Returned the element data stored in Self

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Self : Weak_Cache) return Detached_Data_Access is
   begin
      return Detached_Data_Access (Get (Self.Ref).Get);
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Self : Detached_Element_Access) return Detached_Data_Access is
   begin
      return Detached_Data_Access (Self.Get);
   end Get_Data;

   ------------------
   -- Impl_Factory --
   ------------------

   function Impl_Factory (Data : Pool_Data) return Session_Data is
      DB : constant Database_Connection := Data.Descr.Build_Connection;
   begin
      Assert
        (Me, DB /= null,
         "Could not connect to database. Wrong dbtype set in settings ?");

      return (DB                    => DB,
              Pool                  => Data.Pool,
              Wcache                => Weak_Element_Maps.Empty_Map,
              Cache                 => Element_Maps.Empty_Map,
              Modified_Elements     => Element_Lists.Empty_List,
              User                  => null,
              Store_Unmodified      => Data.Config_Store_Unmodified,
              Weak_Cache            => Data.Config_Weak_Cache,
              Persist_Cascade       => Data.Config_Persist_Cascade,
              Flush_Before_Query    => Data.Config_Flush_Before_Query,
              Factory               => Default_Fact);
   end Impl_Factory;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Pool_Data) is
   begin
      Free (Data.Descr);

      if Data.Config_Default_User_Data /= null then
         Free (Data.Config_Default_User_Data.all);
         Unchecked_Free (Data.Config_Default_User_Data);
      end if;
   end Free;

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache (Data : in out Session_Data) is
      WC   : Weak_Element_Maps.Cursor;
      WRef : Weak_Cache;
      E    : Detached_Element_Access;
      C    : Element_Maps.Cursor;
      D    : Detached_Data_Access;
   begin
      if Data.Weak_Cache then
         WC := Data.Wcache.First;
         while Has_Element (WC) loop
            WRef := Element (WC);

            if Active (Me) then
               Trace_Debug (Me, WRef, "Removed from cache: ");
            end if;

            --  The element no longer belongs to the session (it might actually
            --  be freed by Unchecked_Free, but we do not know that for sure)
            D := Get_Data (WRef);
            if D /= null then
               D.Session := Null_Weak_Session;
            end if;

            Next (WC);
         end loop;
         Data.Wcache.Clear;
      else
         C := Data.Cache.First;
         while Has_Element (C) loop
            E := Element (C);

            if Active (Me) then
               Trace_Debug (Me, E, "Removed from cache: ");
            end if;

            D := Get_Data (E);
            if D /= null then
               D.Session := Null_Weak_Session;
            end if;
            Unchecked_Free (E);

            Next (C);
         end loop;
         Data.Cache.Clear;
      end if;

      Data.Modified_Elements.Clear;
   end Clear_Cache;

   ---------------------
   -- Impl_On_Release --
   ---------------------

   procedure Impl_On_Release (Data : in out Session_Data) is
   begin
      Increase_Indent (Me, "Releasing session in pool "
                       & Session_Pool'Image (Data.Pool));
      Clear_Cache (Data);

      if Data.DB.Automatic_Transactions then
         Rollback (Data.DB, "");  --  Release any pending transaction
      end if;

      if Data.User /= null then
         Trace (Me, "Freeing session data");
         Free (Data.User.all);
         Unchecked_Free (Data.User);
      end if;

      Decrease_Indent (Me, "Done releasing session");
   end Impl_On_Release;

   ---------------
   -- Impl_Free --
   ---------------

   procedure Impl_Free (Data : in out Session_Data) is
   begin
      if Active (Me) then
         Trace (Me, "Freeing a session and its cache, closing SQL connection"
                & " in pool " & Session_Pool'Image (Data.Pool));
      end if;
      Clear_Cache (Data);  --  Should have been done in Impl_On_Release

      Data.Factory := Default_Fact;

      Free (Data.DB);  --  Close connection
   end Impl_Free;

   ---------------------
   -- Get_New_Session --
   ---------------------

   function Get_New_Session
     (Pool : Session_Pool := Default_Pool) return Session_Type
   is
      Self : Session_Type;
   begin
      if Active (Me) then
         Trace (Me, "Getting new session from pool "
                & Session_Pool'Image (Pool));
      end if;
      Impl.Get (Self, Set => Pool);
      Reset_Connection (Self.DB);
      return Self;
   end Get_New_Session;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Descr              : GNATCOLL.SQL.Exec.Database_Description;
      Max_Sessions       : Positive;
      Default_User_Data  : User_Data'Class := No_User_Data;
      Store_Unmodified   : Boolean := False;
      Weak_Cache         : Boolean := True;
      Flush_Before_Query : Boolean := True;
      Persist_Cascade    : Boolean := True;
      Pool               : Session_Pool := Default_Pool)
   is
   begin
      Impl.Set_Factory
        (Pool_Data'
           (Descr                     => Descr,
            Pool                      => Pool,
            Config_Store_Unmodified   => Store_Unmodified,
            Config_Weak_Cache         => Weak_Cache,
            Config_Flush_Before_Query => Flush_Before_Query,
            Config_Default_User_Data  =>
               new User_Data'Class'(Default_User_Data),
            Config_Persist_Cascade    => Persist_Cascade),
         Max_Sessions, Set => Pool);
   end Setup;

   -------------------------
   -- Set_Default_Factory --
   -------------------------

   procedure Set_Default_Factory (Factory : Element_Factory) is
   begin
      Default_Fact := Factory;
   end Set_Default_Factory;

   -----------------
   -- Set_Factory --
   -----------------

   procedure Set_Factory
     (Self    : in out Session_Type'Class;
      Factory : Element_Factory) is
   begin
      Self.Element.Factory := Factory;
   end Set_Factory;

   ------------------
   -- Null_Factory --
   ------------------

   function Null_Factory
     (From    : Base_Element'Class;
      Default : Detached_Element'Class) return Detached_Element'Class
   is
      pragma Unreferenced (From);
   begin
      return Default;
   end Null_Factory;

   -------------
   -- Factory --
   -------------

   function Factory
      (Self    : Session_Type'Class;
       From    : Base_Element'Class;
       Default : Detached_Element'Class) return Detached_Element'Class is
   begin
      return Self.Element.Factory (From, Default);
   end Factory;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Detached_Element) return Boolean is
   begin
      return Self.Get = null;
   end Is_Null;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Detached_Data) is
      WC      : Weak_Element_Maps.Cursor;
      Session : constant Session_Type := Get (Self.Session);
      D       : access Session_Data;
      K       : Element_Key;
      Template : Detached_Element_Access;
   begin
      --  This procedure is only called for elements that have a weak-ref (or
      --  no ref at all) in a session. If the session is holding a real
      --  reference, Self cannot be freed anyway.
      --  The goal here is to save memory by removing the element's weak ref
      --  from the session cache. This is not mandatory, since the weak ref
      --  remains usable, but ensures memory usage does not grow up too much.

      --  This will actually free Self, and properly reset all weakref to it.
      --  At this point, the weak references in the cache that are no longer
      --  pointing to anything can simply be removed.

      Free (Weak_Refcounted (Self));

      if Session /= No_Session then
         D := Session.Element;
         if D.Weak_Cache then
            K := Key (Detached_Data'Class (Self));
            WC := D.Wcache.Find (K);
            if Has_Element (WC) then
               if Active (Me) then
                  Trace (Me, "Removing from cache: freed weakref "
                         & Image (K));
               end if;

               Template := Element (WC).Template;
               Unchecked_Free (Template);
               D.Wcache.Delete (WC);
            end if;
         end if;
      end if;
   end Free;

   -----------
   -- Image --
   -----------

   function Image (K : Element_Key) return String is
   begin
      return "<" & Image (K.Table, Min_Width => 1)
        & ',' & Image (K.Key, Min_Width => 1) & ">";
   end Image;

   -----------------
   -- Trace_Debug --
   -----------------

   procedure Trace_Debug
     (Me  : GNATCOLL.Traces.Trace_Handle;
      K   : Weak_Cache;
      Msg : String := "")
   is
      Data  : constant Detached_Data_Access := Get_Data (K);
   begin
      if Was_Freed (K.Ref) then
         Trace (Me, Msg & "weakref to freed '" & Image (Key (Data.all)) & "'");
      else
         Trace (Me, Msg & "weakref to '" & Image (Key (Data.all)) & "'");
      end if;
   end Trace_Debug;

   -----------------
   -- Trace_Debug --
   -----------------

   procedure Trace_Debug
     (Me  : GNATCOLL.Traces.Trace_Handle;
      E   : Detached_Element_Access;
      Msg : String := "")
   is
      Data  : constant Detached_Data_Access := Get_Data (E);
      Count : Integer;
   begin
      Count := Get_Refcount (E.all);
      if not Active (Me) and then Count /= 1 then
         --  Always show when an element has remaining references, since it
         --  helps debugging memory issues.
         Trace (Me_Info, Msg & " (remaining refs to '"
                & Image (Key (Data.all))
                & "' refcount=" & Count'Img & ")");
      end if;

      if Data = null then
         Trace (Me, Msg & "ref to <null> refcount=" & Count'Img);
      elsif Is_Dirty (Data) then
         Trace (Me, Msg & "ref to modified '"
                & Image (Key (Data.all))
                & "' refcount=" & Count'Img);
      else
         Trace (Me, Msg & "ref to unmodified '"
                & Image (Key (Data.all))
                & "' refcount=" & Count'Img);
      end if;
   end Trace_Debug;

   -----------------
   -- Trace_Debug --
   -----------------

   procedure Trace_Debug
     (Me      : GNATCOLL.Traces.Trace_Handle;
      Session : Session_Type;
      Msg     : String := "")
   is
      C    : Element_Maps.Cursor;
      WC   : Weak_Element_Maps.Cursor;
   begin
      Increase_Indent (Me, Msg);
      Trace (Me, "Refcount=" & Get_Refcount (Session)'Img);

      if Session.Element.Weak_Cache then
         WC := Session.Element.Wcache.First;
         while Has_Element (WC) loop
            Trace_Debug (Me, Element (WC), "");
            Next (WC);
         end loop;

      else
         C := Session.Element.Cache.First;
         while Has_Element (C) loop
            Trace_Debug (Me, Element (C), "");
            Next (C);
         end loop;
      end if;

      Decrease_Indent (Me, "Done " & Msg);

   exception
      when E : others =>
         Decrease_Indent (Me, "Done " & Msg & " Unexpected exception");
         Trace (Me, E);
   end Trace_Debug;

   ---------
   -- "=" --
   ---------

   function "=" (W1, W2 : Weak_Cache) return Boolean is
   begin
      return W1.Ref = W2.Ref;
   end "=";

   ----------
   -- Hash --
   ----------

   function Hash (Key : Element_Key) return Ada.Containers.Hash_Type is
   begin
      if Key.Key = No_Primary_Key then
         return Ada.Containers.Hash_Type (Key.Table);
      else
         return Ada.Containers.Hash_Type
           (Long_Long_Integer (Key.Table + Key.Key)
            mod Long_Long_Integer (Hash_Type'Last));
      end if;
   end Hash;

   ----------------
   -- From_Cache --
   ----------------

   function From_Cache
     (Self         : Session_Type;
      Key          : Element_Key;
      If_Not_Found : Detached_Element'Class) return Detached_Element'Class
   is
      D  : constant access Session_Data := Self.Element;
      C  : Element_Maps.Cursor;
      WC : Weak_Element_Maps.Cursor;
      Wref : Weak_Cache;
   begin
      if D.Weak_Cache then
         --  Find will call Equivalent to find a matching element (after using
         --  the Hash to find the bucket).
         WC := D.Wcache.Find (Key);
         if Has_Element (WC) then
            Wref := Weak_Element_Maps.Element (WC);
            declare
               R : Detached_Element'Class := Wref.Template.all;
            begin
               Set (R, Get_Data (Wref));
               return R;
            end;
         end if;
      else
         C := D.Cache.Find (Key);
         if Has_Element (C) then
            return Element_Maps.Element (C).all;
         end if;
      end if;
      return If_Not_Found;
   end From_Cache;

   ------------------
   -- Set_Modified --
   ------------------

   procedure Set_Modified
     (Self : Detached_Element; Field : Natural)
   is
      D : constant Detached_Data_Access := Detached_Data_Access (Self.Get);
      Session : Session_Type;
      Was_Dirty : constant Boolean := Is_Dirty (D);
   begin
      if Active (Me) and then not D.Dirty (Field) then
         Trace (Me, "Set_Modified, Field=" & Field'Img
                & " key=" & Image (Key (D.all))
                & " Was_Dirty=" & Was_Dirty'Img);
      end if;

      D.Dirty (Field) := True;

      if not Was_Dirty then
         --  Add the element to the list of modified elements. This also
         --  ensures we have a real reference to it so that it isn't finalized
         --  before we have flushed it to the db.

         Session := Get (D.Session);
         if Session /= No_Session then
            Session.Element.Modified_Elements.Append (Self);
            if Active (Me) then
               Trace (Me, "   Set_Modified: added to session's modified list");
            end if;
         end if;

         --  We do not need to add the element to the cache:
         --  either it was retrieved through the session and is already in the
         --  cache, or it is a new element which we would not be able to cache
         --  anyway.
      end if;
   end Set_Modified;

   --------
   -- DB --
   --------

   function DB (Self : Session_Type) return Database_Connection is
   begin
      return Self.Element.DB;
   end DB;

   ----------
   -- Free --
   ----------

   procedure Free is
   begin
      Impl.Free;
   end Free;

   --------------
   -- Get_Weak --
   --------------

   function Get_Weak (Self : Session_Type) return Weak_Session is
      W : Impl.Weak_Resource;
   begin
      W := Impl.Get_Weak (Impl.Resource (Self));
      return Weak_Session'(Ref => W);
   end Get_Weak;

   ---------
   -- Get --
   ---------

   function Get (Self : Weak_Session) return Session_Type is
      Result : Session_Type;
   begin
      if Impl.Was_Freed (Self.Ref) then
         return No_Session;
      else
         Impl.Get (Self.Ref, Result);
         return Result;
      end if;
   end Get;

   -------------
   -- Persist --
   -------------

   procedure Persist
     (Self : Session_Type; Element : Detached_Element'Class)
   is
      D : constant Detached_Data_Access := Detached_Data_Access (Element.Get);
   begin
      if D = null then
         return;
      end if;

      --  Store a weak-reference to the session in the element, so that it can
      --  be used to query further attributes, or register changes
      --  The Element can outlive the session, so we store a weak-reference.

      if Get (D.Session) /= No_Session then
         if Get (D.Session) /= Self then
            raise Already_Persistent
              with "Element already belongs to another session";
         end if;

         --  Element is already in the session, but might not be in the cache
         --  if it wasn't modified before and Config_Store_Unmodified is False.
         --  If it is in the session, and it is dirty, it has already been
         --  added to the list of modified elements.

      else
         D.Session := Get_Weak (Self);

         if Is_Dirty (D) then
            if Active (Me) then
               Trace (Me, "Persisting a modified element: "
                      & Image (Key (D.all)));
            end if;
            Self.Element.Modified_Elements.Append (Element);
         elsif not Self.Element.Store_Unmodified then
            return;
         end if;
      end if;

      Add_To_Cache (Self, Element);
   end Persist;

   ------------------
   -- Add_To_Cache --
   ------------------

   procedure Add_To_Cache (Self : Session_Type; E : Detached_Element'Class) is
      K : constant Element_Key := Key (Detached_Data_Access (E.Get).all);
      T : Detached_Element_Access;
      Inserted : Boolean;
      C : Element_Maps.Cursor;
      WC : Weak_Element_Maps.Cursor;
   begin
      if K.Key = No_Primary_Key then
         --  Can't add an element whose primary key is unknown, since we do not
         --  have a unique key for it. But the element has likely been added
         --  (via Persist) to the list of Modified_Elements, and when the
         --  element is inserted into the database it will also be added to
         --  the database automatically).

         return;
      end if;

      if Self.Element.Weak_Cache then
         T := new Detached_Element'Class'(E);
         Set (T.all, null);   --  We only want the template, not a ref
         Self.Element.Wcache.Insert
           (Key => K,
            New_Item => (Ref      => Pointers.Get_Weak_Ref (E),
                         Template => T),
            Position => WC,
            Inserted => Inserted);
      else
         T := new Detached_Element'Class'(E);
         Self.Element.Cache.Insert
           (Key => K, New_Item => T,
            Position => C,
            Inserted => Inserted);
      end if;

      if Inserted then
         if Active (Me) then
            Trace (Me, "Add to cache: " & Image (K));
         end if;
         On_Persist (E);
      else
         Unchecked_Free (T);
      end if;
   end Add_To_Cache;

   --------------
   -- Is_Dirty --
   --------------

   function Is_Dirty (Data : Detached_Data_Access) return Boolean is
   begin
      for D in Data.Dirty'Range loop
         if Data.Dirty (D) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Dirty;

   -----------------
   -- Cache_Count --
   -----------------

   procedure Cache_Count
     (Self    : Session_Type;
      Refs    : out Natural;
      Weakref : out Natural) is
   begin
      if Self.Element.Weak_Cache then
         Weakref := Integer (Length (Self.Element.Wcache));
         Refs    := 0;
      else
         Weakref := 0;
         Refs := Integer (Length (Self.Element.Cache));
      end if;
   end Cache_Count;

   ---------------------
   -- Persist_Cascade --
   ---------------------

   function Persist_Cascade (Self : Session_Type) return Boolean is
   begin
      return Self /= No_Session
        and then Self.Element.Persist_Cascade;
   end Persist_Cascade;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   procedure Insert_Or_Update
     (Self    : Session_Type;
      Element : in out Detached_Element'Class)
   is
      D  : constant Detached_Data_Access :=
        Detached_Data_Access (Get (Element));
      PK_Modified : Boolean;
   begin
      if D = null or else not Is_Dirty (D) then
         --  Nothing to do
         return;
      end if;

      if Active (Me) then
         Trace_Debug (Me, Element'Unchecked_Access, "Insert_Or_Update: ");
      end if;

      declare
         Dirty : constant Dirty_Mask := D.Dirty;
         Old_K : constant Element_Key := Key (D.all);
      begin

         --  Reset the dirty mask, to prevent infinite recursion when an
         --  element depends (possibly indirectly) on itself

         D.Dirty := (others => False);

         if Dirty (Dirty_Mask_Deleted) then
            Internal_Delete (Element);

            --  Remove the element from the cache. In theory at least, the
            --  element no longer exists
            --
            --  ??? In theory at least, we should only remove elements last,
            --  after all other changes have been done (so that any FK
            --  reference to them has been updated). Likewise, we should add
            --  elements first).
            PK_Modified := True;

         else
            Insert_Or_Update (Element, PK_Modified, Dirty);

            if PK_Modified then
               if Active (Me) then
                  Trace (Me, "PK has changed, adding new value to cache");
               end if;

               Add_To_Cache (Self, Element);  --  Insert with new key
            end if;
         end if;

         if PK_Modified and then Old_K.Key /= No_Primary_Key then
            if Active (Me) then
               Trace (Me, "PK has changed, removing old from cache: "
                      & Image (Old_K));
            end if;

            --  Remove the old element from the cache
            if Self.Element.Weak_Cache then
               Self.Element.Wcache.Exclude (Old_K);
            else
               Self.Element.Cache.Exclude (Old_K);
            end if;
         end if;
      end;
   end Insert_Or_Update;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self : Session_Type; Element : Detached_Element'Class)
   is
      pragma Unreferenced (Self);
   begin
      Element.Set_Modified (Dirty_Mask_Deleted);
   end Delete;

   -----------
   -- Flush --
   -----------

   procedure Flush (Self : Session_Type) is
      procedure Process (E : in out Detached_Element'Class);
      procedure Process (E : in out Detached_Element'Class) is
      begin
         Insert_Or_Update (Self, E);
      end Process;

      Data : constant access Session_Data := Self.Element;
      C    : Element_Lists.Cursor := Data.Modified_Elements.First;
   begin
      if Has_Element (C) then
         Increase_Indent (Me, "Flushing session");
         while Has_Element (C) loop
            Update_Element (Data.Modified_Elements, C, Process'Access);
            Next (C);
         end loop;

         Data.Modified_Elements.Clear;
         Decrease_Indent (Me, "Done flushing session");
      end if;

      --  ??? The only delicate area is when you have default values set
      --  by the database, in which case we might not have the value in
      --  memory and will not fetch it next time.

   exception
      when GNATCOLL.SQL.Orm.Self_Referencing =>
         Decrease_Indent (Me, "Got self-referencing");
         raise;

      when E : others =>
         Trace (Me, E, "while flushing session");
         Decrease_Indent (Me);
   end Flush;

   -----------------------
   -- Begin_Transaction --
   -----------------------

   procedure Begin_Transaction (Self : Session_Type) is
   begin
      if not In_Transaction (Self.DB) then
         Execute (Self.DB, SQL_Begin);
      end if;
   end Begin_Transaction;

   --------------------
   -- In_Transaction --
   --------------------

   function In_Transaction (Self : Session_Type) return Boolean is
   begin
      return In_Transaction (Self.DB);
   end In_Transaction;

   ------------
   -- Commit --
   ------------

   procedure Commit (Self : Session_Type) is
   begin
      Flush (Self);
      if In_Transaction (Self.DB) then
         Execute (Self.DB, SQL_Commit);
      end if;
   end Commit;

   --------------
   -- Rollback --
   --------------

   procedure Rollback (Self : Session_Type) is
   begin
      Trace (Me, "Rollback session");
      if In_Transaction (Self.DB) then
         Execute (Self.DB, SQL_Rollback);
      end if;

      Clear_Cache (Self.Element.all);
   end Rollback;

   -------------
   -- Session --
   -------------

   function Session (Self : Detached_Element'Class) return Session_Type is
   begin
      return Get (Detached_Data_Access (Self.Get).Session);
   end Session;

   ----------------------
   -- Get_Weak_Session --
   ----------------------

   function Get_Weak_Session
     (Self : Detached_Element'Class) return Weak_Session is
   begin
      return Detached_Data_Access (Self.Get).Session;
   end Get_Weak_Session;

   ------------------------
   -- Flush_Before_Query --
   ------------------------

   function Flush_Before_Query (Self : Session_Type) return Boolean is
   begin
      return Self.Element.Flush_Before_Query;
   end Flush_Before_Query;

   ---------------------------
   -- Set_Default_User_Data --
   ---------------------------

   procedure Set_Default_User_Data
     (Default_User_Data  : User_Data'Class := No_User_Data;
      Pool               : Session_Pool := Default_Pool)
   is
      P : constant access Pool_Data := Impl.Get_Factory_Param (Pool);
   begin
      Free (P.Config_Default_User_Data.all);
      Unchecked_Free (P.Config_Default_User_Data);
      P.Config_Default_User_Data := new User_Data'Class'(Default_User_Data);
   end Set_Default_User_Data;

   -------------------
   -- Get_User_Data --
   -------------------

   function Get_User_Data
     (Self : Session_Type; Pool : Session_Pool := Default_Pool)
      return access User_Data'Class
   is
      D : constant access Session_Data := Self.Element;
      P : access Pool_Data;
   begin
      if D.User = null then
         P := Impl.Get_Factory_Param (Pool);
         if P.Config_Default_User_Data /= null then
            D.User := new User_Data'Class'(P.Config_Default_User_Data.all);
         end if;
      end if;

      return D.User;
   end Get_User_Data;

   ------------
   -- Delete --
   ------------

   procedure Delete (Element : Detached_Element) is
   begin
      Element.Session.Delete (Element);
   end Delete;

end GNATCOLL.SQL.Sessions;
