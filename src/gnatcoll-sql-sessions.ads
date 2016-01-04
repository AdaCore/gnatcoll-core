------------------------------------------------------------------------------
--                             M O D E L I N G                              --
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

--  This package implements connection pools to the database: it maintains
--  a set of active connections, and will return one to the user on request.
--  A session acts as a unit-of-work pattern: it provides a local cache for
--  returned objects, so that all queries done within the context of the
--  session always manipulate the same Ada object (this is especially useful
--  when the objects have been modified locally).

pragma Ada_2012;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with GNATCOLL.Refcount;         use GNATCOLL.Refcount;
with GNATCOLL.SQL.Exec;         use GNATCOLL.SQL.Exec;
with GNATCOLL.Traces;
with GNATCOLL.Pools;

package GNATCOLL.SQL.Sessions is

   --  Work around issue with the Ada containers: the tampering checks
   --  mean that the container might be corrupted if used from multiple
   --  tasks, even in read-only.
   --     pragma Suppress (Tampering_Check);

   type Session_Pool is new Integer range 1 .. 3;
   --  Connection to at most three different DBMS.
   --  ??? Would be nice to make this configurable

   Default_Pool : constant Session_Pool := Session_Pool'First;

   type Session_Type is tagged private;
   No_Session : constant Session_Type;
   --  A session implements the "Unit Of Work" design pattern.
   --     See http://martinfowler.com/eaaCatalog/unitOfWork.html
   --  Basically, they are meant for use while a specific task is performed by
   --  the application, and provide caching of instances, based on their types
   --  and ids. Thus they ensure that however an instance is retrieved (either
   --  from the database, or from any other backend, and whether it is
   --  directly retrieved or as part of the properties of another element), the
   --  same instance will always be used for the same id.
   --
   --  For instance:
   --      Session := Get_New_Session;
   --      M1 := Get_Model (Session, Id => 1);
   --      M2 := Get_Model (Session, Id => 1);
   --      --  changes in M2 are also visible in M1, since they are the same.
   --
   --  This ensures consistency when objects can be modified in memory before
   --  writting to the database, and also ensures that local caches for complex
   --  data can be properly reused (for instance if M1.Super_Classes is complex
   --  to compute, the result is automatically reused for M2.Super_Classes).
   --
   --  When exiting from its scope, the session is automatically closed, and
   --  the internal cache is freed.
   --  In practice, sessions are pooled, so only a number of them can exist in
   --  parallel. The reason for this pooling is that they are also associated
   --  with a database connection, which can be long to establish.
   --
   --  This type is not meant to be overridden. It is tagged so that one can
   --  use the dotted notation for method call. If you need to add extra data
   --  to a session, you should extend User_Data instead.

   -----------------
   -- Custom_Data --
   -----------------

   type User_Data is tagged null record;
   No_User_Data : constant User_Data'Class;
   --  User data associated with a session. It is automatically freed (by
   --  calling Free below) when the session is released to the pool, so
   --  calling Get_New_Session will always return a session with a default
   --  user data (as configured in Setup below)

   procedure Free (Self : in out User_Data) is null;
   --  Free the contents of User_Data.

   function Get_User_Data
     (Self : Session_Type; Pool : Session_Pool := Default_Pool)
      return access User_Data'Class;
   --  Return the user data stored in the session.
   --  If none exists yet, a copy of the Default_User_Data set through Setup
   --  is allocated and returned.
   --  The returned value will be null if Setup was never called.

   --------------------------
   -- Configuring sessions --
   --------------------------

   procedure Setup
     (Descr              : GNATCOLL.SQL.Exec.Database_Description;
      Max_Sessions       : Positive;
      Default_User_Data  : User_Data'Class := No_User_Data;
      Store_Unmodified   : Boolean := False;
      Weak_Cache         : Boolean := True;
      Flush_Before_Query : Boolean := True;
      Persist_Cascade    : Boolean := True;
      Pool               : Session_Pool := Default_Pool);
   --  Describes how to connect to the database.
   --  Max_Sessions is the number of concurrent sessions at maximum. Each holds
   --  a connection to the database.
   --  Descr will be freed automatically when calling Sessions.Free below.
   --
   --  Store_Unmodified:
   --  if True, unmodified elements are still stored in the session cache. This
   --  is slightly less efficient (requires some extra memory allocation and
   --  list management), but ensures that we will always return the same
   --  instance of element within a session, as long as the element itself
   --  exists outside of the session (unless Weak_Cache is also False, in which
   --  case the session ensures the element always exists).
   --  Setting this to False might also have a negative impact on the
   --  performance: since elements do some caching of their own, always
   --  restarting with a new element instead of reusing one (even read-only)
   --  from the cache means that this local element cache will not be used.
   --  This local cache is used for the element properties, in particular when
   --  they point to other tables.
   --
   --  Weak_Cache:
   --  If true, the internal cache for the session uses weak-references: an
   --  object that is no longer referenced outside of the session and
   --  unmodified will be removed from the session cache.
   --  If False, objects will be kept in the session cache for as long as
   --  the session exists. This uses more memory, but can save a few
   --  roundtrips to the database (retrieving an element by id will reuse
   --  the element from the cache if it exists, instead of querying).
   --
   --  Flush_Before_Query:
   --  If true, a Flush is performed before a query (in a SQL transaction, not
   --  committed). This ensures that the results of the query will be accurate
   --  although it might be slightly less efficient in some scenarios.
   --
   --  Persist_Cascade:
   --  If true, persisting an element (through a call to Persist) will also
   --  persist its related elements from other tables (if they have been
   --  retrieved yet).
   --
   --  Default_User_Data will be copied every time you call Get_User_Data the
   --  first time for a session. Default_User_Data is freed when this package
   --  is finalized through a call to Free.

   procedure Set_Default_User_Data
     (Default_User_Data  : User_Data'Class := No_User_Data;
      Pool               : Session_Pool := Default_Pool);
   --  Override the default user data. This will affect all sesssions retrieved
   --  via Get_New_Session from now on.
   --  This must be called from a protected region where a single thread has
   --  access, since it modifies shared data.

   type Weak_Session is private;
   Null_Weak_Session : constant Weak_Session;
   --  A weak-reference to a session.
   --  This is convenient to store in several contexts: it will not prevent the
   --  session from being freed, but ensures you will reuse the same session if
   --  it wasn't freed. This can be used to break dependency cycles, where a
   --  reference counted element belongs to the session and at the same time
   --  holds a reference to the session -- none of those would be freed.

   function Get_Weak (Self : Session_Type) return Weak_Session;
   function Get (Self : Weak_Session) return Session_Type;
   --  Return the session referenced by Self, or No_Session if it has already
   --  been released to the pool.

   function Flush_Before_Query (Self : Session_Type) return Boolean;
   function Persist_Cascade (Self : Session_Type) return Boolean;
   --  Return the value of the corresponding setup for the session.
   --  See Setup for more documentation on the various settings.

   function Get_New_Session
     (Pool : Session_Pool := Default_Pool) return Session_Type;
   pragma Inline (Get_New_Session);
   --  Create a new session, that remains active while in scope.

   function DB (Self : Session_Type) return Database_Connection;
   --  Return the database connection wrapped into the connection. You must use
   --  it if the session might have been released.

   procedure Free;
   --  Free the sessions pool.
   --  Get_New_Session must no longer be called afterward.

   --------------
   -- Elements --
   --------------
   --  Although sessions can be used on their own just to benefit from the
   --  pooling, they work best in collaboration with an automatically generated
   --  API that represents your database schema.
   --  For each table, a type of element is declared that represents a row from
   --  this table. Such an element must derive from the type Detached_Element
   --  below. This provides support for caching elements, caching their
   --  properties (in particular the related elements from other tables), or
   --  handling changes to the element and committing them to the database
   --  later on.

   type Base_Detached_Data is abstract tagged null record;
   procedure Free (Self : in out Base_Detached_Data) is null;
   procedure Free_Dispatch (Self : in out Base_Detached_Data'Class);

   type Detached_Data (Field_Count : Natural)
     is abstract new Base_Detached_Data with private;
   type Detached_Data_Access is access all Detached_Data'Class;
   --  Data stored in a Detached_Element.
   --  Field_Count must be the total number of fields, and is used to keep
   --  track of which field has been modified in memory but not reflected into
   --  the database yet.

   package Pointers is new GNATCOLL.Refcount.Shared_Pointers
     (Base_Detached_Data'Class, Free_Dispatch);
   type Detached_Element is abstract new Pointers.Ref with null record;
   type Detached_Element_Access is access all Detached_Element'Class;
   --  An element that represents a row from a database table. Such an element
   --  exists in the application space, without any need for a database
   --  connection to retrieve its properties (except of course when it is
   --  initially created).
   --  Such objects are ref-counted, and thus memory management is handled
   --  automatically. These types are meant as smart pointers (ie they are very
   --  light weight wrappers for an access type). As a result, the functions
   --  that manipulate such types always return a Detached_Element'Class, not a
   --  access on such an element. Any modification you do on the type is really
   --  done on the wrapped data, and therefore shared by all elements that
   --  wrap the same data. This simplifies memory management.
   --
   --  In the user application, such Detached_Elements are generally created in
   --  one of two ways:
   --    - Either directly
   --      in this case, the element does not come from the database. It can
   --      later be added to a session, and when that session is saved the
   --      element is saved in the database.
   --    - As a result of a SQL query
   --      The element is then cached in the session. If it gets modified, the
   --      database will be updated when the session is saved. The element can
   --      be removed from the session (and then we end up in the first case
   --      above).
   --
   --  Testing whether element has been set can be tested with either:
   --      Tmp := Pointers.Is_Null (Element);
   --      Tmp := Element.Is_Null;

   Already_Persistent : exception;
   procedure Persist
     (Self : Session_Type; Element : Detached_Element'Class);
   --  Make the element persistent.
   --  The session will be used for further queries on the element in case we
   --  need to hit the database. It will also be used when the element is
   --  modified to reflect the changes into the database.
   --  An error Already_Persistent is raised if the element is already managed
   --  by another session (no error is raised if this is the same session).
   --  Due to the way the references are owned, the session can still be
   --  terminated while some elements belong to it. At that point, the elements
   --  are automatically detached from the session.

   procedure On_Persist (Self : Detached_Element) is null;
   --  Called when the element was persisted in the session. This is only
   --  called the first time the element is added to the session. That means
   --  that calling Persist again on an element already in the session will not
   --  call On_Persist again.
   --  Use Self.Session to get access to the session.

   function Session (Self : Detached_Element'Class) return Session_Type;
   function Get_Weak_Session
     (Self : Detached_Element'Class) return Weak_Session;
   --  Return the session to which Self is attached, or No_Session if that
   --  session has been closed.

   procedure Delete
     (Self : Session_Type; Element : Detached_Element'Class);
   --  Mark the element as deleted in the database.
   --  The element is first persisted in the session if necessary

   procedure Delete (Element : Detached_Element);
   --  A shortcut for Element.Session.Detach (Element).
   --  This assumes the element belongs to a session.

   ----------------------------
   -- Modifying the database --
   ----------------------------

   procedure Flush (Self : Session_Type);
   --  Execute all pending changes on the database. This does not commit the
   --  SQL transaction though, but might be used if for instance you need to
   --  get the id that will be used for an element.
   --  If your application terminates or crashes, the changes are not
   --  permanent until you call Commit.

   procedure Begin_Transaction (Self : Session_Type);
   --  Start a SQL transaction. This call is not needed in general, since the
   --  session will do it automatically when needed. However, some DBMS
   --  recommend performing the select queries also in a transaction, so you
   --  might want to force the use of transactions in some cases.

   procedure Commit (Self : Session_Type);
   procedure Rollback (Self : Session_Type);
   --  Commit or rollback the session. A Flush is performed as needed, and the
   --  cache might get cleared as well.

   function In_Transaction (Self : Session_Type) return Boolean;
   --  Whether there is an active SQL transaction for this session.

   ---------------
   -- Factories --
   ---------------
   --  Although it is expected you will be using the automatically generated
   --  Ada API to represent the objects, this will only provide one type of
   --  object per table in your database.
   --
   --  Assume for instance that you have a table Contract, with a field
   --  Contract_Type. It is likely that your application will want to represent
   --  the various types of contracts as their own tagged objects, derived from
   --  the automatically generated Contract type.
   --
   --  The generated API will provide the following types:
   --      type Contract is new Base_Element with private;
   --      type Detached_Contract is new Detached_Element with private;
   --
   --  You will thus create the following types:
   --      type Contract_Type1 is new Detached_Contract with private;
   --      type Contract_Type2 is new Detached_Contract with private;
   --
   --  But the queries to the database you make through the generated API will
   --  always return a Contract object.
   --  That's where factories come into play. They act as an intermediate
   --  layer between the binary result of the SQL query, and the object
   --  returned to your application. They are used to build an uninitialized
   --  object of the appropriate application-specific type.
   --  Such a factory is session specific: depending on the task you want to
   --  accomplish within a given session, you might want to represent the
   --  objects differently from the ones in another session. This gives you
   --  full control over the representation of objects.
   --
   --  In the case of the example above, the factory would be something like:
   --      function Factory
   --         (From : Base_Element'Class; Default : Detached_Element'Class)
   --         return Detached_Element'Class is
   --      begin
   --         if From in Contract'Class then
   --            if Contract (From).Contract_Type = 1 then
   --               return R : Contract_Type1 do null; end return;
   --            elsif Contract (From).Contract_Type = 2 then
   --               return R : Contract_Type2 do null; end record;
   --            end if;
   --         end if;
   --         return Default;
   --      end Factory;

   type Base_Element is abstract tagged private;
   --  An element that represents a row from a database table. Such an element
   --  ONLY exists during a database transaction (and sometimes only until you
   --  move to the next row of results). This cannot be stored for further
   --  reuse, but is much lighter weight than a Detached_Element.

   type Element_Factory is not null access
     function (From    : Base_Element'Class;
               Default : Detached_Element'Class) return Detached_Element'Class;
   --  If required, return the actual type that should be used to represent
   --  From.
   --  The returned value must be the of the detached class representing From,
   --  or one of its subtypes (for instance, if a "Contract" is given, a
   --  "Detached_Contract'Class" must be returned).
   --  This factory must return:
   --     - an uninitialized Detached_Element when you simply want to override
   --       the smart pointer type, but not its data
   --     - or a fully initialized smart pointer when you also want to
   --       override the data to add your own. This requires use of
   --       subprograms from Orm_Impl.ads
   --     - or Default if you do not need to override any
   --       of the defaults for From.

   function Null_Factory
     (From    : Base_Element'Class;
      Default : Detached_Element'Class) return Detached_Element'Class;
   --  A special factory that always returns Default.
   --  See the description of Element_Factory.

   procedure Set_Factory
     (Self    : in out Session_Type'Class;
      Factory : Element_Factory);
   --  Override the current factory in the session.

   procedure Set_Default_Factory (Factory : Element_Factory);
   --  The default factory to use when retrieving elements from the database
   --  This is always the factory in use when you just got a session from
   --  the pool, and until you override it with Set_Factory above.
   --  Calling this subprogram does not affect the sessions already retrieved
   --  from the pool, although it is recommended to only call it once at the
   --  beginning of the application.

   -----------
   -- Cache --
   -----------
   --  A session caches the elements that were detached in its context, so that
   --  they are always returned when the SQL query returns an id currently
   --  cached. This ensures that the following scenario works fine:
   --
   --     Session := Sessions.Get_New_Session;
   --     Elem1 := <some_query>.Detach (Session);  --  cached in session
   --     Elem1.Set_<property> (<value>);
   --     Elem2 := <some_query>.Detach (Session);  --  assume Elem1.Id=Elem2.Id
   --     Elem1.Get_<property> = <value>  --  True
   --
   --  The following subprograms are for the implementation of the generated
   --  API, and should not be needed directly in your own code.

   No_Primary_Key : constant := -1;

   type Element_Key is record
      Table : Natural;  --  A unique table id, one for each table.
                        --  We recommend using numbers every 1_000_000 or so.
      Key   : Integer;  --  The element's key (or No_Primary_Key if there is
                        --  no unique id)
   end record;
   --  The key for an element. This must be unique in the database for each
   --  element. Currently, we only support elements with a single integer
   --  primary key, although this could presumably be extended. We should not
   --  use strings, though, since they are expensive to pass around and to
   --  compute hashes from them.

   function Key (Self : Detached_Data) return Element_Key is abstract;
   --  Return the unique key for Self, so that it can be stored in the session
   --  cache.
   --  This function must be overridden, but can't be set abstract

   function From_Cache
     (Self         : Session_Type;
      Key          : Element_Key;
      If_Not_Found : Detached_Element'Class) return Detached_Element'Class;
   --  Returns the element from the cache, if any, or If_Not_Found.

   --------------------
   -- Implementation --
   --------------------
   --  The following subprograms provide support for creating derived types of
   --  elements for your specific database schema. Your application should not
   --  have to use them directly.

   function Factory
      (Self    : Session_Type'Class;
       From    : Base_Element'Class;
       Default : Detached_Element'Class) return Detached_Element'Class;
   --  Wrap, if needed, From into another element.
   --  This calls the Element_Factory set for the session

   subtype Dirty_Mask_Field is Natural;
   Dirty_Mask_Deleted : constant Dirty_Mask_Field := 0;

   type Dirty_Mask is array (Dirty_Mask_Field range <>) of Boolean;
   --  Used internally to memorize which fields have been modified. When an
   --  object is saved into the database, the SQL query will only set those
   --  fields for which the Dirty_Mask is set to True.
   --  Index '0' indicates whether the element should be deleted

   procedure Set_Modified (Self : Detached_Element; Field : Natural);
   --  Mark the Field-th field in Self as modified in memory. This change will
   --  need to be reflected into the database when the session is flushed.

   procedure Insert_Or_Update
     (Self        : in out Detached_Element;
      PK_Modified : in out Boolean;
      Mask        : Dirty_Mask) is abstract;
   --  Insert or update the element in the database.
   --  This can only be called when the element is associated with a session.
   --  If Self has a primary key, it will be updated, otherwise it will be
   --  inserted, and its primary keys updated accordingly.
   --  The element will automatically be marked as clean, this procedure does
   --  not need to do it.
   --  PK_Modified will be set to False if the PK was modified (in particular
   --  when the element was INSERT-ed in the database for the first time, with
   --  an auto-increment integer primary key).
   --
   --  This procedure can in turn call Insert_Or_Update on other elements, most
   --  likely its foreign keys if it needs their id, through the procedure
   --  below.

   procedure Insert_Or_Update
     (Self    : Session_Type;
      Element : in out Detached_Element'Class);
   --  This will call the primitive Insert_Or_Update on the element, and update
   --  the session cache appropriately (taking into account changes in the
   --  primary key, converting references to weak references,...)

   procedure Internal_Delete (Self : Detached_Element) is abstract;
   --  Emit the SQL necessary to delete the element from the database.

   -----------
   -- Debug --
   -----------

   procedure Trace_Debug
     (Me      : GNATCOLL.Traces.Trace_Handle;
      Session : Session_Type;
      Msg     : String := "");
   --  Print debug traces about the Session

   procedure Cache_Count
     (Self    : Session_Type;
      Refs    : out Natural;
      Weakref : out Natural);
   --  Return the number of elements in the cache

private
   type Base_Element is abstract tagged null record;

   type Weak_Cache is record
      Ref      : Pointers.Weak_Ref;
      Template : Detached_Element_Access;
   end record;
   --  a weak reference to an element. Template is always such that
   --  Get (Template) is null, ie we do not hold a reference to the element,
   --  but we still need to know its Ada tag so that we can recreate it when
   --  extracting the element from the cache.

   function Hash (Key : Element_Key) return Ada.Containers.Hash_Type;
   function "=" (W1, W2 : Weak_Cache) return Boolean;
   package Weak_Element_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type         => Element_Key,
      Element_Type     => Weak_Cache,
      Hash             => Hash,
      Equivalent_Keys  => "=",
      "="              => "=");
   --  A map of weak refs to elements.

   package Element_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Element_Key,
      Element_Type    => Detached_Element_Access,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => "=");
   --  A set of elements

   package Element_Lists is new Ada.Containers.Indefinite_Vectors
     (Natural, Detached_Element'Class);

   type User_Data_Access is access all User_Data'Class;

   type Session_Data is record
      Pool     : Session_Pool;
      DB       : Database_Connection;

      Wcache : Weak_Element_Maps.Map;
      Cache  : Element_Maps.Map;
      --  The cache for elements. Depending on the Weak_Cache setting, either
      --  one or the other is used. Modified elements always need to use a full
      --  ref, but they are on the list of modified elements below, which
      --  ensures we have a ref to them and they can't be finalized before they
      --  are flushed to the db.

      Modified_Elements : Element_Lists.Vector;
      --  The list of modified elements, that need to be flushed to the db.

      Factory  : Element_Factory := Null_Factory'Access;  --  not null

      Store_Unmodified   : Boolean;
      Weak_Cache         : Boolean;
      Persist_Cascade    : Boolean;
      Flush_Before_Query : Boolean;
      --  See Setup

      User     : User_Data_Access;
      --  User data for this session, will be allocated when Get is called.
   end record;
   --  Cache is implemented as an access type for efficiency: otherwise, every
   --  time we query Session.Element we would get a copy of the cache.

   type Pool_Data is record
      Pool                      : Session_Pool;
      Descr                     : Database_Description;
      Config_Weak_Cache         : Boolean := True;
      Config_Flush_Before_Query : Boolean := True;
      Config_Store_Unmodified   : Boolean := False;
      Config_Default_User_Data  : User_Data_Access;
      Config_Persist_Cascade    : Boolean := True;
   end record;

   procedure Free (Data : in out Pool_Data);

   function Impl_Factory (Data : Pool_Data) return Session_Data;
   procedure Impl_Free (Data : in out Session_Data);
   procedure Impl_On_Release (Data : in out Session_Data);
   --  Subprograms needed for the instantiation of Pools

   package Impl is new GNATCOLL.Pools
     (Element_Type  => Session_Data,
      Resource_Set  => Session_Pool,
      Factory_Param => Pool_Data,
      Factory       => Impl_Factory,
      Free          => Impl_Free,  --  Close SQL connection
      On_Release    => Impl_On_Release,
      Free_Param    => Free);
   --  Note on usage: calling Get will return a valid database connection.
   --  It is valid to use the following construct:
   --      A := All_<type>.Get (Connections.Get.Element)
   --  A will in fact hold a reference to the connection, which is then not
   --  released to the pool while you keep A.
   --  This means that you should not store such lists permanently in data
   --  structures, since you are otherwise keeping hold of resources.

   type Weak_Session is record
      Ref : Impl.Weak_Resource;
   end record;
   Null_Weak_Session : constant Weak_Session :=
     (Ref => Impl.Null_Weak_Resource);

   type Session_Type is new Impl.Resource with null record;
   No_Session : constant Session_Type := (Impl.No_Resource with null record);

   type Detached_Data (Field_Count : Natural)
   is abstract new Base_Detached_Data with record
      Session : Weak_Session;
      Dirty   : Dirty_Mask (0 .. Field_Count) := (others => False);
   end record;
   procedure Free (Self : in out Detached_Data);

   No_User_Data : constant User_Data'Class := User_Data'(null record);
end GNATCOLL.SQL.Sessions;
