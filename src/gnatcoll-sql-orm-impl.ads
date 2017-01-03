------------------------------------------------------------------------------
--                                   CRM                                    --
--                    [Customer Relationship Management]                    --
--                                                                          --
--                         Copyright (C) 2009-2017, AdaCore                 --
------------------------------------------------------------------------------

--  Implementation facilities for ORM.
--  Most of the subprograms are internal, and those that are meant to be used
--  by applications are described in gnatcoll-sql-orm.ads, as the primitive
--  operations of the managers

with Ada.Calendar;

package GNATCOLL.SQL.Orm.Impl is

   type Orm_Element is new Sessions.Base_Element with record
      Current : Forward_Cursor; --  One of the lists below.
      Data    : Shared_List_Data;
      Index   : Integer;        --  Position of Current at the time of creation
      Column  : Field_Index;    --  First column in Current for the fields
      Depth   : Natural;        --  Depth of sql query (related tables)
   end record;
   No_Orm_Element : constant Orm_Element;

   function Is_Null (Self : Orm_Element) return Boolean;
   --  Returns True if Self wasn't initialized

   function Integer_Value
     (Self : Orm_Element'Class; Field : Field_Index) return Integer;
   function Bigint_Value
     (Self : Orm_Element'Class; Field : Field_Index) return Long_Long_Integer;
   function Boolean_Value
     (Self : Orm_Element'Class; Field : Field_Index) return Boolean;
   function String_Value
     (Self : Orm_Element'Class; Field : Field_Index) return String;
   function String_Value
     (Self : Orm_Element'Class; Field : Field_Index) return Unbounded_String;
   function Time_Value
     (Self : Orm_Element'Class; Field : Field_Index) return Ada.Calendar.Time;
   function Float_Value
     (Self : Orm_Element'Class; Field : Field_Index) return Float;
   function Money_Value
     (Self : Orm_Element'Class; Field : Field_Index) return T_Money;
   --  Retrieve the specific field from the element

   --  Generic implementation of managers.
   --  This package is used to provide primitive operations that all managers
   --  should have, while avoiding the need to duplicate these in the generated
   --  code. The main benefit is to simplify the generator, since the resulting
   --  binary code has the same size.
   --  The design of this package is slightly complex: element_type and
   --  manager_type each have their own set of primitives that depends on the
   --  contents of the tables, but we need to extend them by adding new
   --  primitives. This is thus a mixin inheritance.
   --  But some of the primitives return other instances of managers or
   --  elements, which must have both sets of primitives (the ones in this
   --  package and the specialized ones).
   --
   --  Thus we take both Manager_Type and Element_Type as formal parameters.

   generic
      type Manager_Type is abstract new GNATCOLL.SQL.Orm.Manager with private;
      type Element_Type is new Orm_Element with private;
      type Related_Depth is range <>;
      Table_Type : SQL_Table'Class;

      with procedure Internal_Query
        (Fields    : in out SQL_Field_List;
         From      : out SQL_Table_List;
         Criteria  : in out SQL_Criteria;
         Depth     : Natural;
         Follow_LJ : Boolean;
         PK_Only   : Boolean := False) is <>;

   package Generic_Managers is
      type Manager is new Manager_Type with private;
      All_Managers : constant Manager;
      type List is new GNATCOLL.SQL.Orm.Forward_List with private;
      type Direct_List is new GNATCOLL.SQL.Orm.Direct_List with private;
      Empty_List        : constant List;
      Empty_Direct_List : constant Direct_List;

      function Element (Self : List'Class) return Element_Type;
      function Element (Self : Direct_List'Class) return Element_Type;
      --  Return the current element of the list

      function Get
        (Self    : Manager'Class;
         Session : Session_Type;
         Params  : SQL_Parameters := No_Parameters) return List;
      function Get_Direct
        (Self      : Manager'Class;
         Session   : Session_Type;
         Params    : SQL_Parameters := No_Parameters) return Direct_List;
      --  These take an explicit connection (instead of getting one from
      --  pool), so that you can reuse the same connection for multiple
      --  successive queries. The connection handle is stored in the list,
      --  and therefore not returned to the pool until the list is
      --  destroyed, so it is also better to be able to reuse the same
      --  connection.

      type ORM_Prepared_Statement is tagged record
         Stmt      : Prepared_Statement;
         Related   : Related_Depth := 0;
         Follow_LJ : Boolean := False;
      end record;
      --  A prepared statement that contains enough information to retrieve
      --  the associated element

      function Prepare
        (Self : Manager'Class;
         Use_Cache : Boolean := False;
         On_Server : Boolean := False;
         Name      : String := "") return ORM_Prepared_Statement;
      --  Prepare the statement on the server, for maximum efficiency

      function Get
        (Query     : ORM_Prepared_Statement'Class;
         Session   : Session_Type;
         Params    : SQL_Parameters := No_Parameters) return List;
      function Get_Direct
        (Query     : ORM_Prepared_Statement'Class;
         Session   : Session_Type;
         Params    : SQL_Parameters := No_Parameters) return Direct_List;
      function Get
        (Query     : String;
         Session   : Session_Type;
         Params    : SQL_Parameters := No_Parameters;
         Related   : Related_Depth := 0;
         Follow_LJ : Boolean := False) return List;
      --  This function will be more performance than the other version, where
      --  the query is build via a lot of memory allocations and other
      --  high-level constructs. However, this is more dangerous, since it
      --  expects fields in a specific order, in particular when Related > 0.
      --  Basically, this should only be used when the query was generated
      --  via the usual use of Managers, then looked up in the traces and
      --  copy-pasted.

      procedure Delete (Self : Manager'Class; Session : Session_Type);
      --  Delete all matching elements from the database.

      function Distinct (Self : Manager) return Manager;
      function Filter
        (Self : Manager; Condition : SQL_Criteria) return Manager;
      function Limit
        (Self  : Manager;
         Count : Natural;
         From  : Natural := 0) return Manager;
      function Order_By (Self : Manager; By   : SQL_Field_List) return Manager;
      function Order_By (Self : Manager; By : SQL_Field'Class) return Manager;
      function Select_Related
        (Self             : Manager;
         Depth            : Related_Depth;
         Follow_Left_Join : Boolean := False) return Manager;

      function Internal_Element
        (Self   : Orm_Element'Class;
         Field  : Field_Index) return Element_Type;
      pragma Inline (Internal_Element);
      --  Internal implementation of Element.
      --  Field is counted from the first first representing Self, not from the
      --  start of the SQL query (if for instance Self itself is a FK, we would
      --  have   SELECT orig_table.*, self.field1, self.field2,...

      function Build_Query
        (Self : Manager_Type'Class;
         Override_Fields : SQL_Field_List := Empty_Field_List)
         return SQL_Query;
      pragma Inline (Build_Query);
      --  Return the query to use for Self

      function Build_Delete (Self : Manager_Type'Class) return SQL_Query;
      pragma Inline (Build_Delete);
      --  Return the statement to use to delete all elements from Self

   private
      type Manager is new Manager_Type with null record;
      type List is new GNATCOLL.SQL.Orm.Forward_List with null record;
      type Direct_List is new GNATCOLL.SQL.Orm.Direct_List with null record;
      All_Managers : constant Manager := (Manager_Type with null record);
      Empty_List : constant List :=
        List'(No_Forward_List with null record);
      Empty_Direct_List : constant Direct_List :=
        Direct_List'(No_Direct_List with null record);
   end Generic_Managers;

private
   No_Orm_Element : constant Orm_Element :=
     (Base_Element
      with Current => GNATCOLL.SQL.Exec.No_Element,
      Data    => No_Shared_Lists_Data,
      Index   => -1,
      Column  => 0,
      Depth   => 0);
end GNATCOLL.SQL.Orm.Impl;
