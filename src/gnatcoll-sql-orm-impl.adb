------------------------------------------------------------------------------
--                                   CRM                                    --
--                    [Customer Relationship Management]                    --
--                                                                          --
--                         Copyright (C) 2009-2011, AdaCore                 --
------------------------------------------------------------------------------

with GNAT.Calendar;  use GNAT.Calendar;
with GNATCOLL.Traces;    use GNATCOLL.Traces;

package body GNATCOLL.SQL.Orm.Impl is

   Me : constant Trace_Handle := Create ("ORM");

   function Get_Data
      (Self : Forward_Cursor'Class) return Shared_List_Data_Access;
   pragma Inline (Get_Data);
   --  Return access to the data used by a list. The returned access is only
   --  valid while the list exists.

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Orm_Element) return Boolean is
   begin
      return Self.Index = -1;
   end Is_Null;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
      (Self : Forward_Cursor'Class) return Shared_List_Data_Access is
   begin
      --  Not very elegant, but this avoids duplicating this function for
      --  both types of lists in all the generated packages.
      --  The Unchecked_Access is safe because tagged types are always passed
      --  by reference (ARM)

      if Self in Forward_List'Class then
         return Forward_List (Self).Data'Unchecked_Access;
      elsif Self in Direct_List'Class then
         return Direct_List (Self).Data'Unchecked_Access;
      else
         return null;
      end if;
   end Get_Data;

   -------------------
   -- Integer_Value --
   -------------------

   function Integer_Value
     (Self : Orm_Element'Class; Field : Field_Index) return Integer is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Is_Null (Self.Current, Self.Column + Field) then
         return Integer'First;
      else
         return Integer_Value (Self.Current, Self.Column + Field);
      end if;
   end Integer_Value;

   -------------------
   -- Boolean_Value --
   -------------------

   function Boolean_Value
     (Self : Orm_Element'Class; Field : Field_Index) return Boolean is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Is_Null (Self.Current, Self.Column + Field) then
         return False;
      else
         return Boolean_Value (Self.Current, Self.Column + Field);
      end if;
   end Boolean_Value;

   -------------------
   -- String_Value --
   -------------------

   function String_Value
     (Self : Orm_Element'Class; Field : Field_Index) return String is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Is_Null (Self.Current, Self.Column + Field) then
         return "";
      else
         return Value (Self.Current, Self.Column + Field);
      end if;
   end String_Value;

   ------------------
   -- String_Value --
   ------------------

   function String_Value
     (Self : Orm_Element'Class; Field : Field_Index)
     return Unbounded_String is
   begin
      return To_Unbounded_String (String_Value (Self, Field));
   end String_Value;

   ----------------
   -- Time_Value --
   ----------------

   function Time_Value
     (Self : Orm_Element'Class; Field : Field_Index) return Ada.Calendar.Time
   is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Is_Null (Self.Current, Self.Column + Field) then
         return No_Time;
      else
         return Time_Value (Self.Current, Self.Column + Field);
      end if;
   end Time_Value;

   -----------------
   -- Float_Value --
   ------------------

   function Float_Value
     (Self : Orm_Element'Class; Field : Field_Index) return Float is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Is_Null (Self.Current, Self.Column + Field) then
         return Float'First;
      else
         return Float_Value (Self.Current, Self.Column + Field);
      end if;
   end Float_Value;

   -----------------
   -- Money_Value --
   -----------------

   function Money_Value
     (Self : Orm_Element'Class; Field : Field_Index)
     return GNATCOLL.Sql_Types.T_Money is
   begin
      if Current (Self.Current) /= Self.Index then
         raise Cursor_Has_Moved;
      end if;

      if Is_Null (Self.Current, Self.Column + Field) then
         return GNATCOLL.Sql_Types.T_Money'First;
      else
         return Money_Value (Self.Current, Self.Column + Field);
      end if;
   end Money_Value;

   ----------------------
   -- Generic_Managers --
   ----------------------

   package body Generic_Managers is

      function Internal_Element
        (Self   : Forward_Cursor'Class;
         Column : Field_Index;
         Data   : Shared_List_Data;
         Depth  : Natural) return Element_Type;
      pragma Inline (Internal_Element);

      ----------------------
      -- Internal_Element --
      ----------------------

      function Internal_Element
        (Self   : Forward_Cursor'Class;
         Column : Field_Index;
         Data   : Shared_List_Data;
         Depth  : Natural) return Element_Type
      is
      begin
         return Result : Element_Type do
            Result.Index   := Current (Self);
            Result.Column  := Column;
            Result.Depth   := Depth;
            Result.Data    := Data;
            Result.Current := Forward_Cursor (Self);
         end return;
      end Internal_Element;

      ----------------------
      -- Internal_Element --
      ----------------------

      function Internal_Element
        (Self   : Orm_Element'Class;
         Field  : Field_Index) return Element_Type is
      begin
         return Internal_Element
           (Self.Current, Self.Column + Field, Self.Data, Self.Depth - 1);
      end Internal_Element;

      -------------
      -- Element --
      -------------

      function Element
        (Self   : List'Class) return Element_Type
      is
         Data   : constant Shared_List_Data_Access := Get_Data (Self);
      begin
         Assert (Me, Data /= null, "No data stored in list of elements");
         Assert
           (Me, Data.Session /= No_Session, "Session from list was released");
         return Internal_Element (Self, 0, Get_Data (Self).all, Self.Depth);
      end Element;

      -------------
      -- Element --
      -------------

      function Element
        (Self   : Direct_List'Class) return Element_Type
      is
      begin
         return Internal_Element (Self, 0, Get_Data (Self).all, Self.Depth);
      end Element;

      -----------------
      -- Build_Query --
      -----------------

      function Build_Query
        (Self : Manager_Type'Class;
         Override_Fields : SQL_Field_List := Empty_Field_List)
        return SQL_Query
      is
         Q      : SQL_Query;
         F      : SQL_Field_List;
         T      : SQL_Table_List;
         C      : SQL_Criteria := No_Criteria;
      begin
         Internal_Query (F, T, C, Self.Select_Related, Self.Follow_LJ);
         if Override_Fields /= Empty_Field_List then
            F := Override_Fields;
         end if;

         Query (Self, Q, F, T, C);
         return Q;
      end Build_Query;

      ------------------
      -- Build_Delete --
      ------------------

      function Build_Delete (Self : Manager_Type'Class) return SQL_Query is
         Q      : SQL_Query;
         F      : SQL_Field_List;
         T      : SQL_Table_List;
         C      : SQL_Criteria := No_Criteria;
      begin
         Internal_Query
           (Fields    => F,
            From      => T,
            Criteria  => C,
            PK_Only   => True,
            Depth     => 0,   --  Never select related
            Follow_LJ => False);

         Query (Self, Q, F, T, C);
         Q := SQL_Delete
           (From     => Table_Type,
            Where    => SQL_In (Tuple (F), Q));
         Auto_Complete (Q);
         return Q;
      end Build_Delete;

      ---------
      -- Get --
      ---------

      function Get
        (Self    : Manager'Class;
         Session : Session_Type;
         Params  : SQL_Parameters := No_Parameters) return List
      is
         Result : List;
      begin
         Result.Data  := (Session => Session, Follow_LJ => Self.Follow_LJ);
         Result.Depth := Select_Related (Self);

         if Session.Flush_Before_Query then
            Session.Flush;
         end if;

         Result.Fetch (Session.DB, Build_Query (Self), Params);
         return Result;
      end Get;

      ---------
      -- Get --
      ---------

      function Get
        (Query     : String;
         Session   : Session_Type;
         Params    : SQL_Parameters := No_Parameters;
         Related   : Related_Depth := 0;
         Follow_LJ : Boolean := False) return List
      is
         Result : List;
      begin
         Result.Data  := (Session => Session, Follow_LJ => Follow_LJ);
         Result.Depth := Integer (Related);

         if Session.Flush_Before_Query then
            Session.Flush;
         end if;

         Result.Fetch (Session.DB, Query, Params);
         return Result;
      end Get;

      ------------
      -- Delete --
      ------------

      procedure Delete (Self : Manager'Class; Session : Session_Type) is
      begin
         Execute (Session.DB, Build_Delete (Self));
      end Delete;

      -------------
      -- Prepare --
      -------------

      function Prepare
        (Self : Manager'Class;
         Use_Cache : Boolean := False;
         On_Server : Boolean := False;
         Name      : String := "") return ORM_Prepared_Statement
      is
      begin
         return ORM_Prepared_Statement'
           (Related        => Related_Depth (Select_Related (Self)),
            Follow_LJ      => Self.Follow_LJ,
            Stmt           => Prepare
              (Build_Query (Self),
               Name      => Name,
               Use_Cache => Use_Cache,
               On_Server => On_Server));
      end Prepare;

      ----------------
      -- Get_Direct --
      ----------------

      function Get_Direct
        (Self      : Manager'Class;
         Session   : Session_Type;
         Params  : SQL_Parameters := No_Parameters) return Direct_List
      is
         Result : Direct_List;
      begin
         Result.Data  := (Session => Session, Follow_LJ => Self.Follow_LJ);
         Result.Depth := Select_Related (Self);

         if Session.Flush_Before_Query then
            Session.Flush;
         end if;

         Result.Fetch (Session.DB, Build_Query (Self), Params => Params);
         return Result;
      end Get_Direct;

      ---------
      -- Get --
      ---------

      function Get
        (Query     : ORM_Prepared_Statement'Class;
         Session   : Session_Type;
         Params    : SQL_Parameters := No_Parameters) return List
      is
         Result : List;
      begin
         Result.Data  := (Session => Session, Follow_LJ => Query.Follow_LJ);
         Result.Depth := Integer (Query.Related);

         if Session.Flush_Before_Query then
            Session.Flush;
         end if;

         Result.Fetch (Session.DB, Query.Stmt, Params);
         return Result;
      end Get;

      ----------------
      -- Get_Direct --
      ----------------

      function Get_Direct
        (Query     : ORM_Prepared_Statement'Class;
         Session   : Session_Type;
         Params    : SQL_Parameters := No_Parameters) return Direct_List
      is
         Result : Direct_List;
      begin
         Result.Data  := (Session => Session, Follow_LJ => Query.Follow_LJ);
         Result.Depth := Integer (Query.Related);

         if Session.Flush_Before_Query then
            Session.Flush;
         end if;

         Result.Fetch (Session.DB, Query.Stmt, Params => Params);
         return Result;
      end Get_Direct;

      --------------
      -- Distinct --
      --------------

      function Distinct (Self : Manager) return Manager
      is
         Result : Manager := Self;
      begin
         Distinct (Result);
         return Result;
      end Distinct;

      ------------
      -- Filter --
      ------------

      function Filter
        (Self : Manager; Condition : SQL_Criteria) return Manager
      is
         Result : Manager;
      begin
         --  Bug in GNAT, maybe: "Result := Self" results in a corruption
         --  in the reference counting of the criteria. The workaround is
         --  to do an explicit copy.
         Copy (Self, Into => Result);
         Filter (Result, Condition);
         return Result;
      end Filter;

      -----------
      -- Limit --
      -----------

      function Limit
        (Self  : Manager;
         Count : Natural;
         From  : Natural := 0) return Manager
      is
         Result : Manager := Self;
      begin
         Limit (Result, Count, From);
         return Result;
      end Limit;

      --------------
      -- Order_By --
      --------------

      function Order_By
        (Self : Manager;
         By   : SQL_Field_List) return Manager
      is
         Result : Manager := Self;
      begin
         GNATCOLL.SQL.Orm.Order_By (Result, By);
         return Result;
      end Order_By;

      --------------
      -- Order_By --
      --------------

      function Order_By
        (Self : Manager; By : SQL_Field'Class) return Manager
      is
         Result : Manager := Self;
      begin
         GNATCOLL.SQL.Orm.Order_By (Result, +By);
         return Result;
      end Order_By;

      --------------------
      -- Select_Related --
      --------------------

      function Select_Related
        (Self             : Manager;
         Depth            : Related_Depth;
         Follow_Left_Join : Boolean := False) return Manager
      is
         Result : Manager := Self;
      begin
         Select_Related (Result, Integer (Depth), Follow_Left_Join);
         return Result;
      end Select_Related;

   end Generic_Managers;

end GNATCOLL.SQL.Orm.Impl;
