------------------------------------------------------------------------------
--                                   CRM                                    --
--                    [Customer Relationship Management]                    --
--                                                                          --
--                         Copyright (C) 2009-2015, AdaCore                 --
------------------------------------------------------------------------------

package body GNATCOLL.SQL.Orm is

   --------------
   -- Order_By --
   --------------

   procedure Order_By (Self : in out Manager'Class; By : SQL_Field_List) is
   begin
      Self.Order_By := By;
   end Order_By;

   --------------
   -- Order_By --
   --------------

   procedure Order_By (Self : in out Manager'Class; By : SQL_Field'Class) is
   begin
      Self.Order_By := +By;
   end Order_By;

   -----------
   -- Limit --
   -----------

   procedure Limit
     (Self : in out Manager'Class; Count : Natural; From : Natural := 0)
   is
   begin
      Self.Limit_Count := Count;
      Self.Offset := From;
   end Limit;

   --------------
   -- Distinct --
   --------------

   procedure Distinct (Self : in out Manager'Class) is
   begin
      Self.Distinct := True;
   end Distinct;

   --------------------
   -- Select_Related --
   --------------------

   procedure Select_Related
      (Self  : in out Manager'Class;
       Depth : Natural;
       Follow_Left_Join : Boolean := False)
   is
   begin
      Self.Select_Related := Depth;
      Self.Follow_LJ      := Follow_Left_Join;
   end Select_Related;

   --------------------
   -- Select_Related --
   --------------------

   function Select_Related (Self : Manager'Class) return Natural is
   begin
      return Self.Select_Related;
   end Select_Related;

   ------------
   -- Filter --
   ------------

   procedure Filter (Self : in out Manager'Class; Condition : SQL_Criteria) is
   begin
      Self.Where := Self.Where and Condition;
   end Filter;

   -----------
   -- Query --
   -----------

   procedure Query (Self   : Manager'Class;
                    Query  : out SQL_Query;
                    Fields : SQL_Field_List;
                    From   : SQL_Table_List;
                    Criteria : SQL_Criteria := No_Criteria) is
   begin
      Query := SQL_Select
        (Fields   => Fields,
         From     => From,
         Where    => Self.Where and Criteria,
         Order_By => Self.Order_By,
         Limit    => Self.Limit_Count,
         Offset   => Self.Offset,
         Distinct => Self.Distinct);
      Auto_Complete (Query);
   end Query;

   ----------
   -- Copy --
   ----------

   procedure Copy (Self : Manager'Class; Into : in out Manager'Class) is
   begin
      Into.Where          := Self.Where;
      Into.Order_By       := Self.Order_By;
      Into.Limit_Count    := Self.Limit_Count;
      Into.Offset         := Self.Offset;
      Into.Distinct       := Self.Distinct;
      Into.Select_Related := Self.Select_Related;
      Into.Follow_LJ      := Self.Follow_LJ;
   end Copy;

   ---------------
   -- Follow_LJ --
   ---------------

   function Follow_LJ (Self : Manager'Class) return Boolean is
   begin
      return Self.Follow_LJ;
   end Follow_LJ;

end GNATCOLL.SQL.Orm;
