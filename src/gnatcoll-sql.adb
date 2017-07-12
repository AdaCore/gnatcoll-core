------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

with Ada.Calendar;               use Ada.Calendar;
with Ada.Calendar.Formatting;    use Ada.Calendar.Formatting;
with Ada.Containers;             use Ada.Containers;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;               use GNAT.Strings;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

package body GNATCOLL.SQL is

   use Table_List, Field_List, Criteria_Lists, Table_Sets;
   use When_Lists, Query_Pointers;
   use type Boolean_Fields.Field;

   Comparison_Like        : aliased constant String := " LIKE ";
   Comparison_ILike       : aliased constant String := " ILIKE ";
   Comparison_Not_Like    : aliased constant String := " NOT LIKE ";
   Comparison_Not_ILike   : aliased constant String := " NOT ILIKE ";
   Comparison_Overlaps    : aliased constant String := " OVERLAPS ";
   Comparison_Any         : aliased constant String := " = ANY (";
   Comparison_Parenthesis : aliased constant String := ")";

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (SQL_Table'Class, SQL_Table_Access);

   function Combine
     (Left, Right : SQL_Criteria; Op : Criteria_Combine) return SQL_Criteria;
   --  Combine the two criterias with a specific operator.

   procedure Append_Tables
     (From : SQL_Field_List; To : in out Table_Sets.Set);
   --  Append all tables referenced in From to To.

   function To_String (Names : Table_Names) return String;
   function To_String (Self : Table_Sets.Set) return Unbounded_String;
   --  Various implementations for To_String, for different types

   package Any_Fields is new Data_Fields (SQL_Field);
   type SQL_Field_Any is new Any_Fields.Field with null record;

   -------------------
   -- As field data --
   -------------------
   --  Used when a field is renamed via "anything AS name"

   type As_Field_Internal is new SQL_Field_Internal with record
      As      : GNAT.Strings.String_Access;
      Renamed : SQL_Field_Pointer;
   end record;
   overriding procedure Free (Self : in out As_Field_Internal);
   overriding function To_String
     (Self   : As_Field_Internal;
      Format : Formatter'Class;
      Long   : Boolean) return String;
   overriding procedure Append_Tables
     (Self : As_Field_Internal; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : access As_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

   --------------------------
   -- Multiple args fields --
   --------------------------
   --  Several fields grouped into one via functions, operators or other. Such
   --  fields are not typed ("field1 operator field2 operator field3 ...")

   type Multiple_Args_Field_Internal is new SQL_Field_Internal with record
      Func_Name      : GNAT.Strings.String_Access; --  can be null
      Separator      : GNAT.Strings.String_Access;
      Suffix         : GNAT.Strings.String_Access; --  can be null
      List           : Field_List.Vector;
   end record;
   overriding function To_String
     (Self   : Multiple_Args_Field_Internal;
      Format : Formatter'Class;
      Long   : Boolean) return String;
   overriding procedure Append_Tables
     (Self : Multiple_Args_Field_Internal; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : access Multiple_Args_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);
   overriding procedure Free (Self : in out Multiple_Args_Field_Internal);

   -----------------------
   -- Aggregrate fields --
   -----------------------
   --  Representing an sql aggregate function.

   type Aggregate_Field_Internal is new SQL_Field_Internal with record
      Func     : GNAT.Strings.String_Access;
      --  Func might be null if we only want to represent as criteria as
      --  a field
      Params   : SQL_Field_List;
      Criteria : SQL_Criteria;
      Order_By : SQL_Field_List;
   end record;
   overriding procedure Free (Self : in out Aggregate_Field_Internal);
   overriding function To_String
     (Self   : Aggregate_Field_Internal;
      Format : Formatter'Class;
      Long   : Boolean) return String;
   overriding procedure Append_Tables
     (Self : Aggregate_Field_Internal; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : access Aggregate_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

   -----------------
   -- Query fields --
   -----------------
   --  a SQL query represented as a field

   type Query_Field_Internal is new SQL_Field_Internal with record
      Query  : SQL_Query;
   end record;
   overriding function To_String
     (Self   : Query_Field_Internal;
      Format : Formatter'Class;
      Long   : Boolean) return String;
   overriding procedure Append_Tables
     (Self : Query_Field_Internal; To : in out Table_Sets.Set) is null;
   overriding procedure Append_If_Not_Aggregate
     (Self         : access Query_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean) is null;

   -----------------
   -- Sort fields --
   -----------------
   --  Fields used in the "ORDER BY" clauses

   type Sorted_Field_Internal is new SQL_Field_Internal with record
      Ascending : Boolean;
      Sorted    : SQL_Field_Pointer;
   end record;
   overriding function To_String
     (Self   : Sorted_Field_Internal;
      Format : Formatter'Class;
      Long   : Boolean) return String;
   overriding procedure Append_Tables
     (Self : Sorted_Field_Internal; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : access Sorted_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

   -------------------------
   -- Field_List_Function --
   -------------------------

   function Field_List_Function
     (Fields : SQL_Field_List) return SQL_Field'Class
   is
      Data   : Multiple_Args_Field_Internal;
      F : SQL_Field_Any
        (Table => null, Instance => null, Instance_Index => -1, Name => null);
      C : Field_List.Cursor := First (Fields);
   begin
      if Func_Name /= "" then
         Data.Func_Name := new String'(Func_Name);
      end if;

      Data.Separator := new String'(Separator);

      if Suffix /= "" then
         Data.Suffix := new String'(Suffix);
      end if;

      while Has_Element (C) loop
         declare
            Field    : constant SQL_Field'Class := Element (C);
            C2       : Field_List.Cursor;
            R        : Field_Pointers.Ref;
         begin
            if Field in SQL_Field_Any'Class then
               R := SQL_Field_Any (Field).Data;
               if R.Get.Element.all in Multiple_Args_Field_Internal'Class then
                  declare
                     D : Multiple_Args_Field_Internal'Class renames
                        Multiple_Args_Field_Internal'Class (R.Get.Element.all);
                  begin
                     if D.Separator.all = Separator then
                        --  Avoid nested concatenations, put them all at the
                        --  same level. This simplifies the query. Due to this,
                        --  we are also sure the concatenation itself doesn't
                        --  have sub-expressions

                        C2 := First (D.List);
                        while Has_Element (C2) loop
                           Append (Data.List, Element (C2));
                           Next (C2);
                        end loop;
                     else
                        Append (Data.List, Field);
                     end if;
                  end;
               else
                  Append (Data.List, Field);
               end if;
            else
               Append (Data.List, Field);
            end if;
         end;
         Next (C);
      end loop;

      F.Data.Set (Data);
      return F;
   end Field_List_Function;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : SQL_Left_Join_Table; Format : Formatter'Class) return String
   is
      Result : Unbounded_String;
      C      : Table_List.Cursor := Table_List.No_Element;
   begin
      if not Self.Data.Is_Null then
         C := First (Self.Data.Get.Tables.Data.Get);
      end if;

      Append (Result, "(");
      Append (Result, To_String (Element (C), Format));
      if Self.Data.Get.Is_Left_Join then
         Append (Result, " LEFT JOIN ");
      else
         Append (Result, " JOIN ");
      end if;
      Next (C);
      Append (Result, To_String (Element (C), Format));
      if Self.Data.Get.On /= No_Criteria then
         Append (Result, " ON ");
         Append
           (Result,
            GNATCOLL.SQL_Impl.To_String
              (Self.Data.Get.On, Format, Long => True));
      end if;
      Append (Result, ")");

      if Self.Instance /= null then
         Append (Result, " " & Self.Instance.all);
      end if;
      return To_String (Result);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : Subquery_Table; Format : Formatter'Class) return String is
   begin
      if Self.Instance /= null then
         return "(" & To_String (To_String (Self.Query, Format)) & ") "
           & Self.Instance.all;
      else
         return "(" & To_String (To_String (Self.Query, Format)) & ")";
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : SQL_Table'Class) return String is
   begin
      return To_String (Table_Names'
                          (Name           => Self.Table_Name,
                           Instance       => Self.Instance,
                           Instance_Index => Self.Instance_Index));
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : SQL_Table; Format : Formatter'Class) return String
   is
      pragma Unreferenced (Format);
   begin
      return To_String (Self);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : SQL_Table_List; Format : Formatter'Class) return String
   is
      C      : Table_List.Cursor := Table_List.No_Element;
      Result : Unbounded_String;
   begin
      if not Self.Data.Is_Null then
         C := First (Self.Data.Get);
      end if;

      if Has_Element (C) then
         Append (Result, To_String (Element (C), Format));
         Next (C);
      end if;

      while Has_Element (C) loop
         Append (Result, ", ");
         Append (Result, To_String (Element (C), Format));
         Next (C);
      end loop;

      return To_String (Result);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Names : Table_Names) return String is
      Instance : constant String := Instance_Name (Names);
   begin
      if Instance /= Names.Name.all then
         return Names.Name.all & " " & Instance;
      else
         return Names.Name.all;
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Table_Sets.Set) return Unbounded_String is
      Result : Unbounded_String;
      C      : Table_Sets.Cursor := First (Self);
   begin
      if Has_Element (C) then
         Append (Result, To_String (Element (C)));
         Next (C);
      end if;

      while Has_Element (C) loop
         Append (Result, ", ");
         Append (Result, To_String (Element (C)));
         Next (C);
      end loop;

      return Result;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self   : As_Field_Internal;
      Format : Formatter'Class;
      Long   : Boolean) return String
   is
      Has_Blank : Boolean := False;
   begin
      for J in Self.As'Range loop
         if Self.As (J) = ' ' then
            Has_Blank := True;
            exit;
         end if;
      end loop;

      if Has_Blank
        and then (Self.As (Self.As'First) /= '"'
                  or else Self.As (Self.As'Last) /= '"')
      then
         return To_String (Self.Renamed, Format, Long)
           & " AS """ & Self.As.all & """";
      else
         return To_String (Self.Renamed, Format, Long)
           & " AS " & Self.As.all;
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self   : Sorted_Field_Internal;
      Format : Formatter'Class;
      Long   : Boolean) return String is
   begin
      if Self.Ascending then
         return To_String (Self.Sorted, Format, Long => Long) & " ASC";
      else
         return To_String (Self.Sorted, Format, Long => Long) & " DESC";
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self   : Multiple_Args_Field_Internal;
      Format : Formatter'Class;
      Long   : Boolean) return String
   is
      C      : Field_List.Cursor := First (Self.List);
      Result : Unbounded_String;
   begin
      if Self.Func_Name /= null then
         Append (Result, Self.Func_Name.all);
      end if;

      if Has_Element (C) then
         Append (Result, To_String (Element (C), Format, Long));
         Next (C);
      end if;

      while Has_Element (C) loop
         Append (Result, Self.Separator.all);
         Append (Result, To_String (Element (C), Format, Long));
         Next (C);
      end loop;

      if Self.Suffix /= null then
         Append (Result, Self.Suffix.all);
      end if;

      return To_String (Result);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self   : Case_Stmt_Internal;
      Format : Formatter'Class;
      Long   : Boolean) return String
   is
      C : When_Lists.Cursor := First (Self.Criteria.List);
      Result : Unbounded_String;
   begin
      Append (Result, "CASE");
      while Has_Element (C) loop
         Append (Result, " WHEN "
                 & GNATCOLL.SQL_Impl.To_String (Element (C).Criteria, Format)
                 & " THEN "
                 & To_String (Element (C).Field, Format, Long));
         Next (C);
      end loop;

      if Self.Else_Clause /= No_Field_Pointer then
         Append
           (Result,
            " ELSE " & To_String (Self.Else_Clause, Format, Long));
      end if;

      Append (Result, " END");
      return To_String (Result);
   end To_String;

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : SQL_Table_List) return SQL_Table_List is
   begin
      if Left.Data.Is_Null then
         return Right;
      end if;

      if Right.Data.Is_Null then
         return Left;
      end if;

      Copy_Operands : declare
         Result : SQL_Table_List;
      begin
         Result.Data.Set (Table_List."&" (Left.Data.Get, Right.Data.Get));
         return Result;
      end Copy_Operands;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left : SQL_Table_List;
      Right : SQL_Single_Table'Class) return SQL_Table_List
   is
   begin
      return Left & (+Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : SQL_Single_Table'Class) return SQL_Table_List is
      Result : constant SQL_Table_List := +Left;
   begin
      Result.Data.Get.Append (Right);
      return Result;
   end "&";

   ---------
   -- "+" --
   ---------

   function "+" (Left : SQL_Single_Table'Class) return SQL_Table_List is
      Result : SQL_Table_List;
   begin
      Result.Data.Set (Table_List.To_Vector (Left, 1));
      return Result;
   end "+";

   --------
   -- As --
   --------

   function As
     (Field : SQL_Field'Class; Name : String) return SQL_Field'Class
   is
      Data : As_Field_Internal;
      F    : Field_Pointers.Ref;
   begin
      Data.As      := new String'(Name);
      Data.Renamed := +Field;
      F.Set (Data);
      return SQL_Field_Any'
        (Table          => null,
         Instance       => null,
         Name           => null,
         Instance_Index => -1,
         Data           => F);
   end As;

   ----------
   -- Desc --
   ----------

   function Desc (Field : SQL_Field'Class) return SQL_Field'Class is
      Data : Sorted_Field_Internal;
      F    : Field_Pointers.Ref;
   begin
      Data.Ascending := False;
      Data.Sorted    := +Field;
      F.Set (Data);
      return SQL_Field_Any'
        (Table          => null,
         Instance       => null,
         Name           => null,
         Instance_Index => -1,
         Data           => F);
   end Desc;

   ---------
   -- Asc --
   ---------

   function Asc  (Field : SQL_Field'Class) return SQL_Field'Class is
      Data : Sorted_Field_Internal;
      F    : Field_Pointers.Ref;
   begin
      Data.Ascending := True;
      Data.Sorted    := +Field;
      F.Set (Data);
      return SQL_Field_Any'
        (Table => null, Instance => null, Name => null,
         Instance_Index => -1,
         Data => F);
   end Asc;

   ------------------------
   -- Expression_Or_Null --
   ------------------------

   function Expression_Or_Null
     (Value : String) return Text_Fields.Field'Class
   is
   begin
      if Value = Null_String then
         return Text_Fields.From_String (Null_String);
      else
         return Text_Fields.Expression (Value);
      end if;
   end Expression_Or_Null;

   ----------------
   -- As_Boolean --
   ----------------

   function As_Boolean
      (Criteria : SQL_Criteria) return SQL_Field'Class
   is
      Data : Aggregate_Field_Internal;
      F    : Field_Pointers.Ref;
   begin
      Data.Criteria := Criteria;
      F.Set (Data);
      return SQL_Field_Any'
        (Table => null, Instance => null, Name => null,
         Instance_Index => -1,
         Data => F);
   end As_Boolean;

   -------------
   -- As_Days --
   -------------

   function As_Days (Count : Natural) return Time_Fields.Field'Class is
   begin
      return Time_Fields.From_String
        ("interval '" & Integer'Image (Count) & " days'");
   end As_Days;

   function As_Days (Count : Natural) return Date_Fields.Field'Class is
   begin
      return Date_Fields.From_String (Integer'Image (Count));
   end As_Days;

   ------------------
   -- At_Time_Zone --
   ------------------

   function At_Time_Zone
     (Field : Time_Fields.Field'Class; TZ : String)
      return Time_Fields.Field'Class
   is
      function Internal is new Time_Fields.Apply_Function
        (Time_Fields.Field, "", " at time zone '" & TZ & "'");
   begin
      return Internal (Field);
   end At_Time_Zone;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Multiple_Args_Field_Internal) is
   begin
      Free (Self.Suffix);
      Free (Self.Func_Name);
      Free (Self.Separator);
   end Free;

   ------------
   -- Concat --
   ------------

   function Concat (Fields : SQL_Field_List) return SQL_Field'Class is
      function Internal is new Field_List_Function ("", " || ", "");
   begin
      return Internal (Fields);
   end Concat;

   -----------
   -- Tuple --
   -----------

   function Tuple (Fields : SQL_Field_List) return SQL_Field'Class is
      function Internal is new Field_List_Function ("(", ", ", ")");
   begin
      return Internal (Fields);
   end Tuple;

   --------------
   -- Coalesce --
   --------------

   function Coalesce (Fields : SQL_Field_List) return SQL_Field'Class is
      function Internal is new Field_List_Function ("COALESCE (", ", ", ")");
   begin
      return Internal (Fields);
   end Coalesce;

   ---------
   -- "&" --
   ---------

   function "&" (List1, List2 : When_List) return When_List is
      Result : When_List;

      procedure Copy_Elements (L : When_List);
      --  Copy elements from L into Result

      -------------------
      -- Copy_Elements --
      -------------------

      procedure Copy_Elements (L : When_List) is
         C : When_Lists.Cursor := L.List.First;
      begin
         while Has_Element (C) loop
            Append (Result.List, Element (C));
            Next (C);
         end loop;
      end Copy_Elements;

   --  Start of processing for "&"

   begin
      Copy_Elements (List1);
      Copy_Elements (List2);
      return Result;
   end "&";

   --------------
   -- SQL_When --
   --------------

   function SQL_When
     (Criteria : SQL_Criteria; Field : SQL_Field'Class) return When_List
   is
      Result : When_List;
   begin
      Append (Result.List, (Criteria, +Field));
      return Result;
   end SQL_When;

   --------------
   -- SQL_Case --
   --------------

   function SQL_Case
     (List : When_List; Else_Clause : SQL_Field'Class := Null_Field_Text)
      return SQL_Field'Class
   is
      Data : Case_Stmt_Internal;
      F    : Field_Pointers.Ref;
   begin
      Data.Criteria := List;
      if Else_Clause /= SQL_Field'Class (Null_Field_Text) then
         Data.Else_Clause := +Else_Clause;
      end if;
      F.Set (Data);
      return SQL_Field_Any'
        (Table          => null,
         Instance       => null,
         Name           => null,
         Instance_Index => -1,
         Data           => F);
   end SQL_Case;

   -------------
   -- To_Char --
   -------------

   function To_Char
     (Field : Time_Fields.Field'Class; Format : String)
      return Text_Fields.Field'Class
   is
      function Internal is new Text_Fields.Apply_Function
        (Time_Fields.Field, "TO_CHAR (", ", '" & Format & "')");
   begin
      return Internal (Field);
   end To_Char;

   -------------
   -- Extract --
   -------------

   function Extract
     (Field : Time_Fields.Field'Class; Attribute : String)
      return Time_Fields.Field'Class
   is
      function Internal is new Time_Fields.Apply_Function
        (Time_Fields.Field, "EXTRACT (" & Attribute & " from ");
   begin
      return Internal (Field);
   end Extract;

   -------------
   -- Extract --
   -------------

   function Extract
     (Field : Date_Fields.Field'Class; Attribute : String)
      return Date_Fields.Field'Class
   is
      function Internal is new Date_Fields.Apply_Function
        (Date_Fields.Field, "EXTRACT (" & Attribute & " from ");
   begin
      return Internal (Field);
   end Extract;

   --------------
   -- Absolute --
   --------------

   function Absolute
     (Field : Integer_Fields.Field'Class) return Integer_Fields.Field'Class
   is
      function Internal is new Integer_Fields.Apply_Function
        (Integer_Fields.Field, "ABS (");
   begin
      return Internal (Field);
   end Absolute;

   -----------
   -- Lower --
   -----------

   function Lower
     (Field : Text_Fields.Field'Class) return Text_Fields.Field'Class
   is
      function Internal is new Text_Fields.Apply_Function
        (Text_Fields.Field, "LOWER (");
   begin
      return Internal (Field);
   end Lower;

   -----------
   -- Upper --
   -----------

   function Upper
     (Field : Text_Fields.Field'Class) return Text_Fields.Field'Class
   is
      function Internal is new Text_Fields.Apply_Function
        (Text_Fields.Field, "UPPER (");
   begin
      return Internal (Field);
   end Upper;

   -------------
   -- Initcap --
   -------------

   function Initcap
     (Field : Text_Fields.Field'Class) return Text_Fields.Field'Class
   is
      function Internal is new Text_Fields.Apply_Function
        (Text_Fields.Field, "INITCAP (");
   begin
      return Internal (Field);
   end Initcap;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self   : Aggregate_Field_Internal;
      Format : Formatter'Class;
      Long   : Boolean) return String
   is
      Result : Unbounded_String;

      procedure Append_Field_List (L : SQL_Field_List);
      --  Append L as a comma separated list to Result

      -----------------------
      -- Append_Field_List --
      -----------------------

      procedure Append_Field_List (L : SQL_Field_List) is
         C     : Field_List.Cursor := First (L);
         First : Boolean := True;
      begin
         while Has_Element (C) loop
            if First then
               First := False;
            else
               Append (Result, ", ");
            end if;
            Append (Result, To_String (Element (C), Format, Long));
            Next (C);
         end loop;
      end Append_Field_List;

   --  Start of processing for To_String

   begin
      if Self.Func /= null then
         Result := To_Unbounded_String (Self.Func.all & " (");
      end if;

      Append_Field_List (Self.Params);

      if Self.Criteria /= No_Criteria then
         Append (Result, GNATCOLL.SQL_Impl.To_String (Self.Criteria, Format));
      end if;

      --  Optional ORDER BY clause

      if Self.Order_By /= Empty_Field_List then
         Append (Result, " ORDER BY ");
         Append_Field_List (Self.Order_By);
      end if;

      if Self.Func /= null then
         Append (Result, ")");
      end if;

      return To_String (Result);
   end To_String;

   -----------
   -- Apply --
   -----------

   function Apply
     (Func     : Aggregate_Function;
      Criteria : SQL_Criteria;
      Order_By : SQL_Field_Or_List'Class := Empty_Field_List)
      return SQL_Field'Class
   is
      Data : Aggregate_Field_Internal;
      F    : Field_Pointers.Ref;

   begin
      Data.Criteria := Criteria;
      Data.Func     := new String'(String (Func));
      if Order_By in SQL_Field'Class then
         Data.Order_By := +SQL_Field'Class (Order_By);
      else
         Data.Order_By := SQL_Field_List (Order_By);
      end if;

      F.Set (Data);
      return SQL_Field_Any'
        (Table => null, Instance => null, Name => null,
         Instance_Index => -1,
         Data => F);
   end Apply;

   -----------
   -- Apply --
   -----------

   function Apply
     (Func     : Aggregate_Function;
      Fields   : SQL_Field_List;
      Order_By : SQL_Field_Or_List'Class := Empty_Field_List)
      return SQL_Field'Class
   is
      Data : Aggregate_Field_Internal;
      F    : Field_Pointers.Ref;

   begin
      Data.Params   := Fields;
      Data.Func     := new String'(String (Func));
      if Order_By in SQL_Field'Class then
         Data.Order_By := +SQL_Field'Class (Order_By);
      else
         Data.Order_By := SQL_Field_List (Order_By);
      end if;

      F.Set (Data);
      return SQL_Field_Any'
        (Table          => null,
         Instance       => null,
         Name           => null,
         Instance_Index => -1,
         Data           => F);
   end Apply;

   -----------
   -- Apply --
   -----------

   function Apply
     (Func     : Aggregate_Function;
      Field    : SQL_Field'Class;
      Order_By : SQL_Field_Or_List'Class := Empty_Field_List)
      return SQL_Field'Class
   is
      Data : Aggregate_Field_Internal;
      F    : Field_Pointers.Ref;

   begin
      Data.Params   := +Field;
      Data.Func     := new String'(String (Func));
      if Order_By in SQL_Field'Class then
         Data.Order_By := +SQL_Field'Class (Order_By);
      else
         Data.Order_By := SQL_Field_List (Order_By);
      end if;

      F.Set (Data);
      return SQL_Field_Any'
        (Table          => null,
         Instance       => null,
         Name           => null,
         Instance_Index => -1,
         Data           => F);
   end Apply;

   -------------
   -- To_List --
   -------------

   function To_List (Fields : SQL_Field_Array) return SQL_Field_List is
      S : SQL_Field_List;
   begin
      for A in Fields'Range loop
         Append (S, Fields (A));
      end loop;
      return S;
   end To_List;

   -----------
   -- "not" --
   -----------

   function "not" (Self : SQL_Criteria) return SQL_Criteria is
      Data   : SQL_Criteria_Data (Criteria_Not);
      Result : SQL_Criteria;
   begin
      if Self = No_Criteria then
         return No_Criteria;
      end if;

      Data.Criteria := Self;
      Set_Data (Result, Data);
      return Result;
   end "not";

   ------------
   -- Length --
   ------------

   function Length (Self : SQL_Criteria) return Natural is
      use type SQL_Criteria_Data_Access;
      Ptr : constant SQL_Criteria_Data_Access := Get_Data (Self);
   begin
      if Ptr = null then
         return 0;
      end if;

      if Ptr.all in SQL_Criteria_Data'Class
        and then SQL_Criteria_Data (Ptr.all).Op in Criteria_Combine
      then
         return Natural (SQL_Criteria_Data (Ptr.all).Criterias.Length);
      else
         return 1;
      end if;
   end Length;

   -----------
   -- Is_Or --
   -----------

   function Is_Or (Self : SQL_Criteria) return Boolean is
      use type SQL_Criteria_Data_Access;
      Ptr : constant SQL_Criteria_Data_Access := Get_Data (Self);
   begin
      return Ptr /= null and then Ptr.all in SQL_Criteria_Data'Class
        and then SQL_Criteria_Data (Ptr.all).Op = Criteria_Or;
   end Is_Or;

   ------------
   -- Is_And --
   ------------

   function Is_And (Self : SQL_Criteria) return Boolean is
      use type SQL_Criteria_Data_Access;
      Ptr : constant SQL_Criteria_Data_Access := Get_Data (Self);
   begin
      return Ptr /= null and then Ptr.all in SQL_Criteria_Data'Class
        and then SQL_Criteria_Data (Ptr.all).Op = Criteria_And;
   end Is_And;

   -------------
   -- Combine --
   -------------

   function Combine
     (List : Criteria_List; Op : Criteria_Combine) return SQL_Criteria
   is
      Result : SQL_Criteria;
      Data   : SQL_Criteria_Data (Op);
   begin
      Data.Criterias := List;
      Set_Data (Result, Data);
      return Result;
   end Combine;

   -------------
   -- Combine --
   -------------

   function Combine
     (Left, Right : SQL_Criteria; Op : Criteria_Combine) return SQL_Criteria
   is
      List : Criteria_List;
      C    : Criteria_Lists.Cursor;
   begin
      if Left = No_Criteria then
         return Right;
      elsif Right = No_Criteria then
         return Left;
      elsif Get_Data (Left).all in SQL_Criteria_Data'Class
        and then SQL_Criteria_Data (Get_Data (Left).all).Op = Op
      then
         --  ??? We could optimize when Left.Refcount=1, since we are modifying
         --  the last instance and thus do not need to copy the list

         List := SQL_Criteria_Data (Get_Data (Left).all).Criterias;

         if Get_Data (Right).all in SQL_Criteria_Data'Class
           and then SQL_Criteria_Data (Get_Data (Right).all).Op = Op
         then
            C := First (SQL_Criteria_Data (Get_Data (Right).all).Criterias);
            while Has_Element (C) loop
               Append (List, Element (C));
               Next (C);
            end loop;
         else
            Append (List, Right);
         end if;
      elsif Get_Data (Right).all in SQL_Criteria_Data'Class
        and then SQL_Criteria_Data (Get_Data (Right).all).Op = Op
      then
         List := SQL_Criteria_Data (Get_Data (Right).all).Criterias;
         Prepend (List, Left);
      else
         Append (List, Left);
         Append (List, Right);
      end if;

      return Combine (List, Op);
   end Combine;

   --------------
   -- Overlaps --
   --------------

   function Overlaps (Left, Right : SQL_Field'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Comparison_Overlaps'Access);
   end Overlaps;

   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : SQL_Criteria)  return SQL_Criteria is
   begin
      return Combine (Left, Right, Criteria_And);
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : SQL_Criteria)  return SQL_Criteria is
   begin
      return Combine (Left, Right, Criteria_Or);
   end "or";

   -----------
   -- "and" --
   -----------

   function "and"
     (Left : SQL_Criteria; Right : Boolean_Fields.Field'Class)
      return SQL_Criteria is
   begin
      return Left and (Right = True);
   end "and";

   ----------
   -- "or" --
   ----------

   function "or"
     (Left : SQL_Criteria; Right : Boolean_Fields.Field'Class)
      return SQL_Criteria is
   begin
      return Left or (Right = True);
   end "or";

   -----------
   -- "not" --
   -----------

   function "not" (Left : Boolean_Fields.Field'Class) return SQL_Criteria is
   begin
      return Left = False;
   end "not";

   ------------
   -- SQL_In --
   ------------

   function SQL_In
     (Self : SQL_Field'Class; List : SQL_Field_List) return SQL_Criteria
   is
      Data   : SQL_Criteria_Data (Criteria_In);
      Result : SQL_Criteria;
   begin
      Data.Arg := +Self;
      Data.List := List;
      Set_Data (Result, Data);
      return Result;
   end SQL_In;

   function SQL_In
     (Self : SQL_Field'Class; Subquery : SQL_Query) return SQL_Criteria
   is
      Data   : SQL_Criteria_Data (Criteria_In);
      Result : SQL_Criteria;
   begin
      Data.Arg := +Self;
      Data.Subquery := Subquery;
      Set_Data (Result, Data);
      return Result;
   end SQL_In;

   function SQL_In
     (Self : SQL_Field'Class; List : String) return SQL_Criteria
   is
      Data   : SQL_Criteria_Data (Criteria_In);
      Result : SQL_Criteria;
   begin
      Data.Arg := +Self;
      Data.In_String := To_Unbounded_String (List);
      Set_Data (Result, Data);
      return Result;
   end SQL_In;

   ------------
   -- Exists --
   ------------

   function Exists (Subquery : SQL_Query) return SQL_Criteria is
      Data   : SQL_Criteria_Data (Criteria_Exists);
      Result : SQL_Criteria;
   begin
      Data.Subquery2 := Subquery;
      Set_Data (Result, Data);
      return Result;
   end Exists;

   ----------------
   -- SQL_Not_In --
   ----------------

   function SQL_Not_In
     (Self : SQL_Field'Class; List : SQL_Field_List) return SQL_Criteria
   is
      Data   : SQL_Criteria_Data (Criteria_Not_In);
      Result : SQL_Criteria;
   begin
      Data.Arg := +Self;
      Data.List := List;
      Set_Data (Result, Data);
      return Result;
   end SQL_Not_In;

   function SQL_Not_In
     (Self : SQL_Field'Class; Subquery : SQL_Query) return SQL_Criteria
   is
      Data   : SQL_Criteria_Data (Criteria_Not_In);
      Result : SQL_Criteria;
   begin
      Data.Arg := +Self;
      Data.Subquery := Subquery;
      Set_Data (Result, Data);
      return Result;
   end SQL_Not_In;

   function SQL_Not_In
     (Self : SQL_Field'Class; List : String) return SQL_Criteria
   is
      Data   : SQL_Criteria_Data (Criteria_Not_In);
      Result : SQL_Criteria;
   begin
      Data.Arg := +Self;
      Data.In_String := To_Unbounded_String (List);
      Set_Data (Result, Data);
      return Result;
   end SQL_Not_In;

   -----------------
   -- SQL_Between --
   -----------------

   function SQL_Between
     (Self, Left, Right : SQL_Field'Class) return SQL_Criteria is
   begin
      return Result : SQL_Criteria do
         Set_Data
           (Result,
            SQL_Criteria_Data'
              (Criteria_Between,
               Arg2 => +Self, Left => +Left, Right => +Right));
      end return;
   end SQL_Between;

   ---------------------
   -- SQL_Not_Between --
   ---------------------

   function SQL_Not_Between
     (Self, Left, Right : SQL_Field'Class) return SQL_Criteria is
   begin
      return Result : SQL_Criteria do
         Set_Data
           (Result,
            SQL_Criteria_Data'
              (Criteria_Not_Between,
               Arg2 => +Self, Left => +Left, Right => +Right));
      end return;
   end SQL_Not_Between;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : SQL_Field'Class) return SQL_Criteria is
      Data : SQL_Criteria_Data (Criteria_Null);
      Result : SQL_Criteria;
   begin
      Data.Arg3 := +Self;
      Set_Data (Result, Data);
      return Result;
   end Is_Null;

   -----------------
   -- Is_Not_Null --
   -----------------

   function Is_Not_Null (Self : SQL_Field'Class) return SQL_Criteria is
      Data : SQL_Criteria_Data (Criteria_Not_Null);
      Result : SQL_Criteria;
   begin
      Data.Arg3 := +Self;
      Set_Data (Result, Data);
      return Result;
   end Is_Not_Null;

   ---------
   -- Any --
   ---------

   function Any (Self, Str : Text_Fields.Field'Class) return SQL_Criteria is
   begin
      return Compare
        (Self, Str, Comparison_Any'Access, Comparison_Parenthesis'Access);
   end Any;

   -----------
   -- Ilike --
   -----------

   function Ilike
     (Self : Text_Fields.Field'Class; Str : String) return SQL_Criteria is
   begin
      return Compare (Self, Expression (Str), Comparison_ILike'Access);
   end Ilike;

   ----------
   -- Like --
   ----------

   function Like
     (Self : Text_Fields.Field'Class; Str : String) return SQL_Criteria is
   begin
      return Compare (Self, Expression (Str), Comparison_Like'Access);
   end Like;

   -----------
   -- Ilike --
   -----------

   function Ilike
     (Self : Text_Fields.Field'Class; Field : SQL_Field'Class)
      return SQL_Criteria is
   begin
      return Compare (Self, Field, Comparison_ILike'Access);
   end Ilike;

   ----------
   -- Like --
   ----------

   function Like
     (Self : Text_Fields.Field'Class; Field : Text_Fields.Field'Class)
      return SQL_Criteria
   is
   begin
      return Compare (Self, Field, Comparison_Like'Access);
   end Like;

   ---------------
   -- Not_Ilike --
   ---------------

   function Not_Ilike
     (Self : Text_Fields.Field'Class; Str : String) return SQL_Criteria is
   begin
      return Compare (Self, Expression (Str), Comparison_Not_ILike'Access);
   end Not_Ilike;

   --------------
   -- Not_Like --
   --------------

   function Not_Like
     (Self : Text_Fields.Field'Class; Str : String) return SQL_Criteria is
   begin
      return Compare (Self, Expression (Str), Comparison_Not_Like'Access);
   end Not_Like;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self   : SQL_Criteria_Data;
      Format : Formatter'Class;
      Long   : Boolean := True) return String
   is
      Result : Unbounded_String;
      List   : Unbounded_String;
      C      : Criteria_Lists.Cursor;
      C2     : Field_List.Cursor;
      Is_First : Boolean;
      Criteria : SQL_Criteria;
   begin
      case Self.Op is
         when Criteria_Combine =>
            C := First (Self.Criterias);
            while Has_Element (C) loop
               if C /= First (Self.Criterias) then
                  case Self.Op is
                     when Criteria_And => Append (Result, " AND ");
                     when Criteria_Or  => Append (Result, " OR ");
                     when others       => null;
                  end case;
               end if;

               Criteria := Element (C);
               if Get_Data (Criteria).all in SQL_Criteria_Data'Class
                 and then SQL_Criteria_Data (Get_Data (Criteria).all).Op
                    in Criteria_Combine
               then
                  Append (Result, "(");
                  Append (Result,
                          GNATCOLL.SQL_Impl.To_String (Element (C), Format));
                  Append (Result, ")");
               else
                  Append (Result,
                          GNATCOLL.SQL_Impl.To_String (Element (C), Format));
               end if;
               Next (C);
            end loop;

         when Criteria_In | Criteria_Not_In =>
            List := Null_Unbounded_String;
            Is_First := True;
            C2 := First (Self.List);
            while Has_Element (C2) loop
               if not Is_First then
                  Append (List, ",");
               end if;

               Is_First := False;
               Append (List, To_String (Element (C2), Format, Long));
               Next (C2);
            end loop;
            Append (List, To_String (Self.Subquery, Format));
            Append (List, To_String (Self.In_String));

            if List = "" then
               --  "A in ()" is same as "False"
               --  "A not in ()" is same as "True"

               Result := To_Unbounded_String
                  (Expression (Self.Op = Criteria_Not_In)
                   .To_String (Format, Long));
            else
               Result :=
                  To_Unbounded_String (To_String (Self.Arg, Format, Long));
               Append (Result,
                       (if Self.Op = Criteria_In
                        then " IN (" else " NOT IN ("));
               Append (Result, List);
               Append (Result, ")");
            end if;

         when Criteria_Exists =>
            Result := To_Unbounded_String ("EXISTS (");
            Append (Result, To_String (Self.Subquery2, Format));
            Append (Result, ")");

         when Criteria_Between | Criteria_Not_Between =>
            Result := To_Unbounded_String
                        (To_String (Self.Arg2, Format, Long));

            if Self.Op = Criteria_Not_Between then
               Append (Result, " NOT");
            end if;

            Append
              (Result,
               " BETWEEN " & To_String (Self.Left, Format, Long) & " AND "
               & To_String (Self.Right, Format, Long));

         when Null_Criteria =>
            Result := To_Unbounded_String
              (To_String (Self.Arg3, Format, Long));

            case Self.Op is
               when Criteria_Null     => Append (Result, " IS NULL");
               when Criteria_Not_Null => Append (Result, " IS NOT NULL");
               when others            => null;
            end case;

         when Criteria_Not =>
            Result := To_Unbounded_String
              ("NOT (" & To_String (Self.Criteria, Format, Long) & ")");

      end case;
      return To_String (Result);
   end To_String;

   ----------------
   -- SQL_Values --
   ----------------

   function SQL_Values (Val : Field_List_Array) return SQL_Query is
      Q    : SQL_Query;
      Data : constant Query_Pointers.Encapsulated_Access :=
        new Query_Values_Contents'
          (Query_Contents with Size => Val'Length, Values => Val);
      --  We have to declare and assign Data with definite access type first
      --  and then put it into Q.Set. If we use operator "new" as a parameter
      --  for Q.Set we've got a runtime error "accessibility check failed"
      --  inside of Q.Set. Checked at GNATLS Pro 18.0w (20170525-63).
   begin
      Q.Set (Data);
      return Q;
   end SQL_Values;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Self : Query_Values_Contents;
      Format : Formatter'Class) return Unbounded_String
   is
      Result : Unbounded_String := To_Unbounded_String ("VALUES ");
   begin
      for R in Self.Values'Range loop
         if R /= Self.Values'First then
            Append (Result, ", (");
         else
            Append (Result, '(');
         end if;

         Append (Result, To_String (Self.Values (R), Format, Long => True));
         Append (Result, ')');
      end loop;
      return Result;
   end To_String;

   ----------------
   -- SQL_Select --
   ----------------

   function SQL_Select
     (Fields   : SQL_Field_Or_List'Class;
      From     : SQL_Table_Or_List'Class := Empty_Table_List;
      Where    : SQL_Criteria := No_Criteria;
      Group_By : SQL_Field_Or_List'Class := Empty_Field_List;
      Having   : SQL_Criteria := No_Criteria;
      Order_By : SQL_Field_Or_List'Class := Empty_Field_List;
      Limit    : Integer := -1;
      Offset   : Integer := -1;
      Distinct : Boolean := False;
      Auto_Complete : Boolean := False) return SQL_Query
   is
      Data : Query_Select_Contents;
      Q    : SQL_Query;
   begin
      if Fields in SQL_Field'Class then
         Data.Fields := +SQL_Field'Class (Fields);
      else
         Data.Fields := SQL_Field_List (Fields);
      end if;

      if From in SQL_Table_List'Class then
         Data.Tables   := SQL_Table_List (From);
      else
         Data.Tables := +SQL_Single_Table'Class (From);
      end if;

      Data.Criteria := Where;

      if Group_By in SQL_Field'Class then
         Data.Group_By := +SQL_Field'Class (Group_By);
      else
         Data.Group_By := SQL_Field_List (Group_By);
      end if;

      Data.Having   := Having;

      if Order_By in SQL_Field'Class then
         Data.Order_By := +SQL_Field'Class (Order_By);
      else
         Data.Order_By := SQL_Field_List (Order_By);
      end if;

      Data.Limit    := Limit;
      Data.Offset   := Offset;
      Data.Distinct := Distinct;
      Q.Set (Data);

      if Auto_Complete then
         GNATCOLL.SQL.Auto_Complete (Q);
      end if;

      return Q;
   end SQL_Select;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Self : Query_Select_Contents;
      Format : Formatter'Class) return Unbounded_String
   is
      Result : Unbounded_String;
   begin
      Result := To_Unbounded_String ("SELECT ");
      if Self.Distinct then
         Append (Result, "DISTINCT ");
      end if;
      Append (Result, To_String (Self.Fields, Format, Long => True));
      if Self.Tables /= Empty_Table_List
        or else not Is_Empty (Self.Extra_Tables)
      then
         Append (Result, " FROM ");
         if Self.Tables.Data.Is_Null or else Self.Tables.Data.Get.Is_Empty then
            Append (Result, To_String (Self.Extra_Tables));
         elsif Is_Empty (Self.Extra_Tables) then
            Append (Result, To_String (Self.Tables, Format));
         else
            Append (Result, To_String (Self.Tables, Format));
            Append (Result, ", ");
            Append (Result, To_String (Self.Extra_Tables));
         end if;
      end if;
      if Self.Criteria /= No_Criteria then
         Append (Result, " WHERE ");
         Append (Result, GNATCOLL.SQL_Impl.To_String (Self.Criteria, Format));
      end if;
      if Self.Group_By /= Empty_Field_List then
         Append (Result, " GROUP BY ");
         Append (Result, To_String (Self.Group_By, Format, Long => True));
         if Self.Having /= No_Criteria then
            Append (Result, " HAVING ");
            Append (Result, GNATCOLL.SQL_Impl.To_String (Self.Having, Format));
         end if;
      end if;
      if Self.Order_By /= Empty_Field_List then
         Append (Result, " ORDER BY ");
         Append (Result, To_String (Self.Order_By, Format, Long => True));
      end if;

      --  Need to output LIMIT before OFFSET for sqlite. This seems to be
      --  compatible with other backends

      if Self.Limit >= 0 or else Self.Offset >= 0 then
         Append (Result, " LIMIT" & Integer'Image (Self.Limit));
      end if;
      if Self.Offset >= 0 then
         Append (Result, " OFFSET" & Integer'Image (Self.Offset));
      end if;
      return Result;
   end To_String;

   ---------------
   -- SQL_Union --
   ---------------

   function SQL_Union
     (Query1, Query2 : SQL_Query;
      Order_By : SQL_Field_Or_List'Class := Empty_Field_List;
      Limit    : Integer := -1;
      Offset   : Integer := -1;
      Distinct : Boolean := False) return SQL_Query
   is
      Data : Query_Union_Contents;
      Q    : SQL_Query;
   begin
      Data.Q1 := Query1;
      Data.Q2 := Query2;

      if Order_By in SQL_Field'Class then
         Data.Order_By := +SQL_Field'Class (Order_By);
      else
         Data.Order_By := SQL_Field_List (Order_By);
      end if;

      Data.Limit := Limit;
      Data.Offset := Offset;
      Data.Distinct := Distinct;
      Q.Set (Data);
      return Q;
   end SQL_Union;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Self   : Query_Union_Contents;
      Format : Formatter'Class) return Unbounded_String
   is
      Result : Unbounded_String;
   begin
      Append (Result, To_String (Self.Q1, Format));
      Append (Result, " UNION ");

      if not Self.Distinct then
         Append (Result, "ALL ");
      end if;

      Append (Result, To_String (Self.Q2, Format));
      Append (Result, " ");

      if Self.Order_By /= Empty_Field_List then
         Append (Result, " ORDER BY ");
         Append (Result, To_String (Self.Order_By, Format, Long => True));
      end if;

      --  Need to output LIMIT before OFFSET for sqlite. This seems to be
      --  compatible with other backends

      if Self.Limit >= 0 or else Self.Offset >= 0 then
         Append (Result, " LIMIT" & Integer'Image (Self.Limit));
      end if;
      if Self.Offset >= 0 then
         Append (Result, " OFFSET" & Integer'Image (Self.Offset));
      end if;

      return Result;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : SQL_Query; Format : Formatter'Class) return Unbounded_String is
   begin
      if Self.Get = null then
         return Null_Unbounded_String;
      else
         return To_String (Self.Get.all, Format);
      end if;
   end To_String;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out As_Field_Internal) is
   begin
      Free (Self.As);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Aggregate_Field_Internal) is
   begin
      Free (Self.Func);
   end Free;

   -------------------
   -- Auto_Complete --
   -------------------

   procedure Auto_Complete
     (Self                   : in out Query_Select_Contents;
      Auto_Complete_From     : Boolean := True;
      Auto_Complete_Group_By : Boolean := True)
   is
      List2  : Table_Sets.Set;
      Group_By : SQL_Field_List;
      Has_Aggregate : Boolean := False;
   begin
      if Auto_Complete_From then
         --  For each field, make sure the table is in the list
         Append_Tables (Self.Fields, Self.Extra_Tables);
         Append_Tables (Self.Group_By, Self.Extra_Tables);
         Append_Tables (Self.Order_By, Self.Extra_Tables);
         Append_Tables (Self.Criteria, Self.Extra_Tables);
         Append_Tables (Self.Having, Self.Extra_Tables);

         Append_Tables (Self.Tables, List2);
         Difference (Self.Extra_Tables, List2);
      end if;

      if Auto_Complete_Group_By then
         Append_If_Not_Aggregate (Self.Fields,   Group_By, Has_Aggregate);
         Append_If_Not_Aggregate (Self.Order_By, Group_By, Has_Aggregate);
         Append_If_Not_Aggregate (Self.Having,   Group_By, Has_Aggregate);
         if Has_Aggregate then
            Self.Group_By := Group_By;
         end if;
      end if;
   end Auto_Complete;

   -------------------
   -- Auto_Complete --
   -------------------

   procedure Auto_Complete
     (Self                   : in out SQL_Query;
      Auto_Complete_From     : Boolean := True;
      Auto_Complete_Group_By : Boolean := True) is
   begin
      if Self.Get /= null then
         Auto_Complete
           (Self.Get.all, Auto_Complete_From, Auto_Complete_Group_By);
      end if;
   end Auto_Complete;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (Self : SQL_Criteria_Data; To : in out Table_Sets.Set)
   is
      C : Criteria_Lists.Cursor;
   begin
      case Self.Op is
         when Criteria_Combine =>
            C := First (Self.Criterias);
            while Has_Element (C) loop
               Append_Tables (Element (C), To);
               Next (C);
            end loop;

         when Criteria_In | Criteria_Not_In =>
            Append_Tables (Self.Arg, To);

         when Criteria_Exists =>
            null;

         when Criteria_Between | Criteria_Not_Between =>
            Append_Tables (Self.Arg2, To);

         when Null_Criteria =>
            Append_Tables (Self.Arg3, To);

         when Criteria_Not =>
            Append_Tables (Self.Criteria, To);
      end case;
   end Append_Tables;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (Self : SQL_Left_Join_Table; To : in out Table_Sets.Set)
   is
      C : Table_List.Cursor;
   begin
      if not Self.Data.Get.Tables.Data.Is_Null then
         C := First (Self.Data.Get.Tables.Data.Get);
         while Has_Element (C) loop
            Append_Tables (Element (C), To);
            Next (C);
         end loop;
      end if;
   end Append_Tables;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables (Self : SQL_Table; To : in out Table_Sets.Set) is
   begin
      if Self.Table_Name /= null then
         Include (To, (Name     => Self.Table_Name,
                       Instance => Self.Instance,
                       Instance_Index => Self.Instance_Index));
      end if;
   end Append_Tables;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (Self : SQL_Table_List; To : in out Table_Sets.Set)
   is
      C : Table_List.Cursor;
   begin
      if not Self.Data.Is_Null then
         C := First (Self.Data.Get);
         while Has_Element (C) loop
            Append_Tables (Element (C), To);
            Next (C);
         end loop;
      end if;
   end Append_Tables;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (From : SQL_Field_List; To : in out Table_Sets.Set)
   is
      C : Field_List.Cursor := First (From);
   begin
      while Has_Element (C) loop
         Append_Tables (Element (C), To);
         Next (C);
      end loop;
   end Append_Tables;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (Self : As_Field_Internal; To : in out Table_Sets.Set) is
   begin
      Append_Tables (Self.Renamed, To);
   end Append_Tables;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (Self : Sorted_Field_Internal; To : in out Table_Sets.Set) is
   begin
      Append_Tables (Self.Sorted, To);
   end Append_Tables;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (Self : Multiple_Args_Field_Internal; To : in out Table_Sets.Set)
   is
      C : Field_List.Cursor := First (Self.List);
   begin
      while Has_Element (C) loop
         Append_Tables (Element (C), To);
         Next (C);
      end loop;
   end Append_Tables;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (Self : Case_Stmt_Internal; To : in out Table_Sets.Set)
   is
      C : When_Lists.Cursor := First (Self.Criteria.List);
   begin
      while Has_Element (C) loop
         Append_Tables (Element (C).Field, To);
         Next (C);
      end loop;

      if Self.Else_Clause /= No_Field_Pointer then
         Append_Tables (Self.Else_Clause, To);
      end if;
   end Append_Tables;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (Self : Aggregate_Field_Internal; To : in out Table_Sets.Set)
   is
      C : Field_List.Cursor := First (Self.Params);
   begin
      while Has_Element (C) loop
         Append_Tables (Element (C), To);
         Next (C);
      end loop;
      Append_Tables (Self.Criteria, To);
   end Append_Tables;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_List;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean)
   is
      C : Field_List.Cursor := First (Self);
   begin
      while Has_Element (C) loop
         Append_If_Not_Aggregate (Element (C), To, Is_Aggregate);
         Next (C);
      end loop;
   end Append_If_Not_Aggregate;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   procedure Append_If_Not_Aggregate
     (Self         : access As_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean) is
   begin
      Append_If_Not_Aggregate (Self.Renamed, To, Is_Aggregate);
   end Append_If_Not_Aggregate;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   procedure Append_If_Not_Aggregate
     (Self         : access Sorted_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean) is
   begin
      Append_If_Not_Aggregate (Self.Sorted, To, Is_Aggregate);
   end Append_If_Not_Aggregate;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   procedure Append_If_Not_Aggregate
     (Self         : access Multiple_Args_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean)
   is
      C : Field_List.Cursor := First (Self.List);
   begin
      while Has_Element (C) loop
         Append_If_Not_Aggregate (Element (C), To, Is_Aggregate);
         Next (C);
      end loop;
   end Append_If_Not_Aggregate;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   procedure Append_If_Not_Aggregate
     (Self         : access Case_Stmt_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean)
   is
      C : When_Lists.Cursor := First (Self.Criteria.List);
   begin
      while Has_Element (C) loop
         Append_If_Not_Aggregate (Element (C).Criteria, To, Is_Aggregate);
         Append_If_Not_Aggregate (Element (C).Field, To, Is_Aggregate);
         Next (C);
      end loop;

      if Self.Else_Clause /= No_Field_Pointer then
         Append_If_Not_Aggregate (Self.Else_Clause, To, Is_Aggregate);
      end if;
   end Append_If_Not_Aggregate;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   procedure Append_If_Not_Aggregate
     (Self         : SQL_Criteria_Data;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean)
   is
      C : Criteria_Lists.Cursor;
   begin
      case Self.Op is
         when Criteria_Combine =>
            C := First (Self.Criterias);
            while Has_Element (C) loop
               Append_If_Not_Aggregate (Element (C), To, Is_Aggregate);
               Next (C);
            end loop;

         when Criteria_In | Criteria_Not_In =>
            Append_If_Not_Aggregate (Self.Arg, To, Is_Aggregate);

         when Criteria_Exists =>
            null;

         when Criteria_Between | Criteria_Not_Between =>
            Append_If_Not_Aggregate (Self.Arg2, To, Is_Aggregate);

         when Null_Criteria =>
            Append_If_Not_Aggregate (Self.Arg3, To, Is_Aggregate);

         when Criteria_Not =>
            Append_If_Not_Aggregate (Self.Criteria, To, Is_Aggregate);
      end case;
   end Append_If_Not_Aggregate;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   procedure Append_If_Not_Aggregate
     (Self         : access Aggregate_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean)
   is
      pragma Unreferenced (Self, To);
   begin
      Is_Aggregate := True;
   end Append_If_Not_Aggregate;

   ----------------------
   -- SQL_Create_Table --
   ----------------------

   function SQL_Create_Table
     (Name      : String;
      As        : SQL_Query;
      Temp      : Boolean := False;
      On_Commit : Temp_Table_Behavior := Preserve_Rows) return SQL_Query
   is
      Data : Query_Create_Table_As_Contents;
      Q    : SQL_Query;
   begin
      Data.Name      := To_Unbounded_String (Name);
      Data.As        := As;
      Data.Temp      := Temp;
      Data.On_Commit := On_Commit;
      Q.Set (Data);
      return Q;
   end SQL_Create_Table;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : Query_Create_Table_As_Contents; Format : Formatter'Class)
      return Unbounded_String
   is
      Result : Unbounded_String;
   begin
      Result := To_Unbounded_String ("CREATE ");
      if Self.Temp then
         Append (Result, "TEMPORARY ");
      end if;
      Append (Result, "TABLE ");
      Append (Result, Self.Name);
      if Self.Temp then
         Append (Result, " ON COMMIT ");
         case Self.On_Commit is
            when Preserve_Rows => Append (Result, "PRESERVE ROWS");
            when Delete_Rows   => Append (Result, "DELETE ROWS");
            when Drop          => Append (Result, "DROP");
         end case;
      end if;
      Append (Result, " AS (");
      Append (Result, Unbounded_String'(To_String (Self.As, Format)));
      Append (Result, ')');

      return Result;
   end To_String;

   ----------------
   -- SQL_Delete --
   ----------------

   function SQL_Delete
     (From     : SQL_Table'Class;
      Where    : SQL_Criteria := No_Criteria) return SQL_Query
   is
      Data : Query_Delete_Contents;
      Q    : SQL_Query;
   begin
      Data.Table := +From;
      Data.Where := Where;
      Q.Set (Data);
      return Q;
   end SQL_Delete;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : Query_Delete_Contents; Format : Formatter'Class)
      return Unbounded_String
   is
      Result : Unbounded_String;
   begin
      Result := To_Unbounded_String ("DELETE FROM ");
      Append (Result,
              To_String (Element (First (Self.Table.Data.Get)), Format));

      if Self.Where /= No_Criteria then
         Append (Result, " WHERE ");
         Append
           (Result,
            GNATCOLL.SQL_Impl.To_String (Self.Where, Format, Long => False));
      end if;

      return Result;
   end To_String;

   -------------------------------
   -- SQL_Insert_Default_Values --
   -------------------------------

   function SQL_Insert_Default_Values
     (Table : SQL_Table'Class) return SQL_Query
   is
      Data : Query_Insert_Contents;
      Q    : SQL_Query;
   begin
      Data.Into := (Name     => Table.Table_Name,
                    Instance => Table.Instance,
                    Instance_Index => Table.Instance_Index);
      Data.Default_Values := True;
      Q.Set (Data);
      return Q;
   end SQL_Insert_Default_Values;

   ----------------
   -- SQL_Insert --
   ----------------

   function SQL_Insert
     (Fields    : SQL_Field_Or_List'Class;
      Values    : SQL_Query;
      Qualifier : String := "") return SQL_Query
   is
      Data : Query_Insert_Contents;
      Q    : SQL_Query;
   begin
      if Fields in SQL_Field'Class then
         Data.Fields := +SQL_Field'Class (Fields);
      else
         Data.Fields := SQL_Field_List (Fields);
      end if;

      Data.Into   := No_Names;
      Data.Subquery := Values;
      if Qualifier /= "" then
         Data.Qualifier := To_Unbounded_String (Qualifier);
      end if;

      Q.Set (Data);
      Auto_Complete (Q);
      return Q;
   end SQL_Insert;

   ----------------
   -- SQL_Insert --
   ----------------

   function SQL_Insert
     (Values    : SQL_Assignment;
      Where     : SQL_Criteria := No_Criteria;
      Limit     : Integer := -1;
      Qualifier : String := "") return SQL_Query
   is
      Data : Query_Insert_Contents;
      Q    : SQL_Query;
   begin
      Data.Into   := No_Names;
      Data.Values := Values;
      Data.Where  := Where;
      Data.Limit  := Limit;

      if Qualifier /= "" then
         Data.Qualifier := To_Unbounded_String (Qualifier);
      end if;

      Q.Set (Data);
      Auto_Complete (Q);
      return Q;
   end SQL_Insert;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : Query_Insert_Contents; Format : Formatter'Class)
      return Unbounded_String
   is
      Result : Unbounded_String;
   begin
      if Self.Qualifier = "" then
         Result := To_Unbounded_String ("INSERT INTO ");
      else
         Result := "INSERT " & Self.Qualifier & " INTO ";
      end if;

      Append (Result, To_String (Self.Into));

      if Self.Default_Values then
         Append (Result, " DEFAULT VALUES");
      else
         if Self.Fields /= Empty_Field_List then
            Append (Result, " (");
            Append (Result, To_String (Self.Fields, Format, Long => False));
            Append (Result, ")");
         end if;

         declare
            Assign : constant String :=
              To_String (Self.Values, Format, With_Field => False);
         begin
            if Assign /= "" then
               Append (Result, " VALUES (" & Assign & ")");
            end if;
         end;

         if Self.Subquery /= No_Query then
            Append (Result, " ");
            Append (Result, To_String (Self.Subquery, Format));
         end if;
      end if;

      return Result;
   end To_String;

   -------------------
   -- Auto_Complete --
   -------------------

   procedure Auto_Complete
     (Self                   : in out Query_Insert_Contents;
      Auto_Complete_From     : Boolean := True;
      Auto_Complete_Group_By : Boolean := True)
   is
      pragma Unreferenced (Auto_Complete_Group_By);
      List, List2 : Table_Sets.Set;
      Subfields   : SQL_Field_List;
   begin
      if Auto_Complete_From then

         --  Get the list of fields first, so that we'll also know what table
         --  is being updated

         if Self.Fields = Empty_Field_List then
            Get_Fields (Self.Values, Self.Fields);
         end if;

         if Self.Into = No_Names then
            --  For each field, make sure the table is in the list
            Append_Tables (Self.Fields, List);

            --  We must have a single table here, or that's a bug
            if Length (List) /= 1 then
               raise Program_Error
                 with "Invalid list of fields to insert, they all must modify"
                   & " the same table";
            end if;

            --  Grab the table from the first field
            Self.Into := Element (First (List));
         end if;

         if Self.Subquery = No_Query then
            --  Do we have other tables impacted from the list of values we
            --  set for the fields ? If yes, we'll need to transform the
            --  simple query into a subquery

            Clear (List);
            Append_Tables (Self.Values, List);
            if Self.Into /= No_Names then
               Table_Sets.Include (List2, Self.Into);
            end if;

            Difference (List, List2);  --  Remove tables already in the list
            if Length (List) > 0 then
               To_List (Self.Values, Subfields);
               Self.Subquery := SQL_Select
                 (Fields => Subfields, Where => Self.Where,
                  Limit => Self.Limit);
               Auto_Complete (Self.Subquery);
               Self.Values := No_Assignment;
            end if;
         end if;
      end if;
   end Auto_Complete;

   ----------------
   -- SQL_Update --
   ----------------

   function SQL_Update
     (Table    : SQL_Table'Class;
      Set      : SQL_Assignment;
      Where    : SQL_Criteria := No_Criteria;
      From     : SQL_Table_Or_List'Class := Empty_Table_List) return SQL_Query
   is
      Data : Query_Update_Contents;
      Q    : SQL_Query;
   begin
      Data.Table := +Table;
      Data.Set   := Set;
      Data.Where := Where;

      if From in SQL_Table'Class then
         Data.From := +SQL_Table'Class (From);
      else
         Data.From := SQL_Table_List (From);
      end if;

      Q.Set (Data);
      return Q;
   end SQL_Update;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : Query_Update_Contents; Format : Formatter'Class)
      return Unbounded_String
   is
      Result : Unbounded_String;
   begin
      Result := To_Unbounded_String ("UPDATE ");
      Append (Result,
              To_String (Element (First (Self.Table.Data.Get)), Format));

      Append (Result, " SET ");
      Append (Result, To_String (Self.Set, Format, With_Field => True));

      if Self.From /= Empty_Table_List
        or else not Is_Empty (Self.Extra_From)
      then
         Append (Result, " FROM ");
         if Self.From.Data.Is_Null or else Self.From.Data.Get.Is_Empty then
            Append (Result, To_String (Self.Extra_From));
         elsif Is_Empty (Self.Extra_From) then
            Append (Result, To_String (Self.From, Format));
         else
            Append (Result, To_String (Self.From, Format));
            Append (Result, ", ");
            Append (Result, To_String (Self.Extra_From));
         end if;
      end if;

      if Self.Where /= No_Criteria then
         Append (Result, " WHERE ");
         Append
           (Result,
            GNATCOLL.SQL_Impl.To_String (Self.Where, Format, Long => True));
      end if;
      return Result;
   end To_String;

   -------------------
   -- Auto_Complete --
   -------------------

   procedure Auto_Complete
     (Self                   : in out Query_Update_Contents;
      Auto_Complete_From     : Boolean := True;
      Auto_Complete_Group_By : Boolean := True)
   is
      pragma Unreferenced (Auto_Complete_Group_By);
      List2  : Table_Sets.Set;
   begin
      if Auto_Complete_From then
         --  For each field, make sure the table is in the list
         Append_Tables (Self.Set,   Self.Extra_From);
         Append_Tables (Self.Where, Self.Extra_From);

         --  Remove tables already in the list
         Append_Tables (Self.From,  List2);
         Append_Tables (Self.Table, List2);
         Difference (Self.Extra_From, List2);
      end if;
   end Auto_Complete;

   ---------------
   -- Left_Join --
   ---------------

   function Left_Join
     (Full    : SQL_Single_Table'Class;
      Partial : SQL_Single_Table'Class;
      On      : SQL_Criteria) return SQL_Left_Join_Table
   is
   begin
      return Result : SQL_Left_Join_Table
        (Instance => null, Instance_Index => -1)
      do
         Result.Data.Set
           (Join_Table_Internal'
              (Tables       => Full & Partial,
               Is_Left_Join => True,
               On           => On));
      end return;
   end Left_Join;

   ----------
   -- Join --
   ----------

   function Join
     (Table1 : SQL_Single_Table'Class;
      Table2 : SQL_Single_Table'Class;
      On     : SQL_Criteria := No_Criteria) return SQL_Left_Join_Table
   is
      R : constant SQL_Left_Join_Table := Left_Join (Table1, Table2, On);
   begin
      R.Data.Get.Is_Left_Join := False;
      return R;
   end Join;

   ------------
   -- Rename --
   ------------

   function Rename
     (Self : SQL_Left_Join_Table; Name : Cst_String_Access)
      return SQL_Left_Join_Table'Class
   is
      R : SQL_Left_Join_Table (Instance => Name, Instance_Index => -1);
   begin
      R.Data := Self.Data;
      return R;
   end Rename;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : Simple_Query_Contents; Format : Formatter'Class)
      return Unbounded_String
   is
      pragma Unreferenced (Format);
   begin
      return Self.Command;
   end To_String;

   --------------
   -- SQL_Lock --
   --------------

   function SQL_Lock (Table : SQL_Table'Class) return SQL_Query is
      Data : Simple_Query_Contents;
      Q    : SQL_Query;
   begin
      Data.Command := To_Unbounded_String ("LOCK " & To_String (Table));
      Q.Set (Data);
      return Q;
   end SQL_Lock;

   ---------------
   -- SQL_Begin --
   ---------------

   function SQL_Begin return SQL_Query is
      Data : Simple_Query_Contents;
      Q    : SQL_Query;
   begin
      Data.Command := To_Unbounded_String ("BEGIN");
      Q.Set (Data);
      return Q;
   end SQL_Begin;

   ------------------
   -- SQL_Rollback --
   ------------------

   function SQL_Rollback return SQL_Query is
      Data : Simple_Query_Contents;
      Q    : SQL_Query;
   begin
      Data.Command := To_Unbounded_String ("ROLLBACK");
      Q.Set (Data);
      return Q;
   end SQL_Rollback;

   ----------------
   -- SQL_Commit --
   ----------------

   function SQL_Commit return SQL_Query is
      Data : Simple_Query_Contents;
      Q    : SQL_Query;
   begin
      Data.Command := To_Unbounded_String ("COMMIT");
      Q.Set (Data);
      return Q;
   end SQL_Commit;

   --------------
   -- Subquery --
   --------------

   function Subquery
     (Query : SQL_Query'Class; Table_Name : Cst_String_Access)
      return Subquery_Table
   is
   begin
      return R : Subquery_Table
        (Instance => Table_Name, Instance_Index => -1)
      do
         R.Query := SQL_Query (Query);
      end return;
   end Subquery;

   ----------
   -- Free --
   ----------

   procedure Free (A : in out SQL_Table_Access) is
   begin
      Unchecked_Free (A);
   end Free;

   ---------------
   -- Where_And --
   ---------------

   function Where_And
     (Query : SQL_Query; Where : SQL_Criteria) return SQL_Query
   is
      Q2       : SQL_Query;
   begin
      if Query.Get.all not in Query_Select_Contents'Class then
         raise Program_Error with "not a SELECT query";
      end if;

      declare
         Contents : Query_Select_Contents'Class :=   --  clone contents
            Query_Select_Contents'Class (Query.Get.all);
      begin
         Contents.Criteria := Contents.Criteria and Where;
         Q2.Set (Contents);
         return Q2;
      end;
   end Where_And;

   --------------
   -- Where_Or --
   --------------

   function Where_Or
     (Query : SQL_Query; Where : SQL_Criteria) return SQL_Query
   is
      Q2       : SQL_Query;
   begin
      if Query.Get.all not in Query_Select_Contents'Class then
         raise Program_Error with "not a SELECT query";
      end if;

      declare
         Contents : Query_Select_Contents'Class :=   --  clone contents
            Query_Select_Contents'Class (Query.Get.all);
      begin
         Contents.Criteria := Contents.Criteria or Where;
         Q2.Set (Contents);
         return Q2;
      end;
   end Where_Or;

   --------------
   -- Order_By --
   --------------

   function Order_By
     (Query : SQL_Query; Order_By : SQL_Field_Or_List'Class)
      return SQL_Query
   is
      Q2       : SQL_Query;
   begin
      if Query.Get.all not in Query_Select_Contents'Class then
         raise Program_Error with "not a SELECT query";
      end if;

      declare
         Contents : Query_Select_Contents'Class :=   --  clone contents
            Query_Select_Contents'Class (Query.Get.all);
      begin
         if Order_By in SQL_Field'Class then
            Contents.Order_By :=
               SQL_Field'Class (Order_By) & Contents.Order_By;
         else
            Contents.Order_By := SQL_Field_List (Order_By) & Contents.Order_By;
         end if;

         Q2.Set (Contents);
         return Q2;
      end;
   end Order_By;

   --------------
   -- Distinct --
   --------------

   function Distinct (Query : SQL_Query) return SQL_Query is
      Q2       : SQL_Query;
   begin
      if Query.Get.all not in Query_Select_Contents'Class then
         raise Program_Error with "not a SELECT query";
      end if;

      declare
         Contents : Query_Select_Contents'Class :=   --  clone contents
            Query_Select_Contents'Class (Query.Get.all);
      begin
         Contents.Distinct := True;
         Q2.Set (Contents);
         return Q2;
      end;
   end Distinct;

   -----------
   -- Limit --
   -----------

   function Limit (Query : SQL_Query; Limit : Natural) return SQL_Query is
      Q2       : SQL_Query;
   begin
      if Query.Get.all not in Query_Select_Contents'Class then
         raise Program_Error with "not a SELECT query";
      end if;

      declare
         Contents : Query_Select_Contents'Class :=   --  clone contents
            Query_Select_Contents'Class (Query.Get.all);
      begin
         Contents.Limit := Limit;
         Q2.Set (Contents);
         return Q2;
      end;
   end Limit;

   ------------
   -- Offset --
   ------------

   function Offset (Query : SQL_Query; Offset : Natural) return SQL_Query is
      Q2       : SQL_Query;
   begin
      if Query.Get.all not in Query_Select_Contents'Class then
         raise Program_Error with "not a SELECT query";
      end if;

      declare
         Contents : Query_Select_Contents'Class :=   --  clone contents
            Query_Select_Contents'Class (Query.Get.all);
      begin
         Contents.Offset := Offset;
         Q2.Set (Contents);
         return Q2;
      end;
   end Offset;

   ---------
   -- "=" --
   ---------

   function "="
      (Left  : SQL_Field'Class; Query : SQL_Query) return SQL_Assignment
   is
      Data : Query_Field_Internal;
      F    : Field_Pointers.Ref;
   begin
      Data.Query := Query;
      F.Set (Data);

      return Create
         (Left,
          SQL_Field_Any'
             (Table          => null,
              Instance       => null,
              Name           => null,
              Instance_Index => -1,
              Data           => F));
   end "=";

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Self   : Query_Field_Internal;
      Format : Formatter'Class;
      Long   : Boolean) return String
   is
      pragma Unreferenced (Long);
   begin
      return "(" & To_String (To_String (Self.Query, Format)) & ")";
   end To_String;

   -----------------
   -- Time_To_SQL --
   -----------------

   function Time_To_SQL
     (Self  : Formatter'Class;
      Value : Ada.Calendar.Time;
      Quote : Boolean) return String
   is
   begin
      --  Value is always rendered as GMT, using Ada.Calendar.Formatting

      if Value /= No_Time then
         declare
            Value_Str : constant String := Image (Value, Time_Zone => 0)
              & (if Supports_Timezone (Self) then " +00:00" else "");
         begin
            return (if Quote then ''' & Value_Str & ''' else Value_Str);
         end;
      else
         return "NULL";
      end if;
   end Time_To_SQL;

   -----------------
   -- Date_To_SQL --
   -----------------

   function Date_To_SQL
     (Self  : Formatter'Class;
      Value : Ada.Calendar.Time;
      Quote : Boolean) return String
   is
      pragma Unreferenced (Self);
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Duration;
   begin
      if Value /= No_Time then
         Split (Value, Year, Month, Day, Seconds);

         declare
            Y : constant String := Image (Year, Min_Width => 4);
            M : constant String := Image (Month, Min_Width => 2);
            D : constant String := Image (Day, Min_Width => 2);
            Result : String (1 .. 12);
         begin
            Result (2 .. 5) := Y (Y'First .. Y'First + 3);
            Result (6) := '-';
            Result (7 .. 8) := M (M'First .. M'First + 1);
            Result (9) := '-';
            Result (10 .. 11) := D (D'First .. D'First + 1);

            if Quote then
               Result (1) := ''';
               Result (12) := ''';
               return Result;
            else
               return Result (2 .. 11);
            end if;
         end;
      else
         return "NULL";
      end if;
   end Date_To_SQL;

   -------------------
   -- Date_From_SQL --
   -------------------

   function Date_From_SQL
     (Format : Formatter'Class; Value : String) return Ada.Calendar.Time
   is
      pragma Unreferenced (Format);
      --  Do not use Time_From_SQL, since it tries to handle timezones
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
   begin
      --  SQL standard requires the use of ISO 8601 in the output of dates,
      --  so we parse that format.

      if Value'Length >= 10
         and then Value (Value'First + 4) = '-'
      then
         Year := Year_Number'Value (Value (Value'First .. Value'First + 3));
         Month := Month_Number'Value
            (Value (Value'First + 5 .. Value'First + 6));
         Day := Day_Number'Value (Value (Value'First + 8 .. Value'First + 9));
         return Ada.Calendar.Time_Of (Year, Month, Day, 0.0);

      else
         --  Postgres supports other formats, which are not used in practice.
         --  Better raise an exception to let users know though
         raise Constraint_Error with "Date format not supported";
      end if;
   end Date_From_SQL;

   ---------------
   -- Maps_Text --
   ---------------

   function Maps_Text
      (Schema : String; Value : out Field_Text_Data) return Boolean
   is
   begin
      if Schema = "text"
         or else Schema = "varchar"
         or else
            (Schema'Length >= 10    --  "character varying(...)"
             and then Schema (Schema'First .. Schema'First + 9) = "character ")
      then
         Value.Max_Length := Integer'Last;
         return True;

      elsif Schema'Length >= 8
         and then Schema (Schema'First .. Schema'First + 7) = "varchar("
      then
         begin
            Value.Max_Length := Integer'Value
               (Schema (Schema'First + 8 .. Schema'Last - 1));
            return  True;
         exception
            when Constraint_Error =>
               raise Invalid_Schema with
                  "Missing max length after 'varchar' in " & Schema;
         end;

      elsif Schema'Length >= 5
         and then Schema (Schema'First .. Schema'First + 4) = "char("
      then
         begin
            Value.Max_Length := Integer'Value
               (Schema (Schema'First + 5 .. Schema'Last - 1));
            return  True;
         exception
            when Constraint_Error =>
               raise Invalid_Schema with
                  "Missing length after 'char' in " & Schema;
         end;

      elsif Schema'Length >= 10
        and then Schema (Schema'First .. Schema'First + 9) = "character("
      then
         begin
            Value.Max_Length :=
               Integer'Value (Schema (Schema'First + 10 .. Schema'Last - 1));
            return True;
         exception
            when Constraint_Error =>
               raise Invalid_Schema with
                  "Missing length after 'Character' in " & Schema;
         end;
      end if;

      return False;
   end Maps_Text;

   ------------------
   -- Maps_Integer --
   ------------------

   function Maps_Integer
      (Schema : String; V : out Null_Record) return Boolean
   is
      pragma Unreferenced (V);
   begin
      if Schema in "integer" | "smallint" | "oid" then
         return True;

      elsif Schema'Length >= 7
         and then Schema (Schema'First .. Schema'First + 6) = "numeric"
      then
         --  Check the scale
         for Comma in reverse Schema'Range loop
            if Schema (Comma) = ',' then
               return Schema (Comma + 1 .. Schema'Last - 1) = "0";
            elsif Schema (Comma) = ')' then
               --  "numeric(position)" selects a scale of 0, so
               --  should be an integer;
               return True;
            end if;
         end loop;

         --  "numeric" (without an argument) means maximum
         --  precision, so should be mapped to a long_float.
         return False;
      end if;

      return False;
   end Maps_Integer;

   -------------------
   -- Time_From_SQL --
   -------------------

   function Time_From_SQL
     (Format : Formatter'Class; Value : String) return Ada.Calendar.Time
   is
      pragma Unreferenced (Format);
   begin
      if Value = "" then
         return No_Time;
      else
         --  Workaround bug(?) in GNAT.Calendar.Time_IO: if there is no time,
         --  set one to avoid daylight saving time issues

         if Ada.Strings.Fixed.Index (Value, ":") < Value'First then
            return GNATCOLL.Utils.Time_Value (Value & " 12:00:00");
         else
            return GNATCOLL.Utils.Time_Value (Value);
         end if;
      end if;
   end Time_From_SQL;

end GNATCOLL.SQL;
