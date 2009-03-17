-----------------------------------------------------------------------
--                               G N A T C O L L                     --
--                                                                   --
--                 Copyright (C) 2005-2009, AdaCore                  --
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

with Ada.Calendar;               use Ada.Calendar;
with Ada.Calendar.Time_Zones;    use Ada.Calendar.Time_Zones;
with Ada.Containers;             use Ada.Containers;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;
with GNAT.Calendar.Time_IO;      use GNAT.Calendar.Time_IO;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

package body GNATCOLL.SQL is

   use Table_List, Field_List, Criteria_List, Table_Sets, Assignment_Lists;
   use When_Lists;

   Separator_Comma  : aliased constant String := ", ";
   Separator_Concat : aliased constant String := " || ";
   Separator_Space  : aliased constant String := " ";

   Func_None     : aliased constant String := "";
   Func_Coalesce : aliased constant String := "COALESCE";
   Func_To_Char  : aliased constant String := "TO_CHAR";
   Func_Extract  : aliased constant String := "EXTRACT";
   Func_Lower    : aliased constant String := "lower";
   Func_InitCap  : aliased constant String := "initcap";

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (SQL_Table'Class, SQL_Table_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (SQL_Field'Class, Field_Access);

   function Normalize_String (Str : String) return String;
   --  Escape every apostrophe character "'" and backslash "\".
   --  Useful for strings in SQL commands where "'" means the end
   --  of the current string.

   function Compare
     (Left, Right : SQL_Field'Class; Op : SQL_Criteria_Type)
      return SQL_Criteria;
   --  Internal shared code for all "=", "<", ">", "<=" and ">=" operators

   function Combine
     (Left, Right : SQL_Criteria; Op : SQL_Criteria_Type) return SQL_Criteria;
   --  Combine the two criterias with a specific operator.

   procedure Append_Tables
     (From : SQL_Field_List; To : in out Table_Sets.Set);
   --  Append all tables referenced in From to To.

   procedure Assign
     (R     : out SQL_Assignment;
      Field : SQL_Field'Class;
      Value : GNAT.Strings.String_Access);
   --  Assign Value to Field (or set field to NULL if Value is null)

   function Create_Multiple_Args
     (Fields         : SQL_Field_List;
      Func_Name      : Cst_String_Access;
      Separator      : Cst_String_Access;
      Suffix         : String := "";
      In_Parenthesis : Boolean := False)
      return Multiple_Args_Field_Internal_Access;
   --  Create a multiple_args field from Fields

   function To_String (Names : Table_Names) return String;
   function To_String (Self : Table_Sets.Set) return Unbounded_String;
   --  Various implementations for To_String, for different types

   procedure Append_If_Not_Aggregate
     (Self         : SQL_Criteria;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);
   --  Same as Append_If_Not_Aggregate for fields

   No_Field_Pointer : constant SQL_Field_Pointer :=
                        (Ada.Finalization.Controlled with null);

   ----------------------
   -- Normalize_String --
   ----------------------

   function Normalize_String (Str : String) return String
   is
      Num_Of_Apostrophes : constant Natural :=
        Ada.Strings.Fixed.Count (Str, "'");
      Num_Of_Backslashes : constant Natural :=
        Ada.Strings.Fixed.Count (Str, "\");
      New_Str            : String
        (Str'First .. Str'Last + Num_Of_Apostrophes + Num_Of_Backslashes);
      Index              : Natural := Str'First;
      Prepend_E          : Boolean := False;
   begin
      if Num_Of_Apostrophes = 0
        and then Num_Of_Backslashes = 0
      then
         return "'" & Str & "'";
      end if;

      for I in Str'Range loop
         if Str (I) = ''' then
            New_Str (Index .. Index + 1) := "''";
            Index := Index + 1;
         elsif Str (I) = '\' then
            New_Str (Index .. Index + 1) := "\\";
            Prepend_E := True;
            Index := Index + 1;
         else
            New_Str (Index) := Str (I);
         end if;
         Index := Index + 1;
      end loop;

      if Prepend_E then
         return "E'" & New_Str & "'";
      else
         return "'" & New_Str & "'";
      end if;
   end Normalize_String;

   --------
   -- FK --
   --------

   function FK
     (Self : SQL_Table; Foreign : SQL_Table'Class) return SQL_Criteria
   is
      pragma Unreferenced (Self, Foreign);
   begin
      return No_Criteria;
   end FK;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : SQL_Left_Join_Table) return String is
      Result : Unbounded_String;
      C      : Table_List.Cursor := Table_List.No_Element;
   begin
      if Self.Data.Data /= null then
         C := First (Self.Data.Data.Tables.Data.Data.List);
      end if;

      Append (Result, "(");
      Append (Result, To_String (Element (C)));
      if Self.Data.Data.Is_Left_Join then
         Append (Result, " LEFT JOIN ");
      else
         Append (Result, " JOIN ");
      end if;
      Next (C);
      Append (Result, To_String (Element (C)));
      if Self.Data.Data.On /= No_Criteria then
         Append (Result, " ON ");
         Append (Result, To_String (Self.Data.Data.On, Long => True));
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

   function To_String (Self : Subquery_Table) return String is
   begin
      if Self.Instance /= null then
         return "(" & To_String (To_String (Self.Query)) & ") "
           & Self.Instance.all;
      else
         return "(" & To_String (To_String (Self.Query)) & ")";
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : SQL_Table) return String is
   begin
      if Self.Instance = null then
         return Self.Table_Name.all;
      else
         return Self.Table_Name.all & " " & Self.Instance.all;
      end if;
   end To_String;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Table_Names) return Ada.Containers.Hash_Type is
   begin
      if Self.Instance = null then
         return Ada.Strings.Hash (Self.Name.all);
      else
         return Ada.Strings.Hash (Self.Instance.all);
      end if;
   end Hash;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : SQL_Table_List) return String is
      C      : Table_List.Cursor := Table_List.No_Element;
      Result : Unbounded_String;
   begin
      if Self.Data.Data /= null then
         C := First (Self.Data.Data.List);
      end if;

      if Has_Element (C) then
         Append (Result, To_String (Element (C)));
         Next (C);
      end if;

      while Has_Element (C) loop
         Append (Result, ", ");
         Append (Result, To_String (Element (C)));
         Next (C);
      end loop;

      return To_String (Result);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Names : Table_Names) return String is
   begin
      if Names.Instance = null then
         return Names.Name.all;
      else
         return Names.Name.all & " " & Names.Instance.all;
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
     (Self : Named_Field_Internal; Long : Boolean) return String
   is
      Result : Unbounded_String;
      C      : Field_List.Cursor;
   begin
      if Self.Value /= null then
         if Self.Table = No_Names then
            Result := To_Unbounded_String (Self.Value.all);

         elsif Long then
            if Self.Table.Instance = null then
               Result := To_Unbounded_String
                 (Self.Table.Name.all & '.' & Self.Value.all);
            else
               Result := To_Unbounded_String
                 (Self.Table.Instance.all & '.' & Self.Value.all);
            end if;
         else
            Result := To_Unbounded_String (Self.Value.all);
         end if;
      end if;

      if Self.Operator /= null then
         C := First (Self.List.List);
         Result := To_Unbounded_String (To_String (Element (C)));
         Next (C);

         while Has_Element (C) loop
            Result := Result & " " & Self.Operator.all & " "
              & To_String (Element (C));
            Next (C);
         end loop;
      end if;

      return To_String (Result);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : As_Field_Internal; Long : Boolean) return String
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
         return To_String (Self.Renamed.Data.Field.all, Long)
           & " AS """ & Self.As.all & """";
      else
         return To_String (Self.Renamed.Data.Field.all, Long)
           & " AS " & Self.As.all;
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : Sorted_Field_Internal; Long : Boolean) return String is
   begin
      if Self.Ascending then
         return To_String (Self.Sorted.Data.Field.all, Long => Long) & " ASC";
      else
         return To_String (Self.Sorted.Data.Field.all, Long => Long) & " DESC";
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : SQL_Field_List; Long : Boolean := True) return String
   is
      C      : Field_List.Cursor := First (Self.List);
      Result : Unbounded_String;
   begin
      if Has_Element (C) then
         Append (Result, To_String (Element (C), Long));
         Next (C);
      end if;

      while Has_Element (C) loop
         Append (Result, ", ");
         Append (Result, To_String (Element (C), Long));
         Next (C);
      end loop;
      return To_String (Result);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : SQL_Field; Long : Boolean := True)  return String is
   begin
      if not Long then
         return Self.Name.all;
      elsif Self.Instance /= null then
         return Self.Instance.all & "." & Self.Name.all;

      elsif Self.Table /= null then
         return Self.Table.all & "." & Self.Name.all;

      else
         --  Self.Table could be null in the case of the Null_Field_*
         --  constants
         return Self.Name.all;
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : SQL_Field_Integer_Build; Long : Boolean := True)  return String is
   begin
      return To_String (Self.Data.Data.all, Long);
   end To_String;

   function To_String
     (Self : SQL_Field_Text_Build; Long : Boolean := True)  return String is
   begin
      return To_String (Self.Data.Data.all, Long);
   end To_String;

   function To_String
     (Self : SQL_Field_Float_Build; Long : Boolean := True)  return String is
   begin
      return To_String (Self.Data.Data.all, Long);
   end To_String;

   function To_String
     (Self : SQL_Field_Boolean_Build; Long : Boolean := True)  return String is
   begin
      return To_String (Self.Data.Data.all, Long);
   end To_String;

   function To_String
     (Self : SQL_Field_Time_Build; Long : Boolean := True)  return String is
   begin
      return To_String (Self.Data.Data.all, Long);
   end To_String;

   function To_String
     (Self : SQL_Field_Any; Long : Boolean := True) return String is
   begin
      return To_String (Self.Data.Data.all, Long);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : Multiple_Args_Field_Internal; Long : Boolean) return String
   is
      C      : Field_List.Cursor := First (Self.List);
      Result : Unbounded_String;
   begin
      if Self.Func_Name /= Func_None'Access then
         Append (Result, Self.Func_Name.all & " (");
      elsif Self.In_Parenthesis then
         Append (Result, "(");
      end if;

      if Has_Element (C) then
         Append (Result, To_String (Element (C), Long));
         Next (C);
      end if;

      while Has_Element (C) loop
         Append (Result, Self.Separator.all);
         Append (Result, To_String (Element (C), Long));
         Next (C);
      end loop;

      if Self.Func_Name /= Func_None'Access or else Self.In_Parenthesis then
         Append (Result, ")");
      end if;

      if Self.Suffix /= null then
         Append (Result, Self.Suffix.all);
      end if;

      return To_String (Result);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : Case_Stmt_Internal; Long : Boolean) return String
   is
      C : When_Lists.Cursor := First (Self.Criteria.List);
      Result : Unbounded_String;
   begin
      Append (Result, "CASE");
      while Has_Element (C) loop
         Append (Result, " WHEN " & To_String (Element (C).Criteria)
                 & " THEN "
                 & To_String (Element (C).Field.Data.Field.all, Long));
         Next (C);
      end loop;

      if Self.Else_Clause /= No_Field_Pointer then
         Append
           (Result,
            " ELSE " & To_String (Self.Else_Clause.Data.Field.all, Long));
      end if;

      Append (Result, " END");
      return To_String (Result);
   end To_String;

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : SQL_Table_List) return SQL_Table_List is
      C      : Table_List.Cursor := Table_List.No_Element;
   begin
      if Left.Data.Data = null then
         return Right;
      end if;

      if Right.Data.Data /= null then
         C := First (Right.Data.Data.List);
      end if;

      while Has_Element (C) loop
         Append (Left.Data.Data.List, Element (C));
         Next (C);
      end loop;
      return Left;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left : SQL_Table_List; Right : SQL_Single_Table'Class)
      return SQL_Table_List
   is
   begin
      if Left.Data.Data = null then
         return +Right;
      end if;

      Append (Left.Data.Data.List, Right);
      return Left;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : SQL_Single_Table'Class) return SQL_Table_List is
      Result : SQL_Table_List;
   begin
      Result.Data.Data := new Table_List_Internal;
      Append (Result.Data.Data.List, Left);
      Append (Result.Data.Data.List, Right);
      return Result;
   end "&";

   ---------
   -- "+" --
   ---------

   function "+" (Left : SQL_Single_Table'Class) return SQL_Table_List is
      Result : SQL_Table_List;
   begin
      Result.Data.Data := new Table_List_Internal;
      Append (Result.Data.Data.List, Left);
      return Result;
   end "+";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left, Right : SQL_Field_List) return SQL_Field_List
   is
      Result : SQL_Field_List;
      C      : Field_List.Cursor := First (Right.List);
   begin
      Result.List := Left.List;
      while Has_Element (C) loop
         Append (Result.List, Element (C));
         Next (C);
      end loop;
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : SQL_Field'Class) return SQL_Field_List is
      Result : SQL_Field_List;
   begin
      Append (Result.List, Left);
      Append (Result.List, Right);
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left : SQL_Field_List; Right : SQL_Field'Class) return SQL_Field_List
   is
      Result : SQL_Field_List;
   begin
      Result.List := Left.List;
      Append (Result.List, Right);
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left : SQL_Field'Class; Right : SQL_Field_List) return SQL_Field_List
   is
      Result : SQL_Field_List;
   begin
      Result.List := Right.List;
      Prepend (Result.List, Left);
      return Result;
   end "&";

   ---------
   -- "+" --
   ---------

   function "+" (Left : SQL_Field'Class) return SQL_Field_List is
      Result : SQL_Field_List;
   begin
      Append (Result.List, Left);
      return Result;
   end "+";

   --------
   -- As --
   --------

   function As
     (Field : SQL_Field'Class; Name : String) return SQL_Field'Class
   is
      Data : constant As_Field_Internal_Access := new As_Field_Internal;
   begin
      Data.As      := new String'(Name);
      Data.Renamed := +Field;
      return SQL_Field_Any'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end As;

   -----------
   -- Field --
   -----------

   function Field
     (Table : SQL_Single_Table'Class;
      Field : SQL_Field_Integer) return SQL_Field_Integer'Class
   is
      F : SQL_Field_Integer_Build
        (Table => null, Instance => Table.Instance, Name => null);
      D : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      D.Table := (Name => null, Instance => Table.Instance);
      D.Value := new String'(Field.Name.all);
      F.Data.Data := SQL_Field_Internal_Access (D);
      return F;
   end Field;

   -----------
   -- Field --
   -----------

   function Field
     (Table : SQL_Single_Table'Class;
      Field : SQL_Field_Text) return SQL_Field_Text'Class
   is
      F : SQL_Field_Text_Build
        (Table => null, Instance => Table.Instance, Name => null);
      D : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      D.Table := (Name => null, Instance => Table.Instance);
      D.Value := new String'(Field.Name.all);
      F.Data.Data := SQL_Field_Internal_Access (D);
      return F;
   end Field;

   -----------
   -- Field --
   -----------

   function Field
     (Table : SQL_Single_Table'Class;
      Field : SQL_Field_Time) return SQL_Field_Time'Class
   is
      F : SQL_Field_Time_Build
        (Table => null, Instance => Table.Instance, Name => null);
      D : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      D.Table := (Name => null, Instance => Table.Instance);
      D.Value := new String'(Field.Name.all);
      F.Data.Data := SQL_Field_Internal_Access (D);
      return F;
   end Field;

   -----------
   -- Field --
   -----------

   function Field
     (Table : SQL_Single_Table'Class;
      Field : SQL_Field_Boolean) return SQL_Field_Boolean'Class
   is
      F : SQL_Field_Boolean_Build
        (Table => null, Instance => Table.Instance, Name => null);
      D : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      D.Table := (Name => null, Instance => Table.Instance);
      D.Value := new String'(Field.Name.all);
      F.Data.Data := SQL_Field_Internal_Access (D);
      return F;
   end Field;

   -----------
   -- Field --
   -----------

   function Field
     (Table : SQL_Single_Table'Class;
      Field : SQL_Field_Float) return SQL_Field_Float'Class
   is
      F : SQL_Field_Float_Build
        (Table => null, Instance => Table.Instance, Name => null);
      D : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      D.Table := (Name => null, Instance => Table.Instance);
      D.Value := new String'(Field.Name.all);
      F.Data.Data := SQL_Field_Internal_Access (D);
      return F;
   end Field;

   ----------
   -- Desc --
   ----------

   function Desc (Field : SQL_Field'Class) return SQL_Field'Class is
      Data : constant Sorted_Field_Internal_Access :=
        new Sorted_Field_Internal;
   begin
      Data.Ascending := False;
      Data.Sorted    := +Field;
      return SQL_Field_Any'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end Desc;

   ---------
   -- Asc --
   ---------

   function Asc  (Field : SQL_Field'Class) return SQL_Field'Class is
      Data : constant Sorted_Field_Internal_Access :=
        new Sorted_Field_Internal;
   begin
      Data.Ascending := True;
      Data.Sorted    := +Field;
      return SQL_Field_Any'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end Asc;

   ---------
   -- "&" --
   ---------

   function "&"
     (List : SQL_Field_List; Value : String) return SQL_Field_List is
   begin
      return List & Expression (Value);
   end "&";

   function "&"
     (List : SQL_Field'Class; Value : String) return SQL_Field_List is
   begin
      return List & Expression (Value);
   end "&";

   function "&"
     (Value : String; List : SQL_Field_List) return SQL_Field_List is
   begin
      return Expression (Value) & List;
   end "&";

   function "&"
     (Value : String; List : SQL_Field'Class) return SQL_Field_List is
   begin
      return Expression (Value) & List;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (List : SQL_Field_List; Value : Integer) return SQL_Field_List is
   begin
      return List & Expression (Value);
   end "&";

   function "&"
     (List : SQL_Field'Class; Value : Integer) return SQL_Field_List is
   begin
      return List & Expression (Value);
   end "&";

   function "&"
     (Value : Integer; List  : SQL_Field_List) return SQL_Field_List is
   begin
      return Expression (Value) & List;
   end "&";

   function "&"
     (Value : Integer; List  : SQL_Field'Class) return SQL_Field_List is
   begin
      return Expression (Value) & List;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (List  : SQL_Field_List; Value : Boolean) return SQL_Field_List is
   begin
      return List & Expression (Value);
   end "&";

   function "&"
     (List  : SQL_Field'Class; Value : Boolean) return SQL_Field_List is
   begin
      return List & Expression (Value);
   end "&";

   function "&"
     (Value : Boolean; List : SQL_Field_List) return SQL_Field_List is
   begin
      return Expression (Value) & List;
   end "&";

   function "&"
     (Value : Boolean; List : SQL_Field'Class) return SQL_Field_List is
   begin
      return Expression (Value) & List;
   end "&";

   ----------------
   -- Expression --
   ----------------

   function Expression (Value : String)  return SQL_Field_Text'Class is
      Data : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      Data.Value := new String'(Normalize_String (Value));
      return SQL_Field_Text_Build'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end Expression;

   -----------------
   -- From_String --
   -----------------

   function From_String (Expression : String) return SQL_Field_Text'Class is
      Data : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      Data.Value := new String'(Expression);
      return SQL_Field_Text_Build'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end From_String;

   ------------------
   -- From_Integer --
   ------------------

   function From_Integer
     (Expression : String) return SQL_Field_Integer'Class
   is
      Data : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      Data.Value := new String'(Expression);
      return SQL_Field_Integer_Build'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end From_Integer;

   ------------------------
   -- Expression_Or_Null --
   ------------------------

   function Expression_Or_Null (Value : String) return SQL_Field_Text'Class is
      Data : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      if Value = Null_String then
         Data.Value := new String'(Null_String);
      else
         Data.Value := new String'(Normalize_String (Value));
      end if;
      return SQL_Field_Text_Build'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end Expression_Or_Null;

   ----------------
   -- Expression --
   ----------------

   function Expression (Value : Integer) return SQL_Field_Integer'Class is
      Img  : constant String := Integer'Image (Value);
      Data : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      if Img (Img'First) = ' ' then
         Data.Value := new String'(Img (Img'First + 1 .. Img'Last));
      else
         Data.Value := new String'(Img);
      end if;
      return SQL_Field_Integer_Build'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end Expression;

   ----------------
   -- Expression --
   ----------------

   function Expression (Value : Float) return SQL_Field_Float'Class is
      Img : constant String := Float'Image (Value);
      Data : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      if Img (Img'First) = ' ' then
         Data.Value := new String'(Img (Img'First + 1 .. Img'Last));
      else
         Data.Value := new String'(Img);
      end if;
      return SQL_Field_Float_Build'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end Expression;

   ----------------
   -- Expression --
   ----------------

   function Expression
     (Value : Ada.Calendar.Time; Date_Only : Boolean := False)
      return SQL_Field_Time'Class
   is
      Data : constant Named_Field_Internal_Access := new Named_Field_Internal;
      Adjusted : Time;
   begin
      --  Value is always considered as GMT, which is what we store in the
      --  database. Unfortunately, GNAT.Calendar.Time_IO converts that back to
      --  local time.

      if Value /= No_Time then
         Adjusted := Value - Duration (UTC_Time_Offset (Value)) * 60.0;
         if Date_Only then
            Data.Value := new String'(Image (Adjusted, "'%Y-%m-%d'"));
         else
            Data.Value :=
              new String'(Image (Adjusted, "'%Y-%m-%d %H:%M:%S'"));
         end if;
      else
         Data.Value := new String'("NULL");
      end if;
      return SQL_Field_Time_Build'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end Expression;

   -------------
   -- As_Days --
   -------------

   function As_Days (Count : Natural) return SQL_Field_Time'Class is
      Data : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      Data.Value := new String'("'" & Integer'Image (Count) & "days'");
      return SQL_Field_Time_Build'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end As_Days;

   ------------------
   -- At_Time_Zone --
   ------------------

   function At_Time_Zone
     (Field : SQL_Field_Time'Class; TZ : String) return SQL_Field_Time'Class
   is
      Data : constant Multiple_Args_Field_Internal_Access :=
        Create_Multiple_Args
          (+Field,
           Func_None'Access, Func_None'Access,
           Suffix => " at time zone '" & TZ & "'");
   begin
      return SQL_Field_Time_Build'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled
                  with SQL_Field_Internal_Access (Data)));
   end At_Time_Zone;

   ----------------
   -- Expression --
   ----------------

   function Expression (Value : Boolean) return SQL_Field_Boolean'Class is
      Data : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      Data.Value := new String'(Boolean'Image (Value));
      return SQL_Field_Boolean_Build'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end Expression;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Multiple_Args_Field_Internal) is
   begin
      Free (Self.Suffix);
   end Free;

   --------------------------
   -- Create_Multiple_Args --
   --------------------------

   function Create_Multiple_Args
     (Fields : SQL_Field_List;
      Func_Name : Cst_String_Access;
      Separator : Cst_String_Access;
      Suffix    : String := "";
      In_Parenthesis : Boolean := False)
      return Multiple_Args_Field_Internal_Access
   is
      Data   : constant Multiple_Args_Field_Internal_Access :=
        new Multiple_Args_Field_Internal;
      C : Field_List.Cursor := First (Fields.List);
   begin
      Data.Func_Name      := Func_Name;
      Data.Separator      := Separator;
      Data.In_Parenthesis := In_Parenthesis;

      if Suffix /= "" then
         Data.Suffix         := new String'(Suffix);
      end if;

      while Has_Element (C) loop
         declare
            Field    : constant SQL_Field'Class := Element (C);
            Internal : SQL_Field_Internal_Access;
            D        : Multiple_Args_Field_Internal_Access;
            C2       : Field_List.Cursor;
         begin
            if Field in SQL_Field_Any'Class then
               Internal := SQL_Field_Any (Field).Data.Data;
               if Internal.all in Multiple_Args_Field_Internal'Class then
                  D := Multiple_Args_Field_Internal_Access (Internal);

                  if D.Separator = Separator then
                     --  Avoid nested concatenations, put them all at the same
                     --  level. This simplifies the query. Due to this, we are
                     --  also sure the concatenation itself doesn't have
                     --  sub-expressions

                     C2 := First (D.List);
                     while Has_Element (C2) loop
                        Append (Data.List, Element (C2));
                        Next (C2);
                     end loop;
                  else
                     Append (Data.List, Field);
                  end if;
               else
                  Append (Data.List, Field);
               end if;
            else
               Append (Data.List, Field);
            end if;
         end;
         Next (C);
      end loop;
      return Data;
   end Create_Multiple_Args;

   ------------
   -- Concat --
   ------------

   function Concat (Fields : SQL_Field_List) return SQL_Field'Class is
      Data : constant Multiple_Args_Field_Internal_Access :=
        Create_Multiple_Args
          (Fields, Func_None'Access, Separator_Concat'Access);
   begin
      return SQL_Field_Any'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled
                  with SQL_Field_Internal_Access (Data)));
   end Concat;

   -----------
   -- Tuple --
   -----------

   function Tuple (Fields : SQL_Field_List) return SQL_Field'Class is
      Data : constant Multiple_Args_Field_Internal_Access :=
        Create_Multiple_Args (Fields, Func_None'Access, Separator_Comma'Access,
                              In_Parenthesis => True);
   begin
      return SQL_Field_Any'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled
                  with SQL_Field_Internal_Access (Data)));
   end Tuple;

   --------------
   -- Coalesce --
   --------------

   function Coalesce (Fields : SQL_Field_List) return SQL_Field'Class is
      Data : constant Multiple_Args_Field_Internal_Access :=
        Create_Multiple_Args
          (Fields, Func_Coalesce'Access, Separator_Comma'Access);
   begin
      return SQL_Field_Any'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled
                  with SQL_Field_Internal_Access (Data)));
   end Coalesce;

   ---------
   -- "&" --
   ---------

   function "&" (List1, List2 : When_List) return When_List is
      Result : When_List;
      C      : When_Lists.Cursor := First (List2.List);
   begin
      Result := List1;
      while Has_Element (C) loop
         Append (Result.List, Element (C));
         Next (C);
      end loop;
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
      Data : constant Case_Stmt_Internal_Access :=
        new Case_Stmt_Internal;
   begin
      Data.Criteria := List;
      if Else_Clause /= SQL_Field'Class (Null_Field_Text) then
         Data.Else_Clause := +Else_Clause;
      end if;
      return SQL_Field_Any'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled
                  with SQL_Field_Internal_Access (Data)));
   end SQL_Case;

   -------------
   -- To_Char --
   -------------

   function To_Char
     (Field : SQL_Field_Time; Format : String) return SQL_Field'Class
   is
      Data : constant Multiple_Args_Field_Internal_Access :=
        Create_Multiple_Args (Field & Expression (Format),
                              Func_To_Char'Access,
                              Separator_Comma'Access);
   begin
      return SQL_Field_Any'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled
                  with SQL_Field_Internal_Access (Data)));
   end To_Char;

   -------------
   -- Extract --
   -------------

   function Extract
     (Field : SQL_Field'Class; Attribute : String) return SQL_Field'Class
   is
      Data : constant Multiple_Args_Field_Internal_Access :=
        Create_Multiple_Args
          (From_String (Attribute & " from") & Field,
           Func_Extract'Access,
           Separator_Space'Access);
   begin
      return SQL_Field_Any'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled
                  with SQL_Field_Internal_Access (Data)));
   end Extract;

   -----------
   -- Lower --
   -----------

   function Lower
     (Field : SQL_Field_Text'Class) return SQL_Field_Text'Class
   is
      F : SQL_Field_Text_Build
        (Table => null, Instance => null, Name => null);
      D : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      D.Value := new String'(Func_Lower & "(" & Field.Name.all & ")");
      F.Data.Data := SQL_Field_Internal_Access (D);
      return F;
   end Lower;

   -------------
   -- Initcap --
   -------------

   function Initcap
     (Field : SQL_Field_Text'Class) return SQL_Field_Text'Class
   is
      F : SQL_Field_Text_Build
        (Table => null, Instance => null, Name => null);
      D : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      D.Value := new String'(Func_InitCap & "(" & Field.Name.all & ")");
      F.Data.Data := SQL_Field_Internal_Access (D);
      return F;
   end Initcap;

   --------------------
   -- Cast_To_String --
   --------------------

   function Cast_To_String
     (Field : SQL_Field'Class) return SQL_Field_Text'Class
   is
      F : SQL_Field_Text_Build
        (Table => null, Instance => null, Name => null);
      D : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      D.Value := new String'("CAST (" & Field.Name.all & " AS TEXT)");
      F.Data.Data := SQL_Field_Internal_Access (D);
      return F;
   end Cast_To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : Aggregate_Field_Internal; Long : Boolean) return String
   is
      C      : Field_List.Cursor := First (Self.Params);
      Result : Unbounded_String;
   begin
      Result := To_Unbounded_String (Self.Func.all & " (");

      if Has_Element (C) then
         Append (Result, To_String (Element (C), Long));
         Next (C);
      end if;

      while Has_Element (C) loop
         Append (Result, ", ");
         Append (Result, To_String (Element (C), Long));
         Next (C);
      end loop;

      if Self.Criteria /= No_Criteria then
         Append (Result, To_String (Self.Criteria));
      end if;

      Append (Result, ")");
      return To_String (Result);
   end To_String;

   ------------------
   -- Current_Date --
   ------------------

   function Current_Date return SQL_Field_Time'Class is
      Data : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      Data.Value := new String'("current_date");
      return SQL_Field_Time_Build'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end Current_Date;

   ---------
   -- Now --
   ---------

   function Now return SQL_Field_Time'Class is
      Data : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      Data.Value := new String'("now()");
      return SQL_Field_Time_Build'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end Now;

   ---------
   -- "-" --
   ---------

   function "-"
     (Field1, Field2 : SQL_Field_Time'Class) return SQL_Field_Time'Class
   is
      Data : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      Data.Operator := new String'("-");
      Data.List := Field1 & Field2;
      return SQL_Field_Time_Build'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end "-";

   ---------
   -- "-" --
   ---------

   function "-"
     (Field1 : SQL_Field_Time'Class;
      Days   : Integer) return SQL_Field_Time'Class
   is
      Data : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      Data.Operator := new String'("-");
      Data.List := Field1 & From_String
        ("interval '" & Integer'Image (Days) & "days'");
      return SQL_Field_Time_Build'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end "-";

   ---------
   -- "+" --
   ---------

   function "+"
     (Field : SQL_Field_Integer'Class; Add : Integer)
      return SQL_Field_Integer'Class
   is
      Data : constant Named_Field_Internal_Access := new Named_Field_Internal;
   begin
      Data.Operator := new String'("+");
      Data.List := Field & From_String (Integer'Image (Add));
      return SQL_Field_Integer_Build'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end "+";

   -----------
   -- Apply --
   -----------

   function Apply
     (Func     : Aggregate_Function;
      Criteria : SQL_Criteria) return SQL_Field'Class
   is
      Data : constant Aggregate_Field_Internal_Access :=
        new Aggregate_Field_Internal;
   begin
      Data.Criteria := Criteria;
      Data.Func   := new String'(String (Func));
      return SQL_Field_Any'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end Apply;

   -----------
   -- Apply --
   -----------

   function Apply
     (Func   : Aggregate_Function;
      Fields : SQL_Field_List) return SQL_Field'Class
   is
      Data : constant Aggregate_Field_Internal_Access :=
        new Aggregate_Field_Internal;
   begin
      Data.Params := Fields.List;
      Data.Func   := new String'(String (Func));
      return SQL_Field_Any'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end Apply;

   -----------
   -- Apply --
   -----------

   function Apply
     (Func   : Aggregate_Function;
      Field  : SQL_Field'Class) return SQL_Field'Class
   is
      Data : constant Aggregate_Field_Internal_Access :=
        new Aggregate_Field_Internal;
   begin
      Append (Data.Params, Field);
      Data.Func   := new String'(String (Func));
      return SQL_Field_Any'
        (Table => null, Instance => null, Name => null,
         Data => (Ada.Finalization.Controlled with
                  Data => SQL_Field_Internal_Access (Data)));
   end Apply;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Field_Data) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (SQL_Field_Internal'Class, SQL_Field_Internal_Access);
   begin
      if Self.Data /= null then
         Self.Data.Refcount := Self.Data.Refcount - 1;
         if Self.Data.Refcount = 0 then
            Free (Self.Data.all);
            Unchecked_Free (Self.Data);
         end if;
      end if;
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Table_List_Data) is
   begin
      if Self.Data /= null then
         Self.Data.Refcount := Self.Data.Refcount + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Table_List_Data) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Table_List_Internal, Table_List_Internal_Access);
   begin
      if Self.Data /= null then
         Self.Data.Refcount := Self.Data.Refcount - 1;
         if Self.Data.Refcount = 0 then
            Unchecked_Free (Self.Data);
         end if;
      end if;
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Field_Data) is
   begin
      if Self.Data /= null then
         Self.Data.Refcount := Self.Data.Refcount + 1;
      end if;
   end Adjust;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out SQL_Field_Pointer) is
   begin
      if Self.Data /= null then
         Self.Data.Refcount := Self.Data.Refcount + 1;
      end if;
   end Adjust;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Controlled_SQL_Criteria) is
   begin
      if Self.Data /= null then
         Self.Data.Refcount := Self.Data.Refcount + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Controlled_SQL_Criteria) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (SQL_Criteria_Data, SQL_Criteria_Data_Access);
   begin
      if Self.Data /= null then
         Self.Data.Refcount := Self.Data.Refcount - 1;
         if Self.Data.Refcount = 0 then
            Unchecked_Free (Self.Data);
         end if;
      end if;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out SQL_Field_Pointer) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Field_Pointer_Data, Field_Pointer_Data_Access);
   begin
      if Self.Data /= null then
         Self.Data.Refcount := Self.Data.Refcount - 1;
         if Self.Data.Refcount = 0 then
            Unchecked_Free (Self.Data.Field);
            Unchecked_Free (Self.Data);
         end if;
      end if;
   end Finalize;

   ---------
   -- "+" --
   ---------

   function "+" (Field : SQL_Field'Class) return SQL_Field_Pointer is
   begin
      return SQL_Field_Pointer'
        (Ada.Finalization.Controlled with
         Data => new Field_Pointer_Data'
           (Refcount => 1,
            Field    => new SQL_Field'Class'(Field)));
   end "+";

   -------------
   -- To_List --
   -------------

   function To_List (Fields : SQL_Field_Array) return SQL_Field_List is
      S : SQL_Field_List;
   begin
      for A in Fields'Range loop
         Append (S.List, Fields (A).Data.Field.all);
      end loop;
      return S;
   end To_List;

   -------------
   -- Compare --
   -------------

   function Compare
     (Left, Right : SQL_Field'Class; Op : SQL_Criteria_Type)
      return SQL_Criteria
   is
      Result : SQL_Criteria;
   begin
      Result.Criteria.Data := new SQL_Criteria_Data (Op);
      Result.Criteria.Data.Arg1 := +Left;
      Result.Criteria.Data.Arg2 := +Right;
      return Result;
   end Compare;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : SQL_Field_Integer'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Equal);
   end "=";

   function "=" (Left, Right : SQL_Field_Text'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Equal);
   end "=";

   function "=" (Left, Right : SQL_Field_Boolean'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Equal);
   end "=";

   function "=" (Left, Right : SQL_Field_Float'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Equal);
   end "=";

   function "=" (Left, Right : SQL_Field_Time'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Equal);
   end "=";

   function "="
     (Left : SQL_Field_Integer; Right : Integer) return SQL_Criteria is
   begin
      return Left = Expression (Right);
   end "=";

   function "="
     (Left : SQL_Field_Text;    Right : String)  return SQL_Criteria is
   begin
      return Left = Expression (Right);
   end "=";

   function "="
     (Left : SQL_Field_Boolean; Right : Boolean) return SQL_Criteria is
   begin
      return Left = Expression (Right);
   end "=";

   function "="
     (Left : SQL_Field_Float;   Right : Float)   return SQL_Criteria is
   begin
      return Left = Expression (Right);
   end "=";

   function "="
     (Left : SQL_Field_Time; Right : Ada.Calendar.Time) return SQL_Criteria is
   begin
      return Left = Expression (Right);
   end "=";

   function Date_Equal
     (Left : SQL_Field_Time; Right : Ada.Calendar.Time) return SQL_Criteria is
   begin
      return Left = Expression (Right, Date_Only => True);
   end Date_Equal;

   function "/=" (Left, Right : SQL_Field_Integer'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Not_Equal);
   end "/=";

   function "/=" (Left, Right : SQL_Field_Text'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Not_Equal);
   end "/=";

   function "/=" (Left, Right : SQL_Field_Boolean'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Not_Equal);
   end "/=";

   function "/=" (Left, Right : SQL_Field_Float'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Not_Equal);
   end "/=";

   function "/=" (Left, Right : SQL_Field_Time'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Not_Equal);
   end "/=";

   function "/="
     (Left : SQL_Field_Integer; Right : Integer) return SQL_Criteria is
   begin
      return Left /= Expression (Right);
   end "/=";

   function "/="
     (Left : SQL_Field_Text;    Right : String)  return SQL_Criteria is
   begin
      return Left /= Expression (Right);
   end "/=";

   function "/="
     (Left : SQL_Field_Boolean; Right : Boolean) return SQL_Criteria is
   begin
      return Left /= Expression (Right);
   end "/=";

   function "/="
     (Left : SQL_Field_Float;   Right : Float)   return SQL_Criteria is
   begin
      return Left /= Expression (Right);
   end "/=";

   function "/="
     (Left : SQL_Field_Time; Right : Ada.Calendar.Time) return SQL_Criteria is
   begin
      return Left /= Expression (Right);
   end "/=";

   function "<" (Left, Right : SQL_Field_Integer'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Less_Than);
   end "<";

   function "<" (Left, Right : SQL_Field_Text'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Less_Than);
   end "<";

   function "<" (Left, Right : SQL_Field_Boolean'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Less_Than);
   end "<";

   function "<" (Left, Right : SQL_Field_Float'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Less_Than);
   end "<";

   function "<" (Left, Right : SQL_Field_Time'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Less_Than);
   end "<";

   function "<"
     (Left : SQL_Field_Integer; Right : Integer) return SQL_Criteria is
   begin
      return Left < Expression (Right);
   end "<";

   function "<"
     (Left : SQL_Field_Float;   Right : Float)   return SQL_Criteria is
   begin
      return Left < Expression (Right);
   end "<";

   function "<"
     (Left : SQL_Field_Time; Right : Ada.Calendar.Time) return SQL_Criteria is
   begin
      return Left < Expression (Right);
   end "<";

   function Date_Less_Than
     (Left : SQL_Field_Time; Right : Ada.Calendar.Time) return SQL_Criteria is
   begin
      return Left < Expression (Right, Date_Only => True);
   end Date_Less_Than;

   function ">" (Left, Right : SQL_Field_Integer'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Greater_Than);
   end ">";

   function ">" (Left, Right : SQL_Field_Text'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Greater_Than);
   end ">";

   function ">" (Left, Right : SQL_Field_Boolean'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Greater_Than);
   end ">";

   function ">" (Left, Right : SQL_Field_Float'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Greater_Than);
   end ">";

   function ">" (Left, Right : SQL_Field_Time'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Greater_Than);
   end ">";

   function ">"
     (Left : SQL_Field_Integer; Right : Integer) return SQL_Criteria is
   begin
      return Left > Expression (Right);
   end ">";

   function Greater_Than
     (Left : SQL_Field'Class; Right : Integer) return SQL_Criteria is
   begin
      return Compare (Left, Expression (Right), Criteria_Greater_Than);
   end Greater_Than;

   function Greater_Than
     (Left : SQL_Field'Class; Right : SQL_Field_Time'Class)
     return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Greater_Than);
   end Greater_Than;

   function Less_Than
     (Left : SQL_Field'Class; Right : SQL_Field_Time'Class)
     return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Less_Than);
   end Less_Than;

   function Greater_Or_Equal
     (Left : SQL_Field'Class; Right : Integer) return SQL_Criteria is
   begin
      return Compare (Left, Expression (Right), Criteria_Greater_Or_Equal);
   end Greater_Or_Equal;

   function Equal
     (Left : SQL_Field'Class; Right : Boolean) return SQL_Criteria is
   begin
      return Compare (Left, Expression (Right), Criteria_Equal);
   end Equal;

   function ">"
     (Left : SQL_Field_Float;   Right : Float)   return SQL_Criteria is
   begin
      return Left > Expression (Right);
   end ">";

   function ">"
     (Left : SQL_Field_Text;    Right : String)   return SQL_Criteria is
   begin
      return Left > Expression (Right);
   end ">";

   function ">"
     (Left : SQL_Field_Time; Right : Ada.Calendar.Time) return SQL_Criteria is
   begin
      return Left > Expression (Right);
   end ">";

   function Date_Greater_Than
     (Left : SQL_Field_Time; Right : Ada.Calendar.Time) return SQL_Criteria is
   begin
      return Left > Expression (Right, Date_Only => True);
   end Date_Greater_Than;

   function "<=" (Left, Right : SQL_Field_Integer'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Less_Or_Equal);
   end "<=";

   function "<=" (Left, Right : SQL_Field_Text'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Less_Or_Equal);
   end "<=";

   function "<=" (Left, Right : SQL_Field_Boolean'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Less_Or_Equal);
   end "<=";

   function "<=" (Left, Right : SQL_Field_Float'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Less_Or_Equal);
   end "<=";

   function "<=" (Left, Right : SQL_Field_Time'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Less_Or_Equal);
   end "<=";

   function "<="
     (Left : SQL_Field_Integer; Right : Integer) return SQL_Criteria is
   begin
      return Left <= Expression (Right);
   end "<=";

   function "<="
     (Left : SQL_Field_Float;   Right : Float)   return SQL_Criteria is
   begin
      return Left <= Expression (Right);
   end "<=";

   function "<="
     (Left : SQL_Field_Time; Right : Ada.Calendar.Time) return SQL_Criteria is
   begin
      return Left <= Expression (Right);
   end "<=";

   function Date_Less_Or_Equal
     (Left : SQL_Field_Time; Right : Ada.Calendar.Time) return SQL_Criteria is
   begin
      return Left <= Expression (Right, Date_Only => True);
   end Date_Less_Or_Equal;

   function ">=" (Left, Right : SQL_Field_Integer'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Greater_Or_Equal);
   end ">=";

   function ">=" (Left, Right : SQL_Field_Text'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Greater_Or_Equal);
   end ">=";

   function ">=" (Left, Right : SQL_Field_Boolean'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Greater_Or_Equal);
   end ">=";

   function ">=" (Left, Right : SQL_Field_Float'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Greater_Or_Equal);
   end ">=";

   function ">=" (Left, Right : SQL_Field_Time'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Greater_Or_Equal);
   end ">=";

   function ">="
     (Left : SQL_Field_Integer; Right : Integer) return SQL_Criteria is
   begin
      return Left >= Expression (Right);
   end ">=";

   function ">="
     (Left : SQL_Field_Float;   Right : Float)   return SQL_Criteria is
   begin
      return Left >= Expression (Right);
   end ">=";

   function ">="
     (Left : SQL_Field_Time; Right : Ada.Calendar.Time) return SQL_Criteria is
   begin
      return Left >= Expression (Right);
   end ">=";

   function Date_Greater_Or_Equal
     (Left : SQL_Field_Time; Right : Ada.Calendar.Time) return SQL_Criteria is
   begin
      return Left >= Expression (Right, Date_Only => True);
   end Date_Greater_Or_Equal;

   -------------
   -- Combine --
   -------------

   function Combine
     (Left, Right : SQL_Criteria; Op : SQL_Criteria_Type) return SQL_Criteria
   is
      List : Criteria_List.List;
      C    : Criteria_List.Cursor;
      Result : SQL_Criteria;
   begin
      if Left = No_Criteria then
         return Right;
      elsif Right = No_Criteria then
         return Left;
      elsif Left.Criteria.Data.Op = Op then
         List := Left.Criteria.Data.Criterias;
         if Right.Criteria.Data.Op = Op then
            C := First (Right.Criteria.Data.Criterias);
            while Has_Element (C) loop
               Append (List, Element (C));
               Next (C);
            end loop;
         else
            Append (List, Right);
         end if;
      elsif Right.Criteria.Data.Op = Op then
         List := Right.Criteria.Data.Criterias;
         Prepend (List, Left);
      else
         Append (List, Left);
         Append (List, Right);
      end if;

      Result.Criteria.Data := new SQL_Criteria_Data (Op);
      Result.Criteria.Data.Criterias := List;
      return Result;
   end Combine;

   --------------
   -- Overlaps --
   --------------

   function Overlaps (Left, Right : SQL_Field'Class) return SQL_Criteria is
   begin
      return Compare (Left, Right, Criteria_Overlaps);
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
     (Left : SQL_Criteria; Right : SQL_Field_Boolean) return SQL_Criteria is
   begin
      return Left and (Right = True);
   end "and";

   ----------
   -- "or" --
   ----------

   function "or"
     (Left : SQL_Criteria; Right : SQL_Field_Boolean) return SQL_Criteria is
   begin
      return Left or (Right = True);
   end "or";

   -----------
   -- "not" --
   -----------

   function "not" (Left : SQL_Field_Boolean) return SQL_Criteria is
   begin
      return Left = False;
   end "not";

   ------------
   -- SQL_In --
   ------------

   function SQL_In
     (Self : SQL_Field'Class; List : SQL_Field_List) return SQL_Criteria
   is
      Result : SQL_Criteria;
   begin
      Result.Criteria.Data      := new SQL_Criteria_Data (Criteria_In);
      Result.Criteria.Data.Arg  := +Self;
      Result.Criteria.Data.List := List;
      return Result;
   end SQL_In;

   function SQL_In
     (Self : SQL_Field'Class; Subquery : SQL_Query) return SQL_Criteria
   is
      Result : SQL_Criteria;
   begin
      Result.Criteria.Data      := new SQL_Criteria_Data (Criteria_In);
      Result.Criteria.Data.Arg  := +Self;
      Result.Criteria.Data.Subquery := Subquery;
      return Result;
   end SQL_In;

   function SQL_In
     (Self : SQL_Field'Class; List : String) return SQL_Criteria
   is
      Result : SQL_Criteria;
   begin
      Result.Criteria.Data      := new SQL_Criteria_Data (Criteria_In);
      Result.Criteria.Data.Arg  := +Self;
      Result.Criteria.Data.In_String := To_Unbounded_String (List);
      return Result;
   end SQL_In;

   ----------------
   -- SQL_Not_In --
   ----------------

   function SQL_Not_In
     (Self : SQL_Field'Class; List : SQL_Field_List) return SQL_Criteria
   is
      Result : SQL_Criteria;
   begin
      Result.Criteria.Data      := new SQL_Criteria_Data (Criteria_Not_In);
      Result.Criteria.Data.Arg  := +Self;
      Result.Criteria.Data.List := List;
      return Result;
   end SQL_Not_In;

   function SQL_Not_In
     (Self : SQL_Field'Class; Subquery : SQL_Query) return SQL_Criteria
   is
      Result : SQL_Criteria;
   begin
      Result.Criteria.Data      := new SQL_Criteria_Data (Criteria_Not_In);
      Result.Criteria.Data.Arg  := +Self;
      Result.Criteria.Data.Subquery := Subquery;
      return Result;
   end SQL_Not_In;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : SQL_Field'Class) return SQL_Criteria is
      Result : SQL_Criteria;
   begin
      Result.Criteria.Data := new SQL_Criteria_Data (Criteria_Null);
      Result.Criteria.Data.Arg3 := +Self;
      return Result;
   end Is_Null;

   -----------------
   -- Is_Not_Null --
   -----------------

   function Is_Not_Null (Self : SQL_Field'Class) return SQL_Criteria is
      Result : SQL_Criteria;
   begin
      Result.Criteria.Data := new SQL_Criteria_Data (Criteria_Not_Null);
      Result.Criteria.Data.Arg3 := +Self;
      return Result;
   end Is_Not_Null;

   ---------
   -- Any --
   ---------

   function Any
     (Self : SQL_Field_Text; Str : SQL_Field_Text) return SQL_Criteria is
   begin
      return Compare (Self, Str, Criteria_Any);
   end Any;

   -----------
   -- Ilike --
   -----------

   function Ilike
     (Self : SQL_Field_Text; Str : String) return SQL_Criteria is
   begin
      return Compare (Self, Expression (Str), Criteria_Ilike);
   end Ilike;

   ----------
   -- Like --
   ----------

   function Like
     (Self : SQL_Field_Text; Str : String) return SQL_Criteria is
   begin
      return Compare (Self, Expression (Str), Criteria_Like);
   end Like;

   -----------
   -- Ilike --
   -----------

   function Ilike
     (Self : SQL_Field_Text; Field : SQL_Field'Class) return SQL_Criteria is
   begin
      return Compare (Self, Field, Criteria_Like);
   end Ilike;

   ---------------
   -- Not_Ilike --
   ---------------

   function Not_Ilike
     (Self : SQL_Field_Text; Str : String) return SQL_Criteria is
   begin
      return Compare (Self, Expression (Str), Criteria_Not_Ilike);
   end Not_Ilike;

   --------------
   -- Not_Like --
   --------------

   function Not_Like
     (Self : SQL_Field_Text; Str : String) return SQL_Criteria is
   begin
      return Compare (Self, Expression (Str), Criteria_Not_Like);
   end Not_Like;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : SQL_Criteria; Long : Boolean := True) return Unbounded_String
   is
      function Is_Constant_Boolean2 return Boolean;
      --  Whether the second argument is a constant boolean (False or True)

      function Is_True2 return Boolean;
      --  Whether the second argument (a constant boolean) is "TRUE"

      function Is_Constant_Boolean2 return Boolean is
      begin
         --  No table specified
         return
           Self.Criteria.Data.Arg2.Data.Field.all
             in SQL_Field_Boolean_Build'Class
           and then
             (Self.Criteria.Data.Arg2.Data.Field.Table = null
              and then Self.Criteria.Data.Arg2.Data.Field.Instance = null);
      end Is_Constant_Boolean2;

      function Is_True2 return Boolean is
         Data : constant Named_Field_Internal_Access :=
           Named_Field_Internal_Access
             (SQL_Field_Boolean_Build (Self.Criteria.Data.Arg2.Data.Field.all)
                .Data.Data);
      begin
         return Data.Value.all = "TRUE";
      end Is_True2;

      Result : Unbounded_String;
      C      : Criteria_List.Cursor;
      C2     : Field_List.Cursor;
      Is_First : Boolean;
   begin
      if Self.Criteria.Data /= null then
         case Self.Criteria.Data.Op is
            when Field_Criteria | Like_Criteria | Criteria_Overlaps =>
               if Self.Criteria.Data.Op = Criteria_Equal
                 and then Is_Constant_Boolean2
               then
                  if not Is_True2 then
                     Result := To_Unbounded_String ("not ");
                  end if;
                  Append (Result,
                          To_String
                            (Self.Criteria.Data.Arg1.Data.Field.all, Long));

               elsif Self.Criteria.Data.Op = Criteria_Not_Equal
                 and then Is_Constant_Boolean2
               then
                  if Is_True2 then
                     Result := To_Unbounded_String ("not ");
                  end if;
                  Append (Result,
                          To_String
                            (Self.Criteria.Data.Arg1.Data.Field.all, Long));

               else
                  Result := To_Unbounded_String
                    (To_String (Self.Criteria.Data.Arg1.Data.Field.all, Long));
                  case Self.Criteria.Data.Op is
                     when Criteria_Equal            => Append (Result, "=");
                     when Criteria_Not_Equal        => Append (Result, "!=");
                     when Criteria_Less_Than        => Append (Result, "<");
                     when Criteria_Less_Or_Equal    => Append (Result, "<=");
                     when Criteria_Greater_Than     => Append (Result, ">");
                     when Criteria_Greater_Or_Equal => Append (Result, ">=");
                     when Criteria_Any       =>
                        Append (Result, " = ANY (");
                     when Criteria_Like      => Append (Result, " LIKE ");
                     when Criteria_Ilike     => Append (Result, " ILIKE ");
                     when Criteria_Not_Like  => Append (Result, " NOT LIKE ");
                     when Criteria_Not_Ilike => Append (Result, " NOT ILIKE ");
                     when Criteria_Overlaps  => Append (Result, " OVERLAPS ");
                     when others => null;
                  end case;
                  Append (Result,
                          To_String
                            (Self.Criteria.Data.Arg2.Data.Field.all, Long));

                  case Self.Criteria.Data.Op is
                     when Criteria_Any       =>
                        Append (Result, ")");
                     when others =>
                        null;
                  end case;
               end if;

            when Criteria_Criteria =>
               C := First (Self.Criteria.Data.Criterias);
               while Has_Element (C) loop
                  if C /= First (Self.Criteria.Data.Criterias) then
                     case Self.Criteria.Data.Op is
                        when Criteria_And => Append (Result, " AND ");
                        when Criteria_Or  => Append (Result, " OR ");
                        when others       => null;
                     end case;
                  end if;

                  if Element (C).Criteria.Data.Op in Criteria_Criteria then
                     Append (Result, "(");
                     Append (Result, To_String (Element (C)));
                     Append (Result, ")");
                  else
                     Append (Result, To_String (Element (C)));
                  end if;
                  Next (C);
               end loop;

            when Criteria_In | Criteria_Not_In =>
               Result := To_Unbounded_String
                 (To_String (Self.Criteria.Data.Arg.Data.Field.all,
                  Long));

               if Self.Criteria.Data.Op = Criteria_In then
                  Append (Result, " IN (");
               else
                  Append (Result, " NOT IN (");
               end if;

               Is_First := True;
               C2 := First (Self.Criteria.Data.List.List);
               while Has_Element (C2) loop
                  if not Is_First then
                     Append (Result, ",");
                  end if;

                  Is_First := False;
                  Append (Result, To_String (Element (C2), Long));
                  Next (C2);
               end loop;

               Append (Result, To_String (Self.Criteria.Data.Subquery));
               Append (Result, To_String (Self.Criteria.Data.In_String));
               Append (Result, ")");

            when Null_Criteria =>
               Result := To_Unbounded_String
                 (To_String (Self.Criteria.Data.Arg3.Data.Field.all,
                  Long));

               case Self.Criteria.Data.Op is
                  when Criteria_Null     => Append (Result, " IS NULL");
                  when Criteria_Not_Null => Append (Result, " IS NOT NULL");
                  when others            => null;
               end case;

         end case;
      end if;
      return Result;
   end To_String;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Controlled_SQL_Query) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Query_Contents'Class, SQL_Query_Contents_Access);
   begin
      if Self.Data /= null then
         Self.Data.Refcount := Self.Data.Refcount - 1;
         if Self.Data.Refcount = 0 then
            Free (Self.Data.all);
            Unchecked_Free (Self.Data);
         end if;
      end if;
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Controlled_SQL_Query) is
   begin
      if Self.Data /= null then
         Self.Data.Refcount := Self.Data.Refcount + 1;
      end if;
   end Adjust;

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
      Distinct : Boolean := False) return SQL_Query
   is
      Data : constant Query_Select_Contents_Access :=
        new Query_Select_Contents;
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
      return (Contents =>
                (Ada.Finalization.Controlled
                 with SQL_Query_Contents_Access (Data)));
   end SQL_Select;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : Query_Select_Contents) return Unbounded_String
   is
      Result : Unbounded_String;
   begin
      Result := To_Unbounded_String ("SELECT ");
      if Self.Distinct then
         Append (Result, "DISTINCT ");
      end if;
      Append (Result, To_String (Self.Fields, Long => True));
      if Self.Tables /= Empty_Table_List
        or else not Is_Empty (Self.Extra_Tables)
      then
         Append (Result, " FROM ");
         if Self.Tables.Data.Data = null
           or else Is_Empty (Self.Tables.Data.Data.List)
         then
            Append (Result, To_String (Self.Extra_Tables));
         elsif Is_Empty (Self.Extra_Tables) then
            Append (Result, To_String (Self.Tables));
         else
            Append (Result, To_String (Self.Tables));
            Append (Result, ", ");
            Append (Result, To_String (Self.Extra_Tables));
         end if;
      end if;
      if Self.Criteria /= No_Criteria then
         Append (Result, " WHERE ");
         Append (Result, To_String (Self.Criteria));
      end if;
      if Self.Group_By /= Empty_Field_List then
         Append (Result, " GROUP BY ");
         Append (Result, To_String (Self.Group_By, Long => True));
         if Self.Having /= No_Criteria then
            Append (Result, " HAVING ");
            Append (Result, To_String (Self.Having));
         end if;
      end if;
      if Self.Order_By /= Empty_Field_List then
         Append (Result, " ORDER BY ");
         Append (Result, To_String (Self.Order_By, Long => True));
      end if;
      if Self.Offset >= 0 then
         Append (Result, " OFFSET" & Integer'Image (Self.Offset));
      end if;
      if Self.Limit >= 0 then
         Append (Result, " LIMIT" & Integer'Image (Self.Limit));
      end if;
      return Result;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : SQL_Query) return Unbounded_String is
   begin
      if Self.Contents.Data = null then
         return Null_Unbounded_String;
      else
         return To_String (Self.Contents.Data.all);
      end if;
   end To_String;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Named_Field_Internal) is
   begin
      Free (Self.Operator);
      Free (Self.Value);
   end Free;

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
      Auto_Complete
        (Self.Contents.Data.all, Auto_Complete_From, Auto_Complete_Group_By);
   end Auto_Complete;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (Self : SQL_Criteria; To : in out Table_Sets.Set)
   is
      Data : constant SQL_Criteria_Data_Access := Self.Criteria.Data;
      C    : Criteria_List.Cursor;
   begin
      if Data = null then
         return;
      end if;

      case Data.Op is
         when Field_Criteria | Like_Criteria | Criteria_Overlaps =>
            Append_Tables (Data.Arg1.Data.Field.all, To);
            Append_Tables (Data.Arg2.Data.Field.all, To);

         when Criteria_Criteria =>
            C := First (Data.Criterias);
            while Has_Element (C) loop
               Append_Tables (Element (C), To);
               Next (C);
            end loop;

         when Criteria_In | Criteria_Not_In =>
            Append_Tables (Data.Arg.Data.Field.all, To);

         when Null_Criteria =>
            Append_Tables (Data.Arg3.Data.Field.all, To);
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
      if Self.Data.Data.Tables.Data.Data /= null then
         C := First (Self.Data.Data.Tables.Data.Data.List);
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
                       Instance => Self.Instance));
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
      if Self.Data.Data /= null then
         C := First (Self.Data.Data.List);
         while Has_Element (C) loop
            Append_Tables (Element (C), To);
            Next (C);
         end loop;
      end if;
   end Append_Tables;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables (Self : SQL_Field; To : in out Table_Sets.Set) is
   begin
      if Self.Table /= null then
         Include (To, (Name => Self.Table, Instance => Self.Instance));
      end if;
   end Append_Tables;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (Self : SQL_Field_Integer_Build; To : in out Table_Sets.Set) is
   begin
      Append_Tables (Self.Data.Data.all, To);
   end Append_Tables;

   procedure Append_Tables
     (Self : SQL_Field_Text_Build; To : in out Table_Sets.Set) is
   begin
      Append_Tables (Self.Data.Data.all, To);
   end Append_Tables;

   procedure Append_Tables
     (Self : SQL_Field_Boolean_Build; To : in out Table_Sets.Set) is
   begin
      Append_Tables (Self.Data.Data.all, To);
   end Append_Tables;

   procedure Append_Tables
     (Self : SQL_Field_Float_Build; To : in out Table_Sets.Set) is
   begin
      Append_Tables (Self.Data.Data.all, To);
   end Append_Tables;

   procedure Append_Tables
     (Self : SQL_Field_Time_Build; To : in out Table_Sets.Set) is
   begin
      Append_Tables (Self.Data.Data.all, To);
   end Append_Tables;

   procedure Append_Tables
     (Self : SQL_Field_Any; To : in out Table_Sets.Set) is
   begin
      Append_Tables (Self.Data.Data.all, To);
   end Append_Tables;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (From : SQL_Field_List; To : in out Table_Sets.Set)
   is
      C : Field_List.Cursor := First (From.List);
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
     (Self : Named_Field_Internal; To : in out Table_Sets.Set) is
   begin
      if Self.Table /= No_Names then
         Include (To, Self.Table);
      end if;
   end Append_Tables;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (Self : As_Field_Internal; To : in out Table_Sets.Set) is
   begin
      Append_Tables (Self.Renamed.Data.Field.all, To);
   end Append_Tables;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (Self : Sorted_Field_Internal; To : in out Table_Sets.Set) is
   begin
      Append_Tables (Self.Sorted.Data.Field.all, To);
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
         Append_Tables (Element (C).Field.Data.Field.all, To);
         Next (C);
      end loop;

      if Self.Else_Clause /= No_Field_Pointer then
         Append_Tables (Self.Else_Clause.Data.Field.all, To);
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
     (Self         : SQL_Field_Integer_Build;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean)
   is
   begin
      Append_If_Not_Aggregate (Self.Data.Data, To, Is_Aggregate);
   end Append_If_Not_Aggregate;

   procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_Text_Build;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean)
   is
   begin
      Append_If_Not_Aggregate (Self.Data.Data, To, Is_Aggregate);
   end Append_If_Not_Aggregate;

   procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_Float_Build;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean)
   is
   begin
      Append_If_Not_Aggregate (Self.Data.Data, To, Is_Aggregate);
   end Append_If_Not_Aggregate;

   procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_Boolean_Build;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean)
   is
   begin
      Append_If_Not_Aggregate (Self.Data.Data, To, Is_Aggregate);
   end Append_If_Not_Aggregate;

   procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_Time_Build;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean)
   is
   begin
      Append_If_Not_Aggregate (Self.Data.Data, To, Is_Aggregate);
   end Append_If_Not_Aggregate;

   procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_Any;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean)
   is
   begin
      Append_If_Not_Aggregate (Self.Data.Data, To, Is_Aggregate);
   end Append_If_Not_Aggregate;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   procedure Append_If_Not_Aggregate
     (Self         : SQL_Field;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean)
   is
      pragma Unreferenced (Is_Aggregate);
   begin
      --  Ignore constant fields (NULL,...)
      if Self.Table /= null then
         Append (To.List, Self);
      end if;
   end Append_If_Not_Aggregate;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_List;
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
     (Self         : access Named_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean)
   is
      C : Field_List.Cursor := First (Self.List.List);
   begin
      while Has_Element (C) loop
         Append_If_Not_Aggregate (Element (C), To, Is_Aggregate);
         Next (C);
      end loop;

      --  We create a SQL_Field_Text, but it might be any other type.
      --  This isn't really relevant, however, since the exact type is not used
      --  later on.

      if Self.Table /= No_Names then
         Self.Refcount := Self.Refcount + 1;
         Append
           (To.List, SQL_Field_Text_Build'
              (Table    => Self.Table.Name,
               Instance => Self.Table.Instance,
               Name     => null,
               Data     => (Ada.Finalization.Controlled with
                            SQL_Field_Internal_Access (Self))));
      end if;
   end Append_If_Not_Aggregate;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   procedure Append_If_Not_Aggregate
     (Self         : access As_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean) is
   begin
      Append_If_Not_Aggregate (Self.Renamed.Data.Field.all, To, Is_Aggregate);
   end Append_If_Not_Aggregate;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   procedure Append_If_Not_Aggregate
     (Self         : access Sorted_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean) is
   begin
      Append_If_Not_Aggregate
        (Self.Sorted.Data.Field.all, To, Is_Aggregate);
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
         Append_If_Not_Aggregate
           (Element (C).Field.Data.Field.all, To, Is_Aggregate);
         Next (C);
      end loop;

      if Self.Else_Clause /= No_Field_Pointer then
         Append_If_Not_Aggregate
           (Self.Else_Clause.Data.Field.all, To, Is_Aggregate);
      end if;
   end Append_If_Not_Aggregate;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   procedure Append_If_Not_Aggregate
     (Self         : SQL_Criteria;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean)
   is
      Data : constant SQL_Criteria_Data_Access := Self.Criteria.Data;
      C    : Criteria_List.Cursor;
   begin
      if Data /= null then
         case Data.Op is
            when Field_Criteria | Like_Criteria | Criteria_Overlaps =>
               Append_If_Not_Aggregate
                 (Data.Arg1.Data.Field.all, To, Is_Aggregate);
               Append_If_Not_Aggregate
                 (Data.Arg2.Data.Field.all, To, Is_Aggregate);

            when Criteria_Criteria =>
               C := First (Data.Criterias);
               while Has_Element (C) loop
                  Append_If_Not_Aggregate (Element (C), To, Is_Aggregate);
                  Next (C);
               end loop;

            when Criteria_In | Criteria_Not_In =>
               Append_If_Not_Aggregate
                 (Data.Arg.Data.Field.all, To, Is_Aggregate);

            when Null_Criteria =>
               Append_If_Not_Aggregate
                 (Data.Arg3.Data.Field.all, To, Is_Aggregate);
         end case;
      end if;
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

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Assignment_Controlled) is
   begin
      if Self.Data /= null then
         Self.Data.Refcount := Self.Data.Refcount + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Assignment_Controlled) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Assignment_Item, Assignment_Item_Access);
   begin
      if Self.Data /= null then
         Self.Data.Refcount := Self.Data.Refcount - 1;
         if Self.Data.Refcount = 0 then
            Free (Self.Data.Value);
            Unchecked_Free (Self.Data);
         end if;
      end if;
   end Finalize;

   ------------
   -- Assign --
   ------------

   procedure Assign
     (R     : out SQL_Assignment;
      Field : SQL_Field'Class;
      Value : GNAT.Strings.String_Access)
   is
      A : constant Assignment_Controlled :=
        (Ada.Finalization.Controlled with
         Data => new Assignment_Item'
           (Refcount => 1,
            Field    => +Field,
            Value    => Value,
            To_Field => No_Field_Pointer));
   begin
      Append (R.List, A);
   end Assign;

   ---------
   -- "=" --
   ---------

   function "="
     (Field : SQL_Field_Text'Class; Value : String) return SQL_Assignment
   is
      Result : SQL_Assignment;
   begin
      Assign
        (Result, Field, new String'(Normalize_String (Value)));
      return Result;
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Field : SQL_Field_Integer'Class; Value : Integer) return SQL_Assignment
   is
      Result : SQL_Assignment;
   begin
      Assign (Result, Field, new String'(Integer'Image (Value)));
      return Result;
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Field : SQL_Field_Boolean'Class; Value : Boolean) return SQL_Assignment
   is
      Result : SQL_Assignment;
   begin
      Assign (Result, Field, new String'(Boolean'Image (Value)));
      return Result;
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Field : SQL_Field_Time'Class;
      Value : Ada.Calendar.Time) return SQL_Assignment
   is
      Result   : SQL_Assignment;
      Adjusted : Ada.Calendar.Time;
   begin
      if Ada.Calendar.Year (Value) = Ada.Calendar.Year_Number'First then
         return To_Null (Field);
      else
         --  Value is GMT, but Image insists on converting it to
         --  local time, so we cheat a little
         if Value /= No_Time then
            Adjusted := Value - Duration (UTC_Time_Offset (Value)) * 60.0;
            Assign
              (Result,
               Field, new String'(Image (Adjusted, "'%Y-%m-%d %H:%M:%S'")));
         else
            Assign (Result, Field, new String'("NULL"));
         end if;
         return Result;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Field : SQL_Field_Float'Class; Value : Float) return SQL_Assignment
   is
      Result : SQL_Assignment;
   begin
      Assign (Result, Field, new String'(Float'Image (Value)));
      return Result;
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Field : SQL_Field'Class; To : SQL_Field'Class) return SQL_Assignment
   is
      Result : SQL_Assignment;
      A : constant Assignment_Controlled :=
        (Ada.Finalization.Controlled with
         Data => new Assignment_Item'
           (Refcount => 1,
            Field    => +Field,
            Value    => null,
            To_Field => +To));
   begin
      Append (Result.List, A);
      return Result;
   end "=";

   -------------
   -- To_Null --
   -------------

   function To_Null (Field : SQL_Field'Class) return SQL_Assignment is
      Result : SQL_Assignment;
   begin
      Assign (Result, Field, null);
      return Result;
   end To_Null;

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : SQL_Assignment) return SQL_Assignment is
      Result : SQL_Assignment;
      C      : Assignment_Lists.Cursor := First (Right.List);
   begin
      Result.List := Left.List;
      while Has_Element (C) loop
         Append (Result.List, Element (C));
         Next (C);
      end loop;
      return Result;
   end "&";

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : SQL_Assignment; With_Field : Boolean) return String
   is
      Result : Unbounded_String;
      C      : Assignment_Lists.Cursor := First (Self.List);
      Data   : Assignment_Item_Access;
   begin
      while Has_Element (C) loop
         Data := Element (C).Data;
         if Result /= Null_Unbounded_String then
            Append (Result, ", ");
         end if;

         if Data.Value = null then
            if Data.To_Field.Data /= null then
               if With_Field then
                  Append
                    (Result,
                     To_String (Data.Field.Data.Field.all, Long => False)
                     & "="
                     & To_String (Data.To_Field.Data.Field.all, Long => True));
               else
                  Append
                    (Result,
                     To_String (Data.To_Field.Data.Field.all, Long => True));
               end if;

            elsif With_Field then
               Append
                 (Result, To_String (Data.Field.Data.Field.all, Long => False)
                  & "=" & Null_String);
            else
               Append (Result, Null_String);
            end if;
         else
            if With_Field then
               Append
                 (Result, To_String (Data.Field.Data.Field.all, Long => False)
                  & "=" & Data.Value.all);
            else
               Append (Result, Data.Value.all);
            end if;
         end if;
         Next (C);
      end loop;
      return To_String (Result);
   end To_String;

   ----------------
   -- SQL_Delete --
   ----------------

   function SQL_Delete
     (From     : SQL_Table'Class;
      Where    : SQL_Criteria := No_Criteria) return SQL_Query
   is
      Data : constant Query_Delete_Contents_Access :=
        new Query_Delete_Contents;
   begin
      Data.Table := +From;
      Data.Where := Where;
      return (Contents =>
              (Ada.Finalization.Controlled
               with SQL_Query_Contents_Access (Data)));
   end SQL_Delete;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Query_Delete_Contents) return Unbounded_String is
      Result : Unbounded_String;
   begin
      Result := To_Unbounded_String ("DELETE FROM ");
      Append (Result, To_String (Element (First (Self.Table.Data.Data.List))));

      if Self.Where /= No_Criteria then
         Append (Result, " WHERE ");
         Append (Result, To_String (Self.Where, Long => False));
      end if;

      return Result;
   end To_String;

   -------------------------------
   -- SQL_Insert_Default_Values --
   -------------------------------

   function SQL_Insert_Default_Values
     (Table : SQL_Table'Class) return SQL_Query
   is
      Data : constant Query_Insert_Contents_Access :=
        new Query_Insert_Contents;
   begin
      Data.Into := (Name     => Table.Table_Name,
                    Instance => Table.Instance);
      Data.Default_Values := True;
      return (Contents =>
              (Ada.Finalization.Controlled
               with SQL_Query_Contents_Access (Data)));
   end SQL_Insert_Default_Values;

   ----------------
   -- SQL_Insert --
   ----------------

   function SQL_Insert
     (Fields   : SQL_Field_Or_List'Class;
      Values   : SQL_Query) return SQL_Query
   is
      Data : constant Query_Insert_Contents_Access :=
        new Query_Insert_Contents;
      Q    : SQL_Query;
   begin
      if Fields in SQL_Field'Class then
         Data.Fields := +SQL_Field'Class (Fields);
      else
         Data.Fields := SQL_Field_List (Fields);
      end if;

      Data.Into   := No_Names;
      Data.Subquery := Values;
      Q := (Contents =>
              (Ada.Finalization.Controlled
               with SQL_Query_Contents_Access (Data)));
      Auto_Complete (Q);
      return Q;
   end SQL_Insert;

   ----------------
   -- SQL_Insert --
   ----------------

   function SQL_Insert
     (Values : SQL_Assignment;
      Where  : SQL_Criteria := No_Criteria) return SQL_Query
   is
      Data : constant Query_Insert_Contents_Access :=
        new Query_Insert_Contents;
      Q    : SQL_Query;
   begin
      Data.Into   := No_Names;
      Data.Values := Values;
      Data.Where  := Where;
      Q := (Contents =>
              (Ada.Finalization.Controlled
               with SQL_Query_Contents_Access (Data)));
      Auto_Complete (Q);
      return Q;
   end SQL_Insert;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Query_Insert_Contents) return Unbounded_String is
      Result : Unbounded_String;
   begin
      Result := To_Unbounded_String ("INSERT INTO ");
      Append (Result, To_String (Self.Into));

      if Self.Default_Values then
         Append (Result, " DEFAULT VALUES");
      else
         if Self.Fields /= Empty_Field_List then
            Append (Result, " (");
            Append (Result, To_String (Self.Fields, Long => False));
            Append (Result, ")");
         end if;

         if not Is_Empty (Self.Values.List) then
            Append (Result, " VALUES (");
            Append (Result, To_String (Self.Values, With_Field => False));
            Append (Result, ")");
         end if;

         if Self.Subquery /= No_Query then
            Append (Result, " ");
            Append (Result, To_String (Self.Subquery));
         end if;
      end if;

      return Result;
   end To_String;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (Self : SQL_Assignment; To : in out Table_Sets.Set)
   is
      C : Assignment_Lists.Cursor := First (Self.List);
      D : Field_Pointer_Data_Access;
   begin
      while Has_Element (C) loop
         D := Element (C).Data.Field.Data;
         Append_Tables (D.Field.all, To);

         D := Element (C).Data.To_Field.Data;
         if D /= null then
            Append_Tables (D.Field.all, To);
         end if;

         Next (C);
      end loop;
   end Append_Tables;

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
      C2          : Assignment_Lists.Cursor;
   begin
      if Auto_Complete_From then

         --  Get the list of fields first, so that we'll also know what table
         --  is being updated

         if Self.Fields = Empty_Field_List then
            C2 := First (Self.Values.List);
            while Has_Element (C2) loop
               Append
                 (Self.Fields.List, Element (C2).Data.Field.Data.Field.all);
               Next (C2);
            end loop;
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
               C2 := First (Self.Values.List);
               while Has_Element (C2) loop
                  if Element (C2).Data.Value /= null then
                     Subfields := Subfields
                       & From_String (Element (C2).Data.Value.all);
                  elsif Element (C2).Data.To_Field.Data /= null then
                     Subfields := Subfields
                       & Element (C2).Data.To_Field.Data.Field.all;
                  else
                     --  Setting a field to null ?
                     Subfields := Subfields & Null_Field_Text;
                  end if;
                  Next (C2);
               end loop;

               Self.Subquery := SQL_Select
                 (Fields => Subfields, Where => Self.Where);
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
      Data : constant Query_Update_Contents_Access :=
        new Query_Update_Contents;
   begin
      Data.Table := +Table;
      Data.Set   := Set;
      Data.Where := Where;

      if From in SQL_Table'Class then
         Data.From := +SQL_Table'Class (From);
      else
         Data.From := SQL_Table_List (From);
      end if;

      return (Contents =>
              (Ada.Finalization.Controlled
               with SQL_Query_Contents_Access (Data)));
   end SQL_Update;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Query_Update_Contents) return Unbounded_String is
      Result : Unbounded_String;
   begin
      Result := To_Unbounded_String ("UPDATE ");
      Append (Result, To_String (Element (First (Self.Table.Data.Data.List))));

      Append (Result, " SET ");
      Append (Result, To_String (Self.Set, With_Field => True));

      if Self.From /= Empty_Table_List
        or else not Is_Empty (Self.Extra_From)
      then
         Append (Result, " FROM ");
         if Self.From.Data.Data = null
           or else Is_Empty (Self.From.Data.Data.List)
         then
            Append (Result, To_String (Self.Extra_From));
         elsif Is_Empty (Self.Extra_From) then
            Append (Result, To_String (Self.From));
         else
            Append (Result, To_String (Self.From));
            Append (Result, ", ");
            Append (Result, To_String (Self.Extra_From));
         end if;
      end if;

      if Self.Where /= No_Criteria then
         Append (Result, " WHERE ");
         Append (Result, To_String (Self.Where, Long => True));
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

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Join_Table_Data) is
   begin
      if Self.Data /= null then
         Self.Data.Refcount := Self.Data.Refcount + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Join_Table_Internal, Join_Table_Internal_Access);

   procedure Finalize (Self : in out Join_Table_Data) is
   begin
      if Self.Data /= null then
         Self.Data.Refcount := Self.Data.Refcount - 1;
         if Self.Data.Refcount = 0 then
            Unchecked_Free (Self.Data);
         end if;
      end if;
   end Finalize;

   ---------------
   -- Left_Join --
   ---------------

   function Left_Join
     (Full    : SQL_Single_Table'Class;
      Partial : SQL_Single_Table'Class;
      On      : SQL_Criteria := No_Criteria) return SQL_Left_Join_Table
   is
      Criteria : SQL_Criteria := On;
   begin
      if Criteria = No_Criteria then
         --  We only provide auto-completion if both Full and Partial are
         --  simple tables (not the result of joins), otherwise it is almost
         --  impossible to get things right automatically (which tables should
         --  be involved ? In case of multiple paths between two tables, which
         --  path should we use ? ...)

         if Full not in SQL_Table'Class
           or else Partial in SQL_Table'Class
         then
            raise Program_Error with "Can only auto-complete simple tables";
         end if;

         Criteria :=
           FK (SQL_Table (Full), SQL_Table (Partial))
           and FK (SQL_Table (Partial), SQL_Table (Full)) and Criteria;
      end if;

      return SQL_Left_Join_Table'
        (Instance     => null,
         Data => (Join_Table_Data'
                    (Ada.Finalization.Controlled with
                     Data => new Join_Table_Internal'
                       (Refcount     => 1,
                        Tables       => Full & Partial,
                        Is_Left_Join => True,
                        On           => Criteria))));
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
      R.Data.Data.Is_Left_Join := False;
      return R;
   end Join;

   ------------
   -- Rename --
   ------------

   function Rename
     (Self : SQL_Left_Join_Table; Name : Cst_String_Access)
      return SQL_Left_Join_Table'Class
   is
      Result : constant SQL_Left_Join_Table :=
        (SQL_Table_Or_List with
         Instance   => Name,
         Data       => Self.Data);
   begin
--      Result.Instance := Name;
      return Result;
   end Rename;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Simple_Query_Contents) return Unbounded_String is
   begin
      return Self.Command;
   end To_String;

   --------------
   -- SQL_Lock --
   --------------

   function SQL_Lock (Table : SQL_Table'Class) return SQL_Query is
      Data : constant Simple_Query_Contents_Access :=
        new Simple_Query_Contents;
   begin
      Data.Command := To_Unbounded_String ("LOCK " & To_String (Table));
      return (Contents =>
              (Ada.Finalization.Controlled
               with SQL_Query_Contents_Access (Data)));
   end SQL_Lock;

   ---------------
   -- SQL_Begin --
   ---------------

   function SQL_Begin return SQL_Query is
      Data : constant Simple_Query_Contents_Access :=
        new Simple_Query_Contents;
   begin
      Data.Command := To_Unbounded_String ("BEGIN");
      return (Contents =>
              (Ada.Finalization.Controlled
               with SQL_Query_Contents_Access (Data)));
   end SQL_Begin;

   ------------------
   -- SQL_Rollback --
   ------------------

   function SQL_Rollback return SQL_Query is
      Data : constant Simple_Query_Contents_Access :=
        new Simple_Query_Contents;
   begin
      Data.Command := To_Unbounded_String ("ROLLBACK");
      return (Contents =>
              (Ada.Finalization.Controlled
               with SQL_Query_Contents_Access (Data)));
   end SQL_Rollback;

   ----------------
   -- SQL_Commit --
   ----------------

   function SQL_Commit return SQL_Query is
      Data : constant Simple_Query_Contents_Access :=
        new Simple_Query_Contents;
   begin
      Data.Command := To_Unbounded_String ("COMMIT");
      return (Contents =>
              (Ada.Finalization.Controlled
               with SQL_Query_Contents_Access (Data)));
   end SQL_Commit;

   --------------
   -- Subquery --
   --------------

   function Subquery
     (Query : SQL_Query; Table_Name : Cst_String_Access) return Subquery_Table
   is
   begin
      return Subquery_Table'
        (Instance => Table_Name,
         Query    => Query);
   end Subquery;

   ----------
   -- Free --
   ----------

   procedure Free (A : in out SQL_Table_Access) is
   begin
      Unchecked_Free (A);
   end Free;

end GNATCOLL.SQL;
