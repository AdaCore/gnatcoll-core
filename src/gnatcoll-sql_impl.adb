------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2015, AdaCore                     --
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
with Ada.Calendar.Time_Zones;    use Ada.Calendar.Time_Zones;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNAT.Calendar.Time_IO;      use GNAT.Calendar, GNAT.Calendar.Time_IO;
with GNAT.Strings;               use GNAT.Strings;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

package body GNATCOLL.SQL_Impl is
   use type Field_Pointers.Element_Access;
   use type SQL_Field_Pointers.Element_Access;
   use type SQL_Criteria_Pointers.Element_Access;
   use Field_List, Table_Sets, Assignment_Lists, SQL_Criteria_Pointers,
       Field_Pointers;

   Comparison_Equal         : aliased constant String := "=";
   Comparison_Different     : aliased constant String := "<>";
   Comparison_Less          : aliased constant String := "<";
   Comparison_Less_Equal    : aliased constant String := "<=";
   Comparison_Greater       : aliased constant String := ">";
   Comparison_Greater_Equal : aliased constant String := ">=";

   --------------------------
   --  Named field data --
   --------------------------
   --  Instantiation of field_data for specific types of fields, created for
   --  instance via Expression, From_String, or operators on time. Such fields
   --  are still typed

   type Field_Type is (Field_Std, Field_Operator, Field_Parameter);
   --  The type of the field:
   --     Field_Std: Str_Value is directly the text of the field
   --     Field_Operator:  Str_Value is the operator, that acts on several
   --          fields ("-" for instance")
   --     Field_Parameter: the field's exact value is unknown, and will be
   --          substituted at runtime (for instance "?1" in sqlite3). Str_Value
   --          is then the index of the field.

   type Named_Field_Internal (Typ : Field_Type)
     is new SQL_Field_Internal with record
      Table     : Table_Names := No_Names;

      case Typ is
         when Field_Std =>
            Str_Value : GNAT.Strings.String_Access;
            --  The expression representing the field in SQL. See Field_Type
            --  for more information

         when Field_Operator =>
            Op_Value : GNAT.Strings.String_Access;
            List     : SQL_Field_List;

         when Field_Parameter =>
            Index      : Positive;
            Param_Type : Parameter_Type;
      end case;
   end record;
   overriding procedure Free (Self : in out Named_Field_Internal);
   overriding function To_String
     (Self   : Named_Field_Internal;
      Format : Formatter'Class;
      Long   : Boolean) return String;
   overriding procedure Append_Tables
     (Self : Named_Field_Internal; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : access Named_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

   procedure Assign
     (R     : out SQL_Assignment;
      Field : SQL_Field'Class;
      Value : Named_Field_Internal'Class);
   procedure Unassign
     (R     : out SQL_Assignment;
      Field : SQL_Field'Class);
   --  Assign Value to Field (or set field to NULL if Value is null).
   --  On exit, Value belongs to R and should not be freed by the caller.

   ---------------------
   -- Function fields --
   ---------------------

   type Function_Field is new SQL_Field_Internal with record
      Prefix, Suffix : GNAT.Strings.String_Access;
      To_Field       : SQL_Field_Pointer;
   end record;
   overriding procedure Free (Self : in out Function_Field);
   overriding function To_String
     (Self   : Function_Field;
      Format : Formatter'Class;
      Long   : Boolean) return String;
   overriding procedure Append_Tables
     (Self : Function_Field; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : access Function_Field;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);
   --  A field that applies a function (via Prefix .. Suffix) to another field

   --------------
   -- Criteria --
   --------------

   type Comparison_Criteria is new SQL_Criteria_Data with record
      Op, Suffix : Cst_String_Access;
      Arg1, Arg2 : SQL_Field_Pointer;
   end record;
   overriding function To_String
     (Self   : Comparison_Criteria;
      Format : Formatter'Class;
      Long   : Boolean := True) return String;
   overriding procedure Append_Tables
     (Self : Comparison_Criteria; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : Comparison_Criteria;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

   ----------------
   -- Data_Field --
   ----------------

   package body Data_Fields is
      overriding function To_String
        (Self   : Field;
         Format : Formatter'Class;
         Long   : Boolean := True) return String is
      begin
         if not Self.Data.Is_Null then
            return To_String (Self.Data.Get.Element.all, Format, Long);
         else
            return "";
         end if;
      end To_String;

      overriding procedure Append_Tables
        (Self : Field; To : in out Table_Sets.Set) is
      begin
         if not Self.Data.Is_Null then
            Append_Tables (Self.Data.Get.Element.all, To);
         end if;
      end Append_Tables;

      overriding procedure Append_If_Not_Aggregate
        (Self         : Field;
         To           : in out SQL_Field_List'Class;
         Is_Aggregate : in out Boolean)
      is
      begin
         if not Self.Data.Is_Null then
            Append_If_Not_Aggregate (Self.Data.Get.Element, To, Is_Aggregate);
         end if;
      end Append_If_Not_Aggregate;
   end Data_Fields;

   package Any_Fields is new Data_Fields (SQL_Field);

   -------------------
   -- Instance_Name --
   -------------------

   function Instance_Name (Names : Table_Names) return String is
   begin
      if Names.Instance = null then
         if Names.Instance_Index = -1 then
            if Names.Name = null then
               return "";
            else
               return Names.Name.all;
            end if;
         else
            return Names.Name (Names.Name'First)
              & Image (Names.Instance_Index, Min_Width => 1);
         end if;
      else
         return Names.Instance.all;
      end if;
   end Instance_Name;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Table_Names) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Instance_Name (Self));
   end Hash;

   -------------------
   -- Free_Dispatch --
   -------------------

   procedure Free_Dispatch (Self : in out SQL_Criteria_Data'Class) is
   begin
      Free (Self);
   end Free_Dispatch;

   -------------------
   -- Free_Dispatch --
   -------------------

   procedure Free_Dispatch (Self : in out SQL_Field_Internal'Class) is
   begin
      Free (Self);
   end Free_Dispatch;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Self   : SQL_Field_List;
      Format : Formatter'Class;
      Long   : Boolean := True) return String
   is
      C      : Field_List.Cursor := First (Self.List);
      Result : Unbounded_String;
   begin
      if Has_Element (C) then
         Append (Result, To_String (Element (C), Format, Long));
         Next (C);
      end if;

      while Has_Element (C) loop
         Append (Result, ", ");
         Append (Result, To_String (Element (C), Format, Long));
         Next (C);
      end loop;
      return To_String (Result);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Self   : SQL_Field;
      Format : Formatter'Class;
      Long   : Boolean := True) return String
   is
      pragma Unreferenced (Format);
   begin
      if not Long then
         return Self.Name.all;
      else
         declare
            N : constant String := Instance_Name
              ((Name           => Self.Table,
                Instance       => Self.Instance,
                Instance_Index => Self.Instance_Index));
         begin
            if N /= "" then
               return N & "." & Self.Name.all;
            else
               --  Self.Table could be null in the case of the Null_Field_*
               --  constants
               return Self.Name.all;
            end if;
         end;
      end if;
   end To_String;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Named_Field_Internal) is
   begin
      case Self.Typ is
         when Field_Std       => Free (Self.Str_Value);
         when Field_Operator  => Free (Self.Op_Value);
         when Field_Parameter => null;
      end case;
   end Free;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self   : Named_Field_Internal;
      Format : Formatter'Class;
      Long   : Boolean) return String
   is
      Result : Unbounded_String;
      C      : Field_List.Cursor;
   begin
      case Self.Typ is
         when Field_Std =>
            if Self.Table = No_Names then
               return Self.Str_Value.all;

            elsif Long then
               if Self.Table.Instance = null then
                  return Self.Table.Name.all & '.' & Self.Str_Value.all;
               else
                  return Self.Table.Instance.all & '.' & Self.Str_Value.all;
               end if;
            else
               return Self.Str_Value.all;
            end if;

         when Field_Operator =>
            C := First (Self.List.List);
            Result := To_Unbounded_String (To_String (Element (C), Format));
            Next (C);

            while Has_Element (C) loop
               Result := Result & " " & Self.Op_Value.all & " "
                 & To_String (Element (C), Format);
               Next (C);
            end loop;

            return To_String (Result);

         when Field_Parameter =>
            return Parameter_String (Format, Self.Index, Self.Param_Type);
      end case;
   end To_String;

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

   overriding procedure Append_Tables
     (Self : Function_Field; To : in out Table_Sets.Set) is
   begin
      Append_Tables (Self.To_Field.Get.Element.all, To);
   end Append_Tables;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   overriding procedure Append_If_Not_Aggregate
     (Self         : access Function_Field;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean)
   is
   begin
      Append_If_Not_Aggregate
         (Self.To_Field.Get.Element.all, To, Is_Aggregate);
   end Append_If_Not_Aggregate;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   procedure Append_If_Not_Aggregate
     (Self         : access Named_Field_Internal;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean)
   is
      C : Field_List.Cursor;
      F : Field_Pointers.Ref;
   begin
      if Self.Typ = Field_Operator then
         C := First (Self.List.List);
         while Has_Element (C) loop
            Append_If_Not_Aggregate (Element (C), To, Is_Aggregate);
            Next (C);
         end loop;
      end if;

      --  We create a SQL_Field_Text, but it might be any other type.
      --  This isn't really relevant, however, since the exact type is not used
      --  later on.

      if Self.Table /= No_Names then
         F.From_Element (Field_Pointers.Element_Access (Self));

         Append
           (To.List, Any_Fields.Field'
              (Table    => Self.Table.Name,
               Instance => Self.Table.Instance,
               Instance_Index => Self.Table.Instance_Index,
               Name     => null,
               Data     => F));
      end if;
   end Append_If_Not_Aggregate;

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
      Result.List := Left.List;  --  Does a copy, so we do not modify Left
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
      Result.List := Right.List; --  Does a copy so that we do not modify Right
      Prepend (Result.List, Left);
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left, Right : SQL_Field_List) return SQL_Field_List
   is
      Result : SQL_Field_List;
      C      : Field_List.Cursor := First (Right.List);
   begin
      Result.List := Left.List; --  Does a copy, don't modify Left
      while Has_Element (C) loop
         Append (Result.List, Element (C));
         Next (C);
      end loop;
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

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables (Self : SQL_Field; To : in out Table_Sets.Set) is
   begin
      if Self.Table /= null then
         Include (To, (Name => Self.Table, Instance => Self.Instance,
                       Instance_Index => Self.Instance_Index));
      end if;
   end Append_Tables;

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

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self   : SQL_Criteria;
      Format : Formatter'Class;
      Long   : Boolean := True) return String
   is
   begin
      if not Self.Criteria.Is_Null then
         return To_String (Self.Criteria.Get.Element.all, Format, Long);
      else
         return "";
      end if;
   end To_String;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables (Self : SQL_Criteria; To : in out Table_Sets.Set) is
   begin
      if not Self.Criteria.Is_Null then
         Append_Tables (Self.Criteria.Get.Element.all, To);
      end if;
   end Append_Tables;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   procedure Append_If_Not_Aggregate
     (Self         : SQL_Criteria;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean) is
   begin
      if not Self.Criteria.Is_Null then
         Append_If_Not_Aggregate
            (Self.Criteria.Get.Element.all, To, Is_Aggregate);
      end if;
   end Append_If_Not_Aggregate;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Self : in out SQL_Criteria; Data : SQL_Criteria_Data'Class) is
   begin
      Self.Criteria.Set (Data);
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Self : SQL_Criteria) return SQL_Criteria_Data_Access is
   begin
      return Self.Criteria.Unchecked_Get;
   end Get_Data;

   ---------
   -- "+" --
   ---------

   function "+" (Field : SQL_Field'Class) return SQL_Field_Pointer is
   begin
      return R : SQL_Field_Pointer do
         R.Set (Field);
      end return;
   end "+";

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Self   : Comparison_Criteria;
      Format : Formatter'Class;
      Long   : Boolean := True) return String
   is
      Arg1 : constant String := To_String (Self.Arg1, Format, Long => Long);
      Arg2 : constant String := To_String (Self.Arg2, Format, Long => Long);
   begin
      if Self.Op.all = "="
        and then Arg2 = "TRUE"
      then
         return Arg1;

      elsif Self.Op.all = "="
        and then Arg2 = "FALSE"
      then
         return "not " & Arg1;

      elsif Self.Suffix /= null then
         return Arg1 & Self.Op.all & Arg2 & Self.Suffix.all;

      else
         return Arg1 & Self.Op.all & Arg2;
      end if;
   end To_String;

   -------------------
   -- Append_Tables --
   -------------------

   overriding procedure Append_Tables
     (Self : Comparison_Criteria; To : in out Table_Sets.Set) is
   begin
      Append_Tables (Self.Arg1, To);
      Append_Tables (Self.Arg2, To);
   end Append_Tables;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   overriding procedure Append_If_Not_Aggregate
     (Self         : Comparison_Criteria;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean) is
   begin
      Append_If_Not_Aggregate (Self.Arg1, To, Is_Aggregate);
      Append_If_Not_Aggregate (Self.Arg2, To, Is_Aggregate);
   end Append_If_Not_Aggregate;

   -------------
   -- Compare --
   -------------

   function Compare
     (Left, Right : SQL_Field'Class;
      Op          : Cst_String_Access;
      Suffix      : Cst_String_Access := null) return SQL_Criteria
   is
      Data : constant Comparison_Criteria :=
         (SQL_Criteria_Data with
          Op => Op, Suffix => Suffix, Arg1 => +Left, Arg2 => +Right);
      Result : SQL_Criteria;
   begin
      Set_Data (Result, Data);
      return Result;
   end Compare;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self   : SQL_Field_Pointer;
      Format : Formatter'Class;
      Long   : Boolean) return String is
   begin
      return To_String (Self.Get.Element.all, Format, Long);
   end To_String;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (Self : SQL_Field_Pointer; To : in out Table_Sets.Set) is
   begin
      if not Self.Is_Null then
         Append_Tables (Self.Get.Element.all, To);
      end if;
   end Append_Tables;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   procedure Append_If_Not_Aggregate
     (Self         : SQL_Field_Pointer;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean) is
   begin
      if not Self.Is_Null then
         Append_If_Not_Aggregate (Self.Get.Element.all, To, Is_Aggregate);
      end if;
   end Append_If_Not_Aggregate;

   -----------
   -- First --
   -----------

   function First (List : SQL_Field_List) return Field_List.Cursor is
   begin
      return First (List.List);
   end First;

   ------------
   -- Append --
   ------------

   procedure Append
     (List : in out SQL_Field_List'Class; Field : SQL_Field_Pointer) is
   begin
      if not Field.Is_Null then
         Append (List.List, Field.Get.Element.all);
      end if;
   end Append;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (Self : SQL_Assignment; To : in out Table_Sets.Set)
   is
      C : Assignment_Lists.Cursor := First (Self.List);
   begin
      while Has_Element (C) loop
         Append_Tables (Element (C).Field, To);
         Append_Tables (Element (C).To_Field, To);
         Next (C);
      end loop;
   end Append_Tables;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self       : SQL_Assignment;
      Format     : Formatter'Class;
      With_Field : Boolean) return String
   is
      Result : Unbounded_String;
      C      : Assignment_Lists.Cursor := First (Self.List);
      Data   : Assignment_Item;
   begin
      while Has_Element (C) loop
         Data := Element (C);
         if Result /= Null_Unbounded_String then
            Append (Result, ", ");
         end if;

         if Data.To_Field /= No_Field_Pointer then
            if With_Field then
               Append
                 (Result, To_String (Data.Field, Format, Long => False)
                  & "=" & To_String (Data.To_Field, Format, Long => True));
            else
               Append
                 (Result, To_String (Data.To_Field, Format, Long => True));
            end if;

         elsif With_Field then
            Append
              (Result, To_String (Data.Field, Format, Long => False)
               & "=" & Null_String);
         else
            Append (Result, Null_String);
         end if;

         Next (C);
      end loop;
      return To_String (Result);
   end To_String;

   -------------
   -- To_List --
   -------------

   procedure To_List (Self : SQL_Assignment; List : out SQL_Field_List) is
      N    : Field_Pointers.Ref;
      C    : Assignment_Lists.Cursor := First (Self.List);
      Data : Assignment_Item;
   begin
      while Has_Element (C) loop
         Data := Element (C);

         if Data.To_Field /= No_Field_Pointer then
            Append (List, Data.To_Field);

         else
            --  Setting a field to null
            N.Set
              (Named_Field_Internal'
                 (SQL_Field_Internal with Typ => Field_Std, others => <>));
            Named_Field_Internal (N.Get.Element.all).Str_Value :=
              new String'(Null_String);

            List := List
              & Any_Fields.Field'
                  (Table          => null,
                   Instance       => null,
                   Instance_Index => -1,
                   Name           => null,
                   Data           => N);
         end if;

         Next (C);
      end loop;
   end To_List;

   ----------------
   -- Get_Fields --
   ----------------

   procedure Get_Fields (Self : SQL_Assignment; List : out SQL_Field_List) is
      C    : Assignment_Lists.Cursor := First (Self.List);
   begin
      while Has_Element (C) loop
         Append (List, Element (C).Field);
         Next (C);
      end loop;
   end Get_Fields;

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

   --------------
   -- Unassign --
   --------------

   procedure Unassign
     (R     : out SQL_Assignment;
      Field : SQL_Field'Class)
   is
   begin
      Append (R.List, Assignment_Item'(+Field, No_Field_Pointer));
   end Unassign;

   ------------
   -- Assign --
   ------------

   procedure Assign
     (R     : out SQL_Assignment;
      Field : SQL_Field'Class;
      Value : Named_Field_Internal'Class)
   is

      function To_Ref return Field_Pointers.Ref;

      ------------
      -- To_Ref --
      ------------

      function To_Ref return Field_Pointers.Ref is
         Result : Field_Pointers.Ref;
      begin
         Result.Set (Value);
         return Result;
      end To_Ref;

   begin
      declare
         A : constant Assignment_Item :=
           (Field    => +Field,
            To_Field => +Any_Fields.Field'
              (Table    => null,
               Instance => null,
               Instance_Index => -1,
               Name     => null,
               Data     => To_Ref));
      begin
         Append (R.List, A);
      end;
   end Assign;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Function_Field) is
   begin
      Free (Self.Prefix);
      Free (Self.Suffix);
      Free (SQL_Field_Internal (Self));
   end Free;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Self   : Function_Field;
      Format : Formatter'Class;
      Long   : Boolean) return String
   is
      pragma Unreferenced (Long);
   begin
      return Self.Prefix.all
        & To_String (Self.To_Field.Get.Element.all, Format, Long => True)
        & Self.Suffix.all;
   end To_String;

   -----------------
   -- Field_Types --
   -----------------

   package body Field_Types is

      type Ada_Type_Access is access Ada_Type;

      package Typed_Data_Fields is new Data_Fields (Field);
      type Typed_Named_Field_Internal is new Named_Field_Internal with record
         Data_Value : Ada_Type_Access;
      end record;

      overriding procedure Free (Self : in out Typed_Named_Field_Internal);
      overriding function To_String
        (Self   : Typed_Named_Field_Internal;
         Format : Formatter'Class;
         Long   : Boolean) return String;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Ada_Type, Ada_Type_Access);

      function Internal_From_Data
        (Data : SQL_Field_Internal'Class) return Field'Class;
      pragma Inline (Internal_From_Data);

      ----------
      -- Free --
      ----------

      overriding procedure Free (Self : in out Typed_Named_Field_Internal) is
      begin
         Unchecked_Free (Self.Data_Value);
         Free (Named_Field_Internal (Self));
      end Free;

      ---------------
      -- To_String --
      ---------------

      overriding function To_String
        (Self   : Typed_Named_Field_Internal;
         Format : Formatter'Class;
         Long   : Boolean) return String is
      begin
         if Self.Data_Value /= null then
            return To_SQL (Format, Self.Data_Value.all, Quote => True);
         end if;

         return To_String (Named_Field_Internal (Self), Format, Long);
      end To_String;

      ----------------
      -- From_Table --
      ----------------

      function From_Table
        (Self  : Field;
         Table : SQL_Single_Table'Class) return Field'Class
      is
         F : Typed_Data_Fields.Field
           (Table => null, Instance => Table.Instance,
            Instance_Index => Table.Instance_Index, Name => null);
         D : Named_Field_Internal (Typ => Field_Std);
      begin
         D.Table := (Name => null, Instance => Table.Instance,
                     Instance_Index => Table.Instance_Index);
         D.Str_Value := new String'(Self.Name.all);
         F.Data.Set (D);
         return Field (F);
      end From_Table;

      ------------------------
      -- Internal_From_Data --
      ------------------------

      function Internal_From_Data
        (Data : SQL_Field_Internal'Class) return Field'Class
      is
         F : Field_Pointers.Ref;
      begin
         F.Set (Data);

         return Typed_Data_Fields.Field'
           (Table => null, Instance => null, Name => null,
            Instance_Index => -1,
            Data => F);
      end Internal_From_Data;

      ----------------
      -- Expression --
      ----------------

      function Expression (Value : Ada_Type) return Field'Class is
         Data : Typed_Named_Field_Internal (Field_Std);
      begin
         Data.Data_Value := new Ada_Type'(Value);
         return Internal_From_Data (Data);
      end Expression;

      -----------------
      -- From_String --
      -----------------

      function From_String (SQL : String) return Field'Class is
         Data : Named_Field_Internal (Field_Std);
      begin
         Data.Str_Value := new String'(SQL);
         return Internal_From_Data (Data);
      end From_String;

      -----------
      -- Param --
      -----------

      function Param (Index : Positive) return Field'Class is
         Data : Named_Field_Internal (Field_Parameter);
      begin
         Data.Index := Index;
         Data.Param_Type := Param_Type;
         return Internal_From_Data (Data);
      end Param;

      ---------
      -- "&" --
      ---------

      function "&"
        (Field : SQL_Field'Class; Value : Ada_Type) return SQL_Field_List is
      begin
         return Field & Expression (Value);
      end "&";

      function "&"
        (Value : Ada_Type; Field : SQL_Field'Class) return SQL_Field_List is
      begin
         return Expression (Value) & Field;
      end "&";

      function "&"
        (List : SQL_Field_List; Value : Ada_Type) return SQL_Field_List is
      begin
         return List & Expression (Value);
      end "&";

      function "&"
        (Value : Ada_Type; List : SQL_Field_List) return SQL_Field_List is
      begin
         return Expression (Value) & List;
      end "&";

      --------------
      -- Operator --
      --------------

      function Operator (Field1, Field2 : Field'Class) return Field'Class is
         F : Typed_Data_Fields.Field
           (Table => null, Instance => null,
            Instance_Index => -1, Name => null);
         D : Named_Field_Internal (Typ => Field_Operator);
      begin
         D.Op_Value := new String'(Name);
         D.List := Field1 & Field2;
         F.Data.Set (D);
         return F;
      end Operator;

      ---------------------
      -- Scalar_Operator --
      ---------------------

      function Scalar_Operator
        (Self : Field'Class; Operand : Scalar) return Field'Class
      is
         F : Typed_Data_Fields.Field
           (Table => null, Instance => null, Name => null,
            Instance_Index => -1);
         D : Named_Field_Internal (Typ => Field_Operator);

         F2 : Typed_Data_Fields.Field
           (Table => null, Instance => null, Name => null,
            Instance_Index => -1);
         D2 : Named_Field_Internal (Typ => Field_Std);

      begin
         D2.Str_Value := new String'(Prefix & Scalar'Image (Operand) & Suffix);
         F2.Data.Set (D2);

         D.Op_Value := new String'(Name);
         D.List := Self & F2;
         F.Data.Set (D);
         return F;
      end Scalar_Operator;

      ------------------
      -- SQL_Function --
      ------------------

      function SQL_Function return Field'Class is
         F : Typed_Data_Fields.Field
           (Table => null, Instance => null, Name => null,
            Instance_Index => -1);
         D : Named_Field_Internal (Typ => Field_Std);
      begin
         D.Str_Value := new String'(Name);
         F.Data.Set (D);
         return F;
      end SQL_Function;

      --------------------
      -- Apply_Function --
      --------------------

      function Apply_Function
        (Self : Argument_Type'Class) return Field'Class
      is
         F : Typed_Data_Fields.Field
           (Table => null, Instance => null, Name => null,
            Instance_Index => -1);
         D : Function_Field;
      begin
         if Suffix /= ")" and then Suffix /= "" then
            D.Prefix := new String'(Name);
            D.To_Field := +Self;
            D.Suffix := new String'(" " & Suffix);
         else
            D.Prefix := new String'(Name);
            D.To_Field := +Self;
            D.Suffix := new String'(Suffix);
         end if;
         F.Data.Set (D);
         return F;
      end Apply_Function;

      ---------------
      -- Operators --
      ---------------

      function "=" (Left : Field; Right : Field'Class) return SQL_Criteria is
      begin
         return Compare (Left, Right, Comparison_Equal'Access);
      end "=";

      function "/=" (Left : Field; Right : Field'Class) return SQL_Criteria is
      begin
         return Compare (Left, Right, Comparison_Different'Access);
      end "/=";

      function "<" (Left : Field; Right : Field'Class) return SQL_Criteria is
      begin
         return Compare (Left, Right, Comparison_Less'Access);
      end "<";

      function "<=" (Left : Field; Right : Field'Class) return SQL_Criteria is
      begin
         return Compare (Left, Right, Comparison_Less_Equal'Access);
      end "<=";

      function ">" (Left : Field; Right : Field'Class) return SQL_Criteria is
      begin
         return Compare (Left, Right, Comparison_Greater'Access);
      end ">";

      function ">=" (Left : Field; Right : Field'Class) return SQL_Criteria is
      begin
         return Compare (Left, Right, Comparison_Greater_Equal'Access);
      end ">=";

      function "=" (Left : Field; Right : Ada_Type) return SQL_Criteria
      is
      begin
         return Compare (Left, Expression (Right), Comparison_Equal'Access);
      end "=";

      function "/=" (Left : Field; Right : Ada_Type) return SQL_Criteria
      is
      begin
         return Compare
           (Left, Expression (Right), Comparison_Different'Access);
      end "/=";

      function "<" (Left : Field; Right : Ada_Type) return SQL_Criteria is
      begin
         return Compare (Left, Expression (Right), Comparison_Less'Access);
      end "<";

      function "<=" (Left : Field; Right : Ada_Type) return SQL_Criteria
      is
      begin
         return Compare
           (Left, Expression (Right), Comparison_Less_Equal'Access);
      end "<=";

      function ">" (Left : Field; Right : Ada_Type) return SQL_Criteria is
      begin
         return Compare
           (Left, Expression (Right), Comparison_Greater'Access);
      end ">";

      function ">=" (Left : Field; Right : Ada_Type) return SQL_Criteria is
      begin
         return Compare
           (Left, Expression (Right), Comparison_Greater_Equal'Access);
      end ">=";

      function Greater_Than
        (Left : SQL_Field'Class; Right : Field) return SQL_Criteria
      is
      begin
         return Compare (Left, Right, Comparison_Greater'Access);
      end Greater_Than;

      function Greater_Or_Equal
        (Left : SQL_Field'Class; Right : Field) return SQL_Criteria
      is
      begin
         return Compare (Left, Right, Comparison_Greater_Equal'Access);
      end Greater_Or_Equal;

      function Equal
        (Left : SQL_Field'Class; Right : Field) return SQL_Criteria
      is
      begin
         return Compare (Left, Right, Comparison_Equal'Access);
      end Equal;

      function Less_Than
        (Left : SQL_Field'Class; Right : Field) return SQL_Criteria
      is
      begin
         return Compare (Left, Right, Comparison_Less'Access);
      end Less_Than;

      function Less_Or_Equal
        (Left : SQL_Field'Class; Right : Field) return SQL_Criteria
      is
      begin
         return Compare (Left, Right, Comparison_Less_Equal'Access);
      end Less_Or_Equal;

      function Greater_Than
        (Left : SQL_Field'Class; Right : Ada_Type) return SQL_Criteria is
      begin
         return Compare
           (Left, Expression (Right), Comparison_Greater'Access);
      end Greater_Than;

      function Greater_Or_Equal
        (Left : SQL_Field'Class; Right : Ada_Type) return SQL_Criteria is
      begin
         return Compare
           (Left, Expression (Right), Comparison_Greater_Equal'Access);
      end Greater_Or_Equal;

      function Equal
        (Left : SQL_Field'Class; Right : Ada_Type) return SQL_Criteria is
      begin
         return Compare (Left, Expression (Right), Comparison_Equal'Access);
      end Equal;

      function Less_Than
        (Left : SQL_Field'Class; Right : Ada_Type) return SQL_Criteria is
      begin
         return Compare
           (Left, Expression (Right), Comparison_Less'Access);
      end Less_Than;

      function Less_Or_Equal
        (Left : SQL_Field'Class; Right : Ada_Type) return SQL_Criteria is
      begin
         return Compare
           (Left, Expression (Right), Comparison_Less_Equal'Access);
      end Less_Or_Equal;

      function "=" (Self : Field; Value : Ada_Type) return SQL_Assignment is
         Result : SQL_Assignment;
         F   : Typed_Named_Field_Internal (Field_Std);
      begin
         F.Data_Value := new Ada_Type'(Value);
         Assign (Result, Self, F);
         return Result;
      end "=";

      ---------
      -- "=" --
      ---------

      function "=" (Self : Field; To : Field'Class) return SQL_Assignment is
         Result : SQL_Assignment;
      begin
         --  Special case when assigning to one of the Null_Field constants

         if To.Table = null
           and then To.Instance = null
           and then To.Name = Null_String'Access
         then
            Unassign (Result, Self);

         else
            Append (Result.List, Assignment_Item'(+Self, +To));
         end if;

         return Result;
      end "=";

   end Field_Types;

   -------------------
   -- Boolean_Image --
   -------------------

   function Boolean_Image (Self : Formatter; Value : Boolean) return String is
      pragma Unreferenced (Self);
   begin
      return Boolean'Image (Value);
   end Boolean_Image;

   --------------------
   -- Boolean_To_SQL --
   --------------------

   function Boolean_To_SQL
     (Self  : Formatter'Class;
      Value : Boolean;
      Quote : Boolean) return String
   is
      pragma Unreferenced (Quote);
   begin
      return Boolean_Image (Self, Value);
   end Boolean_To_SQL;

   ------------------
   -- Float_To_SQL --
   ------------------

   function Float_To_SQL
     (Self  : Formatter'Class;
      Value : Float;
      Quote : Boolean) return String
   is
      pragma Unreferenced (Self, Quote);
      Img : constant String := Float'Image (Value);
   begin
      if Img (Img'First) = ' ' then
         return Img (Img'First + 1 .. Img'Last);
      else
         return Img;
      end if;
   end Float_To_SQL;

   --------------------
   -- Integer_To_SQL --
   --------------------

   function Integer_To_SQL
     (Self  : Formatter'Class;
      Value : Integer;
      Quote : Boolean) return String
   is
      pragma Unreferenced (Self, Quote);
      Img : constant String := Integer'Image (Value);
   begin
      if Img (Img'First) = ' ' then
         return Img (Img'First + 1 .. Img'Last);
      else
         return Img;
      end if;
   end Integer_To_SQL;

   -------------------
   -- Bigint_To_SQL --
   -------------------

   function Bigint_To_SQL
     (Self  : Formatter'Class;
      Value : Long_Long_Integer;
      Quote : Boolean) return String
   is
      pragma Unreferenced (Self, Quote);
      Img : constant String := Long_Long_Integer'Image (Value);
   begin
      if Img (Img'First) = ' ' then
         return Img (Img'First + 1 .. Img'Last);
      else
         return Img;
      end if;
   end Bigint_To_SQL;

   -----------------------
   -- Supports_Timezone --
   -----------------------

   function Supports_Timezone (Self : Formatter) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Supports_Timezone;

   -----------------
   -- Time_To_SQL --
   -----------------

   function Time_To_SQL
     (Self  : Formatter'Class;
      Value : Ada.Calendar.Time;
      Quote : Boolean) return String
   is
      Adjusted : Time;
   begin
      --  Value is always considered as GMT, which is what we store in the
      --  database. Unfortunately, GNAT.Calendar.Time_IO converts that back to
      --  local time.

      if Value /= GNAT.Calendar.No_Time then
         Adjusted := Value - Duration (UTC_Time_Offset (Value)) * 60.0;

         if Supports_Timezone (Self) then
            if Quote then
               return Image (Adjusted, "'%Y-%m-%d %H:%M:%S +00:00'");
            else
               return Image (Adjusted, "%Y-%m-%d %H:%M:%S +00:00");
            end if;
         else
            if Quote then
               return Image (Adjusted, "'%Y-%m-%d %H:%M:%S'");
            else
               return Image (Adjusted, "%Y-%m-%d %H:%M:%S");
            end if;
         end if;
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
      Offset   : Duration;
   begin
      if Value /= GNAT.Calendar.No_Time then
         --  Input Value should be interpreted in GMT, but Image assumes this
         --  is local time zone. So we need an offset here.

         Offset := Duration (UTC_Time_Offset (Value)) * 60.0;
         if Quote then
            return Image (Value - Offset, "'%Y-%m-%d'");
         else
            return Image (Value - Offset, "%Y-%m-%d");
         end if;
      else
         return "NULL";
      end if;
   end Date_To_SQL;

   -------------------
   -- Decimal_To_SQL --
   -------------------

   function Money_To_SQL
     (Self  : Formatter'Class;
      Value : T_Money;
      Quote : Boolean) return String
   is
      pragma Unreferenced (Quote);
   begin
      return Self.Money_Image (Value);
   end Money_To_SQL;

   -----------------
   -- Money_Image --
   -----------------

   function Money_Image (Self  : Formatter; Value : T_Money) return String is
      pragma Unreferenced (Self);
      Img : constant String := T_Money'Image (Value);
   begin
      if Img (Img'First) = ' ' then
         return Img (Img'First + 1 .. Img'Last);
      else
         return Img;
      end if;
   end Money_Image;

   -----------------
   -- Json_To_SQL --
   -----------------

   function Json_To_SQL
     (Self : Formatter'Class; Value : String; Quote : Boolean) return String
   is
      pragma Unreferenced (Self, Quote);
   begin
      if Trim (Value, Ada.Strings.Both) = "" then
         return "null";
         --  Json null, not to be confused with SQL NULL.
      else
         return Value;
      end if;
   end Json_To_SQL;

   -----------------
   -- XML_To_SQL --
   -----------------

   function XML_To_SQL
     (Self : Formatter'Class; Value : String; Quote : Boolean) return String
   is
      pragma Unreferenced (Self, Quote);
   begin
      if Trim (Value, Ada.Strings.Both) = "" then
         return "<null/>";
         --  XML null, not to be confused with SQL NULL.
      else
         return Value;
      end if;
   end XML_To_SQL;

   -------------------
   -- String_To_SQL --
   -------------------

   function String_To_SQL
     (Self : Formatter'Class; Value : String; Quote : Boolean) return String
   is
   begin
      return String_Image (Self, Value, Quote);
   end String_To_SQL;

   ------------------
   -- String_Image --
   ------------------

   function String_Image
     (Self  : Formatter;
      Value : String;
      Quote : Boolean) return String
   is
      --  This function used to quote the backslashes as well. However, this
      --  is incorrect, since the SQL backends will already take each
      --  character on its own. For instance, to insert a newline with psql
      --  we need to
      --          SELECT 'a\n' || chr(13) || 'e';
      --  This outputs a string with 5 characters.
      --  Same with sqlite.

      pragma Unreferenced (Self);
      Num_Of_Apostrophes : constant Natural :=
        Ada.Strings.Fixed.Count (Value, "'");
   begin
      if not Quote then
         return Value;
      end if;

      if Num_Of_Apostrophes = 0 then
         return "'" & Value & "'";
      end if;

      declare
         New_Str            : String
           (Value'First ..  Value'Last + Num_Of_Apostrophes + 2);
         Index : Natural := New_Str'First + 1;
      begin
         New_Str (New_Str'First) := ''';
         New_Str (New_Str'Last) := ''';

         for I in Value'Range loop
            if Value (I) = ''' then
               New_Str (Index .. Index + 1) := "''";
               Index := Index + 1;
            else
               New_Str (Index) := Value (I);
            end if;
            Index := Index + 1;
         end loop;

         return New_Str;
      end;
   end String_Image;

   ----------------------
   -- Parameter_String --
   ----------------------

   function Parameter_String
     (Self  : Formatter;
      Index : Positive;
      Typ   : Parameter_Type) return String
   is
      pragma Unreferenced (Self, Index, Typ);
   begin
      return "?";
   end Parameter_String;

end GNATCOLL.SQL_Impl;
