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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Containers;             use Ada.Containers;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Hash;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

package body GNATCOLL.SQL_Impl is
   use Field_List, Table_Sets, Assignment_Lists, SQL_Criteria_Pointers,
       Field_Pointers;

   package Field_Mapping_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Field_Mapping'Class);
   All_Field_Mappings : Field_Mapping_Vectors.Vector;
   --  When you create new field types, they should be registered in this list.
   --  Put an uninitialized instance of the field type in the list. A copy of
   --  it will be used to call Type_From_SQL when parsing the database schema.

   procedure Append_To_String
      (Self       : Table_List.Vector;
       Format     : Formatter'Class;
       Separator  : String;
       Result     : in out GNATCOLL.Strings.XString;
       Show_Types : Boolean);
   --  Helper to dump a list of tables to a string

   --------------------------
   --  Named field data --
   --------------------------
   --  Instantiation of field_data for specific types of fields, created for
   --  instance via Expression, From_String, or operators on time. Such fields
   --  are still typed

   type Field_Kind is (Field_Std, Field_Operator, Field_Parameter);
   --  The type of the field:
   --     Field_Std: Str_Value is directly the text of the field
   --     Field_Operator:  Str_Value is the operator, that acts on several
   --          fields ("-" for instance")
   --     Field_Parameter: the field's exact value is unknown, and will be
   --          substituted at runtime (for instance "?1" in sqlite3). Str_Value
   --          is then the index of the field.

   type Named_Field_Internal (Typ : Field_Kind)
     is new SQL_Field_Internal with record
      Table     : Table_Names := No_Names;

      case Typ is
         when Field_Std =>
            Str_Value : XString;
            --  The expression representing the field in SQL. See Field_Type
            --  for more information

         when Field_Operator =>
            Op_Value : XString;
            List     : SQL_Field_List;

         when Field_Parameter =>
            Index      : Positive;
      end case;
   end record;
   overriding procedure Append_To_String
     (Self       : Named_Field_Internal;
      Format     : Formatter'Class;
      Result     : in out XString;
      Long       : Boolean;
      Show_Types : Boolean);
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
   --  A helper to write functions. The output looks like:
   --      Prefix Fields(1) Seps (2) Fields(2) .. Seps (n-1) Fields(n) Suffix

   type Function_Field (Count : Positive) is new SQL_Field_Internal with record
      Prefix, Suffix : GNATCOLL.Strings.XString;
      Fields : SQL_Field_Array (1 .. Count);
      Seps   : XString_Array (2 .. Count);
   end record;
   overriding procedure Append_To_String
     (Self      : Function_Field;
      Format    : Formatter'Class;
      Result    : in out XString;
      Long      : Boolean;
      Show_Types : Boolean);
   overriding procedure Append_Tables
     (Self : Function_Field; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : access Function_Field;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

   --------------
   -- Criteria --
   --------------

   type Comparison_Criteria is new SQL_Criteria_Data with record
      Op, Suffix : Cst_String_Access;
      Args       : SQL_Field_Array (1 .. 2);
      --  Do not use a SQL_Field_List, since we are already in a refcounted
      --  type.
   end record;
   overriding procedure Append_To_String
     (Self   : Comparison_Criteria;
      Format : Formatter'Class;
      Long   : Boolean := True;
      Result : in out XString);
   overriding procedure Append_Tables
     (Self   : Comparison_Criteria; To : in out Table_Sets.Set);
   overriding procedure Append_If_Not_Aggregate
     (Self         : Comparison_Criteria;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean);

   -----------------------------
   -- Row_Comparison_Criteria --
   -----------------------------

   type Row_Comparison_Criteria is new SQL_Criteria_Data with record
      Op   : Cst_String_Access;
      Rows : Table_List.Vector;
      --  Do not use a SQL_Table_List since we are already in a refcounted
      --  type.
   end record;
   overriding procedure Append_To_String
     (Self   : Row_Comparison_Criteria;
      Format : Formatter'Class;
      Long   : Boolean := True;
      Result : in out XString);
   overriding procedure Append_Tables
     (Self   : Row_Comparison_Criteria; To : in out Table_Sets.Set);

   ----------------
   -- Data_Field --
   ----------------

   package body Data_Fields is

      ----------------------
      -- Append_To_String --
      ----------------------

      overriding procedure Append_To_String
        (Self       : Field;
         Format     : Formatter'Class;
         Result     : in out XString;
         Long       : Boolean;
         Show_Types : Boolean) is
      begin
         if not Self.Data.Is_Null then
            Append_To_String
               (Self.Data.Get.Element.all, Format, Result,
                Long => Long, Show_Types => Show_Types);
         end if;
      end Append_To_String;

      -------------------
      -- Append_Tables --
      -------------------

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
         FC : access SQL_Field_Internal'Class;
      begin
         if not Self.Data.Is_Null then
            --  !!! Could not use Element call result in the
            --  Append_If_Not_Aggregate parameter because of GNAT bug OB03-009

            FC := Self.Data.Get.Element;
            Append_If_Not_Aggregate (FC, To, Is_Aggregate);
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

   ----------------------
   -- Append_To_String --
   ----------------------

   procedure Append_To_String
     (Self       : SQL_Field_List;
      Format     : Formatter'Class;
      Separator  : String;
      Result     : in out XString;
      Long       : Boolean;
      Show_Types : Boolean)
   is
      Is_First : Boolean := True;
   begin
      if not Self.List.Is_Null then
         for C of Self.List.Get loop
            if Is_First then
               Is_First := False;
            else
               Result.Append (Separator);
            end if;

            Append_To_String
               (C, Format, Result, Long => Long, Show_Types => Show_Types);
         end loop;
      end if;
   end Append_To_String;

   overriding procedure Append_To_String
     (Self       : SQL_Field_List;
      Format     : Formatter'Class;
      Result     : in out XString;
      Long       : Boolean;
      Show_Types : Boolean) is
   begin
      Append_To_String
         (Self, Format, ", ", Result, Long => Long, Show_Types => Show_Types);
   end Append_To_String;

   ----------------------
   -- Append_To_String --
   ----------------------

   overriding procedure Append_To_String
     (Self       : SQL_Field;
      Format     : Formatter'Class;
      Result     : in out XString;
      Long       : Boolean;
      Show_Types : Boolean)
   is
      pragma Unreferenced (Format, Show_Types);
   begin
      if Long then
         declare
            N : constant String := Instance_Name
              ((Name           => Self.Table,
                Instance       => Self.Instance,
                Instance_Index => Self.Instance_Index));
         begin
            --  Self.Table could be null in the case of the Null_Field_*
            --  constants
            if N /= "" then
               Result.Append (N);
               Result.Append ('.');
            end if;
         end;
      end if;

      Result.Append (Self.Name.all);
   end Append_To_String;

   ----------------------
   -- Append_To_String --
   ----------------------

   procedure Append_To_String
     (Self       : Named_Field_Internal;
      Format     : Formatter'Class;
      Result     : in out XString;
      Long       : Boolean;
      Show_Types : Boolean)
   is
      Is_First : Boolean := True;
   begin
      case Self.Typ is
         when Field_Std =>
            if Self.Table /= No_Names
               and then Long
            then
               if Self.Table.Instance = null then
                  Result.Append (Self.Table.Name.all);
               else
                  Result.Append (Self.Table.Instance.all);
               end if;
               Result.Append ('.');
            end if;
            Result.Append (Self.Str_Value);

         when Field_Operator =>
            if not Self.List.List.Is_Null then
               for C of Self.List.List.Get loop
                  if Is_First then
                     Is_First := False;
                  else
                     Result.Append (' ');
                     Result.Append (Self.Op_Value);
                     Result.Append (' ');
                  end if;

                  Append_To_String
                     (C, Format, Result, Long => True,
                      Show_Types => Show_Types);
               end loop;
            end if;

         when Field_Parameter =>
            raise Program_Error with "Should have been overridden";
      end case;
   end Append_To_String;

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
      for F of Self.Fields loop
         Append_Tables (F.Get.Element.all, To);
      end loop;
   end Append_Tables;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   overriding procedure Append_If_Not_Aggregate
     (Self         : access Function_Field;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean) is
   begin
      for F of Self.Fields loop
         Append_If_Not_Aggregate (F.Get.Element.all, To, Is_Aggregate);
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
      F : Field_Pointers.Ref;
   begin
      if Self.Typ = Field_Operator
         and then not Self.List.List.Is_Null
      then
         for C of Self.List.List.Get loop
            Append_If_Not_Aggregate (C, To, Is_Aggregate);
         end loop;
      end if;

      --  We create a SQL_Field_Text, but it might be any other type.
      --  This isn't really relevant, however, since the exact type is not used
      --  later on.

      if Self.Table /= No_Names then
         F.From_Element (Field_Pointers.Element_Access (Self));

         Append
           (To, Any_Fields.Field'
              (Table    => Self.Table.Name,
               Instance => Self.Table.Instance,
               Instance_Index => Self.Table.Instance_Index,
               Name     => null,
               Data     => F));
      end if;
   end Append_If_Not_Aggregate;

   ------------
   -- Append --
   ------------

   procedure Append
     (List : in out SQL_Field_List; Field : SQL_Field'Class) is
   begin
      if List.List.Is_Null then
         List.List.Set (Field_List.Empty_Vector);
         List.List.Get.Reserve_Capacity (20);
      end if;

      List.List.Get.Append (Field);
   end Append;

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : SQL_Field'Class) return SQL_Field_List is
   begin
      return Result : SQL_Field_List do
         Result.List.Set (Field_List.Empty_Vector);
         declare
            G : constant Field_List_Pointers.Reference_Type :=
               Result.List.Get;
         begin
            G.Reserve_Capacity (20);
            G.Append (Left);
            G.Append (Right);
         end;
      end return;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left : SQL_Field_List; Right : SQL_Field'Class) return SQL_Field_List is
   begin
      if Left.List.Is_Null then
         return +Right;
      else
         return Result : SQL_Field_List do
            Result.List.Set (Left.List.Get); --  Copy the list to preserve Left
            declare
               G : constant Field_List_Pointers.Reference_Type :=
                  Result.List.Get;
            begin
               G.Reserve_Capacity (20);
               G.Append (Right);
            end;
         end return;
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left : SQL_Field'Class; Right : SQL_Field_List) return SQL_Field_List
   is
      Result : SQL_Field_List;
   begin
      Result.List.Set (Field_List.Empty_Vector);
      declare
         G : constant Field_List_Pointers.Reference_Type := Result.List.Get;
      begin
         G.Reserve_Capacity (20);
         G.Append (Left);
         if not Right.List.Is_Null then
            for C of Right.List.Get loop
               G.Append (C);
            end loop;
         end if;
      end;
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left, Right : SQL_Field_List) return SQL_Field_List
   is
   begin
      if Left.List.Is_Null then
         return Right;
      elsif Right.List.Is_Null then
         return Left;
      end if;

      declare
         GL : constant Field_List_Pointers.Reference_Type := Left.List.Get;
         GR : constant Field_List_Pointers.Reference_Type := Right.List.Get;
      begin
         return Result : SQL_Field_List do
            Result.List.Set (GL);  --  Copy, to preserve Left
            declare
               RR : constant Field_List_Pointers.Reference_Type :=
                  Result.List.Get;
            begin
               RR.Reserve_Capacity (20);
               for C of GR loop
                  RR.Append (C);
               end loop;
            end;
         end return;
      end;
   end "&";

   ---------
   -- "+" --
   ---------

   function "+" (Left : SQL_Field'Class) return SQL_Field_List is
      Result : SQL_Field_List;
   begin
      Result.List.Set (Field_List.Empty_Vector);
      Result.List.Get.Reserve_Capacity (20);
      Result.List.Get.Append (Left);
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
         --  Check that field already exists in list
         --  ???? We should be using a set here
         if not To.List.Is_Null then
            for F of To.List.Get loop
               if SQL_Field (F) = Self then
                  --  Do not need the same field twice
                  return;
               end if;
            end loop;
         end if;

         Append (To, Self);
      end if;
   end Append_If_Not_Aggregate;

   ----------------------
   -- Append_To_String --
   ----------------------

   procedure Append_To_String
     (Self   : SQL_Criteria;
      Format : Formatter'Class;
      Long   : Boolean := True;
      Result : in out XString) is
   begin
      if not Self.Criteria.Is_Null then
         Append_To_String
            (Self.Criteria.Get.Element.all, Format, Long, Result);
      end if;
   end Append_To_String;

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

   ----------------------
   -- Append_To_String --
   ----------------------

   overriding procedure Append_To_String
     (Self   : Comparison_Criteria;
      Format : Formatter'Class;
      Long   : Boolean := True;
      Result : in out XString)
   is
      Arg2 : XString;
   begin
      if not Self.Args (2).Is_Null then
         Append_To_String
            (Self.Args (2), Format, Arg2, Long => Long, Show_Types => False);
      end if;

      if Self.Op.all = "=" and then Arg2 = "TRUE" then
         Append_To_String
            (Self.Args (1), Format, Result, Long => Long, Show_Types => False);
      elsif Self.Op.all = "=" and then Arg2 = "FALSE" then
         Result.Append ("not ");
         Append_To_String
            (Self.Args (1), Format, Result, Long => Long, Show_Types => False);
      else
         if not Self.Args (1).Is_Null then
            Append_To_String
               (Self.Args (1), Format, Result,
                Long => Long, Show_Types => False);
         end if;
         Result.Append (Self.Op.all);
         Result.Append (Arg2);
      end if;

      if Self.Suffix /= null then
         Result.Append (Self.Suffix.all);
      end if;
   end Append_To_String;

   -------------------
   -- Append_Tables --
   -------------------

   overriding procedure Append_Tables
     (Self : Comparison_Criteria; To : in out Table_Sets.Set) is
   begin
      for F of Self.Args loop
         Append_Tables (F, To);
      end loop;
   end Append_Tables;

   -----------------------------
   -- Append_If_Not_Aggregate --
   -----------------------------

   overriding procedure Append_If_Not_Aggregate
     (Self         : Comparison_Criteria;
      To           : in out SQL_Field_List'Class;
      Is_Aggregate : in out Boolean) is
   begin
      for F of Self.Args loop
         Append_If_Not_Aggregate (F, To, Is_Aggregate);
      end loop;
   end Append_If_Not_Aggregate;

   -----------------
   -- Row_Compare --
   -----------------

   function Row_Compare
     (Row1, Row2  : SQL_Single_Table'Class;
      Op          : not null Cst_String_Access) return SQL_Criteria
   is
      Data : constant Row_Comparison_Criteria :=
         (SQL_Criteria_Data with Op => Op, Rows => Table_List.Empty_Vector);
      Result : SQL_Criteria;
   begin
      Result.Criteria.Set (Data);

      --  Only append after the initial copy in Set_Data, to avoid
      --  an extra copy.
      Row_Comparison_Criteria (Result.Criteria.Get.Element.all)
         .Rows.Append (Row1);
      Row_Comparison_Criteria (Result.Criteria.Get.Element.all)
         .Rows.Append (Row2);
      return Result;
   end Row_Compare;

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
          Op => Op, Suffix => Suffix,
          Args => (1 => +Left, 2 => +Right));
      Result : SQL_Criteria;
   begin
      Result.Criteria.Set (Data);
      return Result;
   end Compare;

   --------------
   -- Compare1 --
   --------------

   function Compare1
     (Field       : SQL_Field'Class;
      Op          : Cst_String_Access;
      Suffix      : Cst_String_Access := null) return SQL_Criteria
   is
      Data : constant Comparison_Criteria :=
         (SQL_Criteria_Data with
          Op     => Op,
          Suffix => Suffix,
          Args   => (1 => No_Field_Pointer, 2 => +Field));
      Result : SQL_Criteria;
   begin
      Result.Criteria.Set (Data);
      return Result;
   end Compare1;

   ----------------------
   -- Append_To_String --
   ----------------------

   procedure Append_To_String
     (Self       : SQL_Field_Pointer;
      Format     : Formatter'Class;
      Result     : in out XString;
      Long       : Boolean;
      Show_Types : Boolean) is
   begin
      if not Self.Is_Null then
         Append_To_String
            (Self.Get.Element.all, Format, Result,
             Long => Long, Show_Types => Show_Types);
      end if;
   end Append_To_String;

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
      if List.List.Is_Null then
         return Field_List.No_Element;
      else
         return List.List.Get.First;
      end if;
   end First;

   ------------
   -- Append --
   ------------

   procedure Append
     (List : in out SQL_Field_List'Class; Field : SQL_Field_Pointer) is
   begin
      if not Field.Is_Null then
         if List.List.Is_Null then
            List.List.Set (Field_List.Empty_Vector);
         end if;
         List.List.Get.Append (Field.Get.Element.all);
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

   ----------------------
   -- Append_To_String --
   ----------------------

   procedure Append_To_String
     (Self       : SQL_Assignment;
      Format     : Formatter'Class;
      With_Field : Boolean;
      Result     : in out XString)
   is
      Is_First : Boolean := True;
   begin
      for Data of Self.List loop
         if Is_First then
            Is_First := False;
         else
            Result.Append (", ");
         end if;

         if With_Field then
            Append_To_String
               (Data.Field, Format, Result,
                Long => False, Show_Types => False);
            Result.Append ('=');
         end if;

         if Data.To_Field /= No_Field_Pointer then
            Append_To_String
               (Data.To_Field, Format, Result,
                Long => True, Show_Types => False);
         else
            Result.Append ("NULL");
         end if;
      end loop;
   end Append_To_String;

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
               To_XString (Null_String);

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

   ----------------------
   -- Append_To_String --
   ----------------------

   overriding procedure Append_To_String
     (Self       : Function_Field;
      Format     : Formatter'Class;
      Result     : in out XString;
      Long       : Boolean;
      Show_Types : Boolean)
   is
      pragma Unreferenced (Long);
   begin
      Result.Append (Self.Prefix);
      Append_To_String
         (Self.Fields (1).Get.Element.all, Format, Result,
          Long => True, Show_Types => Show_Types);

      for F in 2 .. Self.Count loop
         Result.Append (Self.Seps (F));
         Append_To_String
            (Self.Fields (F).Get.Element.all, Format, Result,
             Long => True, Show_Types => Show_Types);
      end loop;

      if Self.Suffix /= ")" and then not Self.Suffix.Is_Empty then
         Result.Append (' ');
      end if;

      Result.Append (Self.Suffix);
   end Append_To_String;

   -----------------
   -- Field_Types --
   -----------------

   package body Field_Types is

      package Typed_Data_Fields is new Data_Fields (Field);

      type Typed_Named_Field_Internal is new Named_Field_Internal with record
         Data_Value : Stored_Ada_Type;
      end record;
      overriding procedure Free (Self : in out Typed_Named_Field_Internal);
      overriding procedure Append_To_String
        (Self       : Typed_Named_Field_Internal;
         Format     : Formatter'Class;
         Result     : in out XString;
         Long       : Boolean;
         Show_Types : Boolean);

      function Internal_From_Data
        (Data : SQL_Field_Internal'Class) return Field'Class;
      pragma Inline (Internal_From_Data);

      type Parameter_Field_Internal is
         new Named_Field_Internal (Typ => Field_Parameter)
         with null record;
      overriding procedure Append_To_String
        (Self       : Parameter_Field_Internal;
         Format     : Formatter'Class;
         Result     : in out XString;
         Long       : Boolean;
         Show_Types : Boolean);

      ----------------------
      -- Append_To_String --
      ----------------------

      overriding procedure Append_To_String
        (Self       : Parameter_Field_Internal;
         Format     : Formatter'Class;
         Result     : in out XString;
         Long       : Boolean;
         Show_Types : Boolean)
      is
         pragma Unreferenced (Long, Show_Types);
      begin
         Result.Append
            (Format.Parameter_String
               (Self.Index, SQL_Type (Default_Constraints)));
      end Append_To_String;

      ----------
      -- Free --
      ----------

      overriding procedure Free (Self : in out Typed_Named_Field_Internal) is
      begin
         Free (Self.Data_Value);
         Free (Named_Field_Internal (Self));
      end Free;

      ----------------------
      -- Append_To_String --
      ----------------------

      overriding procedure Append_To_String
        (Self       : Typed_Named_Field_Internal;
         Format     : Formatter'Class;
         Result     : in out XString;
         Long       : Boolean;
         Show_Types : Boolean)
      is
         pragma Unreferenced (Long);

         --  Do not check that Data_Value is valid, it could also
         --  be Nan or Inf when using float
         pragma Validity_Checks (Off);
      begin
         if Show_Types then
            Format.Append_To_String_And_Cast
               (Field    =>
                   To_SQL (Format, Self.Data_Value, Quote => True),
                Result   => Result,
                SQL_Type => SQL_Type (Default_Constraints));
         else
            Result.Append
               (To_SQL (Format, Self.Data_Value, Quote => True));
         end if;
      end Append_To_String;

      ----------------
      -- From_Table --
      ----------------

      function From_Table
        (Self  : Field;
         Table : SQL_Single_Table'Class) return Field'Class
      is
         F : Typed_Data_Fields.Field
           (Table => null, Instance => Table.Instance,
            Instance_Index => Table.Instance_Index, Name => Self.Name);
         D : Named_Field_Internal (Typ => Field_Std);
      begin
         D.Table := (Name => null, Instance => Table.Instance,
                     Instance_Index => Table.Instance_Index);
         D.Str_Value := To_XString (Self.Name.all);
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
            Instance_Index => -1, Constraints => Default_Constraints,
            Data => F);
      end Internal_From_Data;

      ----------------
      -- Expression --
      ----------------

      function Expression (Value : Ada_Type) return Field'Class is
         Data : Typed_Named_Field_Internal (Field_Std);
      begin
         Data.Data_Value  := Ada_To_Stored (Value);
         return Internal_From_Data (Data);
      end Expression;

      ----------------------------
      -- Expression_From_Stored --
      ----------------------------

      function Expression_From_Stored
         (Value : Stored_Ada_Type) return Field'Class
      is
         Data : Typed_Named_Field_Internal (Field_Std);
      begin
         Data.Data_Value := Value;
         return Internal_From_Data (Data);
      end Expression_From_Stored;

      -----------------
      -- From_String --
      -----------------

      function From_String (SQL : String) return Field'Class is
         Data : Named_Field_Internal (Field_Std);
      begin
         Data.Str_Value := To_XString (SQL);
         return Internal_From_Data (Data);
      end From_String;

      -----------
      -- Param --
      -----------

      function Param (Index : Positive) return Field'Class is
         Data : Parameter_Field_Internal;
      begin
         Data.Index := Index;
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
         D.Op_Value := To_XString (Name);
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
         D2.Str_Value := To_XString
            (Prefix & Scalar'Image (Operand) & Suffix);
         F2.Data.Set (D2);

         D.Op_Value := To_XString (Name);
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
         D.Str_Value := To_XString (Name);
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
         D : constant Function_Field :=
            (Count    => 1,
             Prefix   => To_XString (Name),
             Suffix   => To_XString (Suffix),
             Fields   => (1 => +Self),
             Seps     => (1 .. 0 => Null_XString));
      begin
         F.Data.Set (D);
         return F;
      end Apply_Function;

      ---------------------
      -- Apply_Function2 --
      ---------------------

      function Apply_Function2
        (Arg1 : Argument1_Type'Class;
         Arg2 : Argument2_Type'Class) return Field'Class
      is
         F : Typed_Data_Fields.Field
           (Table => null, Instance => null, Name => null,
            Instance_Index => -1);
         D : constant Function_Field :=
            (Count   => 2,
             Prefix  => To_XString (Name),
             Suffix  =>
                (if Suffix /= ")" and then Suffix /= ""
                 then To_XString (' ' & Suffix)
                 else To_XString (Suffix)),
             Fields  => (+Arg1, +Arg2),
             Seps    => (1 => To_XString (Sep)));
      begin
         F.Data.Set (D);
         return F;
      end Apply_Function2;

      ----------
      -- Cast --
      ----------

      function Cast (Self : SQL_Field'Class) return Field'Class is
         function Internal_Cast is new Apply_Function
            (SQL_Field,
             "CAST (",
             "AS " & SQL_Type (Default_Constraints) & ")");
      begin
         return Internal_Cast (Self);
      end Cast;

      ---------------
      -- Operators --
      ---------------

      function "=" (Self : Field; Value : Ada_Type) return SQL_Assignment is
         --  Do not check that Value is valid. It could also be Nan or Inf
         --  when using float
         pragma Validity_Checks (Off);
         Result : SQL_Assignment;
         F   : Typed_Named_Field_Internal (Field_Std);
      begin
         F.Data_Value := Ada_To_Stored (Value);
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

      -------------------
      -- SQL_Type_Name --
      -------------------

      overriding function SQL_Type_Name
         (Self   : Mapping;
          Format : not null access Formatter'Class) return String
      is
         pragma Unreferenced (Format);
      begin
         return SQL_Type (Self.Constraints);
      end SQL_Type_Name;

      --------------
      -- As_Param --
      --------------

      function As_Param (Value : Ada_Type) return SQL_Parameter_Ptr is
         R : SQL_Parameter_Ptr;
         P : constant Parameter := (Val => Ada_To_Stored (Value));
      begin
         R.Set (P);
         return R;
      end As_Param;

      ----------
      -- Free --
      ----------

      procedure Free (Self : in out Parameter) is
      begin
         Free (Self.Val);
      end Free;

   begin
      Register_Field_Mapping (Mapping'(Field_Mapping with others => <>));
   end Field_Types;

   -------------------
   -- Free_Dispatch --
   -------------------

   procedure Free_Dispatch (Self : in out SQL_Parameter_Type'Class) is
   begin
      Self.Free;
   end Free_Dispatch;

   -------------------
   -- Boolean_Image --
   -------------------

   function Boolean_Image (Self : Formatter; Value : Boolean) return String is
      pragma Unreferenced (Self);
   begin
      return Boolean'Image (Value);
   end Boolean_Image;

   ----------------------
   -- Any_Float_To_SQL --
   ----------------------

   function Any_Float_To_SQL
     (Self  : Formatter'Class;
      Value : Base_Type;
      Quote : Boolean) return String
   is
      pragma Unreferenced (Self, Quote);
   begin
      --  Nan ?
      if Value /= Value then
         return "'Nan'";

      --  -Inf ?
      elsif Value < Base_Type'First then
         return "'-Infinity'";
      elsif Value > Base_Type'Last then
         return "'Infinity'";
      end if;

      declare
         Img : constant String := Base_Type'Image (Value);
      begin
         if Img (Img'First) = ' ' then
            return Img (Img'First + 1 .. Img'Last);
         else
            return Img;
         end if;
      end;
   end Any_Float_To_SQL;

   -----------------------
   -- Supports_Timezone --
   -----------------------

   function Supports_Timezone (Self : Formatter) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Supports_Timezone;

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

   ------------
   -- Create --
   ------------

   function Create (F1, F2 : SQL_Field'Class) return SQL_Assignment is
      R : SQL_Assignment;
      It : Assignment_Item;
   begin
      It.Field.Set (F1);
      It.To_Field.Set (F2);
      R.List.Append (It);
      return R;
   end Create;

   ----------------------------
   -- Register_Field_Mapping --
   ----------------------------

   procedure Register_Field_Mapping (Self : Field_Mapping'Class) is
   begin
      All_Field_Mappings.Append (Self);
   end Register_Field_Mapping;

   -------------------------
   -- Mapping_From_Schema --
   -------------------------

   function Mapping_From_Schema
      (Schema : String) return Field_Mapping_Access
   is
      T : constant String := To_Lower (Schema);
   begin
      --  Go into reverse order, so that custom fields take precedence
      --  over the predefined fields
      for F of reverse All_Field_Mappings loop
         if F.Maps_Schema_Type (T) then
            return new Field_Mapping'Class'(F);
         end if;
      end loop;
      return null;
   end Mapping_From_Schema;

   ---------------------
   -- Any_Float_Value --
   ---------------------

   function Any_Float_Value
      (Format : Formatter'Class; S : String) return Base_Type
   is
      pragma Unreferenced (Format);
      pragma Warnings (Off, "*is not modified, could be declared constant");
      Zero : Base_Type := 0.0;
      pragma Warnings (On, "*is not modified, could be declared constant");
   begin
      if S = "NaN" then
         return 0.0 / Zero;
      elsif S = "-Infinity" then
         return -1.0 / Zero;
      elsif S = "Infinity" then
         return 1.0 / Zero;
      else
         return Base_Type'Value (S);
      end if;
   end Any_Float_Value;

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : SQL_Table_List) return SQL_Table_List is
   begin
      if Left.Data.Is_Null then
         return Right;
      elsif Right.Data.Is_Null then
         return Left;
      else
         return Result : SQL_Table_List do
            Result.Data.Set (Table_List.Empty_Vector);
            declare
               G : constant Table_List_Pointers.Reference_Type :=
                  Result.Data.Get;
            begin
               G.Reserve_Capacity (20);
               G.Append (Left.Data.Get);
               G.Append (Right.Data.Get);
            end;
         end return;
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left  : SQL_Table_List;
      Right : SQL_Single_Table'Class) return SQL_Table_List is
   begin
      if Left.Data.Is_Null then
         return +Right;
      else
         return Result : SQL_Table_List do
            Result.Data.Set (Left.Data.Get);  --  Copy Left
            Result.Data.Get.Reserve_Capacity (20);
            Result.Data.Get.Append (Right);
         end return;
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : SQL_Single_Table'Class) return SQL_Table_List is
   begin
      return Result : SQL_Table_List do
         Result.Data.Set (Table_List.Empty_Vector);
         declare
            G : constant Table_List_Pointers.Reference_Type :=
               Result.Data.Get;
         begin
            G.Reserve_Capacity (20);
            G.Append (Left);
            G.Append (Right);
         end;
      end return;
   end "&";

   ---------
   -- "+" --
   ---------

   function "+" (Left : SQL_Single_Table'Class) return SQL_Table_List is
   begin
      return Result : SQL_Table_List do
         Result.Data.Set (Table_List.Empty_Vector);
         Result.Data.Get.Reserve_Capacity (20);
         Result.Data.Get.Append (Left);
      end return;
   end "+";

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : SQL_Table_List) return Boolean is
   begin
      return Self.Data.Is_Null or else Self.Data.Get.Is_Empty;
   end Is_Empty;

   ----------------------
   -- Append_To_String --
   ----------------------

   procedure Append_To_String
      (Self       : Table_List.Vector;
       Format     : Formatter'Class;
       Separator  : String;
       Result     : in out GNATCOLL.Strings.XString;
       Show_Types : Boolean)
   is
      Is_First : Boolean := True;
   begin
      for C of Self loop
         if Is_First then
            Is_First := False;
         else
            Result.Append (Separator);
         end if;
         Append_To_String (C, Format, Result, Show_Types => Show_Types);
      end loop;
   end Append_To_String;

   ----------------------
   -- Append_To_String --
   ----------------------

   procedure Append_To_String
      (Self       : SQL_Table_List;
       Format     : Formatter'Class;
       Separator  : String;
       Result     : in out GNATCOLL.Strings.XString;
       Show_Types : Boolean) is
   begin
      if not Self.Data.Is_Null then
         Append_To_String
            (Self.Data.Get, Format, Separator, Result,
             Show_Types => Show_Types);
      end if;
   end Append_To_String;

   ----------------------
   -- Append_To_String --
   ----------------------

   procedure Append_To_String
      (Self       : SQL_Table_List;
       Format     : Formatter'Class;
       Result     : in out GNATCOLL.Strings.XString;
       Show_Types : Boolean) is
   begin
      Append_To_String (Self, Format, ", ", Result, Show_Types => Show_Types);
   end Append_To_String;

   -------------------
   -- Append_Tables --
   -------------------

   procedure Append_Tables
     (Self : SQL_Table_List; To : in out Table_Sets.Set) is
   begin
      if not Self.Data.Is_Null then
         for C of Self.Data.Get loop
            Append_Tables (C, To);
         end loop;
      end if;
   end Append_Tables;

   ----------------------
   -- Append_To_String --
   ----------------------

   overriding procedure Append_To_String
     (Self   : Row_Comparison_Criteria;
      Format : Formatter'Class;
      Long   : Boolean := True;
      Result : in out XString)
   is
      pragma Unreferenced (Long);
   begin
      Append_To_String
         (Self.Rows, Format, Separator => Self.Op.all, Result => Result,
          Show_Types => False);
   end Append_To_String;

   -------------------
   -- Append_Tables --
   -------------------

   overriding procedure Append_Tables
     (Self   : Row_Comparison_Criteria; To : in out Table_Sets.Set) is
   begin
      for T of Self.Rows loop
         Append_Tables (T, To);
      end loop;
   end Append_Tables;

end GNATCOLL.SQL_Impl;
