------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

package body GNATCOLL.SQL.Ranges is

   package body Impl is
      use Base_Fields;

      ------------------
      -- Create_Range --
      ------------------

      function Create_Range
         (Min, Max     : Base_Fields.Field'Class;
          Min_Included : Boolean := True;
          Max_Included : Boolean := True) return SQL_Ada_Range
      is
         Min_Null : constant Boolean :=
            Base_Fields.Field (Min) = Base_Fields.Null_Field;
         Mi : constant SQL_Field_Pointer :=
            (if Min_Null then No_Field_Pointer else +Min);
         Min_I : constant Boolean := Min_Null or else Min_Included;

         Max_Null : constant Boolean :=
            Base_Fields.Field (Max) = Base_Fields.Null_Field;
         Ma : constant SQL_Field_Pointer :=
            (if Max_Null then No_Field_Pointer else +Max);
         Max_I : constant Boolean := Max_Null or else Max_Included;

      begin
         return SQL_Ada_Range'
            (Min           => Mi,
             Max           => Ma,
             Min_Included  => Min_I,
             Max_Included  => Max_I);
      end Create_Range;

      --------------------------------
      -- Create_Min_Unbounded_Range --
      --------------------------------

      function Create_Min_Unbounded_Range
         (Max          : Base_Fields.Field'Class;
          Max_Included : Boolean := True) return SQL_Ada_Range is
      begin
         return Create_Range
            (Min          => Base_Fields.Null_Field,
             Max          => Max,
             Max_Included => Max_Included);
      end Create_Min_Unbounded_Range;

      --------------------------------
      -- Create_Max_Unbounded_Range --
      --------------------------------

      function Create_Max_Unbounded_Range
         (Min          : Base_Fields.Field'Class;
          Min_Included : Boolean := True) return SQL_Ada_Range is
      begin
         return Create_Range
            (Max          => Base_Fields.Null_Field,
             Min          => Min,
             Min_Included => Min_Included);
      end Create_Max_Unbounded_Range;

      -------------
      -- Convert --
      -------------

      function Convert (Value : Ada_Range) return SQL_Ada_Range is
      begin
         return Result : SQL_Ada_Range do
            case Value.Min.Kind is
               when Zero =>
                  Result.Min := GNATCOLL.SQL_Impl.No_Field_Pointer;
                  Result.Min_Included := False;
               when Infinite =>
                  Result.Min := GNATCOLL.SQL_Impl.No_Field_Pointer;
                  Result.Min_Included := True;
               when Included =>
                  Result.Min := +Expression_From_Stored (Value.Min.Value);
                  Result.Min_Included := True;
               when Excluded =>
                  Result.Min := +Expression_From_Stored (Value.Min.Value);
                  Result.Min_Included := False;
            end case;

            case Value.Max.Kind is
               when Zero =>
                  Result.Max := GNATCOLL.SQL_Impl.No_Field_Pointer;
                  Result.Max_Included := False;
               when Infinite =>
                  Result.Max := GNATCOLL.SQL_Impl.No_Field_Pointer;
                  Result.Max_Included := True;
               when Included =>
                  Result.Max := +Expression_From_Stored (Value.Max.Value);
                  Result.Max_Included := True;
               when Excluded =>
                  Result.Max := +Expression_From_Stored (Value.Max.Value);
                  Result.Max_Included := False;
            end case;
         end return;
      end Convert;

      ------------------
      -- Range_To_SQL --
      ------------------

      function Range_To_SQL
        (Self : Formatter'Class; Value : SQL_Ada_Range; Quote : Boolean)
        return String
      is
         pragma Unreferenced (Quote);
         Min_Null : constant Boolean := Value.Min = No_Field_Pointer;
         Max_Null : constant Boolean := Value.Max = No_Field_Pointer;
         Mi, Ma   : XString;
      begin
         if Min_Null and then Max_Null then
            if not Value.Min_Included and then not Value.Max_Included then
               return "'empty'";
            else
               return "'(,)'";
            end if;
         else
            if not Min_Null then
               Append_To_String
                  (Value.Min, Self, Result => Mi,
                   Long => True, Show_Types => False);
            end if;

            if not Max_Null then
               Append_To_String
                  (Value.Max, Self, Result => Ma,
                   Long => True, Show_Types => False);
            end if;

            return SQL_Type & "("  --  cast
               & (if Min_Null then "null" else Mi.To_String)
               & ","
               & (if Max_Null then "null" else Ma.To_String)
               & (if not Min_Null and then Value.Min_Included
                  then ",'[" else ",'(")
               & (if not Max_Null and then Value.Max_Included
                  then "]')" else ")')");
         end if;
      end Range_To_SQL;

   end Impl;

   ------------------
   -- Create_Range --
   ------------------

   function Create_Range
      (Min, Max     : Ada_Type;
       Min_Included : Boolean := True;
       Max_Included : Boolean := True) return Ada_Range
   is
      VMin : Range_Bound
         (if Min_Included then Included else Excluded);
      VMax : Range_Bound
         (if Max_Included then Included else Excluded);
   begin
      VMin.Value := Base_Fields.Ada_To_Stored (Min);
      VMax.Value := Base_Fields.Ada_To_Stored (Max);
      return Ada_Range'(Min => VMin, Max => VMax);
   end Create_Range;

   --------------------------------
   -- Create_Min_Unbounded_Range --
   --------------------------------

   function Create_Min_Unbounded_Range
      (Max          : Ada_Type;
       Max_Included : Boolean := True) return Ada_Range
   is
      VMax : Range_Bound
         (if Max_Included then Included else Excluded);
   begin
      VMax.Value := Base_Fields.Ada_To_Stored (Max);
      return Ada_Range'
         (Min  => Range_Bound'(Kind => Infinite),
          Max  => VMax);
   end Create_Min_Unbounded_Range;

   --------------------------------
   -- Create_Max_Unbounded_Range --
   --------------------------------

   function Create_Max_Unbounded_Range
      (Min          : Ada_Type;
       Min_Included : Boolean := True) return Ada_Range
   is
      VMin : Range_Bound
         (if Min_Included then Included else Excluded);
   begin
      VMin.Value := Base_Fields.Ada_To_Stored (Min);
      return Ada_Range'
         (Min  => VMin,
          Max  => Range_Bound'(Kind => Infinite));
   end Create_Max_Unbounded_Range;

   --------------
   -- Contains --
   --------------

   function Contains (Self : Ada_Range; Value : Ada_Type) return Boolean is
   begin
      case Self.Min.Kind is
         when Zero =>
            return False;
         when Infinite =>
            null;
         when Included =>
            if Value < Base_Fields.Stored_To_Ada (Self.Min.Value) then
               return False;
            end if;
         when Excluded =>
            if Value <= Base_Fields.Stored_To_Ada (Self.Min.Value) then
               return False;
            end if;
      end case;

      case Self.Max.Kind is
         when Zero =>
            return False;
         when Infinite =>
            null;
         when Included =>
            if Base_Fields.Stored_To_Ada (Self.Max.Value) < Value then
               return False;
            end if;
         when Excluded =>
            if Base_Fields.Stored_To_Ada (Self.Max.Value) <= Value then
               return False;
            end if;
      end case;

      return True;
   end Contains;

   --------------------
   -- Range_From_SQL --
   --------------------

   function Range_From_SQL
      (Self : Formatter'Class; Value : String) return Ada_Range
   is
      function Get (Str : String; Default : Bound_Type) return Range_Bound;
      --  Parse Str and set Val

      function Get
         (Str : String; Default : Bound_Type) return Range_Bound is
      begin
         if Str'Length = 0 then
            return (Kind => Infinite);
         else
            declare
               Result : Range_Bound (Default);
            begin
               --  Remove quotes if any
               if Str (Str'First) = '"' then
                  Result.Value := Base_Fields.Ada_To_Stored
                     (Base_Fields.From_SQL
                        (Self, Str (Str'First + 1 .. Str'Last - 1)));
               else
                  Result.Value := Base_Fields.Ada_To_Stored
                     (Base_Fields.From_SQL (Self, Str));
               end if;
               return Result;
            end;
         end if;
      end Get;

   begin
      if Value = "empty" then
         return Empty_Range;
      end if;

      --  Value is coming straight from postgres, so it always well
      --  formed (for instance there are exactly two bounds).

      declare
         Min, Max : Range_Bound;
         Index : Natural := Value'First + 1;
         Start : Natural;
         In_Quotes : Boolean := False;
      begin
         Start := Index;
         while Index <= Value'Last - 1 loop
            if Value (Index) = '"' then
               if Value (Index + 1) = '"' then
                  --  A "" should be taken as a single quote in postgres
                  Index := Index + 1;
               else
                  In_Quotes := not In_Quotes;
               end if;

            elsif Value (Index) = ',' and not In_Quotes then
               Min := Get (Value (Start .. Index - 1),
                           (if Value (Value'First) = '['
                            then Included else Excluded));
               Start := Index + 1;
            end if;

            Index := Index + 1;
         end loop;

         Max := Get (Value (Start .. Index - 1),
                     (if Value (Value'Last) = ']'
                      then Included else Excluded));

         return Ada_Range'(Min => Min, Max => Max);
      end;
   end Range_From_SQL;

   -----------------
   -- Range_Value --
   -----------------

   function Range_Value
      (Self  : Forward_Cursor'Class;
       Field : Field_Index)
      return Ada_Range is
   begin
      return Range_From_SQL (Self.Get_Formatter.all, Self.Value (Field));
   end Range_Value;

   --------------
   -- As_Param --
   --------------

   function As_Param (Value : Ada_Range) return SQL_Parameter is
      R : SQL_Parameter;
      P : constant Fields.Parameter := (Val => Impl.Convert (Value));
   begin
      R.Set (P);
      return R;
   end As_Param;

end GNATCOLL.SQL.Ranges;
