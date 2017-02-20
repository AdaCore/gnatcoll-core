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
          Max_Included : Boolean := True) return Ada_Range
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
         return Ada_Range'
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
          Max_Included : Boolean := True) return Ada_Range is
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
          Min_Included : Boolean := True) return Ada_Range is
      begin
         return Create_Range
            (Max          => Base_Fields.Null_Field,
             Min          => Min,
             Min_Included => Min_Included);
      end Create_Max_Unbounded_Range;

      ------------------
      -- Range_To_SQL --
      ------------------

      function Range_To_SQL
        (Self : Formatter'Class; Value : Ada_Range; Quote : Boolean)
        return String
      is
         pragma Unreferenced (Quote);
         Min_Null : constant Boolean := Value.Min = No_Field_Pointer;
         Max_Null : constant Boolean := Value.Max = No_Field_Pointer;
      begin
         if Min_Null and then Max_Null then
            if not Value.Min_Included and then not Value.Max_Included then
               return "'empty'";
            else
               return "'(,)'";
            end if;
         else
            return SQL_Type & "("  --  cast
               & (if Min_Null
                  then "null"
                  else To_String (Value.Min, Self, Long => True))
               & ","
               & (if Max_Null
                  then "null"
                  else To_String (Value.Max, Self, Long => True))
               & (if not Min_Null and then Value.Min_Included
                  then ",'[" else ",'(")
               & (if not Max_Null and then Value.Max_Included
                  then "]')" else ")')");
         end if;
      end Range_To_SQL;

   end Impl;

   -----------------
   -- Range_Value --
   -----------------

   function Range_Value
      (Self  : Forward_Cursor'Class;
       Field : Field_Index)
      return Ada_Range
   is
      V : constant String := Self.Value (Field);
      Comma : Integer := Integer'Last;
   begin
      if V = "empty" then
         return Empty_Range;
      end if;

      for S in V'Range loop
         if V (S) = ',' then
            Comma := S;
            exit;
         end if;
      end loop;

      if Comma = Integer'Last then
         --  Invalid range
         return Empty_Range;

      elsif Comma = V'First + 1 then
         if Comma = V'Last - 1 then
            return Doubly_Unbounded_Range;
         else
            return Impl.Create_Min_Unbounded_Range
               (Max   => Base_Fields.From_String (V (Comma + 1 .. V'Last - 1)),
                Max_Included => V (V'Last) = ']');
         end if;

      elsif Comma = V'Last - 1 then
         return Impl.Create_Max_Unbounded_Range
            (Min   => Base_Fields.From_String (V (V'First + 1 .. Comma - 1)),
             Min_Included => V (V'First) = '[');

      else
         return Impl.Create_Range
            (Min   => Base_Fields.From_String (V (V'First + 1 .. Comma - 1)),
             Max   => Base_Fields.From_String (V (Comma + 1 .. V'Last - 1)),
             Min_Included => V (V'First) = '[',
             Max_Included => V (V'Last) = ']');
      end if;
   end Range_Value;

end GNATCOLL.SQL.Ranges;
