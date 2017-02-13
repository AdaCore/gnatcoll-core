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

      ------------------
      -- Create_Range --
      ------------------

      function Create_Range
         (Min, Max     : Base_Fields.Field'Class;
          Min_Included : Boolean := True;
          Max_Included : Boolean := True) return Ada_Range is
      begin
         return Ada_Range'
            (Kind          => Standard,
             Min           => +Min,
             Max           => +Max,
             Min_Included  => Min_Included,
             Max_Included  => Max_Included);
      end Create_Range;

      --------------------------------
      -- Create_Min_Unbounded_Range --
      --------------------------------

      function Create_Min_Unbounded_Range
         (Max          : Base_Fields.Field'Class;
          Max_Included : Boolean := True) return Ada_Range is
      begin
         return Ada_Range'
            (Kind          => Min_Unbounded,
             MaxU          => +Max,
             MaxU_Included => Max_Included);
      end Create_Min_Unbounded_Range;

      --------------------------------
      -- Create_Max_Unbounded_Range --
      --------------------------------

      function Create_Max_Unbounded_Range
         (Min          : Base_Fields.Field'Class;
          Min_Included : Boolean := True) return Ada_Range is
      begin
         return Ada_Range'
            (Kind          => Max_Unbounded,
             MinU          => +Min,
             MinU_Included => Min_Included);
      end Create_Max_Unbounded_Range;

      ------------------
      -- Range_To_SQL --
      ------------------

      function Range_To_SQL
        (Self : Formatter'Class; Value : Ada_Range; Quote : Boolean)
        return String
      is
         pragma Unreferenced (Quote);
      begin
         case Value.Kind is
            when Min_Unbounded =>
               return SQL_Type & "(null,"
                  & To_String (Value.MaxU, Self, Long => True)
                  & (if Value.MaxU_Included then ",'(]')" else ",'()')");
            when Standard =>
               return SQL_Type & "("  --  cast
                  & To_String (Value.Min, Self, Long => True)
                  & ","
                  & To_String (Value.Max, Self, Long => True)
                  & (if Value.Min_Included then ",'[" else ",'(")
                  & (if Value.Max_Included then "]')" else ")')");
            when Max_Unbounded =>
               return SQL_Type & "("  --  cast
                  & To_String (Value.MinU, Self, Long => True)
                  & ",null"
                  & (if Value.MinU_Included then ",'[)')" else ",'()')");
            when Doubly_Unbounded =>
               return "'(,)'";
            when Empty =>
               return "'empty'";
         end case;
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

begin
   Register_Field_Type (Field_Type_Range'(null record));
end GNATCOLL.SQL.Ranges;
