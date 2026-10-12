--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
--

with GNATCOLL.JSON.Utility;

package body GNATCOLL.JSON.Serializers is

   procedure Before_Value (Self : in out Serializer);
   --  Checks that the serializer can accepte a value in its current check

   procedure End_Container (Self : in out Serializer; Kind : Container_Kind);
   --  Ensures that the top-stack containers has the proper kind

   ------------------
   -- Before_Value --
   ------------------

   procedure Before_Value (Self : in out Serializer) is
   begin
      if Self.Finished then
         raise JSON_Serialization_Error with "document already finished";
      elsif Self.Stack.Is_Empty then
         if Self.Root_Written then
            raise JSON_Serialization_Error with "multiple root values";
         end if;
         Self.Root_Written := True;
      else
         declare
            Top : Frame renames Self.Stack.Reference (Self.Stack.Last_Index);
         begin
            if Top.Kind = Object_Kind then
               if not Top.Name_Pending then
                  raise JSON_Serialization_Error
                    with "object member name required";
               end if;
               Top.Name_Pending := False;
            else
               if not Top.First then
                  Serializer'Class (Self).Write (",");
               end if;
               Top.First := False;
            end if;
         end;
      end if;
   end Before_Value;

   -----------
   -- Close --
   -----------

   procedure Close (Self : in out Serializer) is
   begin
      if not Self.Root_Written or else not Self.Stack.Is_Empty then
         raise JSON_Serialization_Error with "incomplete document";
      end if;
      Self.Finished := True;
   end Close;

   --------------------
   -- Encode_Boolean --
   --------------------

   procedure Encode_Boolean (Self : in out Serializer; Value : Boolean) is
   begin
      Before_Value (Self);
      Serializer'Class (Self).Write ((if Value then "true" else "false"));
   end Encode_Boolean;

   -----------------
   -- Encode_Name --
   -----------------

   procedure Encode_Name (Self : in out Serializer; Name : UTF8_String) is
      procedure Emit (Text : String);

      ----------
      -- Emit --
      ----------

      procedure Emit (Text : String) is
      begin
         Serializer'Class (Self).Write (Text);
      end Emit;

      procedure Quote is new Utility.Emit_String (Emit);
   begin
      if Self.Finished or else Self.Stack.Is_Empty then
         raise JSON_Serialization_Error with "name outside object";
      end if;
      declare
         Top : Frame renames Self.Stack.Reference (Self.Stack.Last_Index);
      begin
         if Top.Kind /= Object_Kind or else Top.Name_Pending then
            raise JSON_Serialization_Error
              with "unexpected object member name";
         end if;

         if not Top.First then
            Serializer'Class (Self).Write (",");
         end if;

         Quote (Name);
         Serializer'Class (Self).Write (":");
         Top.First := False;
         Top.Name_Pending := True;
      end;
   end Encode_Name;

   -----------------
   -- Encode_Null --
   -----------------

   procedure Encode_Null (Self : in out Serializer) is
   begin
      Before_Value (Self);
      Serializer'Class (Self).Write ("null");
   end Encode_Null;

   -------------------
   -- Encode_Number --
   -------------------

   procedure Encode_Number
     (Self : in out Serializer; Value : Long_Long_Integer)
   is
      Image : constant String := Value'Image;
   begin
      Before_Value (Self);
      Serializer'Class (Self).Write (Image ((if Image (Image'First) = ' '
                      then Image'First + 1 else Image'First) .. Image'Last));
   end Encode_Number;

   procedure Encode_Number (Self : in out Serializer; Value : Long_Integer) is
   begin
      Self.Encode_Number (Long_Long_Integer (Value));
   end Encode_Number;

   procedure Encode_Number (Self : in out Serializer; Value : Integer) is
   begin
      Self.Encode_Number (Long_Long_Integer (Value));
   end Encode_Number;

   procedure Encode_Number (Self : in out Serializer; Value : Long_Float) is
      Image : constant String := Value'Image;
   begin
      for C of Image loop
         if C not in '0' .. '9' | ' ' | '-' | '+' | '.' | 'E' then
            raise JSON_Serialization_Error with "non-finite number";
         end if;
      end loop;
      Before_Value (Self);
      Serializer'Class (Self).Write (Image ((if Image (Image'First) = ' '
                      then Image'First + 1 else Image'First) .. Image'Last));
   end Encode_Number;

   -------------------
   -- Encode_String --
   -------------------

   procedure Encode_String (Self : in out Serializer; Value : UTF8_String) is
      procedure Emit (Text : String);

      ----------
      -- Emit --
      ----------

      procedure Emit (Text : String) is
      begin
         Serializer'Class (Self).Write (Text);
      end Emit;

      procedure Quote is new Utility.Emit_String (Emit);
   begin
      Before_Value (Self);
      Quote (Value);
   end Encode_String;

   procedure Encode_String
     (Self : in out Serializer; Value : UTF8_Unbounded_String) is
   begin
      Self.Encode_String (Ada.Strings.Unbounded.To_String (Value));
   end Encode_String;

   procedure Encode_String (Self : in out Serializer; Value : UTF8_XString) is
      Text   : GNATCOLL.Strings.Char_Array;
      Length : Natural;
   begin
      Value.Get_String (Text, Length);
      Self.Encode_String (String (Text (1 .. Length)));
   end Encode_String;

   ---------------
   -- End_Array --
   ---------------

   procedure End_Array (Self : in out Serializer) is
   begin
      End_Container (Self, Array_Kind);
   end End_Array;

   -------------------
   -- End_Container --
   -------------------

   procedure End_Container (Self : in out Serializer; Kind : Container_Kind) is
   begin
      if Self.Finished or else Self.Stack.Is_Empty then
         raise JSON_Serialization_Error with "no open container";
      elsif Self.Stack.Last_Element.Kind /= Kind then
         raise JSON_Serialization_Error with "mismatched container";
      elsif Self.Stack.Last_Element.Name_Pending then
         raise JSON_Serialization_Error with "object member value required";
      end if;

      Serializer'Class (Self).Write ((if Kind = Array_Kind then "]" else "}"));
      Self.Stack.Delete_Last;
   end End_Container;

   ----------------
   -- End_Object --
   ----------------

   procedure End_Object (Self : in out Serializer) is
   begin
      End_Container (Self, Object_Kind);
   end End_Object;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out Serializer) is
   begin
      Self.Stack.Clear;
      Self.Root_Written := False;
      Self.Finished := False;
   end Reset;

   -----------------
   -- Start_Array --
   -----------------

   procedure Start_Array (Self : in out Serializer) is
   begin
      Before_Value (Self);
      Serializer'Class (Self).Write ("[");
      Self.Stack.Append (Frame'(Kind => Array_Kind, others => <>));
   end Start_Array;

   ------------------
   -- Start_Object --
   ------------------

   procedure Start_Object (Self : in out Serializer) is
   begin
      Before_Value (Self);
      Serializer'Class (Self).Write ("{");
      Self.Stack.Append (Frame'(Kind => Object_Kind, others => <>));
   end Start_Object;

end GNATCOLL.JSON.Serializers;
