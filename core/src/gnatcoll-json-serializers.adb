--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
--

with Interfaces; use Interfaces;

with Ada.Characters.Wide_Wide_Latin_1; use Ada.Characters.Wide_Wide_Latin_1;

with GNAT.Decode_UTF8_String;

-------------------------------
-- GNATCOLL.JSON.Serializers --
-------------------------------

package body GNATCOLL.JSON.Serializers is

   procedure Before_Value (Self : in out Serializer);
   --  Checks that the serializer can accepte a value in its current check

   procedure End_Container (Self : in out Serializer; Kind : Container_Kind);
   --  Ensures that the top-stack containers has the proper kind

   function Escape_Non_Print_Character (C : Wide_Wide_Character) return String;
   --  Escapes a non-printable character

   procedure Escape_String (Self : in out Serializer; Text : UTF8_String);
   --  Escapes an UTF8_String as a JSON string

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

         Self.Escape_String (Name);
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
   begin
      Before_Value (Self);
      Self.Escape_String (Value);
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

   --------------------------------
   -- Escape_Non_Print_Character --
   --------------------------------

   function Escape_Non_Print_Character
     (C : Wide_Wide_Character) return String
   is
      To_Hex : constant array (Unsigned_16 range 0 .. 15) of Character :=
        "0123456789ABCDEF";

      Code  : constant Unsigned_32 := Wide_Wide_Character'Pos (C);
      Buf   : String (1 .. 12);
      Last  : Natural := Buf'First - 1;

      procedure Append_Escaped (Code : Unsigned_16);

      --------------------
      -- Append_Escaped --
      --------------------

      procedure Append_Escaped (Code : Unsigned_16) is
      begin
         Last := Last + 6;
         Buf (Last - 5 .. Last - 4) := "\u";
         Buf (Last - 3) := To_Hex ((Code / 16#1000#) mod 16#10#);
         Buf (Last - 2) := To_Hex ((Code / 16#100#) mod 16#10#);
         Buf (Last - 1) := To_Hex ((Code / 16#10#) mod 16#10#);
         Buf (Last) := To_Hex (Code mod 16#10#);
      end Append_Escaped;

   begin
      if Code <= 16#FFFF# then
         Append_Escaped (Unsigned_16 (Code));

      else
         --  Represent character as surrogate pair

         Append_Escaped
           (16#D800# + Unsigned_16 ((Code - 16#1_0000#) / 16#400#));
         Append_Escaped (16#DC00# + Unsigned_16 (Code mod 16#400#));
      end if;

      return Buf (Buf'First .. Last);
   end Escape_Non_Print_Character;

   -----------------
   -- Emit_String --
   -----------------

   procedure Escape_String (Self : in out Serializer; Text : UTF8_String) is
      Low   : Natural := Text'First;
      First : Natural := Low;
      W_Chr : Wide_Wide_Character;
   begin
      Serializer'Class (Self).Write ("""");

      while Low <= Text'Last loop
         if Text (Low) in ' ' .. '~'
           and then Text (Low) /= '"' and then Text (Low) /= '\'
         then
            Low := Low + 1;
         else
            if First < Low then
               Serializer'Class (Self).Write (Text (First .. Low - 1));
            end if;
            begin
               GNAT.Decode_UTF8_String.Decode_Wide_Wide_Character
                 (Text (Low .. Natural'Min (Text'Last, Low + 3)), Low, W_Chr);
            exception
               when Constraint_Error =>
                  Low := Low + 1;
                  W_Chr := NUL;
            end;
            case W_Chr is
               when NUL => Serializer'Class (Self).Write ("\u0000");
               when '"' => Serializer'Class (Self).Write ("\""");
               when '\' => Serializer'Class (Self).Write ("\\");
               when BS => Serializer'Class (Self).Write ("\b");
               when FF => Serializer'Class (Self).Write ("\f");
               when LF => Serializer'Class (Self).Write ("\n");
               when CR => Serializer'Class (Self).Write ("\r");
               when HT => Serializer'Class (Self).Write ("\t");
               when others =>
                  if Wide_Wide_Character'Pos (W_Chr) < 32
                    or else Wide_Wide_Character'Pos (W_Chr) >= 16#80#
                  then
                     Serializer'Class (Self).Write
                       (Escape_Non_Print_Character (W_Chr));
                  else
                     Serializer'Class (Self).Write ("" & Character'Val
                       (Wide_Wide_Character'Pos (W_Chr)));
                  end if;
            end case;
            First := Low;
         end if;
      end loop;

      if First <= Text'Last then
         Serializer'Class (Self).Write (Text (First .. Text'Last));
      end if;

      Serializer'Class (Self).Write ("""");
   end Escape_String;

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
