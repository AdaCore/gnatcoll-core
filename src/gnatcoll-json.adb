------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2011-2024, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers;          use Ada.Containers;
with Ada.Exceptions;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Atomic;         use GNATCOLL.Atomic;
with GNATCOLL.JSON.Utility;
with GNATCOLL.Strings;        use GNATCOLL.Strings;
with GNATCOLL.String_Builders; use GNATCOLL.String_Builders;

package body GNATCOLL.JSON is

   procedure Free is
     new Ada.Unchecked_Deallocation (JSON_Array_Internal, JSON_Array_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (JSON_Object_Internal, JSON_Object_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (JSON_String_Internal, JSON_String_Access);

   procedure Write
     (Item    : JSON_Value;
      Compact : Boolean;
      Indent  : Natural;
      Ret     : in out Unbounded_String);
   --  Auxiliary write function

   ------------
   -- Append --
   ------------

   procedure Append (Arr : JSON_Value; Item : JSON_Value) is
   begin
      Append (Arr.Data.Arr_Value.Arr, Item);
   end Append;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Val : JSON_Value) return Boolean is
   begin
      case Val.Kind is
         when JSON_Null_Type   => return True;
         when JSON_Array_Type  => return Val.Data.Arr_Value.Arr.Vals.Is_Empty;
         when JSON_Object_Type => return Val.Data.Obj_Value.Vals.Is_Empty;
         when others           => return False;
      end case;
   end Is_Empty;

   -----------------
   -- Array_First --
   -----------------

   function Array_First (Arr : JSON_Array) return Positive is
      pragma Unreferenced (Arr);
   begin
      return 1;
   end Array_First;

   ----------------
   -- Array_Next --
   ----------------

   function Array_Next (Arr : JSON_Array; Index : Positive) return Positive is
      pragma Unreferenced (Arr);
   begin
      return Index + 1;
   end Array_Next;

   -----------------------
   -- Array_Has_Element --
   -----------------------

   function Array_Has_Element
     (Arr : JSON_Array; Index : Positive) return Boolean
   is
   begin
      return Index <= Length (Arr);
   end Array_Has_Element;

   -------------------
   -- Array_Element --
   -------------------

   function Array_Element
     (Arr : JSON_Array; Index : Positive) return JSON_Value
   is
   begin
      return Get (Arr, Index);
   end Array_Element;

   --------------------------
   -- Format_Parsing_Error --
   --------------------------

   function Format_Parsing_Error (Error : Parsing_Error) return String is
      L : constant String := Error.Line'Img;
      C : constant String := Error.Column'Img;
   begin
      return
        (L (L'First + 1 .. L'Last) & ":"
         & C (C'First + 1 .. C'Last) & ": "
         & To_String (Error.Message));
   end Format_Parsing_Error;

   function Parse_Next
      (Self : in out JSON_Parser; Data : in out GNATCOLL.Buffer.Reader)
      return JSON_Parser_Event
   is

      use GNATCOLL.Buffer;
      Result : JSON_Parser_Event;
      CC     : Character;
      Offset : Integer;

      procedure Skip_Whitespaces with Inline_Always => True;
      procedure Parse_Number with Inline_Always => True;
      procedure Expect_Value with Inline_Always => True;

      procedure Skip_Whitespaces is
         Offset : Integer;
      begin
         Offset := Window_Offset (Data);
         while Next (Data, CC, Offset) loop
            if CC > ' ' then
               exit;
            end if;

            case CC is
               when ' ' | ASCII.HT | ASCII.LF | ASCII.CR =>
                  null;
               when others =>
                  exit;
            end case;
         end loop;
         Set_Window_Offset (Data, Offset);
         Release (Data);
      end Skip_Whitespaces;

      procedure Parse_Number is
         Has_Char : Boolean;
      begin
         Offset := Window_Offset (Data);
         Result.First := Current_Position (Data);

         --  If minus sign is present we expect afterward a number
         if CC = '-' then
            if not Next (Data, CC, Offset) or else CC not in '0' .. '9' then
               raise Invalid_JSON_Stream with "not a valid number";
            end if;
         end if;

         --  At this stage, by construction CC is in '0' .. '9'
         if CC in '1' .. '9' then
            loop
               Has_Char := Next (Data, CC, Offset);
               exit when not Has_Char or else CC not in '0' .. '9';
            end loop;
         else
            Has_Char := Next (Data, CC, Offset);
         end if;

         if Has_Char and then CC = '.' then
            --  We have a fractional part
            Result.Kind := NUMBER_VALUE;
            Has_Char := Next (Data, CC, Offset);
            if not Has_Char or else CC not in '0' .. '9' then
               raise Invalid_JSON_Stream with "not a valid number";
            end if;

            loop
               Has_Char := Next (Data, CC, Offset);
               exit when not Has_Char or else CC not in '0' .. '9';
            end loop;
         end if;

         if Has_Char and then (CC = 'e' or else CC = 'E') then
            --  We have an exponential part
            Result.Kind := NUMBER_VALUE;
            Has_Char := Next (Data, CC, Offset);
            if not Has_Char then
               raise Invalid_JSON_Stream with "not a valid number";
            end if;

            if CC = '+' or else CC = '-' then
               Has_Char := Next (Data, CC, Offset);
            end if;

            if not Has_Char or else CC not in '0' .. '9' then
               raise Invalid_JSON_Stream with "not a valid number";
            end if;

            loop
               Has_Char := Next (Data, CC, Offset);
               exit when not Has_Char or else CC not in '0' .. '9';
            end loop;
         end if;

         if Has_Char then
            Set_Window_Offset (Data, Offset - 1);
            Result.Last := Current_Position (Data);
         else
            Set_Window_Offset (Data, Offset);
            Result.Last := Current_Position (Data);
         end if;
      end Parse_Number;

      procedure Expect_Value is
      begin
         --  Skip initial white spaces
         Skip_Whitespaces;

         if Is_End_Of_Data (Data) then
            Result.Kind := DOC_END;
            return;
         end if;

         case Current_Char (Data) is
            when '{' =>
               --  This is an object
               Result.Kind := OBJECT_START;
               return;
            when '}' =>
               Result.Kind := OBJECT_END;
               return;
            when '[' =>
               --  This is an array
               Result.Kind := ARRAY_START;
            when ']' =>
               Result.Kind := ARRAY_END;
               return;
            when ':' =>
               Result.Kind := NAME_SEP;
               return;
            when ',' =>
               Result.Kind := VALUE_SEP;
               return;
            when 't' =>
               --  This is true
               Result.Kind := TRUE_VALUE;
               if not GNATCOLL.Buffer.Check (Data, "rue") then
                  raise Invalid_JSON_Stream with "invalid token";
               end if;
            when 'f' =>
               --  This is false
               Result.Kind := FALSE_VALUE;
               if not GNATCOLL.Buffer.Check (Data, "alse") then
                  raise Invalid_JSON_Stream with "invalid token";
               end if;
            when 'n' =>
               --  This is null

               Result.Kind := NULL_VALUE;
               if not GNATCOLL.Buffer.Check (Data, "ull") then
                  raise Invalid_JSON_Stream with "invalid token";
               end if;

            when '"' =>
               Result.Kind := STRING_VALUE;
               declare
                  Offset : Integer;
               begin

                  Offset := Window_Offset (Data);
                  Result.First := Current_Position (Data);

                  --  This is a string
                  while Next (Data, CC, Offset) loop
                     case CC is
                        when '"' =>
                           Set_Window_Offset (Data, Offset);
                           Result.Last := Current_Position (Data);
                           return;
                        when ASCII.NUL .. ASCII.US =>
                           raise Invalid_JSON_Stream
                            with "control character not allowed in string";
                        when '\' =>
                           if not Next (Data, CC, Offset) then
                              raise Invalid_JSON_Stream
                              with "non terminated string";
                           end if;

                           case CC is
                              when '\' | '/' | '"' | 'b' | 'f' |
                                 'n' | 'r' | 't' =>
                                 --  This is a single character escape sequence
                                 null;
                              when 'u' =>
                                 for Idx in 1 .. 4 loop
                                    if not Next (Data, CC, Offset) then
                                       raise Invalid_JSON_Stream
                                       with "invalid unicode escape sequence";
                                    elsif CC not in
                                       'a' .. 'f' | 'A' .. 'F' | '0' .. '9'
                                    then
                                       raise Invalid_JSON_Stream
                                       with "invalid unicode escape sequence";
                                    end if;

                                 end loop;
                              when others =>
                                 raise Invalid_JSON_Stream with "unknown case";
                           end case;


                        when others =>
                           null;
                     end case;
                  end loop;
               end;
               raise Invalid_JSON_Stream with "non terminated string";
            when '-' | '0' .. '9' =>
               --  This is a number
               Result.Kind := INTEGER_VALUE;
               Parse_Number;
            when others =>
               raise Invalid_JSON_Stream
                  with "unexpected character '" & Current_Char (Data) & "'";
         end case;

      end Expect_Value;

      Scan_Next : Boolean := True;
   begin
      Offset := Window_Offset (Data);
      --  Release previous data from the buffer
      Release (Data);

      --  Read next token
      Expect_Value;

      if Self.State (Self.State_Current) = EXPECT_OBJECT_NAME_SEP then
         if Result.Kind = NAME_SEP then
            Self.State (Self.State_Current) := EXPECT_OBJECT_VALUE;
            Expect_Value;
         else
            raise Invalid_JSON_Stream with "colon expected";
         end if;
      elsif Self.State (Self.State_Current) = EXPECT_OBJECT_SEP then
         if Result.Kind = VALUE_SEP then
            Self.State (Self.State_Current) := EXPECT_OBJECT_KEY;
            Expect_Value;
         elsif Result.Kind = OBJECT_END then
            Self.State_Current := Self.State_Current - 1;
            Scan_Next := False;
         else
            raise Invalid_JSON_Stream with "coma or } expected";
         end if;
      elsif Self.State (Self.State_Current) = EXPECT_ARRAY_SEP then
         if Result.Kind = VALUE_SEP then
            Self.State (Self.State_Current) := EXPECT_ARRAY_VALUE;
            Expect_Value;
         elsif Result.Kind = ARRAY_END then
            Self.State_Current := Self.State_Current - 1;
            Scan_Next := False;
         else
            raise Invalid_JSON_Stream with "coma or ] expected";
         end if;
      end if;

      if Scan_Next then
         case Self.State (Self.State_Current) is
            when EXPECT_VALUE =>
               if Result.Kind not in STRING_VALUE .. OBJECT_START then
                  raise Invalid_JSON_Stream
                     with "value expected (got " & Result.Kind'Img & ")";
               end if;
               Self.State (Self.State_Current) := EXPECT_DOC_END;

            when EXPECT_OBJECT_KEY =>
               if Result.Kind = STRING_VALUE then
                  Self.State (Self.State_Current) := EXPECT_OBJECT_NAME_SEP;
               else
                  raise Invalid_JSON_Stream with "string expected";
               end if;

            when EXPECT_OBJECT_FIRST_KEY =>
               if Result.Kind = STRING_VALUE then
                  Self.State (Self.State_Current) := EXPECT_OBJECT_NAME_SEP;
               elsif Result.Kind = OBJECT_END then
                  Self.State_Current := Self.State_Current - 1;
               else
                  raise Invalid_JSON_Stream with "string expected";
               end if;

            when EXPECT_OBJECT_VALUE =>
               if Result.Kind not in STRING_VALUE .. OBJECT_START then
                  raise Invalid_JSON_Stream with "json valud expected";
               end if;

               Self.State (Self.State_Current) := EXPECT_OBJECT_SEP;

            when EXPECT_ARRAY_FIRST_VALUE =>
               if Result.Kind = ARRAY_END then
                  Self.State_Current := Self.State_Current - 1;
               elsif Result.Kind not in STRING_VALUE .. OBJECT_START then
                  raise Invalid_JSON_Stream with "array element expected";
               else
                  Self.State (Self.State_Current) := EXPECT_ARRAY_SEP;
               end if;

            when EXPECT_ARRAY_VALUE =>
               if Result.Kind not in STRING_VALUE .. OBJECT_START then
                  raise Invalid_JSON_Stream with "array element expected";
               else
                  Self.State (Self.State_Current) := EXPECT_ARRAY_SEP;
               end if;

            when EXPECT_DOC_END =>
               if Result.Kind /= DOC_END then
                  raise Invalid_JSON_Stream with "end of doc expected";
               end if;
            when others =>
               raise Invalid_JSON_Stream with
                  "unknown state " & Self.State (Self.State_Current)'Img;
         end case;
      end if;

      --  Handle value beginning of object or array
      if Result.Kind = OBJECT_START then
         Self.State_Current := Self.State_Current + 1;
         Self.State (Self.State_Current) := EXPECT_OBJECT_FIRST_KEY;
      end if;

      if Result.Kind = ARRAY_START then
         Self.State_Current := Self.State_Current + 1;
         Self.State (Self.State_Current) := EXPECT_ARRAY_FIRST_VALUE;
      end if;

      return Result;

   end Parse_Next;

   ----------
   -- Read --
   ----------

   function Read
     (Strm     : Unbounded_String;
      Filename : String := "<data>") return JSON_Value
   is
   begin
      return Read (To_String (Strm), Filename);
   end Read;

   function Read
     (Strm     : String;
      Filename : String := "<data>") return JSON_Value
   is
      Result : constant Read_Result := Read (Strm);
   begin
      if Result.Success then
         return Result.Value;
      else
         Ada.Text_IO.New_Line;

         if Filename = "" then
            Ada.Text_IO.Put ("<stream>:");
         else
            Ada.Text_IO.Put (Filename & ":");
         end if;

         Ada.Text_IO.Put_Line (Format_Parsing_Error (Result.Error));
         raise Invalid_JSON_Stream with To_String (Result.Error.Message);
      end if;
   end Read;

   function Read
     (Strm : Ada.Strings.Unbounded.Unbounded_String) return Read_Result
   is
   begin
      return Read (To_String (Strm));
   end Read;

   function Read (Strm : String) return Read_Result is
      use GNATCOLL.Buffer;
      Buf    : Reader := Open_String (Strm);
      Parser : JSON_Parser;
      Event  : JSON_Parser_Event;
      Result : JSON_Value;
      Cursor : JSON_Value;
      Tmp    : JSON_Value;

      Current_Key : String_Builder;
      Expect_Key : array (1 .. 512) of Boolean := (others => False);
      Cursors : array (1 .. 512) of JSON_Value;
      Expect_Key_Last : Integer := 0;
   begin

      loop
         Event := Parse_Next (Parser, Buf);
         exit when Event.Kind = DOC_END;

         if Event.Kind /= OBJECT_END and then
            Expect_Key_Last > 0 and then
            Expect_Key (Expect_Key_Last)
         then
            Set (Current_Key, Token (Buf, Event.First, Event.Last));
            Event := Parse_Next (Parser, Buf);
         end if;

         case Event.Kind is
            when OBJECT_START =>
               Tmp := Create_Object;
            when ARRAY_START =>
               Tmp := Create (Empty_Array);
            when STRING_VALUE =>
               declare
                  Str : constant String :=
                     Token (Buf, Event.First, Event.Last);
               begin
                  Tmp := Create
                     (Utility.Un_Escape_String (Str, Str'First, Str'Last));
               end;
            when INTEGER_VALUE =>
               Tmp := Create
                  (Long_Long_Integer'Value
                     (Token (Buf, Event.First, Event.Last)));
            when NUMBER_VALUE =>
               Tmp := Create
                  (Long_Float'Value (Token (Buf, Event.First, Event.Last)));
            when TRUE_VALUE =>
               Tmp := Create (True);
            when FALSE_VALUE =>
               Tmp := Create (False);
            when NULL_VALUE =>
               Tmp := Create;
            when others =>
               null;
         end case;

         if Event.Kind = OBJECT_END then
            Expect_Key_Last := Expect_Key_Last - 1;
            if Expect_Key_Last > 0 then
               Cursor := Cursors (Expect_Key_Last);
            end if;

         elsif Event.Kind = ARRAY_END then
            Expect_Key_Last := Expect_Key_Last - 1;
            if Expect_Key_Last > 0 then
               Cursor := Cursors (Expect_Key_Last);
            end if;

         else
            if Expect_Key_Last > 0 then
               if Expect_Key (Expect_Key_Last) then
                  declare
                     Str : constant String := As_String (Current_Key);
                  begin
                     Set_Field
                        (Cursor,
                         Utility.Un_Escape_String (Str, Str'First, Str'Last),
                         Tmp);
                  end;
               else
                  Append (Cursor, Tmp);
               end if;
            else
               Result := Tmp;
            end if;

            if Event.Kind = OBJECT_START then
               Expect_Key_Last := Expect_Key_Last + 1;
               Cursors (Expect_Key_Last) := Tmp;
               Expect_Key (Expect_Key_Last) := True;
               Cursor := Tmp;
            elsif Event.Kind = ARRAY_START then
               Expect_Key_Last := Expect_Key_Last + 1;
               Cursors (Expect_Key_Last) := Tmp;
               Expect_Key (Expect_Key_Last) := False;
               Cursor := Tmp;
            end if;
         end if;

      end loop;

      return (Success => True, Value => Result);
   exception
      when E : Invalid_JSON_Stream =>
         declare
            Line, Column : Integer;
         begin
            Buf.Current_Text_Position (Line, Column);

            return (Success => False,
                    Error => (Line, Column,
                    To_Unbounded_String
                       (Ada.Exceptions.Exception_Message (E))));
         end;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Item    : JSON_Value;
      Compact : Boolean;
      Indent  : Natural;
      Ret     : in out Unbounded_String)
   is
      procedure Do_Indent (Val : Natural);
      --  Adds whitespace characters to Ret corresponding to the indentation
      --  level.

      ---------------
      -- Do_Indent --
      ---------------

      procedure Do_Indent (Val : Natural) is
      begin
         if Compact then
            return;
         end if;

         Append (Ret, (1 .. 2 * Val => ' '));
      end Do_Indent;

   begin
      case Item.Kind is
         when JSON_Null_Type =>
            Append (Ret, "null");

         when JSON_Boolean_Type =>
            if Item.Data.Bool_Value then
               Append (Ret, "true");
            else
               Append (Ret, "false");
            end if;

         when JSON_Int_Type =>
            declare
               S : constant String := Item.Data.Int_Value'Img;
            begin
               if S (S'First) = ' ' then
                  Append (Ret, S (S'First + 1 .. S'Last));
               else
                  Append (Ret, S);
               end if;
            end;

         when JSON_Float_Type =>
            declare
               S : constant String := Item.Data.Flt_Value'Img;
            begin
               if S (S'First) = ' ' then
                  Append (Ret, S (S'First + 1 .. S'Last));
               else
                  Append (Ret, S);
               end if;
            end;

         when JSON_String_Type =>
            Append (Ret, JSON.Utility.Escape_String (Item.Data.Str_Value.Str));

         when JSON_Array_Type =>
            Append (Ret, '[');

            if not Compact then
               Append (Ret, ASCII.LF);
            end if;

            for J in Item.Data.Arr_Value.Arr.Vals.First_Index ..
              Item.Data.Arr_Value.Arr.Vals.Last_Index
            loop
               Do_Indent (Indent + 1);
               Write
                 (Item.Data.Arr_Value.Arr.Vals.Element (J),
                  Compact, Indent + 1, Ret);

               if J < Item.Data.Arr_Value.Arr.Vals.Last_Index then
                  Append (Ret, ",");
               end if;

               if not Compact then
                  Append (Ret, ASCII.LF);
               end if;
            end loop;

            Do_Indent (Indent);
            Append (Ret, ']');

         when JSON_Object_Type =>
            declare
               use Object_Items_Pkg;
               J : Object_Items_Pkg.Cursor := Item.Data.Obj_Value.Vals.First;

            begin
               Append (Ret, '{');

               if not Compact then
                  Append (Ret, ASCII.LF);
               end if;

               while Has_Element (J) loop
                  Do_Indent (Indent + 1);
                  Append
                    (Ret,
                     GNATCOLL.JSON.Utility.Escape_String (Key (J)));

                  Append (Ret, ':');
                  if not Compact then
                     Append (Ret, ' ');
                  end if;

                  Write (Element (J), Compact, Indent + 1, Ret);

                  Next (J);

                  if Has_Element (J) then
                     Append (Ret, ",");
                  end if;

                  if not Compact then
                     Append (Ret, ASCII.LF);
                  end if;
               end loop;

               Do_Indent (Indent);
               Append (Ret, '}');
            end;

      end case;
   end Write;

   -----------
   -- Write --
   -----------

   function Write
     (Item : JSON_Value; Compact : Boolean := True) return String
   is
   begin
      return To_String (Write (Item, Compact));
   end Write;

   -----------
   -- Write --
   -----------

   function Write
     (Item : JSON_Value; Compact : Boolean := True) return Unbounded_String
   is
      Ret : Unbounded_String;
   begin
      Write (Item, Compact, 0, Ret);
      return Ret;
   end Write;

   ------------
   -- Length --
   ------------

   function Length (Arr : JSON_Array) return Natural is
   begin
      return Natural (Arr.Vals.Length);
   end Length;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Arr : JSON_Array) return Boolean is
   begin
      return Arr.Vals.Is_Empty;
   end Is_Empty;

   ---------
   -- Get --
   ---------

   function Get (Arr : JSON_Array; Index : Positive) return JSON_Value is
   begin
      return Arr.Vals.Element (Index);
   end Get;

   -----------------
   -- Set_Element --
   -----------------

   procedure Set_Element
     (Arr : in out JSON_Array; Index : Positive; Item : JSON_Value) is
   begin
      Arr.Vals.Replace_Element (Index, Item);
   end Set_Element;

   ----------
   -- Sort --
   ----------

   procedure Sort
     (Arr : in out JSON_Array;
      Less : access function (Left, Right : JSON_Value) return Boolean)
   is
      package Sorting is new Vect_Pkg.Generic_Sorting ("<" => Less.all);
   begin
      Sorting.Sort (Arr.Vals);
   end Sort;

   procedure Sort
     (Val : in out JSON_Value;
      Less : access function (Left, Right : JSON_Value) return Boolean)
   is
      --  package Sorting is new Object_Items_Pkg.Generic_Sorting ("<");

   begin
      case Val.Kind is
         when JSON_Array_Type  => Sort (Val.Data.Arr_Value.Arr, Less);
         when JSON_Object_Type =>
            --  Sorting.Sort (Val.Data.Obj_Value.Vals);
            null;
         when others => null;
      end case;
   end Sort;

   ------------
   -- Append --
   ------------

   procedure Append (Arr : in out JSON_Array; Val : JSON_Value) is
   begin
      Arr.Vals.Append (Val);
   end Append;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (Arr : in out JSON_Array; Val : JSON_Value) is
   begin
      Arr.Vals.Prepend (Val);
   end Prepend;

   ---------
   -- "&" --
   ---------

   function "&" (Arr : JSON_Array; Value : JSON_Value) return JSON_Array is
      Result : JSON_Array := Arr;
   begin
      Append (Result, Value);
      return Result;
   end "&";

   function "&" (Value1, Value2 : JSON_Value) return JSON_Array is
      Result : JSON_Array;
   begin
      Append (Result, Value1);
      Append (Result, Value2);
      return Result;
   end "&";

   -----------
   -- Clear --
   -----------

   procedure Clear (Arr : in out JSON_Array) is
   begin
      Arr.Vals.Clear;
   end Clear;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Obj : in out JSON_Value) is
   begin
      case Obj.Data.Kind is
         when JSON_String_Type =>
            if Obj.Data.Str_Value /= null then
               Increment (Obj.Data.Str_Value.Cnt);
            end if;
         when JSON_Array_Type =>
            if Obj.Data.Arr_Value /= null then
               Increment (Obj.Data.Arr_Value.Cnt);
            end if;
         when JSON_Object_Type =>
            if Obj.Data.Obj_Value /= null then
               Increment (Obj.Data.Obj_Value.Cnt);
            end if;
         when others =>
            null;
      end case;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Obj : in out JSON_Value) is
   begin
      case Obj.Data.Kind is
         when JSON_String_Type =>
            if Obj.Data.Str_Value /= null and then
               Decrement (Obj.Data.Str_Value.Cnt)
            then
               Free (Obj.Data.Str_Value);
            end if;
         when JSON_Array_Type =>
            declare
               Arr : JSON_Array_Access := Obj.Data.Arr_Value;
            begin
               Obj.Data.Arr_Value := null;
               if Arr /= null and then Decrement (Arr.Cnt) then
                  Free (Arr);
               end if;
            end;

         when JSON_Object_Type =>
            declare
               Object : JSON_Object_Access := Obj.Data.Obj_Value;
            begin
               Obj.Data.Obj_Value := null;
               if Object /= null and then Decrement (Object.Cnt) then
                  Free (Object);
               end if;
            end;

         when others =>
            null;
      end case;
   end Finalize;

   ------------
   -- Create --
   ------------

   function Create return JSON_Value is
   begin
      return JSON_Null;
   end Create;

   function Create (Val : Boolean) return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Data := (Kind => JSON_Boolean_Type, Bool_Value => Val);
      return Ret;
   end Create;

   function Create (Val : Integer) return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Data := (JSON_Int_Type, Int_Value => Long_Long_Integer (Val));
      return Ret;
   end Create;

   function Create (Val : Long_Integer) return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Data := (JSON_Int_Type, Int_Value => Long_Long_Integer (Val));
      return Ret;
   end Create;

   function Create (Val : Long_Long_Integer) return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Data := (Kind => JSON_Int_Type, Int_Value => Val);
      return Ret;
   end Create;

   function Create (Val : Float) return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Data := (Kind => JSON_Float_Type, Flt_Value => Long_Float (Val));
      return Ret;
   end Create;

   function Create (Val : Long_Float) return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Data := (Kind => JSON_Float_Type, Flt_Value => Val);
      return Ret;
   end Create;

   function Create (Val : UTF8_String) return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Data := (
         Kind      => JSON_String_Type,
         Str_Value => new JSON_String_Internal'
            (Cnt => 1, Str => Null_XString));
      Ret.Data.Str_Value.Str.Set (Val);
      return Ret;
   end Create;

   function Create (Val : UTF8_Unbounded_String) return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Data := (
         Kind      => JSON_String_Type,
         Str_Value => new JSON_String_Internal'
            (Cnt => 1, Str => Null_XString));
      Ret.Data.Str_Value.Str.Set (To_String (Val));
      return Ret;
   end Create;

   function Create (Val : UTF8_XString) return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Data := (
         Kind      => JSON_String_Type,
         Str_Value => new JSON_String_Internal'(Cnt => 1, Str => Val));
      return Ret;
   end Create;

   function Create (Val : JSON_Array) return JSON_Value is
   begin
      return (Ada.Finalization.Controlled with
                Data => (Kind      => JSON_Array_Type,
                         Arr_Value => new JSON_Array_Internal'
                           (Cnt => 1,
                            Arr => Val)));
   end Create;

   -------------------
   -- Create_Object --
   -------------------

   function Create_Object return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Data := (JSON_Object_Type, Obj_Value => new JSON_Object_Internal);
      return Ret;
   end Create_Object;

   -----------------
   -- Unset_Field --
   -----------------

   procedure Unset_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String)
   is
      use Object_Items_Pkg;
      Vals : Object_Items_Pkg.Map renames Val.Data.Obj_Value.Vals;
   begin
      Exclude (Vals, To_XString (Field_Name));
   end Unset_Field;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : JSON_Value)
   is
      use Object_Items_Pkg;
      Vals : Object_Items_Pkg.Map renames Val.Data.Obj_Value.Vals;
   begin
      Include (Vals, To_XString (Field_Name), Field);
   end Set_Field;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_XString;
      Field      : JSON_Value)
   is
      use Object_Items_Pkg;
      Vals : Object_Items_Pkg.Map renames Val.Data.Obj_Value.Vals;
   begin
      Include (Vals, Field_Name, Field);
   end Set_Field;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Boolean) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Integer) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Long_Integer) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Float) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   procedure Set_Field_Long_Float
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Long_Float) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field_Long_Float;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : UTF8_String) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : UTF8_Unbounded_String) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : JSON_Array)
   is
      F_Val : constant JSON_Value := Create (Field);
   begin
      Set_Field (Val, Field_Name, F_Val);
   end Set_Field;

   ----------------------------
   -- Set_Field_If_Not_Empty --
   ----------------------------

   procedure Set_Field_If_Not_Empty
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : UTF8_Unbounded_String) is
   begin
      if Field /= Null_Unbounded_String then
         Set_Field (Val, Field_Name, Field);
      end if;
   end Set_Field_If_Not_Empty;

   procedure Set_Field_If_Not_Empty
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : UTF8_String) is
   begin
      if Field /= "" then
         Set_Field (Val, Field_Name, Field);
      end if;
   end Set_Field_If_Not_Empty;

   procedure Set_Field_If_Not_Empty
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : JSON_Array) is
   begin
      if Field /= Empty_Array then
         Set_Field (Val, Field_Name, Field);
      end if;
   end Set_Field_If_Not_Empty;
   ----------
   -- Kind --
   ----------

   function Kind (Val : JSON_Value) return JSON_Value_Type is
   begin
      return Val.Data.Kind;
   end Kind;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Boolean is
   begin
      return Val.Data.Bool_Value;
   end Get;

   function Get (Val : JSON_Value) return Integer is
   begin
      return Integer (Val.Data.Int_Value);
   end Get;

   function Get (Val : JSON_Value) return Long_Integer is
   begin
      return Long_Integer (Val.Data.Int_Value);
   end Get;

   function Get (Val : JSON_Value) return Long_Long_Integer is
   begin
      return Val.Data.Int_Value;
   end Get;

   function Get (Val : JSON_Value) return Float is
   begin
      return Float (Val.Data.Flt_Value);
   end Get;

   function Get_Long_Float (Val : JSON_Value) return Long_Float is
   begin
      return Val.Data.Flt_Value;
   end Get_Long_Float;

   function Get (Val : JSON_Value) return UTF8_String is
   begin
      return To_String (Val.Data.Str_Value.Str);
   end Get;

   function Get (Val : JSON_Value) return UTF8_XString is
   begin
      return Val.Data.Str_Value.Str;
   end Get;

   function Get (Val : JSON_Value) return UTF8_Unbounded_String is
   begin
      return To_Unbounded_String (Val.Data.Str_Value.Str.To_String);
   end Get;

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String) return JSON_Value
   is
      use Object_Items_Pkg;
      Vals : Object_Items_Pkg.Map renames Val.Data.Obj_Value.Vals;
      Result : Object_Items_Pkg.Cursor;
   begin
      Result := Find (Vals, To_XString (Field));
      if Has_Element (Result) then
         return Element (Result);
      else
         return JSON_Null;
      end if;
   end Get;

   function Get (Val : JSON_Value) return JSON_Array is
   begin
      return Val.Data.Arr_Value.Arr;
   end Get;

   ---------------
   -- Has_Field --
   ---------------

   function Has_Field (Val : JSON_Value; Field : UTF8_String) return Boolean is
      use Object_Items_Pkg;
      Vals : Object_Items_Pkg.Map renames Val.Data.Obj_Value.Vals;
   begin
      return Has_Element (Find (Vals, To_XString (Field)));
   end Has_Field;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value; Field : UTF8_String) return Boolean is
   begin
      return Get (Get (Val, Field));
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Integer is
   begin
      return Get (Get (Val, Field));
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Long_Integer is
   begin
      return Get (Get (Val, Field));
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Float is
   begin
      return Get (Get (Val, Field));
   end Get;

   function Get_Long_Float
     (Val : JSON_Value; Field : UTF8_String) return Long_Float is
   begin
      return Get_Long_Float (Get (Val, Field));
   end Get_Long_Float;

   function Get (Val : JSON_Value; Field : UTF8_String) return UTF8_String is
   begin
      return Get (Get (Val, Field));
   end Get;

   function Get
     (Val : JSON_Value; Field : UTF8_String) return UTF8_Unbounded_String
   is
   begin
      return Get (Get (Val, Field));
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return JSON_Array is
   begin
      return Get (Get (Val, Field));
   end Get;

   -----------
   -- Clone --
   -----------

   function Clone (Val : JSON_Value) return JSON_Value is
   begin
      case Val.Data.Kind is
         when JSON_Null_Type =>
            return JSON_Null;

         when JSON_Boolean_Type =>
            return Create (Val.Data.Bool_Value);

         when JSON_Int_Type =>
            return Create (Val.Data.Int_Value);

         when JSON_Float_Type =>
            return Create (Val.Data.Flt_Value);

         when JSON_String_Type =>
            return Create (Val.Data.Str_Value.Str);

         when JSON_Array_Type =>
            declare
               Result : constant JSON_Value :=
                 (Ada.Finalization.Controlled with
                  Data => (Kind => JSON_Array_Type,
                           Arr_Value => new JSON_Array_Internal));
            begin
               for E of Val.Data.Arr_Value.Arr.Vals loop
                  Append (Result.Data.Arr_Value.Arr, Clone (E));
               end loop;
               return Result;
            end;

         when JSON_Object_Type =>
            declare
               use Object_Items_Pkg;
               Result : constant JSON_Value := Create_Object;
               From_Cursor : Cursor := Val.Data.Obj_Value.Vals.First;
            begin
               while Has_Element (From_Cursor) loop
                  Result.Set_Field
                     (Key (From_Cursor), Clone (Element (From_Cursor)));
                  Next (From_Cursor);
               end loop;
               return Result;
            end;
      end case;
   end Clone;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : JSON_Value) return Boolean is
   begin
      if Left.Data.Kind /= Right.Data.Kind then
         return False;
      end if;

      case Left.Data.Kind is
         when JSON_Null_Type =>
            return True;

         when JSON_Boolean_Type =>
            return Left.Data.Bool_Value = Right.Data.Bool_Value;

         when JSON_Int_Type =>
            return Left.Data.Int_Value = Right.Data.Int_Value;

         when JSON_Float_Type =>
            return Left.Data.Flt_Value = Right.Data.Flt_Value;

         when JSON_String_Type =>
            return Left.Data.Str_Value.all = Right.Data.Str_Value.all;

         when JSON_Array_Type =>
            --  Same pointer ?
            if Left.Data.Arr_Value = Right.Data.Arr_Value then
               return True;
            elsif Left.Data.Arr_Value.Arr.Vals.Length /=
              Right.Data.Arr_Value.Arr.Vals.Length
            then
               return False;
            else
               return
                 (for all J in
                    Left.Data.Arr_Value.Arr.Vals.First_Index ..
                      Left.Data.Arr_Value.Arr.Vals.Last_Index
                  =>
                    (Left.Data.Arr_Value.Arr.Vals (J) =  --  recursive

                     Right.Data.Arr_Value.Arr.Vals (J)));
            end if;

         when JSON_Object_Type =>
            --  Same pointer ?
            if Left.Data.Obj_Value = Right.Data.Obj_Value then
               return True;
            elsif Left.Data.Obj_Value.Vals.Length /=
              Right.Data.Obj_Value.Vals.Length
            then
               return False;
            else
               declare
                  use Object_Items_Pkg;
               begin
                  return Left.Data.Obj_Value.Vals =
                     Right.Data.Obj_Value.Vals;
               end;
            end if;
      end case;
   end "=";

   ---------------------
   -- Map_JSON_Object --
   ---------------------

   procedure Map_JSON_Object
     (Val : JSON_Value;
      CB  : access procedure (Name : UTF8_String; Value : JSON_Value))
   is
      use Object_Items_Pkg;
      C : Cursor := Val.Data.Obj_Value.Vals.First;
   begin
      while Has_Element (C) loop
         CB (To_String (Key (C)), Element (C));
         Next (C);
      end loop;
   end Map_JSON_Object;

   ---------------------
   -- Map_JSON_Object --
   ---------------------

   procedure Gen_Map_JSON_Object
     (Val         : JSON_Value;
      CB          : access procedure
        (User_Object : in out Mapped;
         Name        : UTF8_String;
         Value       : JSON_Value);
      User_Object : in out Mapped)
   is
      procedure Internal (Name : UTF8_String; Value : JSON_Value);

      --------------
      -- Internal --
      --------------

      procedure Internal (Name : UTF8_String; Value : JSON_Value) is
      begin
         CB (User_Object, Name, Value);
      end Internal;

   begin
      Map_JSON_Object (Val, Internal'Access);
   end Gen_Map_JSON_Object;

end GNATCOLL.JSON;
