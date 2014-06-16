------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2011-2014, AdaCore                     --
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
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNATCOLL.JSON.Utility;

-------------------
-- GNATCOLL.JSON --
-------------------

package body GNATCOLL.JSON is

   procedure Report_Error (File : String; Line, Col : Natural; Msg : String);
   pragma No_Return (Report_Error);

   procedure Write
     (Item    : JSON_Value;
      Compact : Boolean;
      Indent  : Natural;
      Ret     : in out Unbounded_String);
   --  Auxiliary write function

   function Read
     (Strm     :        Unbounded_String;
      Idx      : access Natural;
      Col      : access Natural;
      Line     : access Natural;
      Filename : String) return JSON_Value;
   --  ???

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (File : String; Line, Col : Natural; Msg : String) is
      L : constant String := Line'Img;
      C : constant String := Col'Img;
   begin
      Ada.Text_IO.New_Line;

      if File = "" then
         Ada.Text_IO.Put ("<stream>:");
      else
         Ada.Text_IO.Put (File & ":");
      end if;

      Ada.Text_IO.Put_Line
        (L (L'First + 1 .. L'Last) & ":" & C (C'First + 1 .. C'Last) & ": " &
         Msg);
      raise Invalid_JSON_Stream with Msg;
   end Report_Error;

   ----------
   -- Read --
   ----------

   function Read
     (Strm     :        Unbounded_String;
      Idx      : access Natural;
      Col      : access Natural;
      Line     : access Natural;
      Filename : String) return JSON_Value
   is
      procedure Error (Msg : String);
      pragma No_Return (Error);

      procedure Next_Char (N : Natural := 1);

      procedure Skip_Blancks;
      --  Does Idx + 1 until a non-blanck character is found

      function Read_String return UTF8_Unbounded_String;
      --  Reads a string

      -----------
      -- Error --
      -----------

      procedure Error (Msg : String) is
      begin
         Report_Error (Filename, Line.all, Col.all, Msg);
      end Error;

      ---------------
      -- Next_Char --
      ---------------

      procedure Next_Char (N : Natural := 1) is
      begin
         if N > 1 then
            for J in 1 .. N - 1 loop
               Next_Char;
            end loop;
         end if;

         Idx.all := Idx.all + 1;
         if Idx.all > Length (Strm) then
            Col.all := Col.all + 1;
         elsif Element (Strm, Idx.all) = ASCII.CR then
            Col.all := 0;
         elsif Element (Strm, Idx.all) = ASCII.LF then
            Col.all := 1;
            Line.all := Line.all + 1;
         else
            Col.all := Col.all + 1;
         end if;
      end Next_Char;

      ------------------
      -- Skip_Blancks --
      ------------------

      procedure Skip_Blancks is
      begin
         while Idx.all <= Length (Strm) loop
            exit when Element (Strm, Idx.all) /= ' '
              and then Element (Strm, Idx.all) /= ASCII.HT
              and then Element (Strm, Idx.all) /= ASCII.CR
              and then Element (Strm, Idx.all) /= ASCII.LF;

            Next_Char;
         end loop;
      end Skip_Blancks;

      -----------------
      -- Read_String --
      -----------------

      function Read_String return UTF8_Unbounded_String is
         Prev : Natural;

      begin
         Prev := Idx.all;
         while Idx.all < Length (Strm) loop
            Next_Char;
            if Element (Strm, Idx.all) = '\' then
               Next_Char;
            elsif Element (Strm, Idx.all) = '"' then
               exit;
            end if;
         end loop;

         if Idx.all > Length (Strm)
           or else Element (Strm, Idx.all) /= '"'
         then
            Error ("Invalid string: cannot find ending """);
         end if;

         --  Skip the trailing '"'
         Next_Char;

         return Utility.Un_Escape_String (Strm, Prev, Idx.all - 1);
      end Read_String;

   begin
      Skip_Blancks;

      if Idx.all not in 1 .. Length (Strm) then
         Error ("Nothing to read from stream");
      end if;

      case Element (Strm, Idx.all) is
         when 'n' | 'N' =>
            --  null
            if To_Lower (Slice (Strm, Idx.all, Idx.all + 3)) /= "null" then
               Error ("Invalid token");
            end if;

            Next_Char (4);

            return Create;

         when 't' =>
            --  true
            if To_Lower (Slice (Strm, Idx.all, Idx .all + 3)) /= "true" then
               Error ("Invalid token");
            end if;

            Next_Char (4);

            return Create (True);

         when 'f' =>
            --  false
            if To_Lower (Slice (Strm, Idx.all, Idx.all + 4)) /= "false" then
               Error ("Invalid token");
            end if;

            Next_Char (5);

            return Create (False);

         when '-' | '0' .. '9' =>
            --  Numerical value

            declare
               type Num_Part is (Trail, Int, Frac, Exp);
               Unb      : Unbounded_String;
               Part     : Num_Part := Trail;
               Old_Col  : constant Natural := Col.all;
               Old_Line : constant Natural := Line.all;

            begin
               --  Potential initial '-'
               if Element (Strm, Idx.all) = '-' then
                  Append (Unb, Element (Strm, Idx.all));
                  Next_Char;

                  if Idx.all > Length (Strm)
                    or else Element (Strm, Idx.all) not in '0' .. '9'
                  then
                     Error
                       ("Expecting a digit after the initial '-' when " &
                        "decoding a number");
                  end if;
               end if;

               while Idx.all <= Length (Strm) loop
                  if Part = Trail
                    and then Element (Strm, Idx.all) in '1' .. '9'
                  then
                     --  Non-0 value, we can start adding the digits to the
                     --  Int part of the number
                     Part := Int;

                  elsif (Part = Trail or else Part = Int)
                    and then Element (Strm, Idx.all) = '.'
                  then
                     if Part = Trail then
                        Append (Unb, "0");
                     end if;

                     Part := Frac;
                     Append (Unb, Element (Strm, Idx.all));
                     Next_Char;

                     if Idx.all > Length (Strm)
                       or else Element (Strm, Idx.all) not in '0' .. '9'
                     then
                        Error ("Expecting digits after a '.' when decoding " &
                               "a number");
                     end if;

                  elsif Part /= Exp
                    and then (Element (Strm, Idx.all) = 'e'
                                or else Element (Strm, Idx.all) = 'E')
                  then
                     if Part = Trail then
                        --  Although legal, so handled here, this case is
                        --  a number of the form 0e99.
                        Append (Unb, "0");
                     end if;

                     --  Authorized patterns for exponent: (e|E)(+|-)?[0-9]+
                     Part := Exp;
                     Append (Unb, Element (Strm, Idx.all));
                     Next_Char;

                     if Idx.all > Length (Strm) then
                        Error ("Invalid number");
                     end if;

                     if Element (Strm, Idx.all) = '+'
                       or else Element (Strm, Idx.all) = '-'
                     then
                        Append (Unb, Element (Strm, Idx.all));
                        Next_Char;
                     end if;

                     if Idx.all > Length (Strm)
                       or else Element (Strm, Idx.all) not in '0' .. '9'
                     then
                        Error ("Expecting digits after 'e' when decoding " &
                               "a number");
                     end if;
                  end if;

                  exit when Idx.all > Length (Strm)
                    or else Element (Strm, Idx.all) not in '0' .. '9';

                  --  Ignore trailing zeros
                  if Part /= Trail then
                     Append (Unb, Element (Strm, Idx.all));
                  end if;

                  Next_Char;
               end loop;

               if Part = Trail then
                  --  The number only contains zeros
                  return Create (Long_Long_Integer'(0));

               elsif Part = Int then
                  --  Protect against too large values to fit in the stack: a
                  --  128-bit integer is maximum 39 digits, so any longer
                  --  string won't fit into an integer, whatever the CPU.
                  --  Note that the string representation is already striped
                  --  of trailing zeros, see the decoding part above.
                  if Length (Unb) > 40 then
                     Report_Error
                       (Filename, Old_Line, Old_Col,
                        "Numerical value too large to fit " &
                          "into a Long_Long_Integer");
                  end if;

                  declare
                     Int : Long_Long_Integer;
                  begin
                     Int := Long_Long_Integer'Value (To_String (Unb));

                     return Create (Int);

                  exception
                     when Constraint_Error | Storage_Error =>
                        --  The test above is not sufficient ... We still catch
                        --  too large values here.
                        Report_Error
                          (Filename, Old_Line, Old_Col,
                           "Numerical value too large to fit " &
                             "into a Long_Long_Integer");
                  end;
               else
                  declare
                     Flt : Long_Float;
                  begin
                     Flt := Long_Float'Value (To_String (Unb));

                     if not Flt'Valid then
                        raise Constraint_Error;
                     end if;

                     return Create (Flt);
                  exception
                     when Constraint_Error =>
                        Report_Error
                          (Filename, Old_Line, Old_Col,
                           "Numerical value too large to fit " &
                             "into an IEEE 754 float");
                  end;
               end if;
            end;

         when '"' =>
            return Create (Read_String);

         when '[' =>
            declare
               Arr   : constant JSON_Array_Access := new JSON_Array;
               First : Boolean := True;

            begin
               --  Skip '['
               Next_Char;

               while Idx.all < Length (Strm) loop
                  Skip_Blancks;

                  if Idx.all > Length (Strm) then
                     Error ("Uncomplete JSON array");
                  end if;

                  exit when Element (Strm, Idx.all) = ']';

                  if not First then
                     if Element (Strm, Idx.all) /= ',' then
                        Error ("Expected ',' in the array value");
                     end if;

                     --  Skip the coma
                     Next_Char;
                  end if;

                  First := False;
                  Append (Arr.all, Read (Strm, Idx, Col, Line, Filename));
               end loop;

               if Idx.all > Length (Strm)
                 or else Element (Strm, Idx.all) /= ']'
               then
                  Error ("Unfinished array, expecting ending ']'");
               end if;

               Next_Char;

               declare
                  Ret : JSON_Value;
               begin
                  Ret.Data := (Kind => JSON_Array_Type, Arr_Value => Arr);
                  return Ret;
               end;
            end;

         when '{' =>
            declare
               First : Boolean := True;
               Ret   : JSON_Value;

            begin
               --  Allocate internal container
               Ret.Data := (Kind => JSON_Object_Type,
                            Obj_Value => new JSON_Object_Internal);

               --  Skip '{'
               Next_Char;

               while Idx.all < Length (Strm) loop
                  Skip_Blancks;

                  if Idx.all > Length (Strm) then
                     Error ("Unterminated object value");
                  end if;

                  exit when Element (Strm, Idx.all) = '}';

                  if not First then
                     if Element (Strm, Idx.all) /= ',' then
                        Error ("Expected ',' as object value separator");
                     end if;

                     --  Skip the coma
                     Next_Char;
                  end if;

                  First := False;
                  Skip_Blancks;

                  declare
                     Name : constant UTF8_Unbounded_String := Read_String;
                  begin
                     Skip_Blancks;

                     if Idx.all > Length (Strm) then
                        Error ("Unterminated object value");
                     end if;

                     if Element (Strm, Idx.all) /= ':' then
                        Error
                          ("Expected a value after the name in a JSON object"
                           & " at index" & Idx.all'Img);
                     end if;

                     --  Skip the semi-colon
                     Next_Char;

                     declare
                        Item : constant JSON_Value :=
                                 Read (Strm, Idx, Col, Line, Filename);
                     begin
                        Set_Field (Ret, To_String (Name), Item);
                     end;
                  end;
               end loop;

               if Idx.all > Length (Strm)
                 or else Element (Strm, Idx.all) /= '}'
               then
                  Error ("Unterminated object value");
               end if;

               Next_Char;

               return Ret;
            end;

         when others =>
            Error ("Unexpected token");
            raise Invalid_JSON_Stream;
      end case;
   end Read;

   function Read
     (Strm     : Unbounded_String;
      Filename : String := "<data>") return JSON_Value
   is
      Idx  : aliased Natural := 1;
      Col  : aliased Natural := 1;
      Line : aliased Natural := 1;
   begin
      return Read (Strm, Idx'Access, Col'Access, Line'Access, Filename);
   end Read;

   function Read
     (Strm     : String;
      Filename : String := "<data>") return JSON_Value
   is
   begin
      return Read (To_Unbounded_String (Strm), Filename);
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
            Append (Ret, JSON.Utility.Escape_String (Item.Data.Str_Value));

         when JSON_Array_Type =>
            Append (Ret, '[');

            if not Compact then
               Append (Ret, ASCII.LF);
            end if;

            for J in Item.Data.Arr_Value.Vals.First_Index ..
              Item.Data.Arr_Value.Vals.Last_Index
            loop
               Do_Indent (Indent + 1);
               Write
                 (Item.Data.Arr_Value.Vals.Element (J),
                  Compact, Indent + 1, Ret);

               if J < Item.Data.Arr_Value.Vals.Last_Index then
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
                  Append (Ret, """");
                  Append (Ret, Element (J).Key);
                  Append (Ret, """: ");
                  Write (Element (J).Val, Compact, Indent + 1, Ret);

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

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Obj : in out JSON_Value) is
   begin
      Obj.Cnt := new Natural'(1);
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Obj : in out JSON_Value) is
   begin
      if Obj.Cnt /= null then
         --  Cnt is null for JSON_Null, and we do not want to do reference
         --  counting for it.
         Obj.Cnt.all := Obj.Cnt.all + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Obj : in out JSON_Value) is
      C : Counter := Obj.Cnt;
   begin
      if C = null then
         return;
      end if;

      Obj.Cnt := null;
      --  Prevent multiple calls to Finalize, which is valid in Ada

      C.all := C.all - 1;

      if C.all = 0 then
         Free (C);

         case Obj.Kind is
            when JSON_Null_Type    |
                 JSON_Boolean_Type |
                 JSON_Int_Type     |
                 JSON_Float_Type   =>
               null;
            when JSON_String_Type =>
               Obj.Data.Str_Value := Null_Unbounded_String;

            when JSON_Array_Type =>
               if Obj.Data.Arr_Value /= null then
                  Free (Obj.Data.Arr_Value);
               end if;

            when JSON_Object_Type =>
               if Obj.Data.Obj_Value /= null then
                  Free (Obj.Data.Obj_Value);
               end if;

         end case;
      end if;
   end Finalize;

   ------------
   -- Create --
   ------------

   function Create return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Data := (Kind => JSON_Null_Type);
      return Ret;
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
      Ret.Data := (JSON_String_Type, Str_Value => To_Unbounded_String (Val));
      return Ret;
   end Create;

   function Create (Val : UTF8_Unbounded_String) return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Data := (Kind => JSON_String_Type, Str_Value => Val);
      return Ret;
   end Create;

   function Create (Val : JSON_Array) return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Data := (Kind => JSON_Array_Type, Arr_Value => new JSON_Array'(Val));
      return Ret;
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

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : JSON_Value)
   is
      Key : constant UTF8_Unbounded_String :=
              To_Unbounded_String (Field_Name);
      Vals : Object_Items_Pkg.Vector renames Val.Data.Obj_Value.Vals;
   begin
      for J in Vals.First_Index .. Vals.Last_Index loop
         if Key = Vals.Element (J).Key then
            Vals.Replace_Element (J, (Key, Field));

            return;
         end if;
      end loop;

      Vals.Append
        ((Key => To_Unbounded_String (Field_Name),
          Val => Field));
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

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Long_Float) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

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

   function Get (Val : JSON_Value) return Long_Float is
   begin
      return Val.Data.Flt_Value;
   end Get;

   function Get (Val : JSON_Value) return UTF8_String is
   begin
      return To_String (Val.Data.Str_Value);
   end Get;

   function Get (Val : JSON_Value) return UTF8_Unbounded_String is
   begin
      return Val.Data.Str_Value;
   end Get;

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String) return JSON_Value
   is
      Key : constant UTF8_Unbounded_String :=
              To_Unbounded_String (Field);
      Vals : Object_Items_Pkg.Vector renames Val.Data.Obj_Value.Vals;
   begin
      for J in Vals.First_Index .. Vals.Last_Index loop
         if Key = Vals.Element (J).Key then
            return Vals.Element (J).Val;
         end if;
      end loop;

      return JSON_Null;
   end Get;

   function Get (Val : JSON_Value) return JSON_Array is
   begin
      return Val.Data.Arr_Value.all;
   end Get;

   ---------------
   -- Has_Field --
   ---------------

   function Has_Field (Val : JSON_Value; Field : UTF8_String) return Boolean is
      Key : constant UTF8_Unbounded_String :=
              To_Unbounded_String (Field);
      Vals : Object_Items_Pkg.Vector renames Val.Data.Obj_Value.Vals;
   begin
      for J in Vals.First_Index .. Vals.Last_Index loop
         if Key = Vals.Element (J).Key then
            return True;
         end if;
      end loop;

      return False;
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

   function Get (Val : JSON_Value; Field : UTF8_String) return Long_Float is
   begin
      return Get (Get (Val, Field));
   end Get;

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

   ---------------------
   -- Map_JSON_Object --
   ---------------------

   procedure Map_JSON_Object
     (Val : JSON_Value;
      CB  : access procedure (Name : UTF8_String; Value : JSON_Value))
   is
      Vals : Object_Items_Pkg.Vector renames Val.Data.Obj_Value.Vals;
   begin
      for J in Vals.First_Index .. Vals.Last_Index loop
         CB (To_String (Vals.Element (J).Key), Vals.Element (J).Val);
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
