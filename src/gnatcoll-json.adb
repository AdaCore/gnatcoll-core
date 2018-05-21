------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2011-2018, AdaCore                     --
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
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Atomic;         use GNATCOLL.Atomic;
with GNATCOLL.JSON.Utility;
with GNATCOLL.Strings;        use GNATCOLL.Strings;

package body GNATCOLL.JSON is

   type Text_Position is record
      Index  : Natural := 0;
      Line   : Natural := 0;
      Column : Natural := 0;
   end record;
   --  Record to represent position in a given text

   type Token_Kind is
     (J_NULL,
      J_TRUE,
      J_FALSE,
      J_NUMBER,
      J_INTEGER,
      J_STRING,
      J_ARRAY,
      J_OBJECT,
      J_ARRAY_END,
      J_OBJECT_END,
      J_COMMA,
      J_COLON,
      J_EOF);
   --  JSON Token kinds. Note that in ECMA 404 there is no notion of integer.
   --  Only numbers are supported. In our implementation we return J_INTEGER
   --  if there is no decimal part in the number. The semantic is that this is
   --  a J_NUMBER token that "might" be represented as an integer. Special
   --  token J_EOF means that end of stream has been reached.

   function Is_Value (TK : Token_Kind) return Boolean;
   pragma Inline (Is_Value);
   --  Return True if the token kind is a JSON value: null, false, true,
   --  a string, a number, an array or an object.

   procedure Free is
     new Ada.Unchecked_Deallocation (JSON_Array_Internal, JSON_Array_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (JSON_Object_Internal, JSON_Object_Access);

   procedure Report_Error (File : String; Line, Col : Natural; Msg : String);
   pragma No_Return (Report_Error);

   procedure Write
     (Item    : JSON_Value;
      Compact : Boolean;
      Indent  : Natural;
      Ret     : in out Unbounded_String);
   --  Auxiliary write function

   function Read
     (Strm      : String;
      Pos       : in out Text_Position;
      Kind      : out Token_Kind;
      Filename  : String;
      Check_EOF : Boolean := False)
      return JSON_Value;
   --  Internal function that reads a JSON stream.
   --
   --  Strm is the content to decode,
   --  Pos is the current position in the Strm,
   --  Kind is set to the last read token kind,
   --  Filename is the filename of corresponding to the content Strm (used for
   --  for error reporting only).
   --  If Check_EOF is set to True, check before returning the JSON value that
   --  we have reached the end of the stream.

   function Read_Token
     (Strm        : String;
      Filename    : String;
      Pos         : in out Text_Position;
      Token_Start : out Text_Position;
      Token_End   : out Text_Position)
      return Token_Kind;
   --  Read a token
   --
   --  Strm is the content to decode,
   --  Filename is the filename of the decoded content (error reporting)
   --  Pos is the current position in Strm
   --  Token_Start are Token_End are respectively the position of the first and
   --  last character of the token (outside boundaries of Strm if the return
   --  token is J_EOF).

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

   --------------
   -- Is_Value --
   --------------
   function Is_Value (TK : Token_Kind) return Boolean is
   begin
      return TK = J_NULL or else
        TK = J_TRUE or else
        TK = J_FALSE or else
        TK = J_STRING or else
        TK = J_ARRAY or else
        TK = J_OBJECT or else
        TK = J_INTEGER or else
        TK = J_NUMBER;
   end Is_Value;

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

   ----------------
   -- Read_Token --
   ----------------

   function Read_Token
     (Strm        : String;
      Filename    : String;
      Pos         : in out Text_Position;
      Token_Start : out Text_Position;
      Token_End   : out Text_Position)
       return Token_Kind
   is
      procedure Next_Char;
      --  Update Pos to point to next char

      function Is_Whitespace return Boolean;
      pragma Inline (Is_Whitespace);
      --  Return True of current character is a whitespace

      function Is_Structural_Token return Boolean;
      pragma Inline (Is_Structural_Token);
      --  Return True if current character is one of the structural tokens

      function Is_Token_Sep return Boolean;
      pragma Inline (Is_Token_Sep);
      --  Return True if current character is a token separator

      procedure Error (Msg : String);
      pragma No_Return (Error);

      procedure Delimit_Keyword (Kw : String);
      --  Helper function to parse tokens such as null, false and true

      -----------
      -- Error --
      -----------

      procedure Error (Msg : String) is
      begin
         Report_Error (Filename, Pos.Line, Pos.Column, Msg);
      end Error;

      ---------------
      -- Next_Char --
      ---------------

      procedure Next_Char is
      begin
         Pos.Index := Pos.Index + 1;
         if Pos.Index > Strm'Last then
            Pos.Column := Pos.Column + 1;
         elsif Strm (Pos.Index) = ASCII.LF then
            Pos.Column := 1;
            Pos.Line := Pos.Line + 1;
         else
            Pos.Column := Pos.Column + 1;
         end if;
      end Next_Char;

      -------------------
      -- Is_Whitespace --
      -------------------

      function Is_Whitespace return Boolean is
      begin
         return Pos.Index <= Strm'Last and then
           (Strm (Pos.Index) = ASCII.LF or else
            Strm (Pos.Index) = ASCII.CR or else
            Strm (Pos.Index) = ASCII.HT or else
            Strm (Pos.Index) = ' ');
      end Is_Whitespace;

      -------------------------
      -- Is_Structural_Token --
      -------------------------

      function Is_Structural_Token return Boolean is
      begin
         return Pos.Index <= Strm'Last and then
           (Strm (Pos.Index) = '[' or else
            Strm (Pos.Index) = ']' or else
            Strm (Pos.Index) = '{' or else
            Strm (Pos.Index) = '}' or else
            Strm (Pos.Index) = ',' or else
            Strm (Pos.Index) = ':');
      end Is_Structural_Token;

      ------------------
      -- Is_Token_Sep --
      ------------------

      function Is_Token_Sep return Boolean is
      begin
         return Pos.Index > Strm'Last or else
           Is_Whitespace or else
           Is_Structural_Token;
      end Is_Token_Sep;

      ---------------------
      -- Delimit_Keyword --
      ---------------------

      procedure Delimit_Keyword (Kw : String) is
      begin
         while not Is_Token_Sep loop
            Token_End := Pos;
            Next_Char;
         end loop;
         if Strm (Token_Start.Index .. Token_End.Index) /= Kw then
            Error ("invalid keyword starting with: " &
                     Strm (Token_Start.Index .. Token_End.Index));
         end if;
      end Delimit_Keyword;

      CC             : Character;
      Can_Be_Integer : Boolean := True;
   begin
      --  Skip leading whitespaces
      while Is_Whitespace loop
         Next_Char;
      end loop;

      --  Initialize token delimiters
      Token_Start := Pos;
      Token_End   := Pos;

      --  End of stream reached
      if Pos.Index > Strm'Last then
         return J_EOF;
      end if;

      CC := Strm (Pos.Index);

      if CC = '[' then
         Next_Char;
         return J_ARRAY;
      elsif CC = ']' then
         Next_Char;
         return J_ARRAY_END;
      elsif CC = '{' then
         Next_Char;
         return J_OBJECT;
      elsif CC = '}' then
         Next_Char;
         return J_OBJECT_END;
      elsif CC = ',' then
         Next_Char;
         return J_COMMA;
      elsif CC = ':' then
         Next_Char;
         return J_COLON;
      elsif CC = 'n' then
         Delimit_Keyword ("null");
         return J_NULL;
      elsif CC = 'f' then
         Delimit_Keyword ("false");
         return J_FALSE;
      elsif CC = 't' then
         Delimit_Keyword ("true");
         return J_TRUE;
      elsif CC = '"' then
         --  We expect a string
         --  Just scan till the end the of the string but do not attempt
         --  to decode it. This means that even if we get a string token
         --  it might not be a valid string from the ECMA 404 point of
         --  view.
         Next_Char;
         while Pos.Index <= Strm'Last and then Strm (Pos.Index) /= '"' loop
            if Strm (Pos.Index) in ASCII.NUL .. ASCII.US then
               Error ("control character not allowed in string");
            end if;

            if Strm (Pos.Index) = '\' then
               Next_Char;
               if Pos.Index > Strm'Last then
                  Error ("non terminated string token");
               end if;

               case Strm (Pos.Index) is
                  when 'u' =>
                     for Idx in 1 .. 4 loop
                        Next_Char;
                        if Pos.Index > Strm'Last or else
                          (Strm (Pos.Index) not in 'a' .. 'f' and then
                           Strm (Pos.Index) not in 'A' .. 'F' and then
                           Strm (Pos.Index) not in '0' .. '9')
                        then
                           Error ("invalid unicode escape sequence");
                        end if;

                     end loop;

                  when '\' | '/' | '"' | 'b' | 'f' | 'n' | 'r' | 't' =>
                     null;
                  when others =>
                     Error ("invalid escape sequence");
               end case;
            end if;
            Next_Char;
         end loop;

         --  No quote found report and error
         if Pos.Index > Strm'Last then
            Error ("non terminated string token");
         end if;

         Token_End := Pos;

         --  Go to next char and ensure that this is separator. Indeed
         --  construction such as "string1""string2" are not allowed
         Next_Char;
         if not Is_Token_Sep then
            Error ("invalid syntax");
         end if;
         return J_STRING;
      elsif CC = '-' or else CC in '0' .. '9' then
         --  We expect a number
         if CC = '-' then
            Next_Char;
         end if;

         if Pos.Index > Strm'Last then
            Error ("invalid number");
         end if;

         --  Parse integer part of a number. Superfluous leading zeros are not
         --  allowed.
         if Strm (Pos.Index) = '0' then
            Token_End := Pos;
            Next_Char;
         elsif Strm (Pos.Index) in '1' .. '9' then
            Token_End := Pos;
            Next_Char;
            while Pos.Index <= Strm'Last and then
              Strm (Pos.Index) in '0' .. '9'
            loop
               Token_End := Pos;
               Next_Char;
            end loop;
         else
            Error ("invalid number");
         end if;

         if Is_Token_Sep then
            --  Valid integer number
            return J_INTEGER;
         elsif Strm (Pos.Index) /= '.' and then
           Strm (Pos.Index) /= 'e' and then
           Strm (Pos.Index) /= 'E'
         then
            Error ("invalid number");
         end if;

         --  Check for a fractional part
         if Strm (Pos.Index) = '.' then
            Can_Be_Integer := False;
            Token_End := Pos;
            Next_Char;
            if Pos.Index > Strm'Last or else
              Strm (Pos.Index) not in '0' .. '9'
            then
               Error ("invalid number");
            end if;

            while Pos.Index <= Strm'Last and then
              Strm (Pos.Index) in '0' .. '9'
            loop
               Token_End := Pos;
               Next_Char;
            end loop;

         end if;

         --  Check for exponent part
         if Pos.Index <= Strm'Last and then
           (Strm (Pos.Index) = 'e' or else Strm (Pos.Index) = 'E')
         then
            Token_End := Pos;
            Next_Char;
            if Pos.Index > Strm'Last then
               Error ("invalid number");
            end if;

            if Strm (Pos.Index) = '-' then
               --  Also a few corner cases can lead to an integer, assume that
               --  the number is not an integer.
               Can_Be_Integer := False;
            end if;

            if Strm (Pos.Index) = '-' or else Strm (Pos.Index) = '+' then
               Next_Char;
            end if;

            if Pos.Index > Strm'Last or else
              Strm (Pos.Index) not in '0' .. '9'
            then
               Error ("invalid number");
            end if;

            while Pos.Index <= Strm'Last and then
              Strm (Pos.Index) in '0' .. '9'
            loop
               Token_End := Pos;
               Next_Char;
            end loop;
         end if;

         if Is_Token_Sep then
            --  Valid decimal number
            if Can_Be_Integer then
               return J_INTEGER;
            else
               return J_NUMBER;
            end if;
         else
            Error ("invalid number");
         end if;
      else
         Error ("Unexpected character '" & CC & ''');
      end if;
   end Read_Token;

   ----------
   -- Read --
   ----------

   function Read
     (Strm      : String;
      Pos       : in out Text_Position;
      Kind      : out Token_Kind;
      Filename  : String;
      Check_EOF : Boolean := False)
      return JSON_Value
   is

      procedure Error (Msg : String);
      pragma No_Return (Error);

      -----------
      -- Error --
      -----------

      procedure Error (Msg : String) is
      begin
         Report_Error (Filename, Pos.Line, Pos.Column, Msg);
      end Error;

      Token_Start, Token_End : Text_Position;
      TK                     : Token_Kind;
      Result                 : JSON_Value;
   begin
      TK := Read_Token (Strm, Filename, Pos, Token_Start, Token_End);
      if TK = J_EOF then
         Error ("empty stream");
      end if;

      Kind := TK;

      case TK is
         when J_NULL =>
            Result :=  Create;
         when J_FALSE =>
            Result := Create (False);
         when J_TRUE =>
            Result := Create (True);
         when J_STRING =>
            Result := Create (Utility.Un_Escape_String
                              (Strm, Token_Start.Index, Token_End.Index));
         when J_ARRAY =>
            declare
               Arr : constant JSON_Array_Access := new JSON_Array_Internal;
               ST       : Token_Kind;
               Element  : JSON_Value;
               Is_First : Boolean := True;
            begin
               loop
                  Element := Read (Strm, Pos, ST, Filename);
                  if Is_First and then ST = J_ARRAY_END then
                     exit;
                  elsif Is_Value (ST) then
                     Append (Arr.Arr, Element);
                     Element := Read (Strm, Pos, ST, Filename);
                     if ST = J_ARRAY_END then
                        exit;
                     elsif ST /= J_COMMA then
                        Error ("comma expected");
                     end if;
                  else
                     Error ("syntax error");
                  end if;
                  Is_First := False;
               end loop;
               Result := (Ada.Finalization.Controlled with
                          Data => (Kind => JSON_Array_Type, Arr_Value => Arr));
            end;
         when J_OBJECT =>
            declare
               Is_First   : Boolean := True;
               ST         : Token_Kind;
               Ret        : JSON_Value;
               Key, Value : JSON_Value;
               Key_Str : UTF8_XString;
            begin
               --  Allocate internal container
               Ret.Data := (Kind => JSON_Object_Type,
                            Obj_Value => new JSON_Object_Internal);
               loop
                  Key := Read (Strm, Pos, ST, Filename);
                  if Is_First and then ST = J_OBJECT_END then
                     exit;
                  elsif ST = J_STRING then
                     Value := Read (Strm, Pos, ST, Filename);
                     if ST /= J_COLON then
                        Error ("colon expected");
                     end if;
                     Value := Read (Strm, Pos, ST, Filename);
                     if not Is_Value (ST) then
                        Error ("non expected token");
                     end if;
                     Key_Str := Get (Key);
                     Set_Field (Ret, Key_Str, Value);

                     Value := Read (Strm, Pos, ST, Filename);
                     if ST = J_OBJECT_END then
                        exit;
                     elsif ST /= J_COMMA then
                        Error ("comma expected");
                     end if;
                  else
                     Error ("string value expected");
                  end if;
                  Is_First := False;
               end loop;
               Result := Ret;
            end;
         when J_NUMBER | J_INTEGER =>
            --  This is a number
            declare
               Number_Str : constant String :=
                 Strm (Token_Start.Index .. Token_End.Index);
               Has_Integer : Boolean := False;
            begin
               if TK = J_INTEGER then
                  declare
                     Result_Int : Long_Long_Integer;
                  begin
                     Result_Int := Long_Long_Integer'Value (Number_Str);
                     Result := Create (Result_Int);
                     Has_Integer := True;
                  exception
                     when Constraint_Error =>
                        null;
                  end;
               end if;

               if not Has_Integer then
                  begin
                     Result := Create (Long_Float'Value (Number_Str));
                  exception
                     when Constraint_Error =>
                        Error ("cannot convert JSON number to Long_Float");
                  end;
               end if;
            end;
         when others =>
            if Check_EOF then
               Error ("invalid JSON stream");
            else
               Result := Create;
            end if;
      end case;

      if Check_EOF and then
        Read_Token (Strm, Filename, Pos, Token_Start, Token_End) /= J_EOF
      then
         Error ("additional data after end of JSON stream");
      end if;
      return Result;
   end Read;

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
      Pos : Text_Position := (Strm'First, 1, 1);
      Kind : Token_Kind;
   begin
      return Read (Strm, Pos, Kind, Filename, Check_EOF => True);
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
                     GNATCOLL.JSON.Utility.Escape_String (Element (J).Key));

                  Append (Ret, ':');
                  if not Compact then
                     Append (Ret, ' ');
                  end if;

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
      function "<" (Left, Right : Object_Item) return Boolean;

      function "<" (Left, Right : Object_Item) return Boolean is
      begin
         return Less (Left.Val, Right.Val);
      end "<";

      package Sorting is new Object_Items_Pkg.Generic_Sorting ("<");

   begin
      case Val.Kind is
         when JSON_Array_Type  => Sort (Val.Data.Arr_Value.Arr, Less);
         when JSON_Object_Type => Sorting.Sort (Val.Data.Obj_Value.Vals);
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
      Ret.Data := (JSON_String_Type, Str_Value => <>);
      Ret.Data.Str_Value.Set (Val);
      return Ret;
   end Create;

   function Create (Val : UTF8_Unbounded_String) return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Data := (Kind => JSON_String_Type, Str_Value => Null_XString);
      Ret.Data.Str_Value.Set (To_String (Val));
      return Ret;
   end Create;

   function Create (Val : UTF8_XString) return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Data := (Kind => JSON_String_Type, Str_Value => Val);
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
      Vals : Object_Items_Pkg.Vector renames Val.Data.Obj_Value.Vals;
   begin
      for J in Vals.First_Index .. Vals.Last_Index loop
         if Vals.Element (J).Key = Field_Name then
            Val.Data.Obj_Value.Vals.Delete (J);
            return;
         end if;
      end loop;
   end Unset_Field;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : JSON_Value)
   is
      Vals : Object_Items_Pkg.Vector renames Val.Data.Obj_Value.Vals;
   begin
      for J in Vals.First_Index .. Vals.Last_Index loop
         if Field_Name = Vals.Element (J).Key then
            Vals.Replace_Element (J, (Vals.Element (J).Key, Field));
            return;
         end if;
      end loop;

      Vals.Append
        ((Key => To_XString (Field_Name),
          Val => Field));
   end Set_Field;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_XString;
      Field      : JSON_Value)
   is
      Vals : Object_Items_Pkg.Vector renames Val.Data.Obj_Value.Vals;
   begin
      for J in Vals.First_Index .. Vals.Last_Index loop
         if Field_Name = Vals.Element (J).Key then
            Vals.Replace_Element (J, (Field_Name, Field));
            return;
         end if;
      end loop;

      Vals.Append
        ((Key => Field_Name,
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
      return To_String (Val.Data.Str_Value);
   end Get;

   function Get (Val : JSON_Value) return UTF8_XString is
   begin
      return Val.Data.Str_Value;
   end Get;

   function Get (Val : JSON_Value) return UTF8_Unbounded_String is
   begin
      return To_Unbounded_String (Val.Data.Str_Value.To_String);
   end Get;

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String) return JSON_Value
   is
      Vals : Object_Items_Pkg.Vector renames Val.Data.Obj_Value.Vals;
   begin
      for J in Vals.First_Index .. Vals.Last_Index loop
         if Field = Vals.Element (J).Key then
            return Vals.Element (J).Val;
         end if;
      end loop;

      return JSON_Null;
   end Get;

   function Get (Val : JSON_Value) return JSON_Array is
   begin
      return Val.Data.Arr_Value.Arr;
   end Get;

   ---------------
   -- Has_Field --
   ---------------

   function Has_Field (Val : JSON_Value; Field : UTF8_String) return Boolean is
      Vals : Object_Items_Pkg.Vector renames Val.Data.Obj_Value.Vals;
   begin
      for J in Vals.First_Index .. Vals.Last_Index loop
         if Field = Vals.Element (J).Key then
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
            return Create (Val.Data.Str_Value);

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
               Result : constant JSON_Value := Create_Object;
            begin
               for E of Val.Data.Obj_Value.Vals loop
                  Result.Set_Field (To_String (E.Key), Clone (E.Val));
               end loop;
               return Result;
            end;
      end case;
   end Clone;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : JSON_Value) return Boolean is
      Found : Boolean;
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
            return Left.Data.Str_Value = Right.Data.Str_Value;

         when JSON_Array_Type =>
            --  Same pointer ?
            if Left.Data.Arr_Value = Right.Data.Arr_Value then
               return True;
            elsif Left.Data.Arr_Value.Arr.Vals.Length /=
              Right.Data.Arr_Value.Arr.Vals.Length
            then
               return False;
            else
               for J in Left.Data.Arr_Value.Arr.Vals.First_Index ..
                 Left.Data.Arr_Value.Arr.Vals.Last_Index
               loop
                  if not (Left.Data.Arr_Value.Arr.Vals (J) =  --  recursive
                            Right.Data.Arr_Value.Arr.Vals (J))
                  then
                     return False;
                  end if;
               end loop;
               return True;
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
               --  We have the same number of elements, and no duplicates
               for L of Left.Data.Obj_Value.Vals loop
                  Found := False;
                  for R of Right.Data.Obj_Value.Vals loop
                     if R.Key = L.Key then
                        if not (R.Val = L.Val) then --  recursive
                           return False;
                        end if;
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     return False;
                  end if;
               end loop;
               return True;
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
