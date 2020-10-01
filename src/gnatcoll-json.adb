------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2011-2020, AdaCore                     --
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

package body GNATCOLL.JSON is

   type Text_Position is record
      Index : Natural := 0;
      --  Array index in the input string. For valid positions, this is
      --  positive.

      Line : Natural := 0;
      --  Logical line number. For valid positions, this is positive.

      Column : Natural := 0;
      --  Logical column number. For valid positions, this is positive.
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

   subtype Value_Token is Token_Kind range J_NULL .. J_OBJECT;
   --  Subset of token kinds for JSON values: null, false, true, a string, a
   --  number, an array or an object.

   procedure Free is
     new Ada.Unchecked_Deallocation (JSON_Array_Internal, JSON_Array_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (JSON_Object_Internal, JSON_Object_Access);

   procedure Write
     (Item    : JSON_Value;
      Compact : Boolean;
      Indent  : Natural;
      Ret     : in out Unbounded_String);
   --  Auxiliary write function

   procedure Read
     (Result    : in out Read_Result;
      Strm      : String;
      Pos       : in out Text_Position;
      Kind      : out Token_Kind;
      Check_EOF : Boolean := False)
      with Pre => Result.Success;
   --  Read and decode a JSON value. On success, this returns a Read_Result
   --  with Success => True, otherwise it returns an error with a Success =>
   --  False record.
   --
   --  If Check_EOF is true, return an error if we haven't reached the end of
   --  the input string upon returning. If Check_EOF if false and no value
   --  could be decoded but we still managed to read a token, just skip this
   --  token: in that case, a null JSON value is returned.
   --
   --  Strm is the content to decode,
   --
   --  Pos is the position in Strm from which we start reading. It is updated
   --  to point past that token.
   --
   --  Kind is set to the last read token kind,
   --
   --  Note that we use an IN OUT parameter instead of a mere return value for
   --  the result to avoid a noticeable runtime penalty, probably due to
   --  the secondary stack management involved.

   type Read_Token_Result (Success : Boolean := True) is record
      case Success is
         when True =>
            Kind : Token_Kind;
         when False =>
            Error : Parsing_Error;
      end case;
   end record;

   function Read_Token
     (Strm        : String;
      Pos         : in out Text_Position;
      Token_Start : out Text_Position;
      Token_End   : out Text_Position)
      return Read_Token_Result;
   --  Read a token.
   --
   --  Strm is the content to decode,
   --
   --  Pos is the position in Strm from which the token is read. It is updated
   --  to point past that token.
   --
   --  Token_Start are Token_End are set respectively to the position of the
   --  first and last character of the token (outside boundaries of Strm if the
   --  return token is J_EOF).
   --
   --  If a parsing error is detected, this returns a Read_Token_Error record
   --  with Success => False, including the relevant parsing error information.

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

   ----------------
   -- Read_Token --
   ----------------

   function Read_Token
     (Strm        : String;
      Pos         : in out Text_Position;
      Token_Start : out Text_Position;
      Token_End   : out Text_Position)
      return Read_Token_Result
   is
      procedure Next_Char;
      --  Update Pos to point to next character in Strm

      function Next_Char (Result : Token_Kind) return Read_Token_Result
         with Inline;
      --  Shortcut to call the Next_Char procedure after returning Result

      function Is_Whitespace return Boolean with Inline;
      --  Return True of current character is a whitespace: line feed, carriage
      --  return, space or horizontal tabulation.

      function Is_Structural_Token return Boolean with Inline;
      --  Return True if current character is one of the structural tokens:
      --  brackets, parens, comma or colon.

      function Is_Token_Sep return Boolean with Inline;
      --  Return True if at least of of the following is true:
      --
      --    * we reached the end of input string;
      --    * the current character is a whitespace;
      --    * the current character is a structural token.

      function Error (Msg : String) return Read_Token_Result;
      --  Return a parsing error for the current position and the given error
      --  message.

      function Delimit_Keyword
        (Kw : String; Kind : Token_Kind) return Read_Token_Result;
      --  Advance Pos until we reach the next token separator, updating
      --  Token_End to designate the last character. Return the resulting token
      --  if it matches Kw/Kind, otherwise raise an error.

      -----------
      -- Error --
      -----------

      function Error (Msg : String) return Read_Token_Result is
      begin
         return (Success => False,
                 Error   => (Line    => Pos.Line,
                             Column  => Pos.Column,
                             Message => To_Unbounded_String (Msg)));
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

      function Next_Char (Result : Token_Kind) return Read_Token_Result is
      begin
         Next_Char;
         return (Success => True, Kind => Result);
      end Next_Char;

      -------------------
      -- Is_Whitespace --
      -------------------

      function Is_Whitespace return Boolean is
      begin
         return
           (Pos.Index <= Strm'Last
            and then Strm (Pos.Index) in ASCII.LF | ASCII.CR | ASCII.HT | ' ');
      end Is_Whitespace;

      -------------------------
      -- Is_Structural_Token --
      -------------------------

      function Is_Structural_Token return Boolean is
      begin
         return
           (Pos.Index <= Strm'Last
            and then Strm (Pos.Index) in '[' | ']' | '{' | '}' | ',' | ':');
      end Is_Structural_Token;

      ------------------
      -- Is_Token_Sep --
      ------------------

      function Is_Token_Sep return Boolean is
      begin
         return (Pos.Index > Strm'Last
                 or else Is_Whitespace
                 or else Is_Structural_Token);
      end Is_Token_Sep;

      ---------------------
      -- Delimit_Keyword --
      ---------------------

      function Delimit_Keyword
        (Kw : String; Kind : Token_Kind) return Read_Token_Result is
      begin
         while not Is_Token_Sep loop
            Token_End := Pos;
            Next_Char;
         end loop;
         if Strm (Token_Start.Index .. Token_End.Index) /= Kw then
            return Error ("invalid keyword starting with: "
                          & Strm (Token_Start.Index .. Token_End.Index));
         else
            return (Success => True, Kind => Kind);
         end if;
      end Delimit_Keyword;

      CC : Character;
      --  Buffer for the currently analyzed character

      Can_Be_Integer : Boolean := True;
      --  When reading a number token, this is true if that number can be an
      --  integer: otherwise, it must be interpreted as a decimal number.
   begin
      --  Skip leading whitespaces

      while Is_Whitespace loop
         Next_Char;
      end loop;

      --  Initialize token delimiters

      Token_Start := Pos;
      Token_End   := Pos;

      --  If we reached the end of the input string, just return a EOF token

      if Pos.Index > Strm'Last then
         return (Success => True, Kind => J_EOF);
      end if;

      --  Otherwise, all depends on the first non-whitespace character to read
      --  next...

      CC := Strm (Pos.Index);
      case CC is

      --  Structual tokens are unambiguously designated by standalone
      --  characters.

      when '[' => return Next_Char (J_ARRAY);
      when ']' => return Next_Char (J_ARRAY_END);
      when '{' => return Next_Char (J_OBJECT);
      when '}' => return Next_Char (J_OBJECT_END);
      when ',' => return Next_Char (J_COMMA);
      when ':' => return Next_Char (J_COLON);

      --  Only named value tokens can start with a letter

      when 'n' => return Delimit_Keyword ("null", J_NULL);
      when 'f' => return Delimit_Keyword ("false", J_FALSE);
      when 't' => return Delimit_Keyword ("true", J_TRUE);

      when '"' =>

         --  We expect a string.
         --
         --  Just scan till the end the of the string but do not attempt to
         --  decode it. This means that even if we get a string token it might
         --  not be a valid string from the ECMA 404 point of view.

         Next_Char;
         while Pos.Index <= Strm'Last and then Strm (Pos.Index) /= '"' loop
            CC := Strm (Pos.Index);

            if CC in ASCII.NUL .. ASCII.US then
               return Error ("control character not allowed in string");
            end if;

            if CC = '\' then

               --  This is an escape sequence. Make sure we have at least one
               --  more character to read.

               Next_Char;

               if Pos.Index > Strm'Last then
                  return Error ("non terminated string");
               end if;

               case Strm (Pos.Index) is
                  when 'u' =>
                     --  This is a unicode escape sequence ("\uXXXX")
                     for Idx in 1 .. 4 loop
                        Next_Char;
                        if Pos.Index > Strm'Last then
                           return Error ("non terminated string");
                        elsif Strm (Pos.Index) not in
                           'a' .. 'f' | 'A' .. 'F' | '0' .. '9'
                        then
                           return Error ("invalid unicode escape sequence");
                        end if;
                     end loop;

                  when '\' | '/' | '"' | 'b' | 'f' | 'n' | 'r' | 't' =>
                     --  This is a single-character escape sequence
                     null;

                  when others =>
                     return Error ("invalid escape sequence");
               end case;
            end if;
            Next_Char;
         end loop;

         --  We could not find a closing quote before the end of the input
         --  string: this is an error.

         if Pos.Index > Strm'Last then
            return Error ("non terminated string");
         end if;

         Token_End := Pos;

         --  Go to next char and ensure that this is separator. Indeed,
         --  construction such as "string1""string2" are not allowed.

         Next_Char;
         if not Is_Token_Sep then
            return Error ("invalid syntax");
         end if;
         return (Success => True, Kind => J_STRING);

      when '-' | '0' .. '9' =>

         --  We expect a number. If it's a negative one, just discard the
         --  leading dash.

         if CC = '-' then
            Next_Char;
            if Pos.Index > Strm'Last then
               return Error ("invalid number");
            end if;
         end if;

         --  Parse the integer part of a number. Leading zeros (except a mere
         --  "0" of course) are not allowed.

         case Strm (Pos.Index) is
         when '0' =>
            Token_End := Pos;
            Next_Char;

         when '1' .. '9' =>
            Token_End := Pos;
            Next_Char;
            while Pos.Index <= Strm'Last
                  and then Strm (Pos.Index) in '0' .. '9'
            loop
               Token_End := Pos;
               Next_Char;
            end loop;

         when others =>
            return Error ("invalid number");
         end case;

         if Is_Token_Sep then

            --  The token stops here, so we have a valid integer number

            return (Success => True, Kind => J_INTEGER);

         elsif Strm (Pos.Index) not in '.' | 'e' | 'E' then

            --  At this point, we allow only an exponent or a decimal number

            return Error ("invalid number");
         end if;

         --  If present, handle the decimals

         if Strm (Pos.Index) = '.' then
            Can_Be_Integer := False;
            Token_End := Pos;
            Next_Char;
            if Pos.Index > Strm'Last or else
              Strm (Pos.Index) not in '0' .. '9'
            then
               return Error ("invalid number");
            end if;

            while Pos.Index <= Strm'Last and then
              Strm (Pos.Index) in '0' .. '9'
            loop
               Token_End := Pos;
               Next_Char;
            end loop;

         end if;

         --  If present, handle the exponent

         if Pos.Index <= Strm'Last and then Strm (Pos.Index) in 'e' | 'E' then
            Token_End := Pos;
            Next_Char;
            if Pos.Index > Strm'Last then
               return Error ("invalid number");
            end if;

            --  Skip the sign, if present

            case Strm (Pos.Index) is
               when '-' =>

                  --  The exponent is negative. Even though several corner
                  --  cases (such as having "1" as the prefix) can lead to an
                  --  integer, assume that the number is not an integer.

                  Can_Be_Integer := False;
                  Next_Char;

               when '+'   => Next_Char;
               when others => null;
            end case;

            if Pos.Index > Strm'Last or else Strm (Pos.Index) not in '0' .. '9'
            then
               return Error ("invalid number");
            end if;

            while Pos.Index <= Strm'Last
                  and then Strm (Pos.Index) in '0' .. '9'
            loop
               Token_End := Pos;
               Next_Char;
            end loop;
         end if;

         if Is_Token_Sep then

            --  The token stops here, so we have a valid integer number

            return
              (Success => True,
               Kind    => (if Can_Be_Integer then J_INTEGER else J_NUMBER));
         else
            return Error ("invalid number");
         end if;

      when others =>
         return Error ("Unexpected character '" & CC & ''');
      end case;
   end Read_Token;

   ----------
   -- Read --
   ----------

   procedure Read
     (Result    : in out Read_Result;
      Strm      : String;
      Pos       : in out Text_Position;
      Kind      : out Token_Kind;
      Check_EOF : Boolean := False)
   is

      function Error (Msg : String) return Read_Result;
      --  Return a parsing error for the current position and the given error
      --  message.

      function Error (Result : Read_Token_Result) return Read_Result
         with Pre => not Result.Success;
      --  Transform a parsing error from Read_Token into a Read_Result

      -----------
      -- Error --
      -----------

      function Error (Msg : String) return Read_Result is
      begin
         return (Success => False,
                 Error   => (Line    => Pos.Line,
                             Column  => Pos.Column,
                             Message => To_Unbounded_String (Msg)));
      end Error;

      function Error (Result : Read_Token_Result) return Read_Result is
      begin
         return (Success => False, Error => Result.Error);
      end Error;

      Token_Start, Token_End : Text_Position;
      --  Boundaries for the currently analyzed token

      Token_Result : Read_Token_Result;
      --  Buffer for token reads
   begin
      --  The first token we get determines the kind of JSON value to return

      Token_Result := Read_Token (Strm, Pos, Token_Start, Token_End);
      if not Token_Result.Success then
         Result := Error (Token_Result);
         Kind := J_EOF;
         return;
      end if;
      Kind := Token_Result.Kind;

      case Kind is
         when J_EOF =>
            Result := Error ("empty stream");
            return;

         when J_NULL =>
            Result.Value := Create;

         when J_FALSE =>
            Result.Value := Create (False);

         when J_TRUE =>
            Result.Value := Create (True);

         when J_STRING =>
            begin
               declare
                  Str_Value : constant UTF8_XString := Utility.Un_Escape_String
                    (Strm, Token_Start.Index, Token_End.Index);
               begin
                  Result.Value := Create (Str_Value);
               end;
            exception
               when Exc : Invalid_JSON_Stream =>
                  Result := Error (Ada.Exceptions.Exception_Message (Exc));
                  return;
            end;

         when J_ARRAY =>
            declare
               --  In order to avoid the costly array copy in Create
               --  (JSON_Array), we use an aggregate below in order to build
               --  the result, so directly allocate the JSON_Array_Access here.

               Arr : JSON_Array_Access := new JSON_Array_Internal;

               ST : Token_Kind;
               --  Buffer for the kind of tokens we read

               Element : Read_Result;
               --  Buffer for the JSON values that constitute array elements

               Is_First : Boolean := True;
               --  True if we are still reading the first array element. False
               --  afterwards.
            begin
               --  Read all elements in the array until we reach the closing
               --  token ("]").

               loop
                  Read (Element, Strm, Pos, ST);
                  if not Element.Success then
                     Free (Arr);
                     Result := Element;
                     return;
                  end if;

                  case ST is
                  when J_ARRAY_END =>
                     exit when Is_First;
                     Free (Arr);
                     Result := Error ("syntax error");
                     return;

                  when Value_Token =>
                     --  We got a new array element: append it

                     Append (Arr.Arr, Element.Value);

                     --  Now see what is next: the end of the array or a comma
                     --  (hence another array element after).

                     Read (Element, Strm, Pos, ST);
                     if not Element.Success then
                        Free (Arr);
                        Result := Element;
                        return;
                     end if;

                     case ST is
                     when J_ARRAY_END =>
                        exit;

                     when J_COMMA =>
                        --  We have a comma, so we expect another element in
                        --  the array. Continue reading.

                        null;

                     when others =>
                        Free (Arr);
                        Result := Error ("comma expected");
                        return;
                     end case;

                  when others =>
                     Free (Arr);
                     Result := Error ("syntax error");
                     return;
                  end case;

                  Is_First := False;
               end loop;

               Result.Value :=
                 (Ada.Finalization.Controlled with
                  Data => (Kind => JSON_Array_Type, Arr_Value => Arr));
            end;

         when J_OBJECT =>
            declare
               Is_First : Boolean := True;
               --  True if we are still reading the first object member. False
               --  afterwards.

               ST : Token_Kind;
               --  Buffer for the kind of tokens we read

               Key, Value : Read_Result;
               --  Buffer for the JSON values that constitute keys and member
               --  values.
            begin
               --  Allocate internal container for the result

               Result.Value :=
                 (Ada.Finalization.Controlled with
                  Data => (Kind      => JSON_Object_Type,
                           Obj_Value => new JSON_Object_Internal));

               --  Read all members for this object until we reach the closing
               --  token ("}").

               loop
                  --  First try to read the key for the next member

                  Read (Key, Strm, Pos, ST);
                  if not Key.Success then
                     Result := Key;
                     return;
                  end if;

                  case ST is
                  when J_OBJECT_END =>
                     exit when Is_First;
                     Result := Error ("string value expected");
                     return;

                  when J_STRING =>
                     --  Consume the colon token, then get the member value

                     Read (Value, Strm, Pos, ST);
                     if not Value.Success then
                        Result := Value;
                        return;
                     elsif ST /= J_COLON then
                        Result := Error ("colon expected");
                        return;
                     end if;

                     Read (Value, Strm, Pos, ST);
                     if not Value.Success then
                        Result := Value;
                        return;
                     elsif ST not in Value_Token then
                        Result := Error ("non expected token");
                        return;
                     end if;

                     --  Register this new member.
                     --
                     --  As we checked above that reading Key parsed a string
                     --  token, we know that coercing Key to a string cannot
                     --  fail.

                     declare
                        Key_Str : constant UTF8_XString := Get (Key.Value);
                     begin
                        Set_Field (Result.Value, Key_Str, Value.Value);
                     end;

                     --  Now see what is next: the end of the object or a comma
                     --  (hence another object member after).

                     Read (Value, Strm, Pos, ST);
                     if not Value.Success then
                        Result := Value;
                        return;
                     end if;
                     case ST is
                     when J_OBJECT_END =>
                        exit;

                     when J_COMMA =>
                        --  We have a comma, so we expect another member in the
                        --  object. Continue reading.

                        null;

                     when others =>
                        Result := Error ("comma expected");
                        return;
                     end case;

                  when others =>
                     Result := Error ("string value expected");
                     return;
                  end case;
                  Is_First := False;
               end loop;
            end;

         when J_NUMBER | J_INTEGER =>
            declare
               Number_Str  : constant String :=
                  Strm (Token_Start.Index .. Token_End.Index);
               Has_Integer : Boolean := False;
            begin
               if Kind = J_INTEGER then
                  declare
                     Result_Int : Long_Long_Integer;
                  begin
                     Result_Int := Long_Long_Integer'Value (Number_Str);
                     Result.Value := Create (Result_Int);
                     Has_Integer := True;
                  exception
                     when Constraint_Error =>
                        null;
                  end;
               end if;

               if not Has_Integer then
                  begin
                     Result.Value := Create (Long_Float'Value (Number_Str));
                  exception
                     when Constraint_Error =>
                        Result := Error
                          ("cannot convert JSON number to Long_Float");
                        return;
                  end;
               end if;
            end;

         when others =>
            if Check_EOF then
               Result := Error ("invalid JSON stream");
               return;
            else
               Result.Value := Create;
            end if;
      end case;

      if Check_EOF then
         Token_Result := Read_Token (Strm, Pos, Token_Start, Token_End);
         if not Token_Result.Success or else Token_Result.Kind /= J_EOF then
            Result := Error ("additional data after end of JSON stream");
            return;
         end if;
      end if;
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
      Pos    : Text_Position := (Strm'First, 1, 1);
      Kind   : Token_Kind;
      Result : Read_Result := (Success => True, others => <>);
   begin
      Read (Result, Strm, Pos, Kind, Check_EOF => True);
      return Result;
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
        (Object_Item'(Key => To_XString (Field_Name),
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
        (Object_Item'(Key => Field_Name,
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
