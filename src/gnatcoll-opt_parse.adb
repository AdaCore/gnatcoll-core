------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2022, AdaCore                     --
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

with Ada.Command_Line;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;

with GNATCOLL.VFS;

package body GNATCOLL.Opt_Parse is

   package Cmd_Line renames Ada.Command_Line;

   function "+"
     (Self : String) return XString renames To_XString;

   function "+"
     (Self : XString) return String renames To_String;

   function Get_Arguments return XString_Array;

   function Parse_One_Option
     (Short, Long : String;
      Args        : XString_Array;
      Pos         : Positive;
      Parsed      : out Boolean;
      Next_Pos    : out Positive) return XString;

   ------------------
   -- Text wrapper --
   ------------------

   type Text_Wrapper is tagged record
      Lines     : XString_Vectors.Vector;
      Wrap_Col  : Natural := 80;
      Start_Col : Natural := 0;
   end record;
   --  Simple abstract type to help with formatting of the outputted help text.

   Current_Col : constant Col_Type := -1;
   --  Constant to represent a magic value that represents the current column

   No_Col : constant Col_Type := -2;
   --  Constant used to represent the absence of column

   pragma Warnings (Off, "not dispatching");

   procedure Append_Text
     (Self      : in out Text_Wrapper;
      Text      : String;
      Cut       : Boolean := True);
   --  Append some text to Self. If `Cut` is True, the text will be cut and
   --  wrapped on every white space character encountered.

   procedure Set_Next_Start_Column
     (Self : in out Text_Wrapper; Col : Col_Type := 0);
   --  Trigger the text wrapper so that next time a line implicitly (through
   --  wrapping) or explicitly appended, it will begin at column `Col`.

   procedure Set_Column (Self : in out Text_Wrapper; Col : Col_Type);
   --  This operation will set the column to write on for the *current line*
   --  to Col, and set the next start column to `Col`, so that text on
   --  subsequent lines starts at `Col`.

   subtype XString_Ref is XString_Vectors.Reference_Type;
   --  Shortcut for a reference to a XString

   function Append_Line
     (Self : aliased in out Text_Wrapper) return XString_Ref;
   --  Append a new line to Self

   procedure Append_Line
     (Self       : in out Text_Wrapper;
      Text       : String := "";
      Col_After  : Col_Type := No_Col);
   --  Append a new line to Self, after appending `Text`. If
   --  `Col_After` is not `No_Col`, then set the next start column to
   --  `Col_After`.

   function Current_Line
     (Self : aliased in out Text_Wrapper) return XString_Ref
   is
     (if Self.Lines.Is_Empty
      then Self.Append_Line else Self.Lines.Reference (Self.Lines.Last_Index));
   --  Return a reference to the current line.

   function Render (Self : Text_Wrapper) return String;
   --  Render the content of Self to a String.

   pragma Warnings (On, "not dispatching");

   -----------------
   -- Flag_Parser --
   -----------------

   type Flag_Parser is new Parser_Type with record
      Short, Long : XString;
   end record;

   type Flag_Parser_Result is new Parser_Result with record
      Result : Boolean;
   end record;

   overriding function Usage
     (Self : Flag_Parser) return String;

   overriding function Help_Name
     (Self : Flag_Parser) return String;

   overriding procedure Parse_Args
     (Self         : in out Flag_Parser;
      Args         : in     XString_Array;
      Pos          : in     Positive;
      Result       : in out Parsed_Arguments;
      Recognised   :    out Boolean;
      Parsed       :    out Boolean;
      Next_Pos     :    out Positive;
      Exit_Parsing :    out Boolean);

   overriding procedure Release (Self : in out Flag_Parser_Result) is null;

   type Help_Flag_Parser is new Flag_Parser with null record;
   --  Specific subtype of Flag_Parser to designate the help flag parser.

   -----------
   -- Usage --
   -----------

   overriding function Usage
     (Self : Flag_Parser) return String
   is
   begin
      if Self.Long /= "" and Self.Short /= "" then
         return
           "[" & To_String (Self.Long) & "|" & To_String (Self.Short) & "]";
      elsif Self.Long /= "" then
         return "[" & To_String (Self.Long) & "]";
      end if;
      return "[" & To_String (Self.Short) & "]";
   end Usage;

   ---------------
   -- Help_Name --
   ---------------

   overriding function Help_Name
     (Self : Flag_Parser) return String
   is
   begin
      if Self.Long /= "" and Self.Short /= "" then
         return To_String (Self.Long) & ", " & To_String (Self.Short);
      elsif Self.Long /= "" then
         return To_String (Self.Long);
      end if;
      return To_String (Self.Short);
   end Help_Name;

   -----------------
   -- Append_Line --
   -----------------

   procedure Append_Line (Self       : in out Text_Wrapper;
                          Text       : String := "";
                          Col_After  : Col_Type := No_Col) is
   begin
      if Text /= "" then
         Self.Append_Text (Text);
      end if;

      if Col_After /= No_Col then
         Self.Set_Next_Start_Column (Col_After);
      end if;

      declare
         Dummy : XString_Ref := Self.Append_Line;
      begin
         null;
      end;
   end Append_Line;

   ----------------
   -- Set_Column --
   ----------------

   procedure Set_Column
     (Self : in out Text_Wrapper; Col : Col_Type)
   is
   begin
      Self.Set_Next_Start_Column (Col);

      if Self.Current_Line.Length > Col then
         Self.Append_Line;
      else
         Self.Append_Text
           ((1 .. Col - Self.Current_Line.Length => ' '), False);
      end if;
   end Set_Column;

   ------------
   -- Render --
   ------------

   function Render (Self : Text_Wrapper) return String is
      Res : XString;
   begin
      for El of Self.Lines loop
         Res.Append (El);
         Res.Append (ASCII.LF);
      end loop;
      return +Res;
   end Render;

   -----------------
   -- Append_Line --
   -----------------

   function Append_Line
     (Self : aliased in out Text_Wrapper) return XString_Vectors.Reference_Type
   is
      Ret : XString;
   begin
      Self.Lines.Append (Ret);

      declare
         L : constant XString_Ref := Self.Current_Line;
      begin
         if Self.Start_Col > 0 then

            L.Append ((1 .. Self.Start_Col => ' '));
         end if;

         return L;
      end;
   end Append_Line;

   -----------------
   -- Append_Text --
   -----------------

   procedure Append_Text
     (Self      : in out Text_Wrapper;
      Text      : String;
      Cut       : Boolean := True)
   is
   begin
      if Cut then
         declare
            Cut_Text : constant XString_Array := XString'(+Text).Split (' ');
         begin
            for I in Cut_Text'Range loop
               if I > 1 then
                  Append_Text (Self, " ", False);
               end if;

               Append_Text (Self, +(Cut_Text (I)), False);
            end loop;
         end;
      else
         declare
            Cur_Line : constant XString_Ref := Self.Current_Line;
         begin
            if Cur_Line.Length + Text'Length <= Self.Wrap_Col then
               Cur_Line.Append (Text);
               return;
            end if;
         end;

         declare
            Cur_Line : constant XString_Ref := Append_Line (Self);
         begin
            Cur_Line.Append (Text);
         end;

      end if;
   end Append_Text;

   ---------------------------
   -- Set_Next_Start_Column --
   ---------------------------

   procedure Set_Next_Start_Column
     (Self : in out Text_Wrapper; Col : Col_Type := 0) is
   begin
      if Col = Current_Col then
         Self.Start_Col := Col_Type (Self.Current_Line.Length);
      else
         Self.Start_Col := Col;
      end if;
   end Set_Next_Start_Column;

   -------------------
   -- Get_Arguments --
   -------------------

   function Get_Arguments return XString_Array is
      Args : XString_Array (1 .. Cmd_Line.Argument_Count);
   begin
      for I in Args'Range loop
         Args (I) := +Cmd_Line.Argument (I);
      end loop;
      return Args;
   end Get_Arguments;

   ----------------
   -- Get_Result --
   ----------------

   function Get_Result
     (Self : Parser_Type'Class;
      Args : Parsed_Arguments) return Parser_Result_Access
   is
      --  Due to controlled objects finalization, the following code is not
      --  thread safe, so we use a critical section here, so that Get functions
      --  for arguments are thread safe.

      Dummy : Scoped_Lock (Self.Parser.Mutex'Access);
   begin
      if Args.Parsed then
         return Args.Ref.Get.Results (Self.Position);
      elsif Self.Parser.Default_Result.Parsed then
         return Self.Parser.Default_Result.Ref.Get.Results (Self.Position);
      end if;
      raise Opt_Parse_Error
        with "No arguments parsed for command: "
         & To_String (Self.Parser.Command_Name);
   end Get_Result;

   ----------------
   -- Has_Result --
   ----------------

   function Has_Result
     (Self : Parser_Type'Class;
      Args : Parsed_Arguments) return Boolean is
   begin
      return Self.Get_Result (Args) /= null;
   end Has_Result;

   -----------
   -- Parse --
   -----------

   procedure Parse_Internal
     (Self         : in out Argument_Parser;
      Arguments    : XString_Array;
      Result       : out Parsed_Arguments;
      Exit_Parsing : out Boolean;
      Success      : out Boolean);
   --  Internal procedure used by all of the parse functions.

   function Parse (Self : in out Argument_Parser) return Boolean
   is
      Exit_Parsing : Boolean;
      Success      : Boolean;
   begin
      Self.Parse_Internal
         (Get_Arguments, Self.Data.Default_Result, Exit_Parsing, Success);
      if Exit_Parsing then
         GNAT.OS_Lib.OS_Exit (0);
      elsif not Success then
         Put_Line (Self.Help);
      end if;
      return Success;
   end Parse;

   function Parse
     (Self      : in out Argument_Parser;
      Arguments : XString_Array) return Boolean
   is
      Exit_Parsing : Boolean;
      Success      : Boolean;
   begin
      Self.Parse_Internal
        (Arguments, Self.Data.Default_Result, Exit_Parsing, Success);
      if Exit_Parsing then
         GNAT.OS_Lib.OS_Exit (0);
      elsif not Success then
         Put_Line (Self.Help);
      end if;
      return Success;
   end Parse;

   function Parse
     (Self      : in out Argument_Parser;
      Arguments : XString_Array;
      Result    : out Parsed_Arguments) return Boolean
   is
      Exit_Parsing : Boolean;
      Success      : Boolean;
   begin
      Self.Parse_Internal (Arguments, Result, Exit_Parsing, Success);
      if Exit_Parsing then
         GNAT.OS_Lib.OS_Exit (0);
      elsif not Success then
         Put_Line (Self.Help);
      end if;
      return Success;
   end Parse;

   procedure Parse_Internal
     (Self         : in out Argument_Parser;
      Arguments    : XString_Array;
      Result       : out Parsed_Arguments;
      Exit_Parsing : out Boolean;
      Success      : out Boolean)
   is
      Current_Arg   : Positive := Arguments'First;
      Next_Arg      : Positive := Arguments'First;

      type Attempt_Parse_Result is (Arg_NA, Arg_Recognised, Arg_Parsed);
      Parse_Result : Attempt_Parse_Result;

      procedure Handle_Failure (Error_Msg : String);

      procedure Handle_Failure (Error_Msg : String) is
      begin
         Put_Line ("Argument parsing failed: " & Error_Msg);
      end Handle_Failure;

      function Attempt_Parse
        (Parser : Parser_Access) return Attempt_Parse_Result;
      --  Attempt to parse the next argument using Parser.
      --  It will set Result to NA if this Parser can not parse the argument.
      --  It will set Result to Recognised if should have been able to parse
      --     the argument, but was unsuccessful for some reason.
      --  It will set Result to Parsed if it was able to successfully parse the
      --    argument.
      --  It will return True if Result is Recognised or Parsed.

      function Attempt_Parse
        (Parsers : Parser_Vector) return Attempt_Parse_Result;
      --  Loop over Parsers, calling Attempt_Parse on each parser until one is
      --  able to parse the next argument.
      --  It will set Result to NA if this Parser can not parse the argument.
      --  It will set Result to Recognised if should have been able to parse
      --     the argument, but was unsuccessful for some reason.
      --  It will set Result to Parsed if it was able to successfully parse the
      --    argument.
      --  It will return True if Result is Recognised or Parsed.

      function Attempt_Parse
        (Parser     : Parser_Access) return Attempt_Parse_Result
      is
         Recognised : Boolean := False;
         Parsed     : Boolean := False;
      begin
         if Parser.Has_Result (Result) and then not Parser.Does_Accumulate then
            return Arg_NA;
         end if;
         Parser.Parse_Args
           (Args         => Arguments,
            Pos          => Current_Arg,
            Result       => Result,
            Recognised   => Recognised,
            Parsed       => Parsed,
            Next_Pos     => Next_Arg,
            Exit_Parsing => Exit_Parsing);
         if Parsed then
            return Arg_Parsed;
         elsif Recognised then
            return Arg_Recognised;
         end if;
         return Arg_NA;
      end Attempt_Parse;

      function Attempt_Parse
        (Parsers : Parser_Vector) return Attempt_Parse_Result
      is
         Result : Attempt_Parse_Result := Arg_NA;
      begin
         for Parser of Parsers loop
            Result := Attempt_Parse (Parser);
            exit when Result /= Arg_NA;
         end loop;
         return Result;
      end Attempt_Parse;

   begin
      Success := False;
      Exit_Parsing := False;
      Result.Parsed := True;

      Result.Ref.Set
        (Parsed_Arguments_Type'
           (Raw_Args => new XString_Array'(Arguments),
            Results  => new Parser_Result_Array
              (1 .. Self.Data.All_Parsers.Last_Index)));

      while Current_Arg <= Arguments'Last loop
         --  Check for help flag
         if Attempt_Parse (Self.Data.Help_Flag) = Arg_Parsed then
            Put_Line (Self.Help);
            Success := True;
            Exit_Parsing := True;
            Result.Parsed := False;
            return;
         end if;

         --  Attempt to parse argument using optional, positional, and then
         --  sub-command parsers.
         Parse_Result := Attempt_Parse (Self.Data.Opts_Parsers);
         exit when Parse_Result = Arg_Recognised;

         Parse_Result := Attempt_Parse (Self.Data.Positional_Args_Parsers);
         exit when Parse_Result = Arg_Recognised;

         Parse_Result := Attempt_Parse (Self.Data.Sub_Command_Parsers);
         exit when Parse_Result = Arg_Recognised;

         if Exit_Parsing then
            Result.Parsed := False;
            return;
         end if;

         --  Handle error case where argument was not parsed
         if Parse_Result = Arg_NA then
            Handle_Failure
              ("Unrecognized argument " & (+Arguments (Current_Arg)));
            Put_Line (Self.Help);
            Result.Parsed := False;
            Exit_Parsing := True;
            return;
         end if;

         Current_Arg := Next_Arg;
      end loop;

      if Parse_Result = Arg_Recognised then
         Result.Parsed := False;
         return;
      end if;

      for Parser of Self.Data.All_Parsers loop
         if not Parser.Opt and then not Parser.Has_Result (Result) then
            Handle_Failure ("Missing value for " & (+Parser.Name));
            Put_Line (Self.Help);
            Result.Parsed := False;
            Exit_Parsing := True;
            return;
         end if;
      end loop;

      Success := True;
   exception
      when E : Opt_Parse_Error =>
         Handle_Failure (Ada.Exceptions.Exception_Message (E));
         Result.Parsed := False;
         return;
   end Parse_Internal;

   -----------------------
   -- Parse_Sub_Command --
   -----------------------

   package body Parse_Sub_Command is

      type Sub_Command_Parser is new Parser_Type with record
         null;
      end record;

      overriding procedure Parse_Args
        (Self         : in out Sub_Command_Parser;
         Args         : in     XString_Array;
         Pos          : in     Positive;
         Result       : in out Parsed_Arguments;
         Recognised   :    out Boolean;
         Parsed       :    out Boolean;
         Next_Pos     :    out Positive;
         Exit_Parsing :    out Boolean);

      overriding function Usage
        (Self : Sub_Command_Parser) return String
      is ("");

      type Internal_Result is new Parser_Result with record
         Result : Boolean;
      end record;

      overriding procedure Release (Self : in out Internal_Result) is null;

      Self_Val : aliased Sub_Command_Parser :=
        (Name     => +Name,
         Help     => +Help,
         Parser   => Parser.Data,
         Position => <>,
         Opt      => True);

      Self : constant Parser_Access := Self_Val'Unchecked_Access;

      ---------
      -- Get --
      ---------

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Boolean
      is
      begin
         if not Enabled then
            return False;
         end if;
         declare
            R : constant Parser_Result_Access := Self.Get_Result (Args);
         begin
            if R /= null then
               return Internal_Result (R.all).Result;
            end if;
            return False;
         exception
            when Opt_Parse_Error =>
               return False;
         end;
      end Get;

      ----------------
      -- Parse_Args --
      ----------------

      overriding procedure Parse_Args
        (Self         : in out Sub_Command_Parser;
         Args         : in     XString_Array;
         Pos          : in     Positive;
         Result       : in out Parsed_Arguments;
         Recognised   :    out Boolean;
         Parsed       :    out Boolean;
         Next_Pos     :    out Positive;
         Exit_Parsing :    out Boolean)
      is
         Arg_Text     : constant String := +Args (Pos);
      begin
         Parsed := False;
         Recognised := False;
         Next_Pos := Positive'Last;
         Exit_Parsing := False;

         if Arg_Text /= Name then
            return;
         end if;

         Recognised := True;

         declare
            Int_Res : constant Parser_Result_Access :=
               new Internal_Result'
                  (Start_Pos => Pos,
                   End_Pos   => Pos,
                   Result    => True);
         begin
            Sub_Argument_Parser.Parse_Internal
              (Arguments    => Args (Pos + 1 .. Args'Last),
               Result       => Sub_Argument_Parser.Data.Default_Result,
               Exit_Parsing => Exit_Parsing,
               Success      => Parsed);
            Result.Ref.Get.Results (Self.Position) := Int_Res;
            if Parsed then
               Next_Pos := Args'Last + 1;
               return;
            end if;
         end;
      end Parse_Args;

   begin
      if Enabled then
         Parser.Data.Sub_Command_Parsers.Append (Self);
         Parser.Data.All_Parsers.Append (Self);
         Self.Position := Parser.Data.All_Parsers.Last_Index;
      end if;
   end Parse_Sub_Command;

   -------------------------------
   -- Parse_Positional_Arg_List --
   -------------------------------

   package body Parse_Positional_Arg_List is
      type Result_Array_Access is access all Result_Array;

      type Positional_Arg_List_Parser is new Parser_Type with record
         null;
      end record;

      overriding procedure Parse_Args
        (Self         : in out Positional_Arg_List_Parser;
         Args         : in     XString_Array;
         Pos          : in     Positive;
         Result       : in out Parsed_Arguments;
         Recognised   :    out Boolean;
         Parsed       :    out Boolean;
         Next_Pos     :    out Positive;
         Exit_Parsing :    out Boolean);

      overriding function Usage
        (Self : Positional_Arg_List_Parser) return String
      is (Name & " [" & Name & " ...]");

      type Internal_Result is new Parser_Result with record
         Results : Result_Array_Access;
      end record;

      type Internal_Result_Access is access all Internal_Result;

      overriding procedure Release (Self : in out Internal_Result);

      Self_Val : aliased Positional_Arg_List_Parser :=
        Positional_Arg_List_Parser'
          (Name     => +Name,
           Help     => +Help,
           Parser   => Parser.Data,
           Position => <>,
           Opt      => Allow_Empty);

      Self : constant Parser_Access := Self_Val'Unchecked_Access;

      ---------
      -- Get --
      ---------

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Result_Array
      is
        (if Enabled
         then (if Self.Has_Result (Args)
               then Internal_Result (Self.Get_Result (Args).all).Results.all
               else No_Results)
         elsif Allow_Empty
         then (1 .. 0 => <>)
         else raise Disabled_Error);

      ----------------
      -- Parse_Args --
      ----------------

      overriding procedure Parse_Args
        (Self         : in out Positional_Arg_List_Parser;
         Args         : in     XString_Array;
         Pos          : in     Positive;
         Result       : in out Parsed_Arguments;
         Recognised   :    out Boolean;
         Parsed       :    out Boolean;
         Next_Pos     :    out Positive;
         Exit_Parsing :    out Boolean)
      is
         Arg_Count : Natural := 0;
      begin
         Parsed := False;
         Recognised := False;
         Next_Pos := Pos;
         Exit_Parsing := False;

         for I in Pos .. Args'Last loop
            exit when Args (I).Starts_With ("-");
            Arg_Count := Arg_Count + 1;
         end loop;

         if Arg_Count = 0 then
            return;
         end if;

         Next_Pos := Pos + Arg_Count;
         Parsed := True;
         Recognised := True;

         declare
            R : Result_Array (1 .. Arg_Count);
         begin
            for I in R'Range loop
               R (I) := Convert (+Args (I + Pos - 1));
            end loop;

            declare
               Res : constant Internal_Result_Access := new Internal_Result'
                 (Start_Pos => Pos,
                  End_Pos   => Next_Pos,
                  Results   => new Result_Array'(R));
            begin
               Result.Ref.Get.Results (Self.Position) :=
                  Res.all'Unchecked_Access;
            end;
         end;
      end Parse_Args;

      overriding procedure Release (Self : in out Internal_Result) is
         procedure Free is new Ada.Unchecked_Deallocation
           (Result_Array, Result_Array_Access);
      begin
         Free (Self.Results);
      end Release;

   begin
      if Enabled then
         Parser.Data.Positional_Args_Parsers.Append (Self);
         Parser.Data.All_Parsers.Append (Self);
         Self.Position := Parser.Data.All_Parsers.Last_Index;
      end if;
   end Parse_Positional_Arg_List;

   --------------------------
   -- Parse_Positional_Arg --
   --------------------------

   package body Parse_Positional_Arg is

      type Positional_Arg_Parser is new Parser_Type with record
         null;
      end record;

      overriding procedure Parse_Args
        (Self         : in out Positional_Arg_Parser;
         Args         : in     XString_Array;
         Pos          : in     Positive;
         Result       : in out Parsed_Arguments;
         Recognised   :    out Boolean;
         Parsed       :    out Boolean;
         Next_Pos     :    out Positive;
         Exit_Parsing :    out Boolean);

      overriding function Usage
        (Self : Positional_Arg_Parser) return String
      is (Name);

      type Internal_Result is new Parser_Result with record
         Result : Arg_Type;
      end record;

      type Internal_Result_Access is access all Internal_Result;

      overriding procedure Release (Self : in out Internal_Result) is null;

      Self_Val : aliased Positional_Arg_Parser :=
        (Name     => +Name,
         Help     => +Help,
         Parser   => Parser.Data,
         Position => <>,
         Opt      => False);

      Self : constant Parser_Access := Self_Val'Unchecked_Access;

      ---------
      -- Get --
      ---------

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Arg_Type
      is
        (if Enabled
         then Internal_Result (Self.Get_Result (Args).all).Result
         else raise Disabled_Error);

      ----------------
      -- Parse_Args --
      ----------------

      overriding procedure Parse_Args
        (Self         : in out Positional_Arg_Parser;
         Args         : in     XString_Array;
         Pos          : in     Positive;
         Result       : in out Parsed_Arguments;
         Recognised   :    out Boolean;
         Parsed       :    out Boolean;
         Next_Pos     :    out Positive;
         Exit_Parsing :    out Boolean)
      is
      begin
         Parsed := False;
         Recognised := False;
         Next_Pos := Pos + 1;
         Exit_Parsing := False;

         if Args (Pos).Starts_With ("-") then
            return;
         end if;

         Recognised := True;

         declare
            Res     : constant Arg_Type := Convert (+Args (Pos));
            Int_Res : constant Internal_Result_Access := new Internal_Result'
              (Start_Pos => Pos,
               End_Pos   => Pos,
               Result    => Res);
         begin
            Result.Ref.Get.Results (Self.Position) :=
               Int_Res.all'Unchecked_Access;
         end;

         Parsed := True;
      end Parse_Args;

   begin
      if Enabled then
         Parser.Data.Positional_Args_Parsers.Append (Self);
         Parser.Data.All_Parsers.Append (Self);
         Self.Position := Parser.Data.All_Parsers.Last_Index;
      end if;
   end Parse_Positional_Arg;

   ----------------
   -- Parse_Args --
   ----------------

   overriding procedure Parse_Args
     (Self         : in out Flag_Parser;
      Args         : in     XString_Array;
      Pos          : in     Positive;
      Result       : in out Parsed_Arguments;
      Recognised   :    out Boolean;
      Parsed       :    out Boolean;
      Next_Pos     :    out Positive;
      Exit_Parsing :    out Boolean)
   is
   begin
      Parsed := False;
      Recognised := False;
      Next_Pos := Pos + 1;
      Exit_Parsing := False;

      if Args (Pos) = Self.Long or else Args (Pos) = Self.Short then

         Recognised := True;

         declare
            Res : constant Parser_Result_Access := new Flag_Parser_Result'
              (Start_Pos => Pos,
               End_Pos   => Pos,
               Result    => True);
         begin
            Result.Ref.Get.Results (Self.Position) := Res;
         end;

         Parsed := True;
      end if;
   end Parse_Args;

   ----------------
   -- Parse_Flag --
   ----------------

   package body Parse_Flag is

      Self_Val : aliased Flag_Parser := Flag_Parser'
        (Name     => +(if Name /= "" then Name
                       else Long (3 .. Long'Last)),
         Help     => +Help,
         Long     => +Long,
         Short    => +Short,
         Parser   => Parser.Data,
         Opt      => True,
         Position => <>);

      Self : constant Parser_Access := Self_Val'Unchecked_Access;

      ---------
      -- Get --
      ---------

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Boolean is
      begin
         if not Enabled then
            return False;
         end if;
         declare
            R : constant Parser_Result_Access := Self.Get_Result (Args);
         begin
            if R /= null then
               return Flag_Parser_Result (R.all).Result;
            else
               return False;
            end if;
         end;
      end Get;

   begin
      if Long = "" and Short = "" then
         raise Opt_Parse_Error
           with "A long or short flag must be provided for Parse_Flag";
      elsif Long = "" and Name = "" then
         raise Opt_Parse_Error
           with "Either Long or Name must be provided for Parse_Flag";
      elsif Enabled then
         Parser.Data.Opts_Parsers.Append (Self);
         Parser.Data.All_Parsers.Append (Self);
         Self.Position := Parser.Data.All_Parsers.Last_Index;
      end if;
   end Parse_Flag;

   ------------------
   -- Parse_Option --
   ------------------

   package body Parse_Option is

      type Option_Parser is new Parser_Type with record
         null;
      end record;

      overriding function Usage
        (Self : Option_Parser) return String;

      overriding function Help_Name
        (Dummy : Option_Parser) return String;

      overriding procedure Parse_Args
        (Self         : in out Option_Parser;
         Args         : in     XString_Array;
         Pos          : in     Positive;
         Result       : in out Parsed_Arguments;
         Recognised   :    out Boolean;
         Parsed       :    out Boolean;
         Next_Pos     :    out Positive;
         Exit_Parsing :    out Boolean);

      type Internal_Result is new Parser_Result with record
         Result : Arg_Type;
      end record;

      type Internal_Result_Access is access all Internal_Result;

      procedure Release (Self : in out Internal_Result) is null;

      Self_Val : aliased Option_Parser :=
        Option_Parser'
          (Name     => +(if Name /= "" then Name
                         else Long (3 .. Long'Last)),
           Help     => +Help,
           Parser   => Parser.Data,
           Opt      => True,
           Position => <>);

      Self : constant Parser_Access := Self_Val'Unchecked_Access;

      -----------
      -- Usage --
      -----------

      overriding function Usage
        (Self : Option_Parser) return String
      is
         Usage_Name : constant String :=
           (if Name /= "" then Name else To_Upper (+Self.Name));
      begin
         if Usage_Text = "" then
            if Long /= "" and Short /= "" then
               return "[" & Long & "|" & Short & " " & Usage_Name & "]";
            elsif Long /= "" then
               return "[" & Long & " " & Usage_Name & "]";
            end if;
            return "[" & Short & " " & Usage_Name & "]";
         end if;
         return Usage_Text;
      end Usage;

      ---------------
      -- Help_Name --
      ---------------

      overriding function Help_Name
        (Dummy : Option_Parser) return String
      is
      begin
         if Long /= "" and Short /= "" then
            return Long & ", " & Short;
         elsif Long /= "" then
            return Long;
         end if;
         return Short;
      end Help_Name;

      ---------
      -- Get --
      ---------

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Arg_Type is
      begin
         if not Enabled then
            return Default_Val;
         end if;
         declare
            R : constant Parser_Result_Access := Self.Get_Result (Args);
         begin
            if R /= null then
               return Internal_Result (R.all).Result;
            else
               return Default_Val;
            end if;
         end;
         exception
            when Opt_Parse_Error =>
               return Default_Val;
      end Get;

      ----------------
      -- Parse_Args --
      ----------------

      overriding procedure Parse_Args
        (Self         : in out Option_Parser;
         Args         : in     XString_Array;
         Pos          : in     Positive;
         Result       : in out Parsed_Arguments;
         Recognised   :    out Boolean;
         Parsed       :    out Boolean;
         Next_Pos     :    out Positive;
         Exit_Parsing :    out Boolean)
      is
         Raw : constant XString :=
           Parse_One_Option (Short, Long, Args, Pos, Parsed, Next_Pos);
      begin
         Exit_Parsing := False;
         Recognised := False;

         if Parsed then
            Recognised := True;
            declare
               Res : constant Internal_Result_Access :=
                 new Internal_Result'(Start_Pos => Pos,
                                      End_Pos   => Pos,
                                      Result    => Convert (+Raw));
            begin
               Result.Ref.Get.Results (Self.Position) :=
                  Res.all'Unchecked_Access;
            end;
         end if;
      end Parse_Args;

   begin
      if Long = "" and Short = "" then
         raise Opt_Parse_Error
           with "A long or short flag must be provided for Parse_Option";
      elsif Long = "" and Name = "" then
         raise Opt_Parse_Error
           with "Either Long or Name must be provided for Parse_Option";
      elsif Enabled then
         Parser.Data.Opts_Parsers.Append (Self);
         Parser.Data.All_Parsers.Append (Self);
         Self.Position := Parser.Data.All_Parsers.Last_Index;
      end if;
   end Parse_Option;

   package body Parse_Enum_Option is
      function Convert (Arg : String) return Arg_Type;

      -------------
      -- Convert --
      -------------

      function Convert (Arg : String) return Arg_Type is
      begin
         return Arg_Type'Value (Arg);
      exception
         when Constraint_Error =>
            raise Opt_Parse_Error
              with "Invalid input value for enum: """ & Arg & """";
      end Convert;

      function Alternatives return String;

      function Alternatives return String is
         Alts : XString_Array
           (1 .. Arg_Type'Pos (Arg_Type'Last) + 1);
      begin
         for V in Arg_Type'Range loop
            Alts (Arg_Type'Pos (V) + 1) := To_XString (V'Image).To_Lower;
         end loop;

         return To_String (To_XString (", ").Join (Alts));
      end Alternatives;

      Enriched_Help : constant String :=
        Help
        & (if Help (Help'Last) = '.' then "" else ".")
        & " Possible alternatives: "
        & Alternatives & ". Default: "
        & To_XString (Default_Val'Image).To_Lower.To_String;

      package Internal_Option is new Parse_Option
        (Parser      => Parser,
         Short       => Short,
         Long        => Long,
         Help        => Enriched_Help,
         Arg_Type    => Arg_Type,
         Default_Val => Default_Val,
         Convert     => Convert,
         Enabled     => Enabled,
         Usage_Text  => Usage_Text,
         Name        => Name);

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Arg_Type
      renames Internal_Option.Get;

   begin
      if Long = "" and Short = "" then
         raise Opt_Parse_Error
           with "A long or short flag must be provided for Parse_Enum_Option";
      elsif Long = "" and Name = "" then
         raise Opt_Parse_Error
           with "Either Long or Name must be provided for Parse_Enum_Option";
      end if;
   end Parse_Enum_Option;

   -----------------------
   -- Parse_Option_List --
   -----------------------

   package body Parse_Option_List is

      package Result_Vectors
      is new Ada.Containers.Vectors (Positive, Arg_Type);

      type Option_List_Parser is new Parser_Type with record
         null;
      end record;

      overriding function Usage
        (Self : Option_List_Parser) return String;

      overriding function Help_Name
        (Dummy : Option_List_Parser) return String;

      overriding procedure Parse_Args
        (Self         : in out Option_List_Parser;
         Args         : in     XString_Array;
         Pos          : in     Positive;
         Result       : in out Parsed_Arguments;
         Recognised   :    out Boolean;
         Parsed       :    out Boolean;
         Next_Pos     :    out Positive;
         Exit_Parsing :    out Boolean);

      overriding function Does_Accumulate
        (Self : Option_List_Parser) return Boolean is (Accumulate);

      type Internal_Result is new Parser_Result with record
         Results : Result_Vectors.Vector;
      end record;

      type Internal_Result_Access is access all Internal_Result;

      procedure Release (Self : in out Internal_Result) is null;

      Self_Val : aliased Option_List_Parser :=
        (Name   => +(if Name /= "" then Name
                     else To_Upper (Long (3 .. Long'Last))),
         Help   => +Help,
         Parser => Parser.Data,
         Opt    => True,
         Position => <>);

      Self     : constant Parser_Access := Self_Val'Unchecked_Access;

      -----------
      -- Usage --
      -----------

      overriding function Usage
        (Self : Option_List_Parser) return String
      is
      begin
         if Usage_Text = "" then
            if Long /= "" and Short /= "" then
               return "[" & Long & "|" & Short & " " & (+Self.Name) &
                      " [" & (+Self.Name) & "...]]";
            elsif Long /= "" then
               return "[" & Long & " " & (+Self.Name) &
                      " [" & (+Self.Name) & "...]]";
            end if;
            return "[" & Short & " " & (+Self.Name) &
                   " [" & (+Self.Name) & "...]]";
         end if;
         return Usage_Text;
      end Usage;

      ---------------
      -- Help_Name --
      ---------------

      overriding function Help_Name
        (Dummy : Option_List_Parser) return String
      is
      begin
         if Long /= "" and Short /= "" then
            return Long & ", " & Short;
         elsif Long /= "" then
            return Long;
         end if;
         return Short;
      end Help_Name;

      ---------
      -- Get --
      ---------

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Result_Array is
      begin
         if not Enabled then
            return (1 .. 0 => <>);
         end if;
         declare
            R : constant Parser_Result_Access := Self.Get_Result (Args);
         begin
            if R /= null then
               declare
                  Res : Result_Array
                    (1 .. Internal_Result (R.all).Results.Last_Index);
               begin
                  for I in Res'Range loop
                     Res (I) := Internal_Result (R.all).Results (I);
                  end loop;
                  return Res;
               end;
            else
               return No_Results;
            end if;
         end;
      end Get;

      ----------------
      -- Parse_Args --
      ----------------

      overriding procedure Parse_Args
        (Self         : in out Option_List_Parser;
         Args         : in     XString_Array;
         Pos          : in     Positive;
         Result       : in out Parsed_Arguments;
         Recognised   :    out Boolean;
         Parsed       :    out Boolean;
         Next_Pos     :    out Positive;
         Exit_Parsing :    out Boolean)
      is
         Res  : Parser_Result_Access
         renames Result.Ref.Get.Results (Self.Position);

         Tmp : Internal_Result_Access := null;

         Converted_Arg : Arg_Type;
         Arg_Count     : Natural := 0;

      begin
         Parsed := False;
         Recognised := False;
         Next_Pos := Pos + 1;
         Exit_Parsing := False;

         if Accumulate then
            declare
               Raw : constant XString :=
                 Parse_One_Option (Short, Long, Args, Pos, Parsed, Next_Pos);
            begin
               if Parsed then
                  Recognised := True;
                  if Res = null then
                     Tmp :=
                       new Internal_Result'
                         (Start_Pos => Pos,
                          End_Pos   => Pos,
                          Results   => Result_Vectors.Empty_Vector);

                     Res := Tmp.all'Unchecked_Access;
                  end if;
                  Internal_Result (Res.all).Results.Append (Convert (+Raw));
               end if;
               return;
            end;
         end if;

         if Args (Pos) /= +Long and then Args (Pos) /= +Short then
            return;
         end if;

         for I in Pos + 1 .. Args'Last loop
            exit when Args (I).Starts_With ("-");
            Arg_Count := Arg_Count + 1;
         end loop;

         if Arg_Count = 0 then
            return;
         end if;

         Tmp := new Internal_Result'
           (Start_Pos => Pos,
            End_Pos   => Pos + Arg_Count,
            Results   => Result_Vectors.Empty_Vector);

         Res := Tmp.all'Unchecked_Access;

         for I in 1 .. Arg_Count loop
            Converted_Arg := Convert (+Args (Pos + I));
            Internal_Result (Res.all).Results.Append (Converted_Arg);
         end loop;

         Parsed := True;
         Next_Pos := Pos + Arg_Count + 1;
      end Parse_Args;

   begin
      if Long = "" and Short = "" then
         raise Opt_Parse_Error
           with "A long or short flag must be provided for Parse_Option_List";
      elsif Long = "" and Name = "" then
         raise Opt_Parse_Error
           with "Either Long or Name must be provided for Parse_Option_List";
      elsif Enabled then
         Parser.Data.Opts_Parsers.Append (Self);
         Parser.Data.All_Parsers.Append (Self);
         Self.Position := Parser.Data.All_Parsers.Last_Index;
      end if;
   end Parse_Option_List;

   ----------------------------
   -- Create_Argument_Parser --
   ----------------------------

   function Create_Argument_Parser
     (Help              : String;
      Command_Name      : String := "";
      Help_Column_Limit : Col_Type := 80) return Argument_Parser
   is
      XCommand_Name : constant XString :=
        +(if Command_Name = ""
          then String
            (VFS.Base_Name
               (VFS.Create
                    (VFS.Filesystem_String (Ada.Command_Line.Command_Name))))
          else Command_Name);
   begin
      return Parser : Argument_Parser do
         Parser.Data :=
           new Argument_Parser_Data'(+Help, XCommand_Name, others => <>);
         Parser.Data.Help_Flag := new Help_Flag_Parser'
           (Name     => +"help",
            Help     => +"Show this help message",
            Position => <>,
            Opt      => True,
            Parser   => Parser.Data,
            Short    => +"-h",
            Long     => +"--help");
         Parser.Data.Opts_Parsers.Append (Parser.Data.Help_Flag);
         Parser.Data.All_Parsers.Append (Parser.Data.Help_Flag);
         Parser.Data.Help_Flag.Position := Parser.Data.All_Parsers.Last_Index;
         Parser.Data.Help_Column_Limit := Help_Column_Limit;
      end return;
   end Create_Argument_Parser;

   ----------
   -- Help --
   ----------

   function Help (Self : Argument_Parser) return String
   is
      use type Ada.Containers.Count_Type;
      Ret         : Text_Wrapper;
      Length      : Col_Type;
      Pos_Arg_Col : Col_Type := 0;
      Opt_Arg_Col : Col_Type := 0;
      Sub_Cmd_Col : Col_Type := 0;
   begin

      Ret.Wrap_Col := Self.Data.Help_Column_Limit;

      --  Set column for help text relative to the max length of Help_Name
      for Parser of Self.Data.Positional_Args_Parsers loop
         Length := Parser.Help_Name'Length;
         if Length > Pos_Arg_Col then
            Pos_Arg_Col := Length;
         end if;
      end loop;

      for Parser of Self.Data.Opts_Parsers loop
         Length := Parser.Help_Name'Length;
         if Length > Opt_Arg_Col then
            Opt_Arg_Col := Length;
         end if;
      end loop;

      for Parser of Self.Data.Sub_Command_Parsers loop
         Length := Parser.Help_Name'Length;
         if Length > Sub_Cmd_Col then
            Sub_Cmd_Col := Length;
         end if;
      end loop;

      --  Plus 5, 2 for padding, 3 because Help_Name is Starts at Col 3
      Pos_Arg_Col := Pos_Arg_Col + 5;
      Opt_Arg_Col := Opt_Arg_Col + 5;
      Sub_Cmd_Col := Sub_Cmd_Col + 5;

      --  Check that Pos_Arg_Col doesn't cause the starting position of help
      --  text to exceed the limit at which all text is wrapped. If it does
      --  exceed this limit, then choose some default value (25) for help text
      --  to begin at.
      if Pos_Arg_Col >= Ret.Wrap_Col then
         Pos_Arg_Col := 25;
      end if;

      if Opt_Arg_Col >= Ret.Wrap_Col then
         Opt_Arg_Col := 25;
      end if;

      if Sub_Cmd_Col >= Ret.Wrap_Col then
         Sub_Cmd_Col := 25;
      end if;

      --  Usage

      Ret.Append_Text ("usage: " & (+Self.Data.Command_Name));
      Ret.Set_Next_Start_Column (Current_Col);

      Ret.Append_Text (" ");

      for Parser of Self.Data.All_Parsers loop
         Ret.Append_Text (Parser.Usage);
         Ret.Append_Text (" ");
      end loop;

      if Self.Data.Sub_Command_Parsers.Length > 0 then
         Ret.Append_Text ("<sub-command>");
         Ret.Append_Text (" ");
         Ret.Append_Text ("[<args>]");
      end if;

      Ret.Append_Line (Col_After => 0);
      Ret.Append_Line;

      --  Main help
      Ret.Append_Text (+Self.Data.Help);
      Ret.Append_Line;


      if Self.Data.Positional_Args_Parsers.Length > 0 then
         Ret.Append_Line;
         Ret.Append_Line ("positional arguments:", Col_After => 3);

         for Parser of Self.Data.Positional_Args_Parsers loop
            Ret.Append_Text (Parser.Help_Name);
            Ret.Set_Column (Pos_Arg_Col);

            Ret.Append_Line (+Parser.Help, Col_After => 3);
         end loop;
      end if;

      --  Due to the help flag, there is always optional arguments
      Ret.Append_Line (Col_After => 0);
      Ret.Append_Line ("optional arguments:", Col_After => 3);

      for Parser of Self.Data.Opts_Parsers loop
         Ret.Append_Text (Parser.Help_Name);
         Ret.Set_Column (Opt_Arg_Col);

         Ret.Append_Line (+Parser.Help, Col_After => 3);
      end loop;

      if Self.Data.Sub_Command_Parsers.Length > 0 then
         Ret.Append_Line (Col_After => 0);
         Ret.Append_Line ("sub-commands:", Col_After => 3);
         for Parser of Self.Data.Sub_Command_Parsers loop
            Ret.Append_Text (Parser.Help_Name);
            Ret.Set_Column (Sub_Cmd_Col);

            Ret.Append_Line (+Parser.Help, Col_After => 3);
         end loop;
      end if;

      return Ret.Render;
   end Help;

   -------------
   -- Convert --
   -------------

   function Convert (Arg : String) return Integer is
   begin
      return Integer'Value (Arg);
   exception
      when Constraint_Error =>
         raise Opt_Parse_Error with "wrong value for Integer: """ & Arg & """";
   end Convert;

   ----------------------
   -- Parse_One_Option --
   ----------------------

   function Parse_One_Option
     (Short, Long : String;
      Args        : XString_Array;
      Pos         : Positive;
      Parsed      : out Boolean;
      Next_Pos    : out Positive) return XString
   is
   begin
      Parsed := False;
      Next_Pos := Positive'Last;
      if Args (Pos) = Long or Args (Pos) = Short then
         if Pos + 1 > Args'Last then
            raise Opt_Parse_Error with "Incomplete option";
         end if;
         Parsed := True;
         Next_Pos := Pos + 2;
         return Args (Pos + 1);
      elsif Long /= "" and then Args (Pos).Starts_With (Long & "=") then
         Parsed := True;
         Next_Pos := Pos + 1;
         return Args (Pos).Slice (Long'Last + 2, Args (Pos).Length);
      elsif Short /= "" and then Args (Pos).Starts_With (Short) then
         Parsed := True;
         Next_Pos := Pos + 1;
         return Args (Pos).Slice (Short'Last + 1, Args (Pos).Length);
      end if;
      return +"";
   end Parse_One_Option;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Parsed_Arguments_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Parser_Result'Class, Parser_Result_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Parser_Result_Array, Parser_Result_Array_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (XString_Array, XString_Array_Access);
   begin
      Free (Self.Raw_Args);
      for R of Self.Results.all loop
         if R /= null then
            R.Release;
         end if;
         Free (R);
      end loop;
      Free (Self.Results);
   end Release;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Argument_Parser) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Argument_Parser_Data, Argument_Parser_Data_Access);

      procedure Free is new Ada.Unchecked_Deallocation
        (Parser_Type'Class, Parser_Access);
   begin
      Free (Self.Data.Help_Flag);
      Free (Self.Data);
   end Finalize;

end GNATCOLL.Opt_Parse;
