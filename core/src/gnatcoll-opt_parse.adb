--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1

with Ada.Tags; use Ada.Tags;
with Ada.Command_Line;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;

with GNATCOLL.VFS;

package body GNATCOLL.Opt_Parse is

   package Cmd_Line renames Ada.Command_Line;

   type XString_Vector_Access is access all XString_Vector;

   function Get_Arguments (Arguments : XString_Array) return XString_Array;
   --  Return the arguments in ``Arguments``, if it's not an empty array. Else,
   --  create an array from the application's command line arguments.

   function Parse_One_Option
     (Short, Long               : String;
      Args                      : XString_Array;
      Pos                       : Positive;
      New_Pos                   : out Parser_Return;
      Allow_Collated_Short_Form : Boolean := True) return XString;
   --  Parse one flag option, with the given ``Short`` & ``Long``
   --  specifications, from the ``Args`` array, starting at ``Pos``.
   --  Put the new position in ``New_Pos``. Return the option's raw value
   --
   --  For short arguments, this handles both ``-a B`` and ``-aB`` forms.
   --  For long arguments, this handles both ``--long B`` and ``--long=B``
   --  forms.
   --
   --  If ``Allow_Collated_Short_Form`` is False, then the `-aB` form above
   --  is not accepted. This can be useful in combination with legacy style
   --  long form which starts with only one dash, to avoid conflicts.

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

   subtype XString_Cur is XString_Vectors.Cursor;
   --  Shortcut for a XString vector cursor

   function Append_Line (Self : in out Text_Wrapper) return XString_Cur;
   --  Append a new line to Self

   procedure Append_Line
     (Self       : in out Text_Wrapper;
      Text       : String := "";
      Col_After  : Col_Type := No_Col);
   --  Append a new line to Self, after appending `Text`. If
   --  `Col_After` is not `No_Col`, then set the next start column to
   --  `Col_After`.

   function Current_Line (Self : in out Text_Wrapper) return XString_Cur
   is
     (if Self.Lines.Is_Empty
      then Self.Append_Line else Self.Lines.Last);
   --  Return a reference to the current line.

   function Render (Self : Text_Wrapper) return String;
   --  Render the content of Self to a String.

   pragma Warnings (On, "not dispatching");

   -----------------
   -- Flag_Parser --
   -----------------

   type Flag_Parser is new Subparser_Type with record
      Short, Long : XString;
   end record;

   type Flag_Parser_Result is new Parser_Result with record
      Result : Boolean;
   end record;

   overriding function Usage
     (Self : Flag_Parser) return String;

   overriding function Help_Name
     (Self : Flag_Parser) return String;

   overriding function Parse_Args
     (Self   : in out Flag_Parser;
      Args   : XString_Array;
      Pos    : Positive;
      Result : in out Parsed_Arguments) return Parser_Return;

   overriding procedure Init_JSON_Help (Self : Flag_Parser; Val : JSON_Value);

   overriding function JSON_Kind (Self : Flag_Parser) return String
   is ("flag");

   overriding procedure Release (Self : in out Flag_Parser_Result) is null;

   type Help_Flag_Parser is new Flag_Parser with null record;
   --  Specific subtype of Flag_Parser to designate the help flag parser.

   type JSON_Help_Flag_Parser is new Flag_Parser with null record;
   --  Specific subtype of Flag_Parser to designate the JSON help flag parser.

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
         Dummy : XString_Cur := Self.Append_Line;
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
      Current_Line_Length : constant Natural :=
        Self.Lines.Reference (Self.Current_Line).Length;
   begin
      Self.Set_Next_Start_Column (Col);

      if Current_Line_Length > Col then
         Self.Append_Line;
      else
         Self.Append_Text ((1 .. Col - Current_Line_Length => ' '), False);
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

   function Append_Line (Self : in out Text_Wrapper) return XString_Cur is
      Ret : XString;
   begin
      Self.Lines.Append (Ret);

      declare
         L : constant XString_Cur := Self.Current_Line;
      begin
         if Self.Start_Col > 0 then
            Self.Lines.Reference (L).Append ((1 .. Self.Start_Col => ' '));
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
            Cur_Line : XString renames
              Self.Lines.Reference (Self.Current_Line);
         begin
            if Cur_Line.Length + Text'Length <= Self.Wrap_Col then
               Cur_Line.Append (Text);
               return;
            end if;
         end;

         declare
            Cur_Line : XString renames
              Self.Lines.Reference (Append_Line (Self));
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
         Self.Start_Col :=
           Col_Type (Self.Lines.Reference (Self.Current_Line).Length);
      else
         Self.Start_Col := Col;
      end if;
   end Set_Next_Start_Column;

   -------------------
   -- Get_Arguments --
   -------------------

   function Get_Arguments (Arguments : XString_Array) return XString_Array is
   begin
      if Arguments /= No_Arguments then
         return Arguments;
      end if;

      declare
         Args : XString_Array (1 .. Cmd_Line.Argument_Count);
      begin
         for I in Args'Range loop
            Args (I) := +Cmd_Line.Argument (I);
         end loop;
         return Args;
      end;
   end Get_Arguments;

   ----------------
   -- Get_Result --
   ----------------

   function Get_Result
     (Self : Subparser_Type'Class;
      Args : Parsed_Arguments) return Parser_Result_Access
   is
      --  Due to controlled objects finalization, the following code is not
      --  thread safe, so we use a critical section here, so that Get functions
      --  for arguments are thread safe.

      Dummy : Scoped_Lock (Self.Parser.Mutex'Access);
   begin
      declare
         Real_Args : Parsed_Arguments;
      begin
         if Args = No_Parsed_Arguments then
            Real_Args := Self.Parser.Default_Result;
         else
            Real_Args := Args;
         end if;

         if Real_Args = No_Parsed_Arguments then
            raise Opt_Parse_Error with "No results for command line arguments";
         end if;

         return Real_Args.Ref.Get.Results (Self.Position);
      end;
   end Get_Result;

   ----------------
   -- Has_Result --
   ----------------

   function Has_Result
     (Self : Subparser_Type'Class;
      Args : Parsed_Arguments) return Boolean is
   begin
      return Self.Get_Result (Args) /= null;
   end Has_Result;

   function Parse_Impl
     (Self         : in out Argument_Parser;
      Arguments    : XString_Array := No_Arguments;
      Result       : out Parsed_Arguments;
      Unknown_Args : XString_Vector_Access := null) return Boolean;
   --  Shared implementation for all the ``Parse`` public overloads

   -----------
   -- Parse --
   -----------

   function Parse
     (Self         : in out Argument_Parser;
      Arguments    : XString_Array := No_Arguments) return Boolean
   is
   begin
      return Self.Parse_Impl (Arguments, Self.Data.Default_Result, null);
   end Parse;

   -----------
   -- Parse --
   -----------

   function Parse
     (Self         : in out Argument_Parser;
      Arguments    : XString_Array := No_Arguments;
      Unknown_Arguments : out XString_Vector) return Boolean
   is
   begin
      return Self.Parse_Impl
        (Arguments,
         Self.Data.Default_Result,
         Unknown_Arguments'Unchecked_Access);
   end Parse;

   -----------
   -- Parse --
   -----------

   function Parse
     (Self         : in out Argument_Parser;
      Arguments    : XString_Array := No_Arguments;
      Result       : out Parsed_Arguments) return Boolean
   is
   begin
      return Self.Parse_Impl (Arguments, Result, null);
   end Parse;

   ----------------
   -- Parse_Impl --
   ----------------

   function Parse_Impl
     (Self         : in out Argument_Parser;
      Arguments    : XString_Array := No_Arguments;
      Result       : out Parsed_Arguments;
      Unknown_Args : XString_Vector_Access := null) return Boolean
   is
      Exit_Parsing : exception;
      --  Raised when aborting arguments parsing for --help. We cannot call
      --  directly GNAT.OS_Lib.OS_Exit in that case as this is in the middle of
      --  vector iterations: the call to OS_Exit triggers the finalization of
      --  these vectors, so we would get a vector tampering check failure. Use
      --  an exception to stop the iteration, and only then call OS_Exit to
      --  avoid this situation.

      Current_Arg   : Positive := 1;
      Cmd_Line_Args : constant XString_Array := Get_Arguments (Arguments);

      procedure Handle_Failure (Error_Msg : String);
      function Internal return Boolean;

      --------------------
      -- Handle_Failure --
      --------------------

      procedure Handle_Failure (Error_Msg : String) is
         use Error_Handler_References;
      begin
         Self.Data.Last_Error := +Error_Msg;

         if Self.Data.Custom_Error_Handler /= Null_Ref then
            Self.Data.Custom_Error_Handler.Unchecked_Get.Error (Error_Msg);
         else
            Put_Line
              (Standard_Error, "Argument parsing failed: " & Error_Msg);
         end if;

      end Handle_Failure;

      function Internal return Boolean is
         use type Parsed_Arguments_Shared_Ptrs.Element_Access;
      begin
         --  If we're not in incremental mode, then reset the results.
         if not
           (Self.Data.Incremental
            and then Result.Ref.Unchecked_Get /= null)
         then
            Result.Ref.Set
              (Parsed_Arguments_Type'
                 (Raw_Args => new XString_Array'(Cmd_Line_Args),
                  Results  =>
                    new Parser_Result_Array
                      (1 .. Self.Data.All_Parsers.Last_Index)));
         end if;

         while Current_Arg <= Cmd_Line_Args'Last loop

            for Opt_Parser of Self.Data.Opts_Parsers loop
               begin
                  declare
                     P_Return : constant Parser_Return :=
                       Opt_Parser.Parse (Cmd_Line_Args, Current_Arg, Result);
                  begin
                     if P_Return /= Error_Return then
                        Current_Arg := Positive (P_Return);

                        --  If the subparser that successfully parsed is the
                        --  --help parser then, print help and exit.
                        if Opt_Parser.all in Help_Flag_Parser'Class then
                           Put_Line (Self.Help);
                           raise Exit_Parsing;
                        elsif Opt_Parser.all in JSON_Help_Flag_Parser'Class
                        then
                           Put_Line (Self.JSON_Help.Write (Compact => False));
                           raise Exit_Parsing;
                        end if;

                        goto Next_Iter;
                     end if;
                  end;
               exception
                  when E : Opt_Parse_Error =>
                     Handle_Failure
                       ("for option " & (+Opt_Parser.Name) & " - "
                        & Ada.Exceptions.Exception_Message (E));
                     return False;
               end;
            end loop;

            for Pos_Parser of Self.Data.Positional_Args_Parsers loop
               begin
                  declare
                     P_Return : constant Parser_Return :=
                       Pos_Parser.Parse
                         (Cmd_Line_Args, Current_Arg, Result);
                  begin
                     if P_Return /= Error_Return then
                        Current_Arg := Positive (P_Return);
                        goto Next_Iter;
                     end if;
                  end;
               exception
                  when E : Opt_Parse_Error =>
                     Handle_Failure
                       ("for parser " & (+Pos_Parser.Name) & " - " &
                        Ada.Exceptions.Exception_Message (E));
                     return False;
               end;
            end loop;

            --  If the user passed an Unknown_Args vector, fill it with
            --  arguments that we didn't recognize. Else, raise an error when
            --  we encounter an unknown argument.
            if Unknown_Args = null then
               Handle_Failure
                 ("Unrecognized argument " &
                  (+Cmd_Line_Args (Current_Arg)));
               return False;
            else
               Unknown_Args.Append (Cmd_Line_Args (Current_Arg));
               Current_Arg := Current_Arg + 1;
            end if;

            <<Next_Iter>>
         end loop;

         for Parser of Self.Data.All_Parsers loop
            --  Check that all mandatory parameters have been provided
            if not Parser.Opt and then not Parser.Has_Result (Result)
            then
               Handle_Failure ("Missing value for " & (+Parser.Name));
               return False;

            elsif Parser.Has_Result (Result) then
               --  Reset the ``Already_Parsed_In_Current_Pass`` flag
               Parser.Get_Result
                 (Result).Already_Parsed_In_Current_Pass := False;

            end if;
         end loop;

         return True;
      exception
         when E : Opt_Parse_Error =>
            Handle_Failure (Ada.Exceptions.Exception_Message (E));
            return False;

         when Exit_Parsing =>
            GNAT.OS_Lib.OS_Exit (0);
            return False;
      end Internal;

   begin
      return Ret : constant Boolean := Internal do
         if not Ret and then Self.Data.Print_Help_On_Error then
            --  Print help if parsing has failed
            Put_Line (Standard_Error, Help (Self));
         end if;
      end return;
   end Parse_Impl;

   ---------------
   -- JSON_Help --
   ---------------

   function JSON_Help
     (Self : Subparser_Type) return JSON_Value
   is
      Ret : constant JSON_Value := Create_Object;
   begin
      Ret.Set_Field ("name", +Self.Name);
      Ret.Set_Field ("help", +Self.Help);
      Ret.Set_Field ("kind", Subparser_Type'Class (Self).JSON_Kind);
      Subparser_Type'Class (Self).Init_JSON_Help (Ret);
      return Ret;
   end JSON_Help;

   --------------------
   -- Init_JSON_Help --
   --------------------

   procedure Init_JSON_Help (Self : Subparser_Type; Val : JSON_Value) is
   begin
      null;
   end Init_JSON_Help;

   -----------
   -- Parse --
   -----------

   function Parse
     (Self   : in out Subparser_Type'Class;
      Args   : XString_Array;
      Pos    : Positive;
      Result : in out Parsed_Arguments) return Parser_Return
   is
      Ret : Parser_Return;
      Has_Previous_Result_In_The_Current_Pass : Boolean := False;
   begin
      if Self.Has_Result (Result) then
         Has_Previous_Result_In_The_Current_Pass :=
           Self.Get_Result (Result).Already_Parsed_In_Current_Pass;
         if not Self.Does_Accumulate and then
           not Self.Parser.Incremental
         then
            return Error_Return;
         end if;
      end if;

      Ret := Self.Parse_Args (Args, Pos, Result);

      --  If argument has been parsed then perform post-parsing tests
      if Ret /= Error_Return then
         --  Check that the parser is not disabled
         if Self.Disallow_Msg /= Null_XString then
            raise Opt_Parse_Error with To_String (Self.Disallow_Msg);
         end if;

         --  Check whether the parser had a previous result in the current pass
         if not Self.Does_Accumulate and then
           Has_Previous_Result_In_The_Current_Pass
         then
            raise Opt_Parse_Error with "switch is present multiple times";
         end if;

         Result.Ref.Get.Results
           (Self.Position).Already_Parsed_In_Current_Pass := True;
      end if;

      return Ret;
   end Parse;

   -------------------------------
   -- Parse_Positional_Arg_List --
   -------------------------------

   package body Parse_Positional_Arg_List is
      type Result_Array_Access is access all Result_Array;

      type Positional_Arg_List_Parser is new Subparser_Type with record
         null;
      end record;

      overriding function Parse_Args
        (Self   : in out Positional_Arg_List_Parser;
         Args   : XString_Array;
         Pos    : Positive;
         Result : in out Parsed_Arguments) return Parser_Return;

      overriding function Usage
        (Self : Positional_Arg_List_Parser) return String
      is (Name & " [" & Name & " ...]");

      overriding function JSON_Kind
        (Self : Positional_Arg_List_Parser) return String
      is ("positional_list");

      type Internal_Result is new Parser_Result with record
         Results : Result_Array_Access;
      end record;

      type Internal_Result_Access is access all Internal_Result;

      overriding procedure Release (Self : in out Internal_Result);

      Self_Val : aliased Positional_Arg_List_Parser :=
        Positional_Arg_List_Parser'
          (Name    => +Name,
           Help    => +Help,
           Parser  => Parser.Data,
           Opt     => Allow_Empty,
           others  => <>);

      Self : constant Subparser := Self_Val'Unchecked_Access;

      function This return Subparser is (Self);

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

      overriding function Parse_Args
        (Self   : in out Positional_Arg_List_Parser;
         Args   : XString_Array;
         Pos    : Positive;
         Result : in out Parsed_Arguments) return Parser_Return
      is
         Last : Parser_Return := Error_Return;
      begin
         for I in Pos .. Args'Last loop
            if Args (I).Starts_With ("--") or Args (I).Starts_With ("-") then
               exit;
            end if;

            Last := I;
         end loop;

         if Last = Error_Return then
            return Error_Return;
         end if;

         declare
            R : Result_Array (1 .. Last - Pos + 1);
         begin
            for I in R'Range loop
               R (I) := Convert (+Args (I + Pos - 1));
            end loop;

            declare
               Res : constant Internal_Result_Access := new Internal_Result'
                 (Start_Pos => Pos,
                  End_Pos   => Last,
                  Results   => new Result_Array'(R),
                  others    => <>);
            begin
               Result.Ref.Get.Results (Self.Position) :=
                  Res.all'Unchecked_Access;
            end;

         end;

         return Parser_Return (Last + 1);
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

      type Positional_Arg_Parser is new Subparser_Type with record
         null;
      end record;

      overriding function Parse_Args
        (Self   : in out Positional_Arg_Parser;
         Args   : XString_Array;
         Pos    : Positive;
         Result : in out Parsed_Arguments) return Parser_Return;

      overriding function Usage
        (Self : Positional_Arg_Parser) return String
      is (Name);

      overriding function JSON_Kind
        (Self : Positional_Arg_Parser) return String
      is ("positional_arg");

      type Internal_Result is new Parser_Result with record
         Result : Arg_Type;
      end record;

      type Internal_Result_Access is access all Internal_Result;

      overriding procedure Release (Self : in out Internal_Result) is null;

      Self_Val : aliased Positional_Arg_Parser :=
        (Name   => +Name,
         Help   => +Help,
         Parser => Parser.Data,
         Opt    => False,
         others => <>);

      Self : constant Subparser := Self_Val'Unchecked_Access;

      function This return Subparser is (Self);

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

      overriding function Parse_Args
        (Self   : in out Positional_Arg_Parser;
         Args   : XString_Array;
         Pos    : Positive;
         Result : in out Parsed_Arguments) return Parser_Return
      is
      begin
         if Args (Pos).Starts_With ("--") or Args (Pos).Starts_With ("-") then
            return Error_Return;
         end if;

         declare
            Res     : constant Arg_Type := Convert (+Args (Pos));
            Int_Res : constant Internal_Result_Access := new Internal_Result'
              (Start_Pos => Pos,
               End_Pos   => Pos,
               Result    => Res,
               others    => <>);
         begin
            Result.Ref.Get.Results (Self.Position) :=
               Int_Res.all'Unchecked_Access;
         end;

         return Parser_Return (Pos + 1);
      end Parse_Args;

   begin
      if Enabled then
         Parser.Data.Positional_Args_Parsers.Append (Self);
         Parser.Data.All_Parsers.Append (Self);
         Self.Position := Parser.Data.All_Parsers.Last_Index;
      end if;
   end Parse_Positional_Arg;

   --------------------
   -- Init_JSON_Help --
   --------------------

   overriding procedure Init_JSON_Help (Self : Flag_Parser; Val : JSON_Value)
   is
   begin
      if Self.Short /= "" then
         Val.Set_Field ("short_flag", +Self.Short);
      end if;

      if Self.Long /= "" then
         Val.Set_Field ("long_flag", +Self.Long);
      end if;
   end Init_JSON_Help;

   ----------------
   -- Parse_Args --
   ----------------

   overriding function Parse_Args
     (Self   : in out Flag_Parser;
      Args   : XString_Array;
      Pos    : Positive;
      Result : in out Parsed_Arguments) return Parser_Return
   is
   begin
      if Args (Pos) = Self.Long or else Args (Pos) = Self.Short then

         declare
            Res : constant Parser_Result_Access := new Flag_Parser_Result'
              (Start_Pos => Pos,
               End_Pos   => Pos,
               Result    =>  True,
               others    => <>);
         begin
            Result.Ref.Get.Results (Self.Position) := Res;
         end;

         return Parser_Return (Pos + 1);
      else
         return Error_Return;
      end if;
   end Parse_Args;

   ----------------
   -- Parse_Flag --
   ----------------

   package body Parse_Flag is

      package I is new Flag_Invariants (Short, Long, Name, Legacy_Long_Form);
      pragma Unreferenced (I);

      Self_Val : aliased Flag_Parser := Flag_Parser'
        (Name     => +(if Name /= "" then Name
                       else Long (3 .. Long'Last)),
         Help   => +Help,
         Long   => +Long,
         Short  => +Short,
         Parser => Parser.Data,
         Opt    => True,
         others => <>);

      Self : constant Subparser := Self_Val'Unchecked_Access;

      function This return Subparser is (Self);

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
      if Enabled then
         Parser.Data.Opts_Parsers.Append (Self);
         Parser.Data.All_Parsers.Append (Self);
         Self.Position := Parser.Data.All_Parsers.Last_Index;
      end if;
   end Parse_Flag;

   ------------------
   -- Parse_Option --
   ------------------

   package body Parse_Option is

      package I is new Flag_Invariants (Short, Long, Name, Legacy_Long_Form);
      pragma Unreferenced (I);

      type Option_Parser is new Subparser_Type with record
         null;
      end record;

      overriding function Usage
        (Self : Option_Parser) return String;

      overriding function Help_Name
        (Dummy : Option_Parser) return String;

      overriding procedure Init_JSON_Help
        (Self : Option_Parser; Val : JSON_Value);

      overriding function Parse_Args
        (Self   : in out Option_Parser;
         Args   : XString_Array;
         Pos    : Positive;
         Result : in out Parsed_Arguments) return Parser_Return;

      overriding function JSON_Kind (Self : Option_Parser) return String
      is ("option");

      type Internal_Result is new Parser_Result with record
         Result : Arg_Type;
      end record;

      type Internal_Result_Access is access all Internal_Result;

      procedure Release (Self : in out Internal_Result) is null;

      Self_Val : aliased Option_Parser :=
        Option_Parser'
          (Name   => +(if Name /= "" then Name
                       else Long (3 .. Long'Last)),
           Help   => +Help,
           Parser => Parser.Data,
           Opt    => True,
           others => <>);

      Self : constant Subparser := Self_Val'Unchecked_Access;

      function This return Subparser is (Self);
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
      end Get;

      ----------------
      -- Parse_Args --
      ----------------

      overriding function Parse_Args
        (Self   : in out Option_Parser;
         Args   : XString_Array;
         Pos    : Positive;
         Result : in out Parsed_Arguments) return Parser_Return
      is
         New_Pos : Parser_Return;
         Raw     : constant XString :=
           Parse_One_Option
             (Short, Long, Args, Pos, New_Pos, Allow_Collated_Short_Form);
      begin
         if New_Pos /= Error_Return then
            declare
               Res     : constant Arg_Type := Convert (+Raw);
               Int_Res : constant Internal_Result_Access :=
                 new Internal_Result'(Start_Pos => Pos,
                                      End_Pos   => Pos,
                                      Result    => Res,
                                      others    => <>);
            begin
               Result.Ref.Get.Results (Self.Position) :=
                  Int_Res.all'Unchecked_Access;
            end;
         end if;

         return New_Pos;
      end Parse_Args;

      --------------------
      -- Init_JSON_Help --
      --------------------

      overriding procedure Init_JSON_Help
        (Self : Option_Parser; Val : JSON_Value) is
      begin
         if Short /= "" then
            Val.Set_Field ("short_flag", Short);
         end if;

         if Long /= "" then
            Val.Set_Field ("long_flag", Long);
         end if;
      end Init_JSON_Help;

   begin
      if Enabled then
         Parser.Data.Opts_Parsers.Append (Self);
         Parser.Data.All_Parsers.Append (Self);
         Self.Position := Parser.Data.All_Parsers.Last_Index;
      end if;
   end Parse_Option;

   package body Parse_Enum_Option is

      package I is new Flag_Invariants (Short, Long, Name, Legacy_Long_Form);
      pragma Unreferenced (I);

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

      function This return Subparser renames Internal_Option.This;
   end Parse_Enum_Option;

   -----------------------
   -- Parse_Option_List --
   -----------------------

   package body Parse_Option_List is

      package I is new Flag_Invariants (Short, Long, Name, Legacy_Long_Form);
      pragma Unreferenced (I);

      package Result_Vectors
      is new Ada.Containers.Vectors (Positive, Arg_Type);

      type Option_List_Parser is new Subparser_Type with record
         null;
      end record;

      overriding function Usage
        (Self : Option_List_Parser) return String;

      overriding function Help_Name
        (Dummy : Option_List_Parser) return String;

      overriding function Parse_Args
        (Self   : in out Option_List_Parser;
         Args   : XString_Array;
         Pos    : Positive;
         Result : in out Parsed_Arguments) return Parser_Return;

      overriding function Does_Accumulate
        (Self : Option_List_Parser) return Boolean is (Accumulate);

      overriding function JSON_Kind
        (Self : Option_List_Parser) return String
      is ("list_option" & (if Accumulate then "_accumulate" else ""));

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
         others => <>);

      Self     : constant Subparser := Self_Val'Unchecked_Access;

      function This return Subparser is (Self);

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
            return No_Results;
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

      ------------
      -- Is_Set --
      ------------

      function Is_Set
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Boolean
      is
      begin
         return Self.Get_Result (Args) /= null;
      end Is_Set;

      ----------------
      -- Parse_Args --
      ----------------

      overriding function Parse_Args
        (Self   : in out Option_List_Parser;
         Args   : XString_Array;
         Pos    : Positive;
         Result : in out Parsed_Arguments) return Parser_Return
      is
         Res  : Parser_Result_Access
         renames Result.Ref.Unchecked_Get.Results (Self.Position);

         Tmp : Internal_Result_Access := null;

         Converted_Arg : Arg_Type;
         Arg_Count     : Natural := 0;
      begin
         if Accumulate then
            declare
               New_Pos : Parser_Return;
               Raw     : constant XString :=
                 Parse_One_Option
                   (Short, Long, Args, Pos, New_Pos,
                    Allow_Collated_Short_Form);
            begin
               if New_Pos /= Error_Return then
                  if Res = null then
                     Tmp :=
                       new Internal_Result'
                         (Start_Pos => Pos,
                          End_Pos   => Pos,
                          Results   => Result_Vectors.Empty_Vector,
                          others    => <>);

                     Res := Tmp.all'Unchecked_Access;
                  end if;

                  Internal_Result (Res.all).Results.Append (Convert (+Raw));
               end if;

               return New_Pos;
            end;
         end if;

         if Args (Pos) /= +Long and then Args (Pos) /= +Short then
            return Error_Return;
         end if;

         for I in Pos + 1 .. Args'Last loop
            exit when List_Stop_Predicate (Args (I));
            Arg_Count := Arg_Count + 1;
         end loop;

         if Arg_Count = 0 then
            if Allow_Empty then

               Tmp := new Internal_Result'
                 (Start_Pos => Pos,
                  End_Pos   => Pos,
                  Results   => Result_Vectors.Empty_Vector,
                  others    => <>);

               Res := Tmp.all'Unchecked_Access;

               return Pos + 1;
            else
               return Error_Return;
            end if;
         end if;

         Tmp := new Internal_Result'
           (Start_Pos => Pos,
            End_Pos   => Pos + Arg_Count,
            Results   => Result_Vectors.Empty_Vector,
            others    => <>);

         Res := Tmp.all'Unchecked_Access;

         for I in 1 .. Arg_Count loop
            Converted_Arg := Convert (+Args (Pos + I));
            Internal_Result (Res.all).Results.Append (Converted_Arg);
         end loop;

         return Parser_Return (Pos + Arg_Count + 1);
      end Parse_Args;

   begin
      if Enabled then
         Parser.Data.Opts_Parsers.Append (Self);
         Parser.Data.All_Parsers.Append (Self);
         Self.Position := Parser.Data.All_Parsers.Last_Index;
      end if;
   end Parse_Option_List;

   ----------------------------
   -- Create_Argument_Parser --
   ----------------------------

   function Create_Argument_Parser
     (Help                 : String;
      Command_Name         : String := "";
      Help_Column_Limit    : Col_Type := 80;
      Incremental          : Boolean := False;
      Generate_Help_Flag   : Boolean := True;
      Custom_Error_Handler : Error_Handler_Ref := Null_Ref;
      Print_Help_On_Error  : Boolean := True)
   return Argument_Parser
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
           new Argument_Parser_Data'
             (+Help,
              XCommand_Name,
              Incremental           => Incremental,
              Custom_Error_Handler  => Custom_Error_Handler,
              Print_Help_On_Error   => Print_Help_On_Error,
              others                => <>);

         if Generate_Help_Flag then
            Parser.Data.Help_Flag := new Help_Flag_Parser'
              (Name   => +"help",
               Help   => +"Show this help message",
               Opt    => True,
               Parser => Parser.Data,
               Short  => +"-h",
               Long   => +"--help",
               others => <>);
            Parser.Data.Opts_Parsers.Append (Parser.Data.Help_Flag);
            Parser.Data.All_Parsers.Append (Parser.Data.Help_Flag);
            Parser.Data.Help_Flag.Position
              := Parser.Data.All_Parsers.Last_Index;

            Parser.Data.JSON_Help_Flag := new JSON_Help_Flag_Parser'
              (Name   => +"JSON help",
               Help   => +"Output the help for this command as JSON, and exit",
               Opt    => True,
               Parser => Parser.Data,
               Long   => +"--json-help",
               others => <>);
            Parser.Data.Opts_Parsers.Append (Parser.Data.JSON_Help_Flag);
            Parser.Data.All_Parsers.Append (Parser.Data.JSON_Help_Flag);
            Parser.Data.JSON_Help_Flag.Position
              := Parser.Data.All_Parsers.Last_Index;

            Parser.Data.Help_Column_Limit := Help_Column_Limit;
         end if;
      end return;
   end Create_Argument_Parser;

   ----------------
   -- Last_Error --
   ----------------

   function Last_Error (Self : Argument_Parser) return String is
   begin
      return Self.Data.Last_Error.To_String;
   end Last_Error;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : Argument_Parser) is
   begin
      Self.Data.Default_Result := No_Parsed_Arguments;
   end Reset;

   ---------------
   -- JSON_Help --
   ---------------

   function JSON_Help (Self : Argument_Parser) return JSON_Value is
      Ret : constant JSON_Value := Create_Object;
      Positional_Parsers : JSON_Array := Empty_Array;
      Optional_Parsers : JSON_Array := Empty_Array;
   begin
      Ret.Set_Field ("help", +Self.Data.Help);

      for Parser of Self.Data.Positional_Args_Parsers loop
         Append (Positional_Parsers, Parser.JSON_Help);
      end loop;

      for Parser of Self.Data.Opts_Parsers loop
         Append (Optional_Parsers, Parser.JSON_Help);
      end loop;

      Ret.Set_Field ("positional_parsers", Positional_Parsers);
      Ret.Set_Field ("optional_parsers", Optional_Parsers);

      return Ret;
   end JSON_Help;

   ----------
   -- Help --
   ----------

   function Help (Self : Argument_Parser) return String is
      Ret         : Text_Wrapper;
      Length      : Col_Type;
      Pos_Arg_Col : Col_Type := 0;
      Opt_Arg_Col : Col_Type := 0;
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

      --  Plus 5, 2 for padding, 3 because Help_Name is Starts at Col 3
      Pos_Arg_Col := Pos_Arg_Col + 5;
      Opt_Arg_Col := Opt_Arg_Col + 5;

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

      --  Usage

      Ret.Append_Text ("usage: " & (+Self.Data.Command_Name));
      Ret.Set_Next_Start_Column (Current_Col);

      Ret.Append_Text (" ");

      for Parser of Self.Data.All_Parsers loop
         Ret.Append_Text (Parser.Usage);
         Ret.Append_Text (" ");
      end loop;

      Ret.Append_Line (Col_After => 0);
      Ret.Append_Line;

      --  Main help
      Ret.Append_Text (+Self.Data.Help);
      Ret.Append_Line;
      Ret.Append_Line;

      Ret.Append_Line ("positional arguments:", Col_After => 3);

      for Parser of Self.Data.Positional_Args_Parsers loop
         Ret.Append_Text (Parser.Help_Name);
         Ret.Set_Column (Pos_Arg_Col);

         Ret.Append_Line (+Parser.Help, Col_After => 3);
      end loop;

      Ret.Append_Line (Col_After => 0);
      Ret.Append_Line ("optional arguments:", Col_After => 3);

      for Parser of Self.Data.Opts_Parsers loop
         Ret.Append_Text (Parser.Help_Name);
         Ret.Set_Column (Opt_Arg_Col);

         Ret.Append_Line (+Parser.Help, Col_After => 3);
      end loop;

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
     (Short, Long               : String;
      Args                      : XString_Array;
      Pos                       : Positive;
      New_Pos                   : out Parser_Return;
      Allow_Collated_Short_Form : Boolean := True) return XString
   is
   begin
      if Args (Pos) = Long or Args (Pos) = Short then
         --  Case 1: `-a b` or `--arg b`

         if Pos + 1 > Args'Last then
            raise Opt_Parse_Error with "Incomplete option";
         end if;
         New_Pos := Pos + 2;
         return Args (Pos + 1);

      elsif Long /= "" and then Args (Pos).Starts_With (Long & "=") then
         --  Case 2: `--arg=b`

         New_Pos := Pos + 1;
         return Args (Pos).Slice (Long'Last + 2, Args (Pos).Length);

      elsif Short /= "" and then Args (Pos).Starts_With (Short & "=") then
         --  Case 3: `-a=b`

         New_Pos := Pos + 1;
         return Args (Pos).Slice (Short'Last + 2, Args (Pos).Length);

      elsif Allow_Collated_Short_Form
         and then Short /= "" and then Args (Pos).Starts_With (Short)
      then
         --  Case 4: `-ab`

         New_Pos := Pos + 1;
         return Args (Pos).Slice (Short'Last + 1, Args (Pos).Length);

      else
         New_Pos := Error_Return;
         return +"";
      end if;
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
        (Subparser_Type'Class, Subparser);
   begin
      Free (Self.Data.Help_Flag);
      Free (Self.Data.JSON_Help_Flag);
      Free (Self.Data);
   end Finalize;

   ------------
   -- Create --
   ------------

   function Create (Handler : Error_Handler'Class) return Error_Handler_Ref is
      Ret : Error_Handler_Ref;
   begin
      Ret.Set (Handler);
      return Ret;
   end Create;

   -------------
   -- Release --
   -------------

   procedure Release_Wrapper (Handler : in out Error_Handler'Class) is
   begin
      Handler.Release;
   end Release_Wrapper;

   --------------
   -- Disallow --
   --------------

   procedure Disallow (Self : Subparser; Message : String) is
   begin
      Self.Disallow_Msg := To_XString (Message);
   end Disallow;

   -----------
   -- Allow --
   -----------

   procedure Allow (Self : Subparser) is
   begin
      Self.Disallow_Msg := Null_XString;
   end Allow;

end GNATCOLL.Opt_Parse;
