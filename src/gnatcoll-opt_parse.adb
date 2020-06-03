------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2021, AdaCore                     --
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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;

with GNATCOLL.VFS;

package body GNATCOLL.Opt_Parse is

   package Cmd_Line renames Ada.Command_Line;

   function "+"
     (Self : String) return XString renames To_XString;

   function "+"
     (Self : XString) return String renames To_String;

   function Get_Arguments (Arguments : XString_Array) return XString_Array;

   function Parse_One_Option
     (Short, Long : String;
      Args        : XString_Array;
      Pos         : Positive;
      New_Pos     : out Parser_Return) return XString;

   ------------------
   -- Text wrapper --
   ------------------

   type Text_Wrapper is tagged record
      Lines     : XString_Vectors.Vector;
      Wrap_Col  : Natural := 80;
      Start_Col : Natural := 0;
   end record;
   --  Simple abstract type to help with formatting of the outputted help text.

   subtype Col_Type is Integer range -2 .. Integer'Last;
   --  Type for a column in the text wrapper.

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
     (Self : Flag_Parser) return String
   is ("[" & To_String (Self.Long) &
       (if Self.Short = "" then "" else "|" & To_String (Self.Short)) & "]");

   overriding function Help_Name
     (Self : Flag_Parser) return String
   is
     (To_String (Self.Long) & ", " & To_String (Self.Short));

   overriding function Parse_Args
     (Self   : in out Flag_Parser;
      Args   : XString_Array;
      Pos    : Positive;
      Result : in out Parsed_Arguments) return Parser_Return;

   overriding procedure Release (Self : in out Flag_Parser_Result) is null;

   type Help_Flag_Parser is new Flag_Parser with null record;
   --  Specific subtype of Flag_Parser to designate the help flag parser.

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
     (Self : Parser_Type'Class;
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
     (Self : Parser_Type'Class;
      Args : Parsed_Arguments) return Boolean is
   begin
      return Self.Get_Result (Args) /= null;
   end Has_Result;

   -----------
   -- Parse --
   -----------

   function Parse
     (Self      : in out Argument_Parser;
      Arguments : XString_Array := No_Arguments) return Boolean
   is
   begin
      if Self.Data.Default_Result /= No_Parsed_Arguments then
         Self.Data.Default_Result := No_Parsed_Arguments;
      end if;

      return Ret : constant Boolean
        := Self.Parse (Arguments, Self.Data.Default_Result)
      do
         if not Ret then
            Put_Line (Help (Self));
         end if;
      end return;
   end Parse;

   -----------
   -- Parse --
   -----------

   function Parse
     (Self      : in out Argument_Parser;
      Arguments : XString_Array := No_Arguments;
      Result    : out Parsed_Arguments) return Boolean
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

      --------------------
      -- Handle_Failure --
      --------------------

      procedure Handle_Failure (Error_Msg : String) is
      begin
         Put_Line
           ("Argument parsing failed: " & Error_Msg);
      end Handle_Failure;

   begin
      Result.Ref.Set
        (Parsed_Arguments_Type'
           (Raw_Args => new XString_Array'(Cmd_Line_Args),
            Results  => new Parser_Result_Array
              (1 .. Self.Data.All_Parsers.Last_Index)));

      while Current_Arg <= Cmd_Line_Args'Last loop

         for Opt_Parser of Self.Data.Opts_Parsers loop
            begin
               declare
                  P_Return : constant Parser_Return :=
                    Opt_Parser.Parse (Cmd_Line_Args, Current_Arg, Result);
               begin
                  if P_Return /= Error_Return then
                     Current_Arg := Positive (P_Return);

                     if Opt_Parser.all in Help_Flag_Parser'Class then
                        Put_Line (Self.Help);
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
                    Pos_Parser.Parse (Cmd_Line_Args, Current_Arg, Result);
               begin
                  if P_Return /= Error_Return then
                     Current_Arg := Positive (P_Return);
                     goto Next_Iter;
                  end if;
               end;
            exception
               when E : Opt_Parse_Error =>
                  Handle_Failure
                    ("for parser " & (+Pos_Parser.Name) & " - "
                     & Ada.Exceptions.Exception_Message (E));
                  return False;
            end;
         end loop;

         Handle_Failure
           ("Unrecognized argument " & (+Cmd_Line_Args (Current_Arg)));
         return False;

         <<Next_Iter>>
      end loop;

      for Parser of Self.Data.All_Parsers loop
         if not Parser.Opt and then not Parser.Has_Result (Result) then
            Handle_Failure ("Missing value for " & (+Parser.Name));
            return False;
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
   end Parse;

   -----------
   -- Parse --
   -----------

   function Parse
     (Self   : in out Parser_Type'Class;
      Args   : XString_Array;
      Pos    : Positive;
      Result : in out Parsed_Arguments) return Parser_Return
   is
   begin
      if Self.Has_Result (Result) and then not Self.Does_Accumulate then
         return Error_Return;
      end if;

      return Self.Parse_Args (Args, Pos, Result);
   end Parse;

   -------------------------------
   -- Parse_Positional_Arg_List --
   -------------------------------

   package body Parse_Positional_Arg_List is
      type Result_Array_Access is access all Result_Array;

      type Positional_Arg_List_Parser is new Parser_Type with record
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
                  Results   => new Result_Array'(R));
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

      type Positional_Arg_Parser is new Parser_Type with record
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
               Result    => Res);
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
               Result    =>  True);
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

      Self_Val : aliased Flag_Parser := Flag_Parser'
        (Name     => +Long (3 .. Long'Last),
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

      type Option_Parser is new Parser_Type with record
         null;
      end record;

      overriding function Usage
        (Self : Option_Parser) return String
      is ("[" & Long & (if Short = "" then "" else "|" & Short) & " "
          & To_Upper (Long (3 .. Long'Last)) & "]");

      overriding function Help_Name
        (Dummy : Option_Parser) return String
      is
        (Long & ", " & Short);

      overriding function Parse_Args
        (Self   : in out Option_Parser;
         Args   : XString_Array;
         Pos    : Positive;
         Result : in out Parsed_Arguments) return Parser_Return;

      type Internal_Result is new Parser_Result with record
         Result : Arg_Type;
      end record;

      type Internal_Result_Access is access all Internal_Result;

      procedure Release (Self : in out Internal_Result) is null;

      Self_Val : aliased Option_Parser :=
        Option_Parser'
          (Name     => +Long (3 .. Long'Last),
           Help     => +Help,
           Parser   => Parser.Data,
           Opt      => True,
           Position => <>);

      Self : constant Parser_Access := Self_Val'Unchecked_Access;

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
           Parse_One_Option (Short, Long, Args, Pos, New_Pos);
      begin

         if New_Pos /= Error_Return then
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

         return New_Pos;
      end Parse_Args;

   begin
      if Enabled then
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
         Enabled     => Enabled);

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Arg_Type
      renames Internal_Option.Get;
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
        (Self : Option_List_Parser) return String
      is ("[" & Long & (if Short = "" then "" else "|" & Short) & " "
          & To_Upper (Long (3 .. Long'Last))
          & "[" & To_Upper (Long (3 .. Long'Last)) & "...]]");

      overriding function Help_Name
        (Dummy : Option_List_Parser) return String
      is
        (Long & ", " & Short);

      overriding function Parse_Args
        (Self   : in out Option_List_Parser;
         Args   : XString_Array;
         Pos    : Positive;
         Result : in out Parsed_Arguments) return Parser_Return;

      overriding function Does_Accumulate
        (Self : Option_List_Parser) return Boolean is (Accumulate);

      type Internal_Result is new Parser_Result with record
         Results : Result_Vectors.Vector;
      end record;

      type Internal_Result_Access is access all Internal_Result;

      procedure Release (Self : in out Internal_Result) is null;

      Self_Val : aliased Option_List_Parser :=
        (Name   => +Long (3 .. Long'Last),
         Help   => +Help,
         Parser => Parser.Data,
         Opt    => True,
         Position => <>);

      Self     : constant Parser_Access := Self_Val'Unchecked_Access;

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

      overriding function Parse_Args
        (Self   : in out Option_List_Parser;
         Args   : XString_Array;
         Pos    : Positive;
         Result : in out Parsed_Arguments) return Parser_Return
      is
         Last : Parser_Return := Error_Return;

         Res  : Parser_Result_Access
         renames Result.Ref.Get.Results (Self.Position);

         Tmp : Internal_Result_Access := null;

      begin
         if Accumulate then
            declare
               New_Pos : Parser_Return;
               Raw     : constant XString :=
                 Parse_One_Option (Short, Long, Args, Pos, New_Pos);
            begin
               if New_Pos /= Error_Return then
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

               return New_Pos;
            end;
         end if;

         if Args (Pos) /= +Long and then Args (Pos) /= +Short then
            return Error_Return;
         end if;

         for I in Pos + 1 .. Args'Last loop
            if Args (I).Starts_With ("--") or Args (I).Starts_With ("-") then
               exit;
            end if;

            Last := I;
         end loop;

         if Last = Error_Return then
            return Error_Return;
         end if;

         Tmp := new Internal_Result'
           (Start_Pos => Pos,
            End_Pos   => Last,
            Results   => Result_Vectors.Empty_Vector);

         Res := Tmp.all'Unchecked_Access;

         for I in 1 .. Last - Pos + 1 loop
            Internal_Result (Res.all).Results (I) := Convert (+Args (I + Pos));
         end loop;

         return Parser_Return (Last + 1);
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
     (Help : String; Command_Name : String := "") return Argument_Parser
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
      end return;
   end Create_Argument_Parser;

   ----------
   -- Help --
   ----------

   function Help (Self : Argument_Parser) return String is
      Ret : Text_Wrapper;
   begin
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
         Ret.Set_Column (25);

         Ret.Append_Line (+Parser.Help, Col_After => 3);
      end loop;

      Ret.Append_Line (Col_After => 0);
      Ret.Append_Line ("optional arguments:", Col_After => 3);

      for Parser of Self.Data.Opts_Parsers loop
         Ret.Append_Text (Parser.Help_Name);
         Ret.Set_Column (25);

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
     (Short, Long : String;
      Args        : XString_Array;
      Pos         : Positive;
      New_Pos     : out Parser_Return) return XString
   is
   begin

      if
        Args (Pos) = Long
        or else (Short /= "" and then Args (Pos) = Short)
      then
         if Pos + 1 > Args'Last then
            raise Opt_Parse_Error with "Incomplete option";
         end if;
         New_Pos := Pos + 2;
         return Args (Pos + 1);

      elsif Args (Pos).Starts_With (Long & "=") then
         New_Pos := Pos + 1;
         return Args (Pos).Slice (Long'Last + 2, Args (Pos).Length);

      elsif Short /= "" and then Args (Pos).Starts_With (Short) then
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
        (Parser_Type'Class, Parser_Access);
   begin
      Free (Self.Data.Help_Flag);
      Free (Self.Data);
   end Finalize;

end GNATCOLL.Opt_Parse;
