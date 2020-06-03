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

with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  We need this to check that the passed type when instantiating an
--  Enum_Option is really an enum type, via 'Type_Class, which is an Aux_Dec
--  specific attribute, returning an enum declared in System.Aux_Dec. There is
--  no general Ada capability or GNAT extension to do that yet, but this
--  capability is in every GNAT version since a very long time, so it's safe to
--  use.
with System.Aux_DEC; use System.Aux_DEC;

with GNATCOLL.Strings; use GNATCOLL.Strings;

private with Ada.Containers.Vectors;
private with GNATCOLL.Refcount;
private with GNATCOLL.Locks;

package GNATCOLL.Opt_Parse is

   --  WARNING: The interface of this package is still unstable. No guarantees
   --  of API stability. USE AT YOUR OWN RISK.
   --
   --  This package is meant to create powerful command line argument parsers
   --  in a declarative fashion. The generated argument parsers have a typed
   --  interface, in that, you can specify the types of expected arguments and
   --  options, and get a statically typed API to access the results.
   --
   --  Here is a small example of how to create a command line argument parser
   --  and how to use it:
   --
   --  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
   --  with Ada.Text_IO;           use Ada.Text_IO;
   --  with GNATCOLL.Opt_Parse;    use GNATCOLL.Opt_Parse;
   --
   --  .. code:: ada
   --
   --     procedure Main is
   --
   --        package Arg is
   --           Parser : Argument_Parser := Create_Argument_Parser
   --             (Help => "Help string for the parser");
   --
   --           package Files is new Parse_Positional_Arg_List
   --             (Parser   => Parser,
   --              Name     => "files",
   --              Arg_Type => Unbounded_String,
   --              Help     => "The files to parse");
   --
   --           package Quiet is new Parse_Flag
   --             (Parser => Parser,
   --              Short  => "-q",
   --              Long   => "--quiet",
   --              Help   => "Whether the tool should be quiet or not");
   --
   --           package Charset is new Parse_Option
   --             (Parser      => Parser,
   --              Short       => "-C",
   --              Long        => "--charset",
   --              Arg_Type    => Unbounded_String,
   --              Help        =>
   --                 "What charset to use for the analysis context. "
   --                 & "Default is ""latin-1""",
   --              Default_Val => To_Unbounded_String ("latin-1"));
   --        end Arg;
   --
   --     begin
   --
   --        if Arg.Parser.Parse then
   --
   --           Put_Line ("Charset = " & To_String (Arg.Charset.Get));
   --           for F of Arg.Files.Get loop
   --              if not Arg.Quiet.Get then
   --                 Put_Line ("Got file " & To_String (F));
   --              end if;
   --           end loop;
   --        end if;
   --
   --     end Main;
   --
   --  All generic packages for argument parsers accept an ``Enabled`` formal,
   --  set to True by default. When set to False, it cancels the registration
   --  of the argument parser. In this case, depending on the argument
   --  specifics, calling its ``Get`` primitive may return a default value or
   --  raise a ``Disabled_Error`` exception. This feature is useful to disable
   --  one or several options depending on some compile-time configuration
   --  without using complex declarations blocks nested in ``if`` statements.

   ------------------------
   --  General API types --
   ------------------------

   type Argument_Parser is tagged limited private;
   --  Base type for the Opt_Parse API. Represents a general parser to which
   --  you will associate specific argument parsers.

   type Parsed_Arguments is private;
   --  Type containing the result of an argument parse. Please note you do
   --  not need to handle the return value if you don't want to, in which case
   --  you will be able to access argument values directly via the generic Get
   --  functions.

   No_Arguments : constant XString_Array (1 .. 0) := (others => <>);
   --  Constant for the absence of command line arguments

   No_Parsed_Arguments : constant Parsed_Arguments;
   --  Constant for a null Parsed_Arguments value

   function Parse
     (Self      : in out Argument_Parser;
      Arguments : XString_Array := No_Arguments) return Boolean;
   --  Parse the command line arguments for Self.

   function Parse
     (Self      : in out Argument_Parser;
      Arguments : XString_Array := No_Arguments;
      Result    : out Parsed_Arguments) return Boolean;
   --  Parse command line arguments for Self. Return arguments explicitly.

   function Create_Argument_Parser
     (Help : String; Command_Name : String := "") return Argument_Parser;
   --  Create an argument parser with the provided help string.

   function Help (Self : Argument_Parser) return String;
   --  Return the help for this parser as a String.

   --------------------------
   -- Conversion functions --
   --------------------------

   --  Convenience conversion functions that are meant to be used in
   --  instantiations.

   function Convert
     (Arg : String) return XString renames To_XString;

   function Convert
     (Arg : String) return Unbounded_String renames To_Unbounded_String;

   function Convert (Arg : String) return Integer;

   Opt_Parse_Error : exception;
   --  Exception signaling an error in the parser. This is the error that you
   --  will get in the rare cases where you do something invalid with a Parser
   --  (such as querying results without calling parse first), and this is
   --  also the exception that you should raise in conversion functions when
   --  receiving an invalid value.

   Disabled_Error : exception;
   --  Exception raised when trying to get the value of a disabled argument
   --  parser that is not a list and provides no default value.

   --------------------------------
   --  Specific argument parsers --
   --------------------------------

   generic
      Parser : in out Argument_Parser;
      --  Argument_Parser owning this argument.

      Name : String;
      --  Name of the argument in the parser. Used mainly to formal the help
      --  output.

      Help : String;
      --  Help string for the argument.

      Allow_Empty : Boolean := False;
      --  Whether empty lists are allowed or not.

      type Arg_Type is private;
      --  Type of the elements contained in the list.

      with function Convert (Arg : String) return Arg_Type is <>;
      --  Conversion function to convert from a raw string argument to the
      --  argument type.

      Enabled : Boolean := True;
      --  Whether to add this argument parser. Note that if it is disabled and
      --  Allow_Empty is False, Get will raise a Disabled_Error.

   package Parse_Positional_Arg_List is
      type Result_Array is array (Positive range <>) of Arg_Type;

      No_Results : constant Result_Array (1 .. 0) := (others => <>);

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Result_Array;

   end Parse_Positional_Arg_List;
   --  Parse a list of positional arguments. This parser can only be the last
   --  positional parser, since it will parse every remaining argument on the
   --  command line.

   generic
      Parser : in out Argument_Parser;
      --  Argument_Parser owning this argument.

      Name : String;
      --  Name of the argument in the parser. Used mainly to formal the help
      --  output.

      Help : String := "";
      --  Help string for the argument.

      type Arg_Type is private;
      --  Type of the positional argument.

      with function Convert (Arg : String) return Arg_Type is <>;
      --  Conversion function to convert from a raw string argument to the
      --  argument type.

      Enabled : Boolean := True;
      --  Whether to add this argument parser. Note that if it is disabled, Get
      --  will raise a Disabled_Error.

   package Parse_Positional_Arg is
      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Arg_Type;
   end Parse_Positional_Arg;
   --  Parse a positional argument. A positional argument is any argument. If
   --  the conversion fails, then it will make the whole argument parser fail.

   generic
      Parser : in out Argument_Parser;
      --  Argument_Parser owning this argument.

      Short : String := "";
      --  Short form for this flag. Should start with one dash and be followed
      --  by one or two alphanumeric characters.

      Long : String;
      --  Long form for this flag. Should start with two dashes.

      Help : String := "";
      --  Help string for the argument.

      Enabled : Boolean := True;
      --  Whether to add this argument parser

   package Parse_Flag is
      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Boolean;
   end Parse_Flag;
   --  Parse a Flag option. A flag takes no other argument, and its result is a
   --  boolean: False if the flag is not passed, True otherwise.

   generic
      Parser : in out Argument_Parser;
      --  Argument_Parser owning this argument.

      Short : String := "";
      --  Short form for this flag. Should start with one dash and be followed
      --  by one or two alphanumeric characters.

      Long : String;
      --  Long form for this flag. Should start with two dashes.

      Help : String := "";
      --  Help string for the argument.

      type Arg_Type is private;
      --  Type of the option.

      with function Convert (Arg : String) return Arg_Type is <>;
      --  Conversion function to convert from a raw string argument to the
      --  argument type.

      Default_Val : Arg_Type;
      --  Default value if the option is not passed.

      Enabled : Boolean := True;
      --  Whether to add this argument parser

   package Parse_Option is
      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Arg_Type;
   end Parse_Option;
   --  Parse a regular option. A regular option is of the form "--option val",
   --  or "--option=val", or "-O val", or "-Oval". If option is not passed,
   --  takes the default value.

   generic
      Parser : in out Argument_Parser;
      --  Argument_Parser owning this argument.

      Short : String := "";
      --  Short form for this flag. Should start with one dash and be followed
      --  by one or two alphanumeric characters.

      Long : String;
      --  Long form for this flag. Should start with two dashes.

      Help : String := "";
      --  Help string for the argument.

      type Arg_Type is (<>);
      --  Type of the option.

      Default_Val : Arg_Type;
      --  Default value if the option is not passed.

      Enabled : Boolean := True;
      --  Whether to add this argument parser

   package Parse_Enum_Option is
      pragma Compile_Time_Error
        (Arg_Type'Type_Class /= Type_Class_Enumeration,
         "Arg_Type for Parse_Enum_Option needs to be an enum type");

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Arg_Type;
   end Parse_Enum_Option;
   --  Parse a regular option whose type is an enum type. See ``Parse_Option``
   --  for the format. This is an helper around ``Parse_Option`` that will
   --  automatically generate a converter and enrich the help message with
   --  the possible alternatives.

   generic
      Parser : in out Argument_Parser;
      --  Argument_Parser owning this argument.

      Short : String := "";
      --  Short form for this flag. Should start with one dash and be followed
      --  by one or two alphanumeric characters.

      Long : String;
      --  Long form for this flag. Should start with two dashes.

      Help : String := "";
      --  Help string for the argument.

      Accumulate : Boolean := False;
      --  If True, then this argument can be passed several times and behaves
      --  each time as a regular option, only with results accumulated in the
      --  result list. If False, user needs to pass a list of values after the
      --  flag name.

      type Arg_Type is private;
      --  Type of the option list.

      with function Convert (Arg : String) return Arg_Type is <>;
      --  Conversion function to convert from a raw string argument to the
      --  argument type.

      Enabled : Boolean := True;
      --  Whether to add this argument parser

   package Parse_Option_List is
      type Result_Array is array (Positive range <>) of Arg_Type;

      No_Results : constant Result_Array (1 .. 0) := (others => <>);

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Result_Array;
   end Parse_Option_List;
   --  Parse an option list. A regular option is of the form
   --  "--option val, val2, val3", or "-O val val2 val3".
   --
   --  Values cannot start with - or --.
   --
   --  If Accumulate is True, mix between option and option list. Parses like
   --  regular option, which you can parse several time, and put results in a
   --  list.

private

   use GNATCOLL.Locks;

   type Argument_Parser_Data;
   type Argument_Parser_Data_Access is access all Argument_Parser_Data;

   package XString_Vectors is new Ada.Containers.Vectors (Positive, XString);

   type Parser_Type is abstract tagged record
      Name : XString;
      --  Name of the parser

      Help : XString;
      --  Help string for the parser

      Position : Positive;
      --  Position of this parser in its enclosing Arguments_Parser

      Opt : Boolean := True;
      --  Whether this parser is optional or not

      Parser : Argument_Parser_Data_Access;
   end record;

   subtype Parser_Return is Integer range -1 .. Integer'Last;
   --  Return value of a Parser. Represents a position, except for the special
   --  value Error_Return.

   Error_Return : constant Parser_Return := 0;
   --  Special value for Parser_Return when there was an error

   function Parse_Args
     (Self   : in out Parser_Type;
      Args   : XString_Array;
      Pos    : Positive;
      Result : in out Parsed_Arguments) return Parser_Return
   is abstract;
   --  Return the result of parsing arguments for this parser. Abstract method
   --  that must be overloaded by implementations.

   function Parse
     (Self   : in out Parser_Type'Class;
      Args   : XString_Array;
      Pos    : Positive;
      Result : in out Parsed_Arguments) return Parser_Return;
   --  Return the result of parsing arguments for this parser. Function wrapper
   --  around `Parse_Args` that is called by Arguments_Parser.

   function Usage
     (Self : Parser_Type) return String is abstract;
   --  Return a usage string for this parser. Abstract method that must be
   --  overloaded.

   function Help_Name
     (Self : Parser_Type) return String
   is
     (To_String (Self.Name));
   --  Return the help name for this parser.

   function Does_Accumulate
     (Self : Parser_Type) return Boolean is (False);
   --  Whether this parser accumulates results or not. If it does, then it is
   --  valid to call Parse on it several time, which will add to results.

   type Parser_Access is access all Parser_Type'Class;

   package Parsers_Vectors
   is new Ada.Containers.Vectors (Positive, Parser_Access);

   subtype Parser_Vector is Parsers_Vectors.Vector;

   type Argument_Parser_Data is record
      Help, Command_Name                    : XString;
      Positional_Args_Parsers, Opts_Parsers : Parser_Vector;
      All_Parsers                           : Parser_Vector;
      Default_Result                        : Parsed_Arguments
        := No_Parsed_Arguments;
      Help_Flag                             : Parser_Access := null;

      Mutex : aliased Mutual_Exclusion;
      --  Mutex used to make Get_Result thread safe
   end record;

   type Parser_Result is abstract tagged record
      Start_Pos, End_Pos : Positive;
   end record;

   procedure Release (Result : in out Parser_Result) is abstract;
   --  Derived types must override this to clean-up internal data when the
   --  Parser_Result object is about to be deallocated.

   type Parser_Result_Access is access all Parser_Result'Class;

   function Get_Result
     (Self : Parser_Type'Class;
      Args : Parsed_Arguments) return Parser_Result_Access;

   function Has_Result
     (Self : Parser_Type'Class;
      Args : Parsed_Arguments) return Boolean;

   type Parser_Result_Array
   is array (Positive range <>) of Parser_Result_Access;

   type Parser_Result_Array_Access is access all Parser_Result_Array;

   type XString_Array_Access is access all XString_Array;

   type Parsed_Arguments_Type is record
      Raw_Args : XString_Array_Access;
      Results  : Parser_Result_Array_Access;
   end record;

   procedure Release (Self : in out Parsed_Arguments_Type);

   package Parsed_Arguments_Shared_Ptrs
   is new GNATCOLL.Refcount.Shared_Pointers
     (Parsed_Arguments_Type, Release => Release, Atomic_Counters => True);

   type Parsed_Arguments is record
      Ref : Parsed_Arguments_Shared_Ptrs.Ref
        := Parsed_Arguments_Shared_Ptrs.Null_Ref;
   end record;

   No_Parsed_Arguments : constant Parsed_Arguments :=
     (Ref => Parsed_Arguments_Shared_Ptrs.Null_Ref);

   type Argument_Parser is new Ada.Finalization.Limited_Controlled with record
      Data : Argument_Parser_Data_Access := null;
   end record;

   overriding procedure Finalize (Self : in out Argument_Parser);

end GNATCOLL.Opt_Parse;
