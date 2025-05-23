--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1

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

with Ada.Containers.Vectors;
with GNATCOLL.Refcount;
with GNATCOLL.JSON; use GNATCOLL.JSON;
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
   --
   --  .. note:: If you can, you should activate the ``-gnatw.a`` warning when
   --     using ``GNATCOLL.Opt_Parse``. This will emit warnings when you're not
   --     respecting invariants in your parser declaration. For example:
   --
   --  .. code:: ada
   --
   --     package Opt is new Parse_Option
   --       (Parser      => Parser,
   --        Arg_Type    => Unbounded_String,
   --        Name        => "Option",
   --        Help        => "Help");
   --
   --  Will emit a warning because your option has neither a short or a long
   --  flag name.


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

   subtype Col_Type is Integer range -2 .. Integer'Last;
   --  Type for a column in the text wrapper.

   No_Arguments : constant XString_Array (1 .. 0) := (others => <>);
   --  Constant for the absence of command line arguments.

   No_Parsed_Arguments : constant Parsed_Arguments;
   --  Constant for a null Parsed_Arguments value.

   package XString_Vectors is new Ada.Containers.Vectors (Positive, XString);

   subtype XString_Vector is XString_Vectors.Vector;
   --  Vector of XStrings. Used to fill unknown args in calls to ``Parse``.

   -----------------
   -- Parser type --
   -----------------

   type Subparser is private;
   --  This represents a subparser inside an argument parser.
   --
   --  You generally don't have to use this type directly, since you'll declare
   --  subparsers by instantiating generic packages, and get results via the
   --  generic ``Get`` procedures.
   --
   --  Having access to parsers allow you to do some introspection which can be
   --  useful in some cases.

   procedure Allow (Self : Subparser);
   --  Allow the parser

   procedure Disallow (Self : Subparser; Message : String);
   --  Disallow the parser. If the corresponding argument is successfully
   --  parsed, an error will be raised with the corresponding message.

   --------------------
   -- Error handlers --
   --------------------

   --  Machinery to allow the user to specify custom error handling mechanisms
   --  if an error should occur during argument processing in Opt_Parse

   type Error_Handler is abstract tagged null record;
   procedure Error (Self : in out Error_Handler; Msg : String) is abstract;
   procedure Warning (Self : in out Error_Handler; Msg : String) is abstract;
   procedure Release (Self : in out Error_Handler) is null;

   procedure Release_Wrapper (Handler : in out Error_Handler'Class);
   --  Wrapper around Error_Handler.Release

   package Error_Handler_References is new GNATCOLL.Refcount.Shared_Pointers
     (Error_Handler'Class,
      Release => Release_Wrapper,
      Atomic_Counters => True);

   subtype Error_Handler_Ref is Error_Handler_References.Ref;

   Null_Ref : Error_Handler_Ref renames Error_Handler_References.Null_Ref;

   function Create (Handler : Error_Handler'Class) return Error_Handler_Ref;

   -------------------------------
   -- General parser primitives --
   -------------------------------

   function Create_Argument_Parser
     (Help                 : String;
      Command_Name         : String := "";
      Help_Column_Limit    : Col_Type := 80;
      Incremental          : Boolean := False;
      Generate_Help_Flag   : Boolean := True;
      Custom_Error_Handler : Error_Handler_Ref := Null_Ref;
      Print_Help_On_Error  : Boolean := True)
   return Argument_Parser;
   --  Create an argument parser with the provided help string.
   --
   --  ``Command_Name`` refers to the name of your command/executable. This
   --  will be used when generating the help string.
   --
   --  ``Help_Column_Limit`` is the number of columns you want the help string
   --  to be formatted to.
   --
   --  ``Incremental`` activates the incremental mode. In this mode, you can
   --  call ``Parse`` several times on your parser, with a given set of
   --  ``Parsed_Arguments``, without those results being reset every time.
   --  Instead, results will be accumulated. The consequence is also that a
   --  given argument can be passed several times without triggering an
   --  error in ``Parse``. This is useful in the context of GNAT's tools,
   --  where you often need to process arguments in several passes.
   --
   --  ``Generate_Help_Flag`` will condition the generation of the ``--help``
   --  flag. Some tools might wish to deactivate it to handle it manually.
   --
   --  ``Error_Handler`` is the handler that will be used in case of error
   --  or warning, to process the associated error message.

   function Help (Self : Argument_Parser) return String;
   --  Return the help for this parser as a String.

   function JSON_Help (Self : Argument_Parser) return JSON_Value;
   --  Return the help for this parser as JSON.
   --
   --  The format of the emitted json will be::
   --
   --     { "help": "global help string",
   --       "optional_parsers": [list of optional subparsers],
   --       "positional_parsers": [list of positional subparsers] }
   --
   --  The format of invididual subparsers will be::
   --
   --     { "name": subparser name,
   --       "kind": subparser kind: one of "flag", "option", "list_option",
   --               "list_option_accumulate", "positional_arg",
   --               "positional_list"
   --       "help": subparser help,
   --
   --       for flag parsers:
   --
   --       "short_flag": short flag string
   --       "long_flag": long flag string
   --     }

   function Last_Error (Self : Argument_Parser) return String;
   --  Return the last error produced by this parser if there is one, the empty
   --  string otherwise.

   procedure Reset (Self : Argument_Parser);
   --  Reset the implicit default results for this parser. This is useful for
   --  users who use the incremental mode, in conjunction with the implicit
   --  default results.

   ------------------------
   -- Parse entry points --
   ------------------------

   --  Those ``Parse`` functions are the entry points to run the argument parse
   --  on a set of command line arguments.
   --
   --  In every case, Arguments can be an explicit argument array. If not
   --  passed, arguments will be parsed from the application's command line.
   --
   --  Those functions will return ``False`` if there is an error during
   --  parsing, after printing the error on stdout.
   --
   --  .. note:: todo, we probably want to print errors on stderr rt. stdout.
   --
   --  In overloads without an explicit ``Result``, Results are stored in the
   --  implicit default ``Parsed_Arguments`` instance. This means that you can
   --  directly call the corresponding ``Get`` function in parsers to get the
   --  parsed result.

   function Parse
     (Self         : in out Argument_Parser;
      Arguments    : XString_Array := No_Arguments) return Boolean;
   --  Parse the command line arguments for Self.

   function Parse
     (Self         : in out Argument_Parser;
      Arguments    : XString_Array := No_Arguments;
      Unknown_Arguments : out XString_Vector) return Boolean;
   --  Parse the command line arguments for Self.
   --  Unknown arguments will be put in ``Unknown_Arguments``, and no error
   --  will be raised.

   function Parse
     (Self         : in out Argument_Parser;
      Arguments    : XString_Array := No_Arguments;
      Result       : out Parsed_Arguments) return Boolean;
   --  Parse command line arguments for Self. Return arguments explicitly in
   --  ``Result``.

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

   function List_Stop_Predicate (S : XString) return Boolean
   is (S.Starts_With ("-"));
   --  Default ``List_Stop_Predicate`` for ``Parse_Option_List``. Will stop
   --  when the next argument starts with '-'.

   Opt_Parse_Error : exception;
   --  Exception signaling an error in the parser. This is the error that you
   --  will get in the rare cases where you do something invalid with a Parser
   --  (such as querying results without calling parse first), and this is
   --  also the exception that you should raise in conversion functions when
   --  receiving an invalid value.

   Disabled_Error : exception;
   --  Exception raised when trying to get the value of a disabled argument
   --  parser that is not a list and provides no default value.

   -----------------------------------
   --  Specific argument subparsers --
   -----------------------------------

   --  Subparser are created by instantiating generic packages. This allows
   --  having precise type signatures for parser's result. Every subparser's
   --  generic package will have at least a signature like:
   --
   --  .. code-block:: ada
   --
   --     generic
   --        Parser  : in out Argument_Parser;
   --        Name    : String;
   --        Help    : String;
   --        Enabled : Boolean := True;
   --     package <...> is
   --       function Get
   --         (Args : Parsed_Arguments := No_Parsed_Arguments)
   --       return Result_Type;
   --       --  Get the result for this parser
   --
   --       function This return Subparser;
   --       --  Return the subparser instance created by this package
   --       --  instantiation.
   --     end <...>;
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

      function This return Subparser;
      --  Return the subparser instantiated by this package

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

      function This return Subparser;
      --  Return the subparser instantiated by this package

   end Parse_Positional_Arg;
   --  Parse a positional argument. A positional argument is any argument. If
   --  the conversion fails, then it will make the whole argument parser fail.

   generic
      Parser : in out Argument_Parser;
      --  Argument_Parser owning this argument.

      Short : String := "";
      --  Short form for this flag. Should start with one dash and be followed
      --  by one or two alphanumeric characters.
      --
      --  This can be left empty (i.e. ``Short = ""``) if you don't want this
      --  argument to have a short form.

      Long : String := "";
      --  Long form for this flag. Should start with two dashes.
      --  This can be left empty (i.e. ``Long = ""``) if you don't want this
      --  argument to have a long form. In this case you must provide a
      --  non-empty Name (i.e. Name /= "") to be used in help text.

      Help : String := "";
      --  Help string for the argument.

      Enabled : Boolean := True;
      --  Whether to add this argument parser

      Name : String := "";
      --  Name of the parser. Must be provided if Long is not provided.
      --  This is used to build up the --help text.
      --  Name will be used if both Name and Long are non-empty strings.

      Legacy_Long_Form : Boolean := False;
      --  If true, relax the rule about long flags starting with two dashes,
      --  and allow long names starting with only one dash.
      --
      --  .. warning:: This is only for legacy tools. Use of this flag in new
      --     tools is strictly discouraged!
   package Parse_Flag is

      ----------------------
      -- Public interface --
      ----------------------

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Boolean;

      function This return Subparser;
      --  Return the subparser instantiated by this package

   end Parse_Flag;
   --  Parse a Flag option. A flag takes no other argument, and its result is a
   --  boolean: False if the flag is not passed, True otherwise.

   generic
      Parser : in out Argument_Parser;
      --  Argument_Parser owning this argument.

      Short : String := "";
      --  Short form for this flag. Should start with one dash and be followed
      --  by one or two alphanumeric characters.
      --
      --  This can be left empty (i.e. ``Short = ""``) if you don't want this
      --  argument to have a short form.

      Long : String := "";
      --  Long form for this flag. Should start with two dashes.
      --  This can be left empty (i.e. ``Long = ""``) if you don't want this
      --  argument to have a long form. In this case you must provide a
      --  non-empty Name (i.e. Name /= "") to be used in help text.

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

      Usage_Text : String := "";
      --  Usage string for the argument. When left empty default usage text
      --  will be generated in the form of [--Long|-Short LONG].

      Name : String := "";
      --  Name of the parser. Must be provided if Long is not provided.
      --  This is used to build up the --help text.
      --  Name will be used if both Name and Long are non-empty strings.

      Allow_Collated_Short_Form : Boolean := True;
      --  Whether to allow the collated short form -Ovalue. Deactivating this
      --  form can help workaround ambiguities if you have legacy-style long
      --  options with one dash, such as ``-rules`` in GNATcheck.

      Legacy_Long_Form : Boolean := False;
      --  If true, relax the rule about long flags starting with two dashes,
      --  and allow long names starting with only one dash.
      --
      --  .. warning:: This is only for legacy tools. Use of this flag in new
      --     tools is strictly discouraged!
   package Parse_Option is

      ----------------------
      -- Public interface --
      ----------------------

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Arg_Type;

      function This return Subparser;
      --  Return the subparser instantiated by this package
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
      --
      --  This can be left empty (i.e. ``Short = ""``) if you don't want this
      --  argument to have a short form.

      Long : String := "";
      --  Long form for this flag. Should start with two dashes.
      --  This can be left empty (i.e. ``Long = ""``) if you don't want this
      --  argument to have a long form. In this case you must provide a
      --  non-empty Name (i.e. Name /= "") to be used in help text.

      Help : String := "";
      --  Help string for the argument.

      type Arg_Type is (<>);
      --  Type of the option.

      Default_Val : Arg_Type;
      --  Default value if the option is not passed.

      Enabled : Boolean := True;
      --  Whether to add this argument parser

      Usage_Text : String := "";
      --  Usage string for the argument. When left empty default usage text
      --  will be generated in the form of [--Long|-Short LONG].

      Name : String := "";
      --  Name of the parser. Must be provided if Long is not provided.
      --  This is used to build up the --help text.
      --  Name will be used if both Name and Long are non-empty strings.

      Legacy_Long_Form : Boolean := False;
      --  If true, relax the rule about long flags starting with two dashes,
      --  and allow long names starting with only one dash.
      --
      --  .. warning:: This is only for legacy tools. Use of this flag in new
      --     tools is strictly discouraged!
   package Parse_Enum_Option is
      pragma Compile_Time_Error
        (Arg_Type'Type_Class /= Type_Class_Enumeration,
         "Arg_Type for Parse_Enum_Option needs to be an enum type");

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Arg_Type;

      function This return Subparser;
      --  Return the subparser instantiated by this package
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
      --
      --  This can be left empty (i.e. ``Short = ""``) if you don't want this
      --  argument to have a short form.

      Long : String := "";
      --  Long form for this flag. Should start with two dashes.
      --  This can be left empty (i.e. ``Long = ""``) if you don't want this
      --  argument to have a long form. In this case you must provide a
      --  non-empty Name (i.e. Name /= "") to be used in help text.

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

      Usage_Text : String := "";
      --  Usage string for the argument. When left empty default usage text
      --  will be generated in the form of [--Long|-Short LONG [LONG...]].

      Name : String := "";
      --  Name of the parser. Must be provided if Long is not provided.
      --  This is used to build up the --help text.
      --  Name will be used if both Name and Long are non-empty strings.

      Allow_Empty : Boolean := False;
      --  Whether empty lists are allowed or not.

      with function List_Stop_Predicate (S : XString) return Boolean is <>;
      --  Predicate used to detect that we should stop parsing. Customizing
      --  that allows to implement "section-like" behavior.
      --  By default, it will stop on the first argument that starts with a '-'
      --  character.

      Allow_Collated_Short_Form : Boolean := True;
      --  Whether to allow the collated short form -Ovalue. Deactivating this
      --  form can help workaround ambiguities if you have legacy-style long
      --  options with one dash, such as ``-rules`` in GNATcheck.

      Legacy_Long_Form : Boolean := False;
      --  If true, relax the rule about long flags starting with two dashes,
      --  and allow long names starting with only one dash.
      --
      --  .. warning:: This is only for legacy tools. Use of this flag in new
      --     tools is strictly discouraged!
   package Parse_Option_List is
      type Result_Array is array (Positive range <>) of Arg_Type;

      No_Results : constant Result_Array (1 .. 0) := (others => <>);

      ----------------------
      -- Public interface --
      ----------------------

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Result_Array;

      function Is_Set
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Boolean;
      --  Whether this list has been explicitly set. Useful if explicit empty
      --  lists are allowed, if an explicit empty list has a different
      --  meaning than an implicit empty list.

      function This return Subparser;
      --  Return the subparser instantiated by this package
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

   type Subparser_Type is abstract tagged record
      Name : XString;
      --  Name of the parser

      Help : XString;
      --  Help string for the parser

      Position : Positive;
      --  Position of this parser in its enclosing Arguments_Parser

      Opt : Boolean := True;
      --  Whether this parser is optional or not

      Disallow_Msg : XString := Null_XString;
      --  Error message to return if the parser is disallowed. This also serves
      --  as a flag: if the message is null, then the parser is allowed.

      Parser : Argument_Parser_Data_Access;
   end record;

   subtype Parser_Return is Integer range -1 .. Integer'Last;
   --  Return value of a Parser. Represents a position, except for the special
   --  value Error_Return.

   Error_Return : constant Parser_Return := 0;
   --  Special value for Parser_Return when there was an error

   function Parse_Args
     (Self   : in out Subparser_Type;
      Args   : XString_Array;
      Pos    : Positive;
      Result : in out Parsed_Arguments) return Parser_Return
   is abstract;
   --  Return the result of parsing arguments for this parser. Abstract method
   --  that must be overloaded by implementations.

   function Usage
     (Self : Subparser_Type) return String is abstract;
   --  Return a usage string for this parser. Abstract method that must be
   --  overloaded.

   function JSON_Kind (Self : Subparser_Type) return String is abstract;
   --  Return the kind of the parser, for JSON introspection purposes.

   function Parse
     (Self   : in out Subparser_Type'Class;
      Args   : XString_Array;
      Pos    : Positive;
      Result : in out Parsed_Arguments) return Parser_Return;
   --  Return the result of parsing arguments for this parser. Function wrapper
   --  around `Parse_Args` that is called by Arguments_Parser.

   function JSON_Help
     (Self : Subparser_Type) return JSON_Value;
   --  Return a JSON object representing the help for this subparser. This
   --  procedure is not to be overriden, fields should be added by overriding
   --  the ``Init_JSON_Help`` procedure.

   procedure Init_JSON_Help (Self : Subparser_Type; Val : JSON_Value);
   --  This procedure can be overriden to add fields to ``Val``, which will be
   --  the JSON representation returned by ``JSON_Help``.

   function Help_Name
     (Self : Subparser_Type) return String
   is
     (To_String (Self.Name));
   --  Return the help name for this parser.

   function Does_Accumulate
     (Self : Subparser_Type) return Boolean is (False);
   --  Whether this parser accumulates results or not. If it does, then it is
   --  valid to call Parse on it several time, which will add to results.

   type Subparser is access all Subparser_Type'Class;

   package Parsers_Vectors
   is new Ada.Containers.Vectors (Positive, Subparser);

   subtype Parser_Vector is Parsers_Vectors.Vector;

   type Argument_Parser_Data is record
      Help, Command_Name : XString;

      Last_Error : XString;
      --  Last error that was generated by the parser

      Positional_Args_Parsers, Opts_Parsers : Parser_Vector;
      All_Parsers                           : Parser_Vector;
      Default_Result                        : Parsed_Arguments
        := No_Parsed_Arguments;

      Help_Flag                             : Subparser := null;
      JSON_Help_Flag                        : Subparser := null;
      --  Subparsers for Help/JSON Help flags

      Mutex : aliased Mutual_Exclusion;
      --  Mutex used to make Get_Result thread safe

      Help_Column_Limit : Col_Type := 80;

      Incremental : Boolean := False;
      --  Whether this parse is in incremental or normal mode. See the
      --  documentation in `Create_Argument_Parser`.

      Custom_Error_Handler : Error_Handler_Ref
        := Error_Handler_References.Null_Ref;
      --  Callback to call in case of error/warning. If null, errors and
      --  warnings will be emitted on stderr.

      Print_Help_On_Error : Boolean := True;
   end record;

   type Parser_Result is abstract tagged record
      Start_Pos, End_Pos : Positive;
      Already_Parsed_In_Current_Pass : Boolean := False;
      --  Internal flag used to represents the fact that a value has already
      --  been parsed for the associated option in the current parsing pass.
      --  This flag is reset to ``False`` at the end of the ``Parse_Impl``
      --  function.
   end record;

   procedure Release (Result : in out Parser_Result) is abstract;
   --  Derived types must override this to clean-up internal data when the
   --  Parser_Result object is about to be deallocated.

   type Parser_Result_Access is access all Parser_Result'Class;

   function Get_Result
     (Self : Subparser_Type'Class;
      Args : Parsed_Arguments) return Parser_Result_Access;

   function Has_Result
     (Self : Subparser_Type'Class;
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

   generic
      Short            : String;
      Long             : String;
      Name             : String;
      Legacy_Long_Form : Boolean := False;
   package Flag_Invariants is
      pragma Assertion_Policy (Assert => Check);
      --  We always want to check those assertions

      pragma Assert
        (Short'Length = 0 or else Short (1) = '-',
         "Short flag should start with a dash");

      pragma Assert
         (Legacy_Long_Form
          or else (Long'Length = 0 or else Long (1 .. 2) = "--"),
         "Long flag should start with two dashes");

      pragma Assert
         ((not Legacy_Long_Form)
          or else (Long'Length = 0 or else Long (1 .. 1) = "-"),
         "Legacy long flag should start with one dash");

      pragma Assert
        (Long'Length > 0 or else Name'Length > 0,
         "Name should be non empty if there is no long flag");

      pragma Assert
         (Long'Length > 0 or else Short'Length > 0,
          "You should have either a long or a short flag");

      pragma Assert
        (Short'Length = 0 or else Short (2)  in 'a' .. 'z' | 'A' .. 'Z',
         "Short flag should start with an alphabetic character");

      pragma Assert
        (Long'Length = 0 or else Long (3)  in 'a' .. 'z' | 'A' .. 'Z',
         "Long flag should start with an alphabetic character");
   end Flag_Invariants;
   --  This package is an helper package, helping check some invariants at
   --  runtime. The neat thing about using `pragma Assert` is that in a wide
   --  variety of use cases, GNAT is actually able to warn you about violating
   --  those invariants at compile time.

   function "+" (Self : String) return XString renames To_XString;
   function "+" (Self : XString) return String renames To_String;

end GNATCOLL.Opt_Parse;
