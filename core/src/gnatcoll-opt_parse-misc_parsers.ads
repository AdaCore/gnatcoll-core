--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Hash;

package GNATCOLL.Opt_Parse.Misc_Parsers is

   --  .. warning:: This is not part of the supported surface of
   --     GNATCOLL.Opt_Parse, but instead is meant for internal AdaCore use. As
   --     such, no guarantees are provided in terms of API stability. Use at
   --     your own risk.
   --
   --  This package contains AdaCore specific parsers, meant to be used in
   --  AdaCore products.

   generic
      Parser : in out Argument_Parser;
      --  Argument_Parser owning this argument.

      Flag : String := "";
      --  Name of the flag (only long flags are supported for indexed options)

      Help : String := "";
      --  Help string for the argument.

      type Arg_Type is private;
      --  Type of the option.

      with function Convert (Arg : String) return Arg_Type is <>;
      --  Conversion function to convert from a raw string argument to the
      --  argument type.

      Enabled : Boolean := True;
      --  Whether to add this argument parser

      Usage_Text : String := "";
      --  Usage string for the argument. When left empty default usage text
      --  will be generated in the form of [--Long|-Short LONG].

      Name : String := "";
      --  Name of the parser. Must be provided if Long is not provided.
      --  This is used to build up the --help text.
      --  Name will be used if both Name and Long are non-empty strings.
   package Parse_Indexed_Option_List is

      package Result_Vector is new Ada.Containers.Indefinite_Vectors
        (Natural, Arg_Type);
      package Result_Maps is new Ada.Containers.Indefinite_Hashed_Maps
        (String,
         Result_Vector.Vector,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "=",
         "="             => Result_Vector."=");

      type Result_Map_Access is access all Result_Maps.Map;

      ----------------------
      -- Public interface --
      ----------------------

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments)
      return Result_Map_Access;

      function This return Subparser;
      --  Return the subparser instantiated by this package

   end Parse_Indexed_Option_List;
   --  Parse an indexed option. An indexed option is like a regular option + an
   --  index in the name. This allows to have a key-value association map
   --  mapping indices to values. Values are automatically stored as lists,
   --  and if you pass several values for a given key, the values are
   --  concatenated. For example, given the following command line:
   --
   --  --foo:bar 12 --foo:baz 15 --foo:baz 42
   --
   --  Will associate the value "12" with the key "bar", and the values
   --  "15" and "42" with the key "baz".

end GNATCOLL.Opt_Parse.Misc_Parsers;
