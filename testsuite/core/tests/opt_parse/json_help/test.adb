with Ada.Containers.Vectors;
with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Strings;   use GNATCOLL.Strings;
with Test_Assert;

function Test return Integer is
   pragma Warnings (Off);

   function "+"
     (Self : String) return XString renames To_XString;

   package Int_Vectors is new Ada.Containers.Vectors (Positive, Integer);

   type Context is record
      Ints : Int_Vectors.Vector;
   end record;
   type Context_Array is array (Positive range <>) of Context;

   type User_Enum is (Foo, Bar, Baz);

   package Args is
      Parser : Argument_Parser :=
         Create_Argument_Parser (Help => "Test program");

      package Files is new Parse_Positional_Arg_List
        (Parser   => Parser,
         Name     => "files",
         Arg_Type => XString,
         Help     => "The files to parse");

      package Quiet is new Parse_Flag
        (Parser => Parser,
         Short  => "-q",
         Long   => "--quiet",
         Help   => "Whether the tool should be quiet or not");

      package Charset is new Parse_Option
        (Parser      => Parser,
         Short       => "-C",
         Long        => "--charset",
         Arg_Type    => XString,
         Help        =>
            "What charset to use for the analysis context. "
            & "Default is ""latin-1""",
         Default_Val => +"latin-1");

      package Jobs is new Parse_Option
        (Parser      => Parser,
         Short       => "-j",
         Long        => "--jobs",
         Arg_Type    => Integer,
         Help        => "Number of jobs",
         Default_Val => 1);

      package Scenario_Vars is new Parse_Option_List
        (Parser      => Parser,
         Short       => "-X",
         Long        => "--scenario-variable",
         Arg_Type    => XString,
         Help        => "Scenario variables",
         Accumulate  => True);

      package Enum_Opt is new Parse_Enum_Option
        (Parser      => Parser,
         Short       => "-E",
         Long        => "--enum-opt",
         Arg_Type    => User_Enum,
         Default_Val => Foo,
         Help        => "Enum option");
   end Args;

   Dummy : constant Boolean := Args.Parser.Parse
     ((1 => GNATCOLL.Strings.To_XString ("--json-help")));
begin
   return Test_Assert.Report;
end Test;
