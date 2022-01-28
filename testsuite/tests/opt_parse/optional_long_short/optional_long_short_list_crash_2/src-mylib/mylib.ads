with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

package Mylib is

   package Args is
      Parser : Argument_Parser :=
         Create_Argument_Parser (Help => "Test program");

      package Files_Option1 is new Parse_Option_List
        (Parser      => Parser,
         Short       => "-F1",
         Arg_Type    => Unbounded_String,
         Help        => "List of files to be parsed.");

   end Args;

   procedure Run;

end Mylib;
