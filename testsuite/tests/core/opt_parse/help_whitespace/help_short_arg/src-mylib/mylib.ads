with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

package Mylib is

   package Args is
      Parser : Argument_Parser :=
         Create_Argument_Parser (Help => "Test program");

      package Charset_Option1 is new Parse_Option
         (Parser      => Parser,
         Short       => "-C",
         Long        => "--char",
         Arg_Type    => Unbounded_String,
         Help        =>
            "What charset to use when parsing files. "
            & "Default is ""latin-1""",
         Default_Val => To_Unbounded_String ("latin-1"));

   end Args;

   procedure Run;

end Mylib;
