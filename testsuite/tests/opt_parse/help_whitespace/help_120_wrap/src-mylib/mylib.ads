with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

package Mylib is

   package Args is
      Parser : Argument_Parser :=
         Create_Argument_Parser (Help => "Test program",
                                 Help_Column_Limit => 120);

      package Charset_Option1 is new Parse_Option
         (Parser      => Parser,
         Short       => "-C",
         Long        => "--char",
         Arg_Type    => Unbounded_String,
         Help        =>
            "A long help message that exceeds the default 80"
             & " character limit but does not exceed a 120 limit",
         Default_Val => To_Unbounded_String ("latin-1"));

   end Args;

   procedure Run;

end Mylib;
