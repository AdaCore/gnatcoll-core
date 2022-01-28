with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

package Mylib is

   package Args is
      Parser : Argument_Parser :=
         Create_Argument_Parser (Help => "Test program");

      package Flag_Option1 is new Parse_Flag
        (Parser      => Parser,
         Short       => "-F1",
         Help        => "Some random flag help text");

   end Args;

   procedure Run;

end Mylib;
