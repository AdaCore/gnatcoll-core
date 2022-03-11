with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

package Mylib is

   package Args is

      Parser : Argument_Parser :=
         Create_Argument_Parser (Help => "Some_Tool help txt.", Command_Name => "some_tool");
      Sub_Parser_1 : Argument_Parser :=
         Create_Argument_Parser (Help => "Some_Tool Cmd1 help text.", Command_Name => "some_tool sub1");
      Sub_Parser_2 : Argument_Parser :=
         Create_Argument_Parser (Help => "Some_Tool Cmd2 help text.", Command_Name => "some_tool sub2");

      package Sub1_Option1 is new Parse_Option
        (Parser     => Sub_Parser_1,
         Long        => "--option1",
         Arg_Type    => Unbounded_String,
         Help        => "Some help text for sub command 1 option",
         Default_Val => To_Unbounded_String ("default-val"));

      package Sub2_Option1 is new Parse_Option
        (Parser     => Sub_Parser_2,
         Long        => "--option2",
         Arg_Type    => Unbounded_String,
         Help        => "Some help text for sub command 2 option",
         Default_Val => To_Unbounded_String ("default-val"));

      package Sub_Command_1 is new Parse_Sub_Command
        (Parser              => Parser,
         Name                => "sub1",
         Sub_Argument_Parser => Sub_Parser_1,
         Help                => "Some help text for sub command 1");

      package Sub_Command_2 is new Parse_Sub_Command
        (Parser              => Parser,
         Name                => "sub2",
         Sub_Argument_Parser => Sub_Parser_2,
         Help                => "Some help text for sub command 2");

   end Args;

   procedure Run;

end Mylib;
