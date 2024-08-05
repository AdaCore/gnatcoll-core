
with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Strings;   use GNATCOLL.Strings;

with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;

   function "+"
     (Self : String) return XString renames To_XString;

   package Arg is
      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Test");

      package Numbers is new Parse_Option_List
        (Parser                    => Parser,
         Short                     => "-n",
         Long                      => "--numbers",
         Arg_Type                  => Integer,
         Help                      => "The numbers",
         Accumulate                => True,
         Allow_Collated_Short_Form => False);

      package Number is new Parse_Option
        (Parser                    => Parser,
         Short                     => "-N",
         Long                      => "--number",
         Arg_Type                  => Integer,
         Default_Val               => 0,
         Help                      => "The number",
         Allow_Collated_Short_Form => False);

      pragma Unreferenced (Numbers);
      pragma Unreferenced (Number);
   end Arg;

begin

   A.Assert
     (not Arg.Parser.Parse ((1 => +"-n123")),
      "Collated form shouldn't be accepted");

   A.Assert
     (not Arg.Parser.Parse ((1 => +"-N123")),
      "Collated form shouldn't be accepted");

   return A.Report;

end Test;
