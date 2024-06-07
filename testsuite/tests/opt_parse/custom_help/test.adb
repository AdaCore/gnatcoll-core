with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Strings;   use GNATCOLL.Strings;

with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;

   function "+"
     (Self : String) return XString renames To_XString;

   package Arg is
      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Empty parser", Generate_Help_Flag => False);

      package Help is new Parse_Flag
        (Parser => Parser,
         Short  => "-h",
         Long   => "--help",
         Help   => "Custom help");

   end Arg;
begin

   if Arg.Parser.Parse ((1 => +"--help"))
   then
      A.Assert (Arg.Help.Get, "Custom help flag not triggered");
   else
      A.Assert (False, "Arg parsing failed, should have succeeded");
   end if;

   return A.Report;
end Test;
