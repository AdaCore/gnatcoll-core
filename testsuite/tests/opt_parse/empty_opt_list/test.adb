
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
        (Parser      => Parser,
         Short       => "-n",
         Long        => "--numbers",
         Arg_Type    => Integer,
         Help        => "The numbers",
         Allow_Empty => True);

      package Numbers_2 is new Parse_Option_List
        (Parser      => Parser,
         Short       => "-n2",
         Name        => "Numbers 2",
         Arg_Type    => Integer,
         Help        => "The other numbers");
      pragma Unreferenced (Numbers_2);
   end Arg;

begin

   if Arg.Parser.Parse ((1 => +"--numbers")) then
      A.Assert (Arg.Numbers.Is_Set, "Args.Numbers should be set");
   else
      A.Assert (False, "Parsing failed, should have succeeded");
   end if;

   if Arg.Parser.Parse ((1 => +"-n2")) then
      A.Assert (False, "Parsing succeeded, should have failed");
   else
      A.Assert (True, "");
   end if;
   return A.Report;

end Test;
