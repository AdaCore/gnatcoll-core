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

      package Quiet is new Parse_Flag
        (Parser => Parser,
         Short  => "-q",
         Long   => "--quiet",
         Help   => "Whether the tool should be quiet or not");

   end Arg;

begin

   Disallow (Arg.Quiet.This, "Disabled");

   if Arg.Parser.Parse ((1 => +"-q")) then
      A.Assert (False, "Parsing succeeded, should have failed");
   else
      A.Assert
        (Arg.Parser.Last_Error,
         "for option quiet - Disabled", "Wrong exception message");
   end if;

   Allow (Arg.Quiet.This);

   if not Arg.Parser.Parse ((1 => +"-q")) then
      A.Assert (False, "Parsing failed, should have succeeded");
   end if;

   return A.Report;

end Test;
