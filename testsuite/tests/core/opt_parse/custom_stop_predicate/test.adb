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

      function Stop_On_Non_Rule (S : XString) return Boolean is
        (not (S.Starts_With ("+R") or S.Starts_With ("-R")));

      package Rules is new Parse_Option_List
        (Parser              => Parser,
         Short               => "-r",
         Long                => "--rules",
         Arg_Type            => XString,
         Help                => "Rules",
         List_Stop_Predicate => Stop_On_Non_Rule);

      package Quiet is new Parse_Flag
        (Parser => Parser,
         Short  => "-q",
         Long   => "--quiet",
         Help   => "Whether the tool should be quiet or not");

   end Arg;

   use type Arg.Rules.Result_Array;

   Rules : constant Arg.Rules.Result_Array :=
     (+"+Rgoto_statements", +"-Rdeactivate");

begin

   if Arg.Parser.Parse
     ((+"--rules", +"+Rgoto_statements", +"-Rdeactivate", +"--quiet"))
   then
      A.Assert (Arg.Rules.Get = Rules, "Wrong num array");
      A.Assert (Arg.Quiet.Get);
   else
      A.Assert (False, "Parsing failed, should have succeeded");
   end if;

   return A.Report;

end Test;
