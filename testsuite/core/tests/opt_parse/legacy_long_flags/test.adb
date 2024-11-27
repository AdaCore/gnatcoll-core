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

      package Rules is new Parse_Option_List
        (Parser                    => Parser,
         Short                     => "-r",
         Long                      => "--rule",
         Arg_Type                  => XString,
         Help                      => "",
         Accumulate                => True,
         Allow_Collated_Short_Form => False);

      package Rule_File is new Parse_Option
        (Parser                    => Parser,
         Long                      => "-rule-file",
         Arg_Type                  => XString,
         Default_Val               => +"",
         Help                      => "The rule file",
         Legacy_Long_Form          => True);
   end Arg;

   use all type Arg.Rules.Result_Array;

begin

   if Arg.Parser.Parse
     ((+"-rule-file", +"rules.txt", +"-r", +"foo", +"-r", +"bar"))
   then
      A.Assert (Arg.Rules.Get = (+"foo", +"bar"), "Wrong rules array");
      A.Assert (Arg.Rule_File.Get = +"rules.txt", "Wrong rule file");
   else
      A.Assert (False, "Parsing failed, should have succeeded");
   end if;

   return A.Report;

end Test;
