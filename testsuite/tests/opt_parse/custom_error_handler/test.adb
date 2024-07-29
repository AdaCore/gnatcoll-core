with Support;

with Ada.Exceptions;     use Ada.Exceptions;
with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Strings;   use GNATCOLL.Strings;

with Test_Assert;

function Test return Integer is
   pragma Warnings (Off);
   pragma Style_Checks (Off);
   pragma Extensions_Allowed (On);

   package A renames Test_Assert;

   function "+"
     (Self : String) return XString renames To_XString;

   type User_Enum is (Foo, Bar, Baz);

   Handler_Ref : Error_Handler_Ref := Create (Support.Custom_Error_Handler'(others => <>));

   Handler : Support.Custom_Error_Handler
   renames Support.Custom_Error_Handler (Handler_Ref.Unchecked_Get.all);

   package Arg is
      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Help", Custom_Error_Handler => Handler_Ref);

      package Files is new Parse_Positional_Arg_List
        (Parser   => Parser,
         Name     => "files",
         Arg_Type => XString,
         Help     => "The files to parse");

      package Quiet is new Parse_Flag
        (Parser => Parser,
         Short  => "-q",
         Long   => "--quiet",
         Help   => "Whether the tool should be quiet or not");

      package Charset is new Parse_Option
        (Parser      => Parser,
         Short       => "-C",
         Long        => "--charset",
         Arg_Type    => XString,
         Help        =>
            "What charset to use for the analysis context. "
            & "Default is ""latin-1""",
         Default_Val => +"latin-1");

      package Jobs is new Parse_Option
        (Parser      => Parser,
         Short       => "-j",
         Long        => "--jobs",
         Arg_Type    => Integer,
         Help        => "Number of jobs",
         Default_Val => 1);

      package Scenario_Vars is new Parse_Option_List
        (Parser      => Parser,
         Short       => "-X",
         Long        => "--scenario-variable",
         Arg_Type    => XString,
         Help        => "Scenario variables",
         Accumulate  => True);

      package Enum_Opt is new Parse_Enum_Option
        (Parser      => Parser,
         Short       => "-E",
         Long        => "--enum-opt",
         Arg_Type    => User_Enum,
         Default_Val => Foo,
         Help        => "Enum option");
   end Arg;

begin

   begin
      --  Trying to get a param before any parsing happened should raise an
      --  error.
      if Arg.Quiet.Get then
         null;
      end if;
      A.Assert (False, "An error should have been raised");
   exception
      when E : Opt_Parse_Error =>
         A.Assert
           (Exception_Message (E),
            "No results for command line arguments",
            "Wrong exception message");
   end;

   if Arg.Parser.Parse ((+"-E", +"Boz")) then
      A.Assert (False, "An error should have been raised");
   else
      A.Assert
        (Handler.Last_Error.To_String,
         "for option enum-opt - Invalid input value for enum: ""Boz""",
         "Wrong error message");
   end if;

   if Arg.Parser.Parse ((+"-j", +"wrong")) then
      A.Assert (False, "An error should have been raised");
   else
      A.Assert
        (Handler.Last_Error.To_String,
         "for option jobs - wrong value for Integer: ""wrong""",
         "Wrong error message");
   end if;

   if Arg.Parser.Parse ((1 => +"-j")) then
      A.Assert (False, "An error should have been raised");
   else
      A.Assert
        (Handler.Last_Error.To_String,
         "for option jobs - Incomplete option",
         "Wrong error message");
   end if;

   if Arg.Parser.Parse ((1 => +"--what")) then
      A.Assert (False, "An error should have been raised");
   else
      A.Assert
        (Handler.Last_Error.To_String,
         "Unrecognized argument --what",
         "Wrong error message");
   end if;

   return A.Report;
end Test;
