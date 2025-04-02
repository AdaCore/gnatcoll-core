--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Opt_Parse.Misc_Parsers; use GNATCOLL.Opt_Parse.Misc_Parsers;
with GNATCOLL.Strings;   use GNATCOLL.Strings;

with Test_Assert;

function Test return Integer is

   pragma Extensions_Allowed (On);

   package A renames Test_Assert;

   function "+"
     (Self : String) return XString renames To_XString;

   package Arg is
      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Test");

      package The_Flag is new Parse_Indexed_Option_List
        (Parser   => Parser,
         Flag     => "--flag",
         Arg_Type => Integer,
         Help     => "The flag");
   end Arg;

begin

   --  Basic accumulative workflow: the flag can be passed several times with
   --  different indices, which will accumulate in the map.
   if Arg.Parser.Parse ((+"--flag:foo", +"219", +"--flag:bar", +"220")) then
      A.Assert (Arg.The_Flag.Get.Element ("foo").Contains (219),
         "foo should have contained 219");
      A.Assert (Arg.The_Flag.Get.Element ("bar").Contains (220),
         "bar should have contained 220");
   else
      A.Assert (False, "Parsing failed, should have succeeded");
   end if;

   Arg.Parser.Reset;

   --  Basic workflow with --flag:index=value format
   if Arg.Parser.Parse ((+"--flag:foo=219", +"--flag:bar=220")) then
      A.Assert (Arg.The_Flag.Get.Element ("foo").Contains (219),
         "foo should have contained 219");
      A.Assert (Arg.The_Flag.Get.Element ("bar").Contains (220),
         "bar should have contained 220");
   else
      A.Assert (False, "Parsing failed, should have succeeded");
   end if;

   --  Invalid flag: Should result in a parsing error
   if Arg.Parser.Parse ((+"--flag:", +"123")) then
      A.Assert (False);
   end if;

   --  Invalid flag: Should result in a parsing error
   if Arg.Parser.Parse ((+"--flag", +"123")) then
      A.Assert (Arg.The_Flag.Get.Element ("").Contains (123), "Wrong num");
   end if;

   --  Flag value passed twice
   if Arg.Parser.Parse
      ((+"--flag:foo", +"123", +"--flag:foo", +"456", +"--flag:bar", +"789"))
   then
      A.Assert (Arg.The_Flag.Get.Element ("foo").Contains (123));
      A.Assert (Arg.The_Flag.Get.Element ("foo").Contains (456));
      A.Assert (not Arg.The_Flag.Get.Element ("foo").Contains (789));
   end if;

   --  Check the regexp supports "+" in the keys
   if Arg.Parser.Parse ((+"--flag:c++=1", +"--flag:cxx=2")) then
      A.Assert (Arg.The_Flag.Get.Element ("c++").Contains (1));
      A.Assert (Arg.The_Flag.Get.Element ("cxx").Contains (2));
   else
      A.Assert (False, "Parsing failed, should have succeeded");
   end if;

   --  Incomplete option
   A.Assert
     (not Arg.Parser.Parse ((1 => +"--flag:foo")));

   return A.Report;

end Test;
