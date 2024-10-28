------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;        use Ada.Text_IO;
with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Strings;   use GNATCOLL.Strings;

with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;

   function "+"
     (Self : String) return XString renames To_XString;

   type User_Enum is (Foo, Bar, Baz);

   package Arg is
      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Run Libadalang name resolution on a number of files");

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

   Out_Args : Parsed_Arguments := No_Parsed_Arguments;
begin

   if Arg.Parser.Parse
      ((+"-C", +"utf-8", +"a", +"b", +"c", +"d"), Result => Out_Args)
   then

      Put_Line ("Charset = " & To_String (Arg.Charset.Get (Out_Args)));
      for F of Arg.Files.Get (Out_Args) loop
         if not Arg.Quiet.Get (Out_Args) then
            Put_Line ("Got file " & To_String (F));
         end if;
      end loop;
      A.Assert (True, "Parsing");
   else
      A.Assert (False, "Parsing");
   end if;

   Out_Args := No_Parsed_Arguments;
   --  Test erroneous option
   A.Assert (not Arg.Parser.Parse ((+"-D", +"utf-8", +"a", +"b"), Out_Args),
             "Invalid option");

   --  Test option after list
   Out_Args := No_Parsed_Arguments;
   A.Assert
     (Arg.Parser.Parse ((+"-C", +"utf-8", +"a", +"b", +"--quiet"), Out_Args),
      "Flag after positional list");

   Out_Args := No_Parsed_Arguments;
   A.Assert
     (Arg.Parser.Parse ((+"-j", +"12", +"a"), Out_Args), "Integer value");

   Out_Args := No_Parsed_Arguments;
   A.Assert (not Arg.Parser.Parse ((+"-j", +"lol", +"a"), Out_Args),
             "Invalid integer value");

   Out_Args := No_Parsed_Arguments;
   A.Assert
     (not Arg.Parser.Parse ((+"a", +"-j"), Out_Args), "Incomplete option");

   A.Assert (Arg.Parser.Help /= "", "Printing help");

   Out_Args := No_Parsed_Arguments;
   if Arg.Parser.Parse ((+"--quiet", +"a", +"b"), Out_Args) then
      A.Assert (Arg.Quiet.Get (Out_Args), "Get flag with explicit val");
      A.Assert
        (Arg.Charset.Get (Out_Args) = "latin-1",
         "Get option with default val");
      A.Assert (Arg.Jobs.Get (Out_Args) = 1, "Check default job value");
   else
      A.Assert (False);
   end if;

   --  Test accumulate mode of option list
   Out_Args := No_Parsed_Arguments;
   if Arg.Parser.Parse ((+"-C", +"utf-8", +"a", +"-Xa=b", +"-Xc=d"), Out_Args)
   then
      A.Assert (Arg.Scenario_Vars.Get (Out_Args) (1) = "a=b");
      A.Assert (Arg.Scenario_Vars.Get (Out_Args) (2) = "c=d");
   else
      A.Assert (False);
   end if;

   --  Test erroneous option
   Out_Args := No_Parsed_Arguments;
   A.Assert
     (not Arg.Parser.Parse ((+"-E", +"boo"), Out_Args), "Invalid enum option");
   Out_Args := No_Parsed_Arguments;
   if Arg.Parser.Parse ((+"-E", +"foo")) then
      Put_Line (Arg.Enum_Opt.Get (Out_Args)'Image);
   end if;

   return A.Report;
end Test;
