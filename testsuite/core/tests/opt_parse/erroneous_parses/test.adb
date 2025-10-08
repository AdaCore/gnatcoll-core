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

with Ada.Exceptions;     use Ada.Exceptions;
with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Strings;   use GNATCOLL.Strings;

with Test_Assert;

function Test return Integer is
   pragma Warnings (Off);
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

   package Arg2 is
      Parser : Argument_Parser := Create_Argument_Parser (Help => "Parser");
      package F is new Parse_Positional_Arg
        (Parser => Parser, Name => "file", Arg_Type => XString);
      package Jobs is new Parse_Option
        (Parser => Parser, Short => "-j", Long => "--jobs",
         Arg_Type => Integer, Default_Val => 1);
   end Arg2;

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
        (Arg.Parser.Last_Error,
         "for option enum-opt - Invalid input value for enum: ""Boz""",
         "Wrong error message");
   end if;

   if Arg.Parser.Parse ((+"-j", +"wrong")) then
      A.Assert (False, "An error should have been raised");
   else
      A.Assert
        (Arg.Parser.Last_Error,
         "for option jobs - wrong value for Integer: ""wrong""",
         "Wrong error message");
   end if;

   if Arg.Parser.Parse ((1 => +"-j")) then
      A.Assert (False, "An error should have been raised");
   else
      A.Assert
        (Arg.Parser.Last_Error,
         "for option jobs - Incomplete option",
         "Wrong error message");
   end if;

   if Arg.Parser.Parse ((1 => +"--what")) then
      A.Assert (False, "An error should have been raised");
   else
      A.Assert
        (Arg.Parser.Last_Error,
         "Unrecognized argument ""--what""",
         "Wrong error message");
   end if;

   if Arg2.Parser.Parse ((+"-j", +"1")) then
      A.Assert (False, "An error should have been raised");
   else
      A.Assert
        (Arg2.Parser.Last_Error,
         "Missing value for file",
         "Wrong error message");
   end if;
   return A.Report;
end Test;
