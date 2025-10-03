------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2025, AdaCore                     --
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

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Strings;   use GNATCOLL.Strings;

with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;

   function "+" (Self : String) return XString renames To_XString;

   package One_Flag_Args is
      Parser : Argument_Parser :=
        Create_Argument_Parser
          (Help => "Dummy parser", Generate_Help_Flag => False);

      package Flag is new
        Parse_Flag
          (Parser => Parser,
           Long   => "--flag",
           Help   => "testing flag");

   end One_Flag_Args;

   package One_Option_Args is
      Parser : Argument_Parser :=
        Create_Argument_Parser
          (Help => "Dummy parser", Generate_Help_Flag => False);

      package Option is new
        Parse_Option
          (Parser      => Parser,
           Long        => "--option",
           Arg_Type    => XString,
           Default_Val => +"default",
           Help        => "testing option");

   end One_Option_Args;

   Empty_Args : XString_Array (1 .. 0);

begin
   --  Test a parser with only one flag
   --  Parse an empty argument list
   if One_Flag_Args.Parser.Parse (Empty_Args) then
      A.Assert (not One_Flag_Args.Flag.Get, "Flag should be false");
   else
      A.Assert (False, "Parsing failed");
   end if;

   --  Parse a argument list with the --flag flag
   if One_Flag_Args.Parser.Parse ((1 => +"--flag")) then
      A.Assert (One_Flag_Args.Flag.Get, "Flag should be true");
   else
      A.Assert (False, "Parsing failed");
   end if;

   --  Parse a argument list with an empty argument
   if One_Flag_Args.Parser.Parse ((1 => +"")) then
      A.Assert (False, "Parsing shouldn't succeed");
   else
      A.Assert
        (not One_Flag_Args.Flag.Get,
         "Flag should be false because of parsing failure");
   end if;

   --  Test a parser with only one option
   --  Parse an empty argument list
   if One_Option_Args.Parser.Parse (Empty_Args) then
      A.Assert
        (One_Option_Args.Option.Get = +"default",
         "Option should be 'default'");
   else
      A.Assert (False, "Parsing failed");
   end if;

   --  Parse a valid option
   if One_Option_Args.Parser.Parse ((+"--option", +"test")) then
      A.Assert
        (One_Option_Args.Option.Get = +"test", "Option should be 'test'");
   else
      A.Assert (False, "Parsing failed");
   end if;

   --  Parse an empty argument
   if One_Option_Args.Parser.Parse ((+"", +"test")) then
      A.Assert (False, "Parsing shouldn't succeed");
   else
      A.Assert
        (One_Option_Args.Option.Get = +"default",
         "Option should be 'default' because of parsing failure");
   end if;

   return A.Report;
end Test;
