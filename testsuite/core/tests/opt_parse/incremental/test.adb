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

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Strings;   use GNATCOLL.Strings;

with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;

   function "+"
     (Self : String) return XString renames To_XString;

   package Arg is
      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Test", Incremental => True);

      package Numbers is new Parse_Option_List
        (Parser     => Parser,
         Short      => "-n",
         Long       => "--number",
         Accumulate => True,
         Arg_Type   => Integer,
         Help       => "The numbers");

      package Val is new Parse_Option
        (Parser => Parser,
         Short  => "-v",
         Long   => "--value",
         Arg_Type => Integer,
         Default_Val => 0,
         Help   => "Value");

      package Quiet is new Parse_Flag
        (Parser => Parser,
         Short  => "-q",
         Long   => "--quiet",
         Help   => "Whether the tool should be quiet or not");

   end Arg;

   use type Arg.Numbers.Result_Array;

   pragma Extensions_Allowed (On);
begin

   if Arg.Parser.Parse ((+"-v", +"12", +"--number", +"219", +"-q")) and then
     Arg.Parser.Parse ((+"-v", +"15", +"-q"))
   then
      A.Assert (Arg.Numbers.Get = (1 => 219), "Wrong num array");
      A.Assert (Arg.Val.Get = 15, "Wrong value");
      A.Assert (Arg.Quiet.Get, "Wrong value");
   else
      A.Assert (False, "Argument parsing failed");
   end if;

   if Arg.Parser.Parse
     ((+"-n", +"25", +"-n", +"28"))
   then
      A.Assert (Arg.Numbers.Get = (219, 25, 28),
                "Wrong num array: " & Arg.Numbers.Get'Image);
      A.Assert (Arg.Val.Get = 15, "Wrong value");
   else
      A.Assert (False, "Argument parsing failed");
   end if;

   --  After this call, the parser's default results should be reset
   Arg.Parser.Reset;
   if Arg.Parser.Parse
     ((+"-n", +"25", +"-n", +"28"))
   then
      A.Assert (Arg.Numbers.Get = (25, 28),
                "Wrong num array: " & Arg.Numbers.Get'Image);
      A.Assert (Arg.Val.Get = 0, "Wrong value");
      A.Assert (not Arg.Quiet.Get, "Wrong value");
   else
      A.Assert (False, "Argument parsing failed");
   end if;

   --  Try parsing the same option multiple times in the same arg array
   if Arg.Parser.Parse ((+"-v", +"10", +"-v", +"12")) then
      A.Assert (False, "Argument parsing should have failed");
   else
      A.Assert (True, "Parsing failed as expected");
   end if;

   return A.Report;

end Test;
