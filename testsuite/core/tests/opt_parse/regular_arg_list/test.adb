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
        (Help => "Test");

      package Numbers is new Parse_Option_List
        (Parser   => Parser,
         Short     => "-n",
         Long     => "--numbers",
         Arg_Type => Integer,
         Help     => "The numbers");

      package Quiet is new Parse_Flag
        (Parser => Parser,
         Short  => "-q",
         Long   => "--quiet",
         Help   => "Whether the tool should be quiet or not");

   end Arg;

   use type Arg.Numbers.Result_Array;

   Nums : constant Arg.Numbers.Result_Array := (219, 221, 224);

begin

   if Arg.Parser.Parse ((+"--numbers", +"219", +"221", +"224", +"-q")) then
      A.Assert (Arg.Numbers.Get = Nums, "Wrong num array");
      A.Assert (Arg.Quiet.Get);
   else
      A.Assert (False, "Parsing failed, should have succeeded");
   end if;

   A.Assert (Arg.Parser.Parse ((+"-q", +"-n", +"1924")));

   if Arg.Parser.Parse ((+"pouet", +"-q")) then
      A.Assert (False, "Parsing should fail");
   else
      A.Assert (Arg.Parser.Last_Error,
                "Unrecognized argument ""pouet""",
                "Wrong error message");
   end if;

   return A.Report;

end Test;
