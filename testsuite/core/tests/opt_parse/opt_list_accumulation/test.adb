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

   package Arg is
      Parser : Argument_Parser :=
        Create_Argument_Parser (Help => "Dummy parser");

      package No_Accumulate is new
        Parse_Option_List
          (Parser     => Parser,
           Long       => "--no-acc",
           Arg_Type   => XString,
           Accumulate => False,
           Help       => "Dummy option");

      package Single_Accumulate is new
        Parse_Option_List
          (Parser             => Parser,
           Long               => "--single-acc",
           Short              => "-s",
           Arg_Type           => XString,
           Accumulate         => True,
           Arg_Number         => Single_Arg,
           Help               => "Dummy option");

      package List_Accumulate is new
        Parse_Option_List
          (Parser             => Parser,
           Long               => "--list-acc",
           Arg_Type           => XString,
           Accumulate         => True,
           Arg_Number         => Multiple_Args,
           Help               => "Dummy option");
   end Arg;

   use Arg.No_Accumulate;
   use Arg.Single_Accumulate;
   use Arg.List_Accumulate;

   procedure Parse_And_Check_Failure (Args : XString_Array);

   procedure Parse_And_Check_Failure (Args : XString_Array) is
   begin
      if Arg.Parser.Parse (Args) then
         A.Assert (False, "Parsing should fail");
      end if;
   end Parse_And_Check_Failure;
begin
   --  Try parsing a valid command-line
   if Arg.Parser.Parse
        ((+"--no-acc",
          +"A",
          +"B",
          +"C",
          +"--single-acc",
          +"D",
          +"-sE",
          +"-s=F",
          +"--list-acc",
          +"G",
          +"H",
          +"--list-acc",
          +"I",
          +"J"))
   then
      A.Assert
        (Arg.No_Accumulate.Get = (+"A", +"B", +"C"),
         "Check 'No_Accumulate' value");
      A.Assert
        (Arg.Single_Accumulate.Get = (+"D", +"E", +"F"),
         "Check 'Single_Accumulate' value");
      A.Assert
        (Arg.List_Accumulate.Get = (+"G", +"H", +"I", +"J"),
         "Check 'List_Accumulate' value");
   else
      A.Assert (False, "Parsing error");
   end if;

   --  Now try invalid cases
   Parse_And_Check_Failure ((+"--no-acc", +"A", +"--no-acc", +"B"));
   Parse_And_Check_Failure ((+"--single-acc", +"A", +"B"));
   Parse_And_Check_Failure
     ((+"--single-acc", +"A", +"--single-acc", +"B", +"C"));
   Parse_And_Check_Failure ((+"--list-acc", +"A", +"--list-acc"));

   return A.Report;
end Test;
