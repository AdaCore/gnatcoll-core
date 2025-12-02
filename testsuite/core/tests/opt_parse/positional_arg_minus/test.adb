------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2025, AdaCore                     --
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
      Parser : Argument_Parser := Create_Argument_Parser (Help => "Test");

      package File is new
        Parse_Positional_Arg
          (Parser   => Parser,
           Name     => "Positional_Argument",
           Arg_Type => XString,
           Help     => "Any positional argument");
   end Arg;
begin

   if Arg.Parser.Parse ((1 => +"-")) then
      A.Assert (Arg.File.Get = "-");
   else
      A.Assert (False, "Parsing failed, should have succeeded");
   end if;

   return A.Report;
end Test;
