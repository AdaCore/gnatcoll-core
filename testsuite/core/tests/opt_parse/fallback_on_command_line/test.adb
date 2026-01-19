------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2026, AdaCore                     --
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

      package Test_Option is new
        Parse_Option
          (Parser      => Parser,
           Long        => "--test",
           Arg_Type    => XString,
           Help        => "Test",
           Default_Val => +"default_value");
   end Arg;
begin
   --  Try parsing with fallback on the application's command-line
   if Arg.Parser.Parse (No_Arguments, Fallback_On_Command_Line => True) then
      A.Assert
        (Arg.Test_Option.Get = +"value_from_cli",
         "Test options hasn't a valid value");
   else
      A.Assert (False, "Parsing should success");
   end if;

   --  Then try parsing without this fallback
   if Arg.Parser.Parse (No_Arguments, Fallback_On_Command_Line => False) then
      A.Assert
        (Arg.Test_Option.Get = +"default_value",
         "Test options hasn't a valid value");
   else
      A.Assert (False, "Parsing should success");
   end if;

   return A.Report;
end Test;
