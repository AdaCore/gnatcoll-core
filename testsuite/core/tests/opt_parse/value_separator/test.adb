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

with GNATCOLL.Opt_Parse;    use GNATCOLL.Opt_Parse;
with GNATCOLL.Strings;      use GNATCOLL.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;

   function "+" (Self : String) return XString renames To_XString;

   package Arg is
      Parser : Argument_Parser :=
        Create_Argument_Parser (Help => "Dummy parser");

      package Option is new
        Parse_Option
          (Parser      => Parser,
           Long        => "--option",
           Short       => "-o",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        => "Dummy option");
   end Arg;

   procedure Parse_And_Check (Args : XString_Array);
   --  Parse provided args and check that parsing give valid result.

   procedure Parse_And_Check (Args : XString_Array) is
   begin
      if Arg.Parser.Parse (Args) then
         A.Assert
           (Arg.Option.Get = "valid_value",
            "Expected ""valid_value"", got """
            & To_String (Arg.Option.Get)
            & '"');
      else
         A.Assert (False, "Parsing failed");
      end if;
   end Parse_And_Check;

begin
   --  Test with the long form, with a whitespace separator
   Parse_And_Check ((+"--option", +"valid_value"));

   --  Test with the long form, with a "=" separator
   Parse_And_Check ((1 => +"--option=valid_value"));

   --  Test with the short form, with a whitespace separator
   Parse_And_Check ((+"-o", +"valid_value"));

   --  Test with the short form, with a "=" separator
   Parse_And_Check ((1 => +"-o=valid_value"));

   --  Test with the short form, with no separator
   Parse_And_Check ((1 => +"-ovalid_value"));

   return A.Report;
end Test;
