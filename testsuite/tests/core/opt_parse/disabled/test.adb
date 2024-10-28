------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C)      2019, AdaCore                     --
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
         Create_Argument_Parser (Help => "Test program");

      package Flag is new Parse_Flag
        (Parser  => Parser,
         Short   => "-f",
         Long    => "--flag",
         Help    => "Test flag",
         Enabled => False);

      package Option is new Parse_Option
        (Parser      => Parser,
         Short       => "-o",
         Long        => "--option",
         Arg_Type    => XString,
         Help        => "Test option",
         Default_Val => +"default",
         Enabled     => False);

      package Option_List is new Parse_Option_List
        (Parser      => Parser,
         Short       => "-O",
         Long        => "--option-list",
         Arg_Type    => XString,
         Help        => "Test option list",
         Accumulate  => True,
         Enabled     => False);

      package Positional is new Parse_Positional_Arg
        (Parser      => Parser,
         Name        => "positional",
         Help        => "Test positional argument",
         Arg_Type    => XString,
         Enabled     => False);

      package Empty_Positional_List is new Parse_Positional_Arg_List
        (Parser      => Parser,
         Name        => "empty-positional-list",
         Help        => "Test empty positional list argument",
         Allow_Empty => True,
         Arg_Type    => XString,
         Enabled     => False);

      package Positional_List is new Parse_Positional_Arg_List
        (Parser      => Parser,
         Name        => "positional-list",
         Help        => "Test positional list argument",
         Allow_Empty => False,
         Arg_Type    => XString,
         Enabled     => False);
   end Arg;

   use type Arg.Option_List.Result_Array;
   use type Arg.Empty_Positional_List.Result_Array;

begin
   A.Assert (Arg.Parser.Parse ((1 .. 0 => <>)));
   A.Assert (not Arg.Flag.Get);
   A.Assert (Arg.Option.Get = +"default");
   A.Assert (Arg.Option_List.Get = (1 .. 0 => <>));

   begin
      declare
         Dummy : constant XString := Arg.Positional.Get;
      begin
         A.Assert (False);
      end;
   exception
      when Disabled_Error => null;
   end;

   A.Assert (Arg.Empty_Positional_List.Get = (1 .. 0 => <>));

   begin
      declare
         Dummy : constant Arg.Positional_List.Result_Array :=
            Arg.Positional_List.Get;
      begin
         A.Assert (False);
      end;
   exception
      when Disabled_Error => null;
   end;

   return A.Report;
end Test;
