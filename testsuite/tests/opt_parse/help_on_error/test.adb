--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
--
--  The unit provides functions to generate random data using the OS CSPRNG
--  This means that this functions are suitable for cryptographic contexts
--  The downside is that that they around one order of magnitud slower than
--  implementation provided in the default Ada runtime.

with GNATCOLL.Strings; use GNATCOLL.Strings;
with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;

   function "+" (Self : String) return XString renames To_XString;

   package Arg2 is
      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Test", Print_Help_On_Error => False);
   end Arg2;
begin
   A.Assert (not Arg2.Parser.Parse ((1 => +"-Q")));
   return A.Report;
end Test;
