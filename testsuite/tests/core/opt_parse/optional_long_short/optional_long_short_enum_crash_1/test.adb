with System.Assertions; use System.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;
begin
   declare
      pragma Warnings (Off);
      --  We ignore warnings, because GNAT is actually smart enough to catch
      --  the assert failure at compile time and emit a warning for it

      package Args is
         type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

         Parser : Argument_Parser :=
            Create_Argument_Parser (Help => "Test program");

         package Day_Option1 is new Parse_Enum_Option
           (Parser      => Parser,
            Name        => "Day",
            Arg_Type    => Day,
            Help        =>
               "What day of the week is it? "
               & "Default is ""Mon""",
            Default_Val => Mon);

      end Args;

   begin
      if Args.Parser.Parse ((1 .. 0 => <>)) then
         A.Assert (False, "Should not be reached");
      end if;
   end;

   return A.Report;
exception
   when E : Assert_Failure =>
      A.Assert
        (Exception_Message (E)
         = "You should have either a long or a short flag",
         "Wrong exception msg");
      return A.Report;
end Test;
