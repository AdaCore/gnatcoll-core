with Ada.Containers.Vectors;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

package Mylib is

   package Int_Vectors is new Ada.Containers.Vectors (Positive, Integer);

   type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

   type Context is record
      Ints : Int_Vectors.Vector;
   end record;
   type Context_Array is array (Positive range <>) of Context;

   package Args is
      Parser : Argument_Parser :=
         Create_Argument_Parser (Help => "Test program");

      package Day_Option1 is new Parse_Enum_Option
        (Parser      => Parser,
         Short       => "-D1",
         Long        => "--day1",
         Arg_Type    => Day,
         Help        =>
            "What day of the week is it? "
            & "Default is ""Mon""",
         Default_Val => Mon);

      package Day_Option2 is new Parse_Enum_Option
        (Parser      => Parser,
         Short       => "-D2",
         Name        => "Day",
         Arg_Type    => Day,
         Help        =>
            "What day of the week is it? "
            & "Default is ""Mon""",
         Default_Val => Mon);

      package Day_Option3 is new Parse_Enum_Option
        (Parser      => Parser,
         Long        => "--day3",
         Arg_Type    => Day,
         Help        =>
            "What day of the week is it? "
            & "Default is ""Mon""",
         Default_Val => Mon);

   end Args;

   procedure Run;

end Mylib;
