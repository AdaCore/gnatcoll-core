with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

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
         Arg_Type    => Day,
         Help        =>
            "What day of the week is it? "
            & "Default is ""Mon""",
         Default_Val => Mon);

   end Args;

   procedure Run;

end Mylib;
