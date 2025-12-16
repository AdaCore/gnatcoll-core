with Ada.Containers.Vectors;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Strings;   use GNATCOLL.Strings;

package Mylib is

   package Int_Vectors is new Ada.Containers.Vectors (Positive, Integer);

   type Context is record
      Ints : Int_Vectors.Vector;
   end record;
   type Context_Array is array (Positive range <>) of Context;

   function "+"
     (Self : String) return XString renames To_XString;

   package Args is
      Parser : Argument_Parser :=
         Create_Argument_Parser (Help => "Test program");

      package Flag is new Parse_Flag
        (Parser   => Parser,
         Short    => "-f",
         Long     => "--flag",
         Help     => "FAIL if shown",
         Hidden   => True);

      package Option is new Parse_Option
        (Parser      => Parser,
         Short       => "-o",
         Long        => "--option",
         Arg_Type    => XString,
         Help        => "FAIL if shown",
         Default_Val => +"default",
         Hidden      => True);

      package Option_List is new Parse_Option_List
        (Parser      => Parser,
         Short       => "-O",
         Long        => "--option-list",
         Arg_Type    => XString,
         Help        => "FAIL if shown",
         Accumulate  => True,
         Hidden      => True);
   end Args;

   procedure Run;

end Mylib;
