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

      package Files_Option1 is new Parse_Option_List
        (Parser      => Parser,
         Short       => "-F1",
         Long        => "--files1",
         Arg_Type    => Unbounded_String,
         Help        => "List of files to be parsed.");

      package Files_Option2 is new Parse_Option_List
        (Parser      => Parser,
         Short       => "-F2",
         Name        => "Files",
         Arg_Type    => Unbounded_String,
         Help        => "List of files to be parsed.");

      package Files_Option3 is new Parse_Option_List
        (Parser      => Parser,
         Long        => "--files3",
         Arg_Type    => Unbounded_String,
         Help        => "List of files to be parsed.");

   end Args;

   procedure Run;

end Mylib;
