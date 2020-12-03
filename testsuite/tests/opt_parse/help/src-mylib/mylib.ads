with Ada.Containers.Vectors;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

package Mylib is

   package Int_Vectors is new Ada.Containers.Vectors (Positive, Integer);

   type Context is record
      Ints : Int_Vectors.Vector;
   end record;
   type Context_Array is array (Positive range <>) of Context;

   package Args is
      Parser : Argument_Parser :=
         Create_Argument_Parser (Help => "Test program");
   end Args;

   procedure Run;

end Mylib;
