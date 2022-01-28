with GNATCOLL.Strings;

package body Mylib is

   procedure Run is
      Dummy : constant Boolean := Args.Parser.Parse;
   begin
      null;
   end Run;

end Mylib;
