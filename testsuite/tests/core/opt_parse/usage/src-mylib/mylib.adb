with GNATCOLL.Strings;

package body Mylib is

   procedure Run is
      Dummy : constant Boolean := Args.Parser.Parse
        ((1 => GNATCOLL.Strings.To_XString ("--help")));
   begin
      null;
   end;

end Mylib;
