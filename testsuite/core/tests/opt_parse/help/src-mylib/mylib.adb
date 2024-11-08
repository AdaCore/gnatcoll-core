with GNATCOLL.Strings;

package body Mylib is

   procedure Run is
      type Context_Array_Access is access Context_Array;
      pragma Unreferenced (Context_Array_Access);

      --  Parse calls GNAT.OS_Lib.OS_Exit, which triggers finalization. This
      --  used to raise a Program_Error because that call used to happen in the
      --  middle of the iteration of a vector that was finalized.

      Dummy : constant Boolean := Args.Parser.Parse
        ((1 => GNATCOLL.Strings.To_XString ("--help")));
   begin
      null;
   end;

end Mylib;
