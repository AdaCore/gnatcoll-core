with GNATCOLL.Strings;
with Text_IO;

package body Mylib is

   procedure Run is
      Dummy : constant Boolean := Args.Parser.Parse;
   begin
      Text_IO.PUT_Line (Args.Day_Option1.Get'Image);
   end Run;

end Mylib;
