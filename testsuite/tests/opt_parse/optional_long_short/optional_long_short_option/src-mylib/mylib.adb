with GNATCOLL.Strings;
with Text_IO;

package body Mylib is

   procedure Run is
      Dummy : constant Boolean := Args.Parser.Parse;
   begin
      Text_IO.PUT_Line (To_String (Args.Charset_Option1.Get));
      Text_IO.PUT_Line (To_String (Args.Charset_Option2.Get));
      Text_IO.PUT_Line (To_String (Args.Charset_Option3.Get));
   end Run;

end Mylib;
