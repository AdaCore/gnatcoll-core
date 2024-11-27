with GNATCOLL.Strings;
with Text_IO;

package body Mylib is

   procedure Run is
      Dummy : constant Boolean := Args.Parser.Parse;
   begin
      null;
      for F1 of Args.Files_Option1.Get loop
         Text_IO.Put (To_String (F1) & " ");
      end loop;
      Text_IO.Put_Line ("");
      for F2 of Args.Files_Option2.Get loop
         Text_IO.Put (To_String (F2) & " ");
      end loop;
      Text_IO.Put_Line ("");
      for F3 of Args.Files_Option3.Get loop
         Text_IO.Put (To_String (F3) & " ");
      end loop;
      Text_IO.Put_Line ("");
   end Run;

end Mylib;
