with Text_IO;

package body Mylib is

   procedure Run is
      Dummy : constant Boolean := Args.Parser.Parse;
   begin
      Text_IO.PUT_Line ("Flag 1 " & Args.Flag_Option1.Get'Image);
      Text_IO.PUT_Line ("Flag 2 " & Args.Flag_Option2.Get'Image);
      Text_IO.PUT_Line ("Flag 3 " & Args.Flag_Option3.Get'Image);
   end Run;

end Mylib;
