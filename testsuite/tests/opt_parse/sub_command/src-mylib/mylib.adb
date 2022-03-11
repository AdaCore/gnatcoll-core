with Text_IO; use Text_IO;

package body Mylib is

   procedure Run is
      Dummy : constant Boolean := Args.Parser.Parse;
   begin
      Put_Line ("Sub1_Option1: " & To_String (Args.Sub1_Option1.Get));
      Put_Line ("Sub2_Option2: " & To_String (Args.Sub2_Option1.Get));
      Put_Line ("Sub_Command_1: " & Args.Sub_Command_1.Get'Img);
      Put_Line ("Sub_Command_2: " & Args.Sub_Command_2.Get'Img);
   end Run;

end Mylib;
