with GNATCOLL.Utils;

with Test_Assert;

with Ada.Text_IO;

function Test return Integer is

   package A renames Test_Assert;
   package IO renames Ada.Text_IO;

   procedure Test_Image;
   procedure Test_Find_Char;
   procedure Test_Line_Start;
   procedure Test_Next_Line;
   procedure Test_Line_End;
   procedure Test_Previous_Line;

   procedure Test_Image is
      S1   : constant String := "005";
      S2   : constant String := '+' & S1;
      S3   : constant String := "-00000" & S1;
      Int1 : constant Integer := 5;
      Int2 : constant Integer := -5;

      S4   : constant String := "-    455";
      Int3 : constant Integer := -455;

   begin
      IO.Put_Line ("Test Image");
      A.Assert (GNATCOLL.Utils.Image (Int1,
                                      Int1'Image'Length,
                                      Padding => ' ') = Int1'Image,
                Msg => "positive, mimic attribute Image");
      A.Assert (GNATCOLL.Utils.Image (Int1, S1'Length) = S1,
                Msg => "positive, several 0 (default padding)");
      A.Assert (GNATCOLL.Utils.Image (Int1,
                                      S1'Length,
                                      Force_Sign => True) = S2,
                Msg => "positive, several 0, force sign");
      A.Assert (GNATCOLL.Utils.Image (Int2,
                                      S3'Length - 1) = S3,
                Msg => "negative, several 0");
      A.Assert (GNATCOLL.Utils.Image (Int3,
                                      S4'Length - 1,
                                      Padding => ' ',
                                      Force_Sign => True) = S4,
                Msg => "negative, several ' ', force sign");

   end Test_Image;

   procedure Test_Find_Char is
      Str  : String (1 .. Character'Pos (Character'Last) + 1);
      Str2 : constant String := "AAAAAAAAAAAAAAAAAAA";
      Str3 : constant String := Str2 & ASCII.LF & "BBBBB";
   begin
      IO.Put_Line ("Test Find_Char");
      for Char in Character loop
         Str (Character'Pos (Char) + 1) := Char;
      end loop;

      for Char in Character loop
         A.Assert (GNATCOLL.Utils.Find_Char (Str, Char) =
                   Character'Pos (Char) + 1,
                   Msg => "find all char in in all char string");
      end loop;

      A.Assert (GNATCOLL.Utils.Find_Char (Str2, 'B') = Str2'Last + 1,
                Msg => "not found");

      A.Assert (GNATCOLL.Utils.Find_Char (Str2, 'A') = Str2'First,
                Msg => "several occurence => take first");

      A.Assert (GNATCOLL.Utils.Find_Char ("", 'A') = 1,
                Msg => "in empty string = found at begining");

      A.Assert (GNATCOLL.Utils.Find_Char (Str2, ASCII.NUL)
                = Str2'Last + 1,
                Msg => "null char in string = not found");

      IO.Put_Line ("Test EOL");

      A.Assert (GNATCOLL.Utils.EOL (Str2) = Str2'Last + 1,
                Msg => "EOL not found");
      A.Assert (GNATCOLL.Utils.EOL (Str3) = Str2'Length + 1,
                Msg => "first occurence of EOL");
      A.Assert (GNATCOLL.Utils.EOL ("") = 1,
                Msg => "EOL in empty string");

   end Test_Find_Char;

   procedure Test_Line_Start is
      S1     : constant String := "This is line 1";
      S2     : constant String := "This is the second line";
      CRLF   : constant String := ASCII.CR & ASCII.LF;
      LFCR   : constant String := ASCII.LF & ASCII.CR;
      S_1LF  : constant String := S1 & ASCII.LF & S2;
      S_1CR  : constant String := S1 & ASCII.CR & S2;
      S_6LF  : constant String := S1 & ASCII.LF & ASCII.LF
          & ASCII.LF & ASCII.LF & ASCII.LF & ASCII.LF & S2;
      S_6CR  : constant String := S1 & ASCII.CR & ASCII.CR
          & ASCII.CR & ASCII.CR & ASCII.CR & ASCII.CR & S2;
      S_6CRLF : constant String := S1 & CRLF & CRLF & CRLF & CRLF & CRLF
          & CRLF & S2;
      S_6LFCR : constant String := S1 & LFCR & LFCR & LFCR & LFCR & LFCR
          & LFCR & S2;

   begin
      IO.Put_Line ("Test Line_Start");

      A.Assert (GNATCOLL.Utils.Line_Start (S1, 0) = 0,
                Msg => "P equals 0 (under S1 lower bound)");
      A.Assert (GNATCOLL.Utils.Line_Start (S1, S1'Last + 100) = 1,
                Msg => "P over S1 upper bound");

      A.Assert (GNATCOLL.Utils.Line_Start (S1, 1) = 1,
                Msg => "P on first char");
      A.Assert (GNATCOLL.Utils.Line_Start (S1, S1'Last) = 1,
                Msg => "one line, P on last char");
      A.Assert (GNATCOLL.Utils.Line_Start (S1, S1'Length - 2) = 1,
                Msg => "one line, P in string");

      A.Assert (GNATCOLL.Utils.Line_Start (S_1LF, S1'Length) = 1,
                Msg => "P is before an LF char (no CR)");
      A.Assert (GNATCOLL.Utils.Line_Start (S_1LF, S1'Length + 2) =
          S1'Length + 2,
                Msg => "P is after an LF char (no CR)");
      A.Assert (GNATCOLL.Utils.Line_Start (S_1LF, S1'Length + 1) = 1,
                Msg => "P is on a LF char (no CR)");
      A.Assert (GNATCOLL.Utils.Line_Start (S_6LF, S1'Length + 6) =
          S1'Length + 6,
                Msg => "P is on 6th LF char (no CR)");
      A.Assert (GNATCOLL.Utils.Line_Start (S_6LF, S1'Length + 7) =
          S1'Length + 7,
                Msg => "P is after last LF char (no CR)");

      A.Assert (GNATCOLL.Utils.Line_Start (S_1CR, S1'Length) = 1,
                Msg => "P is before a CR char (no LF)");
      A.Assert (GNATCOLL.Utils.Line_Start (S_1CR, S1'Length + 2) =
          S1'Length + 2,
                Msg => "P is after a CR char");
      A.Assert (GNATCOLL.Utils.Line_Start (S_1CR, S1'Length + 1) = 1,
                Msg => "P is on a CR char");
      A.Assert (GNATCOLL.Utils.Line_Start (S_6CR, S1'Length + 6) =
          S1'Length + 6,
                Msg => "P is on 6th CR char (no CR)");
      A.Assert (GNATCOLL.Utils.Line_Start (S_6CR, S1'Length + 7) =
          S1'Length + 7,
                Msg => "P is after last CR char (no CR)");

      A.Assert (GNATCOLL.Utils.Line_Start (S_6CRLF, S1'Length + 2) = 1,
                Msg => "P on the LF of a CRLF");
      A.Assert (GNATCOLL.Utils.Line_Start (S_6LFCR, S1'Length + 2) =
          S1'Length + 2,
                Msg => "P on the CR of a LFCR");

      A.Assert (GNATCOLL.Utils.Line_Start (S_6CRLF, S1'Length + 5) =
          S1'Length + 5,
                Msg => "P on the CR of the 3rd CRLF");
      A.Assert (GNATCOLL.Utils.Line_Start (S_6CRLF, S1'Length + 6) =
          S1'Length + 5,
                Msg => "P on the LF of the 3rd CRLF");

      A.Assert (GNATCOLL.Utils.Line_Start (S_6LFCR, S1'Length + 5) =
          S1'Length + 4,
                Msg => "P on the LF of the 3rd LFCR");
      A.Assert (GNATCOLL.Utils.Line_Start (S_6LFCR, S1'Length + 6) =
          S1'Length + 6,
                Msg => "P on the CR of the 3rd LFCR");

      A.Assert (GNATCOLL.Utils.Line_Start (S_6CRLF, S1'Length + 13) =
          S1'Length + 13,
                Msg => "P after last CRLF");

      A.Assert (GNATCOLL.Utils.Line_Start (CRLF, 2) = CRLF'First,
                Msg => "P on the LF of a string only made of 1 CRLF");
      A.Assert (GNATCOLL.Utils.Line_Start (LFCR, 2) = CRLF'First + 1,
                Msg => "P on the CR of a string only made of 1 LFCR");

   end Test_Line_Start;

   procedure Test_Next_Line is
      S1      : constant String := "This is line 1";
      S2      : constant String := "This is the second line";
      S3      : constant String := "This is the third line";
      CRLF    : constant String := ASCII.CR & ASCII.LF;
      LFCR    : constant String := ASCII.LF & ASCII.CR;
      S_CRLF  : constant String := S1 & CRLF & S2 & CRLF & S3;
      S_LFCR  : constant String := S1 & LFCR & S2 & LFCR & S3;
   begin
      IO.Put_Line ("Test Next_Line");

      A.Assert (GNATCOLL.Utils.Next_Line (S1, 0) = S1'Length,
           Msg => "P equals 0 (under S1 lower bound)");
      A.Assert (GNATCOLL.Utils.Next_Line (S1, S1'Last + 100) = S1'Length,
           Msg => "P over S1 upper bound");

      A.Assert (GNATCOLL.Utils.Next_Line (S1, S1'Length - 2) = S1'Length,
           Msg => "one line, P in string");

      A.Assert (GNATCOLL.Utils.Next_Line (S_CRLF, S1'Length + 1) =
      S1'Length + 3,
           Msg => "P on a CR of a CRLF (string continue after that)");
      A.Assert (GNATCOLL.Utils.Next_Line (S_CRLF, S1'Length + 2) =
      S1'Length + 3,
           Msg => "P on an LF of a CRLF (string continue after that)");

      A.Assert (GNATCOLL.Utils.Next_Line (S_LFCR, S1'Length + 1) =
      S1'Length + 2,
           Msg => "P on a LF of a LFCR (string continues after that)");
      A.Assert (GNATCOLL.Utils.Next_Line (S_LFCR, S1'Length + 2) =
      S1'Length + 2 + S2'Length + 2,
           Msg => "P on a CR of a LFCR (string continues after that)");

   end Test_Next_Line;

   procedure Test_Previous_Line is
      S1      : constant String := "This is line 1";
      S2      : constant String := "This is the second line";
      S3      : constant String := "This is the third line";
      CRLF    : constant String := ASCII.CR & ASCII.LF;
      LFCR    : constant String := ASCII.LF & ASCII.CR;
      S_CRLF  : constant String := S1 & CRLF & S2 & CRLF & S3;
      S_LFCR  : constant String := S1 & LFCR & S2 & LFCR & S3;
   begin
      IO.Put_Line ("Test Previous_Line");

      A.Assert (GNATCOLL.Utils.Previous_Line (S1, 0) = 1,
           Msg => "P equals 0 (under S1 lower bound)");
      A.Assert (GNATCOLL.Utils.Previous_Line (S1, S1'Last + 100) = S1'First,
           Msg => "P over S1 upper bound");

      A.Assert (GNATCOLL.Utils.Previous_Line (S1, S1'Length - 2) = S1'First,
           Msg => "one line, P in string");

      A.Assert (GNATCOLL.Utils.Previous_Line (S_CRLF, S1'Length + 5) =
      S1'First,
           Msg => "In a string, after a CRLF");
      A.Assert (GNATCOLL.Utils.Previous_Line (S_LFCR, S1'Length + 5) =
      S1'Length + 2,
           Msg => "In a string, after a LFCR");

      A.Assert (GNATCOLL.Utils.Previous_Line (S_LFCR, S1'Length + 1) =
      S1'First,
           Msg => "P on a LF of a string with LFCR");
      A.Assert (GNATCOLL.Utils.Previous_Line (S_LFCR, S1'Length + 2) =
      S1'First,
           Msg => "P on a CR of a string with LFCR");

      A.Assert (GNATCOLL.Utils.Previous_Line (S_CRLF, S1'Length + 1) =
      S1'First,
           Msg => "P on a CR of a string with CRLF");
      A.Assert (GNATCOLL.Utils.Previous_Line (S_CRLF, S1'Length + 2) =
      S1'First,
           Msg => "P on an LF of a string with CRLF");

   end Test_Previous_Line;

   procedure Test_Line_End is
      S1     : constant String := "This is line 1";
      S2     : constant String := "This is the second line";
      CRLF   : constant String := ASCII.CR & ASCII.LF;
      S_6LF  : constant String := S1 & ASCII.LF & ASCII.LF
      & ASCII.LF & ASCII.LF & ASCII.LF & ASCII.LF & S2;
      S_6CR  : constant String := S1 & ASCII.CR & ASCII.CR
      & ASCII.CR & ASCII.CR & ASCII.CR & ASCII.CR & S2;

   begin
      IO.Put_Line ("Test Line_End");

      A.Assert (GNATCOLL.Utils.Line_End (S1, 0) = S1'Length,
                Msg => "P equals 0 (under S1 lower bound)");
      A.Assert (GNATCOLL.Utils.Line_End (S1, S1'Last + 100) = S1'Length,
                Msg => "P over S1 upper bound");

      A.Assert (GNATCOLL.Utils.Line_End (S_6LF, S1'First + 1) = S1'Length,
                Msg => "P is before an LF char (no CR)");
      A.Assert (GNATCOLL.Utils.Line_End (S_6CR, S1'First + 1) = S1'Length,
                Msg => "P is before an CR char (no LF)");

      --  ??? Line_End of "CRLF" on 1st caracter is 0
      A.Assert (GNATCOLL.Utils.Line_End (CRLF, 1) = CRLF'First - 1,
                Msg => "P is the CR of a CRLF string");
      A.Assert (GNATCOLL.Utils.Line_End (CRLF, 2) = 2 - 1,
                Msg => "P is the LF of a CRLF string");

   end Test_Line_End;

begin
   IO.New_Line;
   Test_Image;
   IO.New_Line;
   Test_Find_Char;
   IO.New_Line;
   Test_Line_Start;
   IO.New_Line;
   Test_Line_End;
   IO.New_Line;
   Test_Next_Line;
   IO.New_Line;
   Test_Previous_Line;

   return A.Report;

end Test;
