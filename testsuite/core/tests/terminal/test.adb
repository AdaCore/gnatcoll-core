------------------------------------------------------------------------------
--                                                                          --
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with GNATCOLL.Terminal;   use GNATCOLL.Terminal;
with Ada.Text_IO;         use Ada.Text_IO;

with Test_Assert;

function Test return Integer is

   Info : Terminal_Info;

   procedure Header (Name : String; Fg : ANSI_Color);
   --  Displays the table header cell using color Fg

   procedure Show (Name : String; Bg : ANSI_Color);
   --  Displays the table body cell using background color Bg

   procedure Put_No_Overflow (Str : String);
   --  Displays the string, but truncates it to the width of the screen

   ------------
   -- Header --
   ------------

   procedure Header (Name : String; Fg : ANSI_Color) is
   begin
      Info.Set_Color (Standard_Output, Fg, Reset, Normal);
      Put (Name);
   end Header;

   ---------------------
   -- Put_No_Overflow --
   ---------------------

   procedure Put_No_Overflow (Str : String) is
      Width : constant Integer := Info.Get_Width;
   begin
      if Width = -1 then
         Put (Str);
      else
         Put (Str (Str'First .. Integer'Min (Str'Last, Width)));
      end if;
   end Put_No_Overflow;

   ----------
   -- Show --
   ----------

   procedure Show (Name : String; Bg : ANSI_Color) is
   begin
      Info.Set_Color (Standard_Output, Reset, Bg, Normal);
      Put (Name);

      for Fg in Black .. Grey loop
         Info.Set_Color (Standard_Output, Fg, Bg, Normal);
         Put ("X ");
         Info.Set_Color (Standard_Output, Style => Dim);
         Put ("X ");
         Info.Set_Color (Standard_Output, Style => Bright);
         Put ("X ");
         Info.Set_Color (Standard_Output, Style => Reset_All);
         Put (" ");
      end loop;

      New_Line;
   end Show;

begin
   Info.Init_For_Stdout (Auto);

   Header ("        ", Reset);
   Header ("black  ", Black);
   Header ("red    ", Red);
   Header ("green  ", Green);
   Header ("yellow ", Yellow);
   Header ("blue   ", Blue);
   Header ("magenta", Magenta);
   Header ("cyan   ", Cyan);
   Header ("white  ", Grey);
   New_Line;

   Show ("black   ", Black);
   Show ("red     ", Red);
   Show ("green   ", Green);
   Show ("yellow  ", Yellow);
   Show ("blue    ", Blue);
   Show ("magenta ", Magenta);
   Show ("cyan    ", Cyan);
   Show ("white   ", Grey);

   for J in 1 .. 50 loop
      if J mod 10 = 0 then
         Put_No_Overflow ("Processing file" & J'Img & " with long name");
      elsif J mod 5 = 0 then
         Put_No_Overflow
            ("Some very long string, in theory should be larger than the "
             & "actual size of the terminal, although some people use large "
             & "windows");
      else
         Put ("Processing file" & J'Img);
      end if;
      delay 0.001;
      Info.Beginning_Of_Line;
      Info.Clear_To_End_Of_Line;
   end loop;

   return Test_Assert.Report;

end Test;
