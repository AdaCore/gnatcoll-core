-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  This software was originally contributed by William A. Duff

with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Para_Fill;       use GNATCOLL.Para_Fill;
with GNATCOLL.Para_Fill.Tests; use GNATCOLL.Para_Fill.Tests;

procedure Fill_Text is
   --  Test program that runs one of the formating algorithms in
   --  GNATCOLL.Para_Fill on a text file.

   Input  : File_Type;
   Output : File_Type;
begin
   Process_Command_Line (Command_Name => "fill_text");

   Open (Input, In_File, Input_Name.all);
   Create (Output, Out_File, Output_Name.all, Form => "Text_Translation=No");
   while not End_Of_File (Input) loop
      declare
         Current_Paragraph : constant String := Get_Paragraph (Input);
      begin
         --  ??? Where do empty paragraphs come from, and should the Fill
         --  subprograms work on empty paragraphs?
         if Current_Paragraph = "" then
            New_Line (Output);
         else
            Put_Line (Output, Format (Current_Paragraph, Max_Line_Length));
         end if;
      end;
   end loop;
   Close (Input);
   Close (Output);
end Fill_Text;
