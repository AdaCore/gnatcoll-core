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

with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Text_IO;              use Ada.Text_IO;

with GNATCOLL.Paragraph_Filling;       use GNATCOLL.Paragraph_Filling;
with GNATCOLL.Paragraph_Filling.Words; use GNATCOLL.Paragraph_Filling.Words;

procedure GNATCOLL.Paragraph_Filling.Test_Words is
   --  Test program for Paragraph_Filling.Words. Prints each word in a file and
   --  then the length of that word.

   Input  : File_Type;
begin
   Open (Input, In_File, Argument (1));
   while not End_Of_File (Input) loop
      declare
         Current_Paragraph : constant Paragraph_Filling.Words.Words :=
            Index_Paragraph (Get_Paragraph (Input));
      begin
         for Word in 1 .. Current_Paragraph.Num_Words - 1 loop
            Put (Word'Img);
            Put ("""" & Nth_Word (Current_Paragraph, Word) & """");
            Put_Line (Word_Length (Current_Paragraph, Word)'Img);
         end loop;
      end;
   end loop;
   Close (Input);
end GNATCOLL.Paragraph_Filling.Test_Words;
