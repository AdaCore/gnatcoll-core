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

with Ada.Text_IO; use Ada; use Ada.Text_IO;

package GNATCOLL.Para_Fill.Tests is

   function Get_Paragraph (File : File_Type) return String;
   --  Takes a file and returns the next paragraph from the current position in
   --  the file. Paragraphs are separated by blank lines. Considers extra
   --  blank lines to be paragraphs.

   procedure Format_Ada_File
     (Input_Name, Output_Name : String;
      Format                  : not null access function
        (Paragraph       : String;
         Max_Line_Length : Positive)
         return            String;
      Max_Line_Length : Positive);
   --  Reads Ada source code from the file named by Input_Name. Calls Format on
   --  each block comment, and sends the output to the file named by
   --  Output_Name. Text that is not part of a comment, and comments appearing
   --  after other non-whitespace text on the same line, is sent to the output
   --  unchanged.

   procedure Format_Ada_File
     (Input, Output : Text_IO.File_Type;
      Format        : not null access function
      (Paragraph       : String;
       Max_Line_Length : Positive)
       return            String;
      Max_Line_Length : Positive);
   --  Same as above, except it takes open Input and Output files

   ----------------

   procedure Process_Command_Line (Command_Name : String);

   --  Variables set from command-line arguments:

   type Formatting_Methods is (None, Greedy, Pretty, Knuth, Slow);
   Method : Formatting_Methods := Knuth;

   Format : not null access function
     (Paragraph       : String;
      Max_Line_Length : Positive)
      return            String
     := Knuth_Fill'Access;

   Max_Line_Length : Positive := Default_Max_Line_Length;

   Input_Name : access String;
   Output_Name : access String;

end GNATCOLL.Para_Fill.Tests;
