-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                    Copyright (C) 2011, AdaCore                    --
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

with Ada.Strings.Unbounded;

package GNATCOLL.Paragraph_Filling is

   --  This purpose of this package is to format paragraphs to take up the
   --  minimal number of lines and to look better.

   --  Note: All subprograms in this package that take or return a String
   --  representing a paragraph represent multiple lines by using ASCII.LF
   --  as the line terminator.
   --  They return an unbounded_string to avoid extra copies (since internally
   --  they manipulate an unbounded_string).

   Default_Max_Line_Length : Positive := 79;
   --  This value is used as a default for the Max_Line_Length parameter of
   --  various subprograms. Note that 79 is the standard max line length used
   --  at AdaCore.

   function Greedy_Fill
     (Paragraph       : String;
      Max_Line_Length : Positive := Default_Max_Line_Length)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Formats a paragraph with the greedy algorithm (by putting as many words
   --  as possible on each line).

   function Pretty_Fill
     (Paragraph       : String;
      Max_Line_Length : Positive := Default_Max_Line_Length)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Formats a paragraph by first performing Greedy_Fill and then comparing
   --  adjacent lines and deciding whether a word should be moved to the next
   --  line to make the lines more even.  For example:
   --
   --  Reads Ada source code from the file named by Input_Name. Calls Format on
   --  each block comment, and sends the output to the file named by
   --  Output_Name. Text that is not part of a comment, and comments appearing
   --  after other non-whitespace text on the same line, is sent to the output
   --  unchanged.
   --
   --  would be changed to:
   --
   --  Reads Ada source code from the file named by Input_Name. Calls Format
   --  on each block comment, and sends the output to the file named by
   --  Output_Name. Text that is not part of a comment, and comments appearing
   --  after other non-whitespace text on the same line, is sent to the output
   --  unchanged.
   --
   --  if the max line length is set to 72.

   function Knuth_Fill
     (Paragraph       : String;
      Max_Line_Length : Positive := Default_Max_Line_Length)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Fill the paragraph in the best possible way, based on an algorithm
   --  invented by Knuth. This algorithm uses dynamic programming techniques in
   --  order to fill paragraphs so that they have the lowest possible badness
   --  and line count. Badness is calculated by the Line_Badness function in
   --  Paragraph_Filling.Badnesses. For details see the paper, "Breaking
   --  Paragraphs into Lines", by Donald E. Knuth and Michael F. Plass,
   --  Software Practice and Experience, 11 (1981).

   function Slow_Fill
    (Paragraph        : String;
     Max_Line_Length : Positive)
     return Ada.Strings.Unbounded.Unbounded_String;
   --  Fill the paragraph in the best possible way, using an extremely slow
   --  algorithm that tries all the possibilities. Used for testing the Knuth
   --  algorithm. This should produce the same result as the the Knuth
   --  algorithm.

   function No_Fill
    (Paragraph       : String;
     Max_Line_Length : Positive := Default_Max_Line_Length)
     return Ada.Strings.Unbounded.Unbounded_String;
   --  Return Paragraph unchanged

private

   function Is_Whitespace (Char : Character) return Boolean;
   --  Returns True if Char is a space, new line, or tab; otherwise returns
   --  False.

end GNATCOLL.Paragraph_Filling;
