------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2011-2014, AdaCore                     --
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
      Max_Line_Length : Positive := Default_Max_Line_Length;
      Line_Prefix     : String := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Formats a paragraph with the greedy algorithm (by putting as many words
   --  as possible on each line).
   --  Line_Prefix is added at the beginning of each line.

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
      Max_Line_Length : Positive := Default_Max_Line_Length;
      Line_Prefix     : String := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Fill the paragraph in the best possible way, based on an algorithm
   --  invented by Knuth. This algorithm uses dynamic programming techniques in
   --  order to fill paragraphs so that they have the lowest possible badness
   --  and line count. Badness is calculated by the Line_Badness function in
   --  Paragraph_Filling.Badnesses. For details see the paper, "Breaking
   --  Paragraphs into Lines", by Donald E. Knuth and Michael F. Plass,
   --  Software Practice and Experience, 11 (1981).

   function No_Fill
    (Paragraph       : String;
     Max_Line_Length : Positive := Default_Max_Line_Length;
     Line_Prefix     : String := "")
     return Ada.Strings.Unbounded.Unbounded_String;
   --  Return Paragraph unchanged

end GNATCOLL.Paragraph_Filling;
