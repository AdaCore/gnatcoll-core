------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2014, AdaCore                     --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

private package GNATCOLL.Paragraph_Filling.Words is

   --  Provides ways of differentiating words by reformatting a paragraph and
   --  pointing to the first character of each word in that paragraph.

   type Word_Index is new Positive;

   package Word_Vectors is new Ada.Containers.Vectors (
      Index_Type => Word_Index,
      Element_Type => Word_Index);
   use Word_Vectors;

   subtype Word_Vector is Word_Vectors.Vector;

   subtype Word_Count is Word_Index'Base range 0 .. Word_Index'Last;

   type Words (After_Last_Word : Word_Count) is limited private;

   function Index_Paragraph (Paragraph : String) return Words;
   --  Creates a record with an array of the indexes to the first character of
   --  each word in Paragraph, plus an index pointing one past the end of the
   --  Paragraph.

   function Nth_Word (W : Words; N : Word_Index) return String;
   --  Returns the Nth word in W.PAragraph

   function Last_Word (W : Words) return Word_Count;
   --  Returns the word number of the last word (in other words the number of
   --  words) in W.Paragraph.

   function Word_Length (W : Words; N : Word_Index) return Positive;
   pragma Inline (Word_Length);
   --  Returns the length of the Nth word in W.Paragraph

   function Line_Length (W : Words; X, Y : Word_Index) return Positive;
   --  Returns the length of a line beginning with the Xth word and ending with
   --  the Yth word.

   function Merge_Lines
     (W                 : Words;
      Split_Before_Word : Word_Vector;
      Line_Prefix       : String := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Return the formatted paragraph, by splitting lines before each word
   --  in Split_Before_Word.
   --  Each created line will start with Line_Prefix.

private

   type Word_Starts is array (Word_Index range <>) of Word_Index;

   type Words (After_Last_Word : Word_Count) is limited record
      Paragraph : Ada.Strings.Unbounded.Unbounded_String;
      Starts    : Word_Starts (1 .. After_Last_Word);
   end record;
   --  Paragraph is the actual text of the Words. Starts contains indexes to
   --  the first character of each word in Paragraph. This facilitates the
   --  formatting algorithms in Paragraph_Filling because some only need to
   --  know the word lengths and positions most of the time.

end GNATCOLL.Paragraph_Filling.Words;
