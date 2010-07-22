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

with Ada.Strings.Unbounded;

private package GNATCOLL.Paragraph_Filling.Words is

   --  Provides ways of differentiating words by reformatting a paragraph and
   --  pointing to the first character of each word in that paragraph.

   type Word_Index is new Positive;

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

   procedure Add_New_Line (W : in out Words; Before : Word_Index);
   --  Replaces the space before a word in W.Paragraph with a new line

   function To_String
     (W : Words) return Ada.Strings.Unbounded.Unbounded_String;
   --  Returns W.Paragraph

private

   type Word_Starts is array (Word_Index range <>) of Positive;

   type Words (After_Last_Word : Word_Count) is limited record
      Paragraph : Ada.Strings.Unbounded.Unbounded_String;
      Starts    : Word_Starts (1 .. After_Last_Word);
   end record;
   --  Paragraph is the actual text of the Words. Starts contains indexes to
   --  the first character of each word in Paragraph. This facilitates the
   --  formatting algorithms in Paragraph_Filling because some only need to
   --  know the word lengths and positions most of the time.

end GNATCOLL.Paragraph_Filling.Words;
