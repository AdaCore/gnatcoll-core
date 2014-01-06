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

with Ada.Containers;        use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.Utils;        use GNATCOLL.Utils;

package body GNATCOLL.Paragraph_Filling.Words is

   -----------------
   -- Merge_Lines --
   -----------------

   function Merge_Lines
     (W                 : Words;
      Split_Before_Word : Word_Vector;
      Line_Prefix       : String := "")
      return Ada.Strings.Unbounded.Unbounded_String
   is
      Result : Unbounded_String;
      From   : Integer := 1;
      Start  : Integer;
      To     : Integer;
      Before_Word : Word_Index;
   begin
      for Count in reverse 2 .. Length (Split_Before_Word) - 1 loop
         Before_Word := Element (Split_Before_Word, Word_Index (Count));
         Start := Integer (W.Starts (Before_Word));

         To := Start - 1;
         while Is_Whitespace (Element (W.Paragraph, To)) loop
            To := To - 1;
         end loop;

         Append (Result, Line_Prefix);
         Append (Result, Slice (W.Paragraph, From, To));
         Append (Result, ASCII.LF);
         From := Start;
      end loop;

      To := Length (W.Paragraph);
      while Is_Whitespace (Element (W.Paragraph, To)) loop
         To := To - 1;
      end loop;

      Append (Result, Line_Prefix);
      Append (Result, Slice (W.Paragraph, From, To));
      Append (Result, ASCII.LF);
      return Result;
   end Merge_Lines;

   ---------------------
   -- Index_Paragraph --
   ---------------------

   function Index_Paragraph (Paragraph : String) return Words is
      Fixed_Para : Unbounded_String;
      Count      : Positive := Paragraph'First;
      Result     : Word_Vector;
   begin
      Append (Result, 1);  --  First word always starts on first character

      --  Takes out all spaces, tabs, and new line characters and creates a
      --  string with exactly one space between each word, plus one space at
      --  the end.

      if Paragraph /= "" then
         --  Skip leading whitespaces

         while Is_Whitespace (Paragraph (Count)) loop
            Count := Count + 1;
         end loop;

         while Count <= Paragraph'Last loop
            if Is_Whitespace (Paragraph (Count)) then
               loop
                  Count := Count + 1;
                  exit when Count > Paragraph'Last
                    or else not Is_Whitespace (Paragraph (Count));
               end loop;

               Append (Fixed_Para, ' ');
               Append (Result, Word_Index (Length (Fixed_Para) + 1));

               if Count <= Paragraph'Last then
                  Append (Fixed_Para, Paragraph (Count));
               end if;

            else
               --  ??? Might be more efficient to find the longuest substring
               --  with no multiple-space sequence, and append it at once. If
               --  the paragraph is already correct, we avoid a whole copy.
               Append (Fixed_Para, Paragraph (Count));
            end if;

            Count := Count + 1;
         end loop;

         if Element (Fixed_Para, Length (Fixed_Para)) /= ' ' then
            Append (Fixed_Para, ' ');
         end if;
      end if;

      --  Avoid extra copies of Starts array by building the result in place

      return W : Words (After_Last_Word => Word_Index (Length (Result))) do
         W.Paragraph := Fixed_Para;

         for Count in 1 .. Length (Result) loop
            W.Starts (Word_Index (Count)) :=
              Word_Vectors.Element (Result, Word_Index (Count));
         end loop;
      end return;
   end Index_Paragraph;

   ---------------
   -- Last_Word --
   ---------------

   function Last_Word (W : Words) return Word_Count is
   begin
      return W.After_Last_Word - 1;
   end Last_Word;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length (W : Words; X, Y : Word_Index) return Positive is
   begin
      return Positive (W.Starts (Y + 1) - W.Starts (X) - 1);
   end Line_Length;

   --------------
   -- Nth_Word --
   --------------

   function Nth_Word (W : Words; N : Word_Index) return String is
   begin
      return Slice (W.Paragraph,
                    Low  => Integer (W.Starts (N)),
                    High => Integer (W.Starts (N + 1) - 2));
   end Nth_Word;

   -----------------
   -- Word_Length --
   -----------------

   function Word_Length (W : Words; N : Word_Index) return Positive is
   begin
      return Integer (W.Starts (N + 1) - W.Starts (N) - 1);
   end Word_Length;

end GNATCOLL.Paragraph_Filling.Words;
