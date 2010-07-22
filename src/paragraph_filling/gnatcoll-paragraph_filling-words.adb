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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body GNATCOLL.Paragraph_Filling.Words is

   package Word_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Word_Index,
      Element_Type => Positive);
   use Word_Vectors;

   subtype Word_Vector is Word_Vectors.Vector;

   ------------------
   -- Add_New_Line --
   ------------------

   procedure Add_New_Line (W : in out Words; Before : Word_Index) is
   begin
      Replace_Element (W.Paragraph,
                       W.Starts (Before) - 1,
                       ASCII.LF);
   end Add_New_Line;

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
               Append (Result, Length (Fixed_Para) + 1);

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
      return W.Starts (Y + 1) - W.Starts (X) - 1;
   end Line_Length;

   --------------
   -- Nth_Word --
   --------------

   function Nth_Word (W : Words; N : Word_Index) return String is
   begin
      return Slice (W.Paragraph,
                    Low  => W.Starts (N),
                    High => W.Starts (N + 1) - 2);
   end Nth_Word;

   ---------------
   -- To_String --
   ---------------

   function To_String (W : Words) return Unbounded_String is
   begin
      return W.Paragraph;
   end To_String;

   -----------------
   -- Word_Length --
   -----------------

   function Word_Length (W : Words; N : Word_Index) return Positive is
   begin
      return W.Starts (N + 1) - W.Starts (N) - 1;
   end Word_Length;

end GNATCOLL.Paragraph_Filling.Words;
