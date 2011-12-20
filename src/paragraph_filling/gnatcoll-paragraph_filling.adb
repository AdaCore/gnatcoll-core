------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Strings;                  use Ada.Strings;

with GNATCOLL.Paragraph_Filling.Words;
use GNATCOLL.Paragraph_Filling.Words;
with GNATCOLL.Paragraph_Filling.Badnesses;
use GNATCOLL.Paragraph_Filling.Badnesses;

package body GNATCOLL.Paragraph_Filling is
   use Word_Vectors;

   type Badness_Values is array (Word_Index range <>) of Badness_Value;

   type Word_Indexes is array (Word_Index range <>) of Word_Index;

   function Greedy_Fill
     (Paragraph       : Paragraph_Filling.Words.Words;
      Max_Line_Length : Positive;
      Line_Prefix     : String := "") return Unbounded_String;
   --  Formats a paragraph with the greedy algorithm (by putting as many words
   --  as possible on each line).

   function Nth_Index
     (Source  : Unbounded_String;
      Pattern : String;
      N       : Natural)
      return    Natural;
   --  Returns the Nth instance of Pattern within Source. If the Nth index does
   --  not exist returns 0.

   function Can_And_Should_Move_A_Word
     (Max_Line_Length       : Positive;
      Length_Of_First_Line  : Positive;
      Length_Of_Second_Line : Positive;
      Length_Of_Last_Word   : Positive)
      return                  Boolean;
   --  Decides whether the last word of the first line can fit on the second
   --  line and if so whether the badness is reduced by moving the word.

   function Count_Lines (Source : Unbounded_String) return Natural;
   --  Number of lines in Source

   --------------------------------
   -- Can_And_Should_Move_A_Word --
   --------------------------------

   function Can_And_Should_Move_A_Word
     (Max_Line_Length       : Positive;
      Length_Of_First_Line  : Positive;
      Length_Of_Second_Line : Positive;
      Length_Of_Last_Word   : Positive) return Boolean
   is
   begin
      if Length_Of_Second_Line + Length_Of_Last_Word <= Max_Line_Length
        and then not (Line_Badness (Length_Of_First_Line, Max_Line_Length) +
                      Line_Badness (Length_Of_Second_Line, Max_Line_Length) <
                      Line_Badness
                         (Length_Of_First_Line - Length_Of_Last_Word,
                          Max_Line_Length) +
                      Line_Badness
                         (Length_Of_Second_Line + Length_Of_Last_Word,
                          Max_Line_Length))
      then
         return True;
      else
         return False;
      end if;
   end Can_And_Should_Move_A_Word;

   -----------------
   -- Count_Lines --
   -----------------

   function Count_Lines (Source : Unbounded_String) return Natural is
      Result : Natural := 0;
   begin
      for J in 1 .. Length (Source) loop
         if Element (Source, J) = ASCII.LF then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count_Lines;

   -----------------
   -- Greedy_Fill --
   -----------------

   function Greedy_Fill
     (Paragraph       : Paragraph_Filling.Words.Words;
      Max_Line_Length : Positive;
      Line_Prefix     : String := "") return Unbounded_String
   is
      Max : constant Integer := Max_Line_Length - Line_Prefix'Length;
      Result      : Unbounded_String;
      Current     : Natural;   --  current line length
   begin
      if Paragraph.After_Last_Word /= 1 then
         Result := To_Unbounded_String (Line_Prefix)
           & Nth_Word (Paragraph, 1);
         Current := Length (Result);

         --  Go through the rest of the words, and for each one check if it
         --  fits on the current line.
         for Count in 2 .. Paragraph.After_Last_Word - 1 loop
            declare
               W : constant String := Nth_Word (Paragraph, Count);
            begin
               if Current + W'Length + 1 <= Max then
                  Current := Current + 1;
                  Append (Result, ' ');
               else
                  Append (Result, ASCII.LF);
                  Append (Result, Line_Prefix);
                  Current := Line_Prefix'Length;
               end if;

               Current := Current + W'Length;
               Append (Result, W);
            end;
         end loop;

         Append (Result, ASCII.LF);
      end if;

      return Result;
   end Greedy_Fill;

   -----------------
   -- Greedy_Fill --
   -----------------

   function Greedy_Fill
     (Paragraph       : String;
      Max_Line_Length : Positive := Default_Max_Line_Length;
      Line_Prefix     : String := "")
      return Ada.Strings.Unbounded.Unbounded_String
   is
      Para : String renames Paragraph;
      W    : GNATCOLL.Paragraph_Filling.Words.Words := Index_Paragraph (Para);
   begin
      return Greedy_Fill (W, Max_Line_Length, Line_Prefix);
   end Greedy_Fill;

   ----------------
   -- Knuth_Fill --
   ----------------

   function Knuth_Fill
     (Paragraph       : String;
      Max_Line_Length : Positive := Default_Max_Line_Length;
      Line_Prefix     : String := "")
      return Unbounded_String
   is
      Para : Paragraph_Filling.Words.Words := Index_Paragraph (Paragraph);

      function Calculate_Best
        (Paragraph       : Paragraph_Filling.Words.Words;
         Max_Line_Length : Positive) return Word_Indexes;
      --  Determines the best division of lines and returns the word that
      --  begins each line in this setup as an array.

      function Determine_First_Words
        (Last_Word  : Word_Count;
         Best_Words : Word_Indexes) return Word_Vector;
      --  Takes an array of the first words, reverses it, and stores it in a
      --  vector.

      --------------------
      -- Calculate_Best --
      --------------------

      function Calculate_Best
        (Paragraph       : Paragraph_Filling.Words.Words;
         Max_Line_Length : Positive) return Word_Indexes
      is
         Minimum_Badnesses : Badness_Values (1 .. Last_Word (Paragraph) + 1);
         Result            : Word_Indexes (1 .. Last_Word (Paragraph));

      begin
         Minimum_Badnesses (1) := Zero;

         for Y in 1 .. Last_Word (Paragraph) loop
            Minimum_Badnesses (Y + 1) := Infinity;

            for X in reverse 1 .. Y loop
               declare
                  Badness : constant Badness_Value :=
                     Line_Badness (Paragraph, X, Y, Max_Line_Length) +
                     Minimum_Badnesses (X);
               begin
                  exit when Badness = Infinity;
                  --  ??? Badness might be uninitialized here
                  if Badness < Minimum_Badnesses (Y + 1) then
                     Minimum_Badnesses (Y + 1) := Badness;
                     Result (Y)                := X;
                  end if;
               end;
            end loop;

         end loop;

         for Count in Result'First .. Result'Last - 1 loop
            pragma Assert (Result (Count) <= Result (Count + 1));
            null;
         end loop;
         return Result;

      end Calculate_Best;

      ---------------------------
      -- Determine_First_Words --
      ---------------------------

      function Determine_First_Words
        (Last_Word  : Word_Count;
         Best_Words : Word_Indexes) return Word_Vector
      is
         X      : Word_Index;
         Y      : Word_Index := Last_Word;
         Result : Word_Vector;
      begin
         Append (Result, Word_Index'Last);
         loop
            X := Best_Words (Y);
            Append (Result, X);
            exit when X = 1;
            pragma Assert (Y /= X - 1);
            Y := X - 1;
         end loop;
         return Result;
      end Determine_First_Words;

   --  Start of processing for Knuth_Fill

   begin
      if Paragraph = "" then
         return To_Unbounded_String (Paragraph);
      end if;

      return Merge_Lines
        (W                 => Para,
         Split_Before_Word =>
           Determine_First_Words
             (Last_Word (Para), Calculate_Best (Para, Max_Line_Length)),
         Line_Prefix => Line_Prefix);
   end Knuth_Fill;

   --------------------
   -- No_Fill --
   --------------------

   function No_Fill
     (Paragraph       : String;
      Max_Line_Length : Positive := Default_Max_Line_Length;
      Line_Prefix     : String := "")
      return Unbounded_String
   is
      pragma Unreferenced (Max_Line_Length, Line_Prefix);
   begin
      return To_Unbounded_String (Paragraph);
   end No_Fill;

   ---------------
   -- Nth_Index --
   ---------------

   function Nth_Index
     (Source  : Unbounded_String;
      Pattern : String;
      N       : Natural) return Natural
   is
      Result : Natural := 0;
   begin
      for Count in 1 .. N loop
         Result := Index (Source, Pattern, Result + 1);
         --  If there are > N instances the next intstance after the last
         --  existing instance will return 0. If the loop continues to
         --  run Index will start from the beginning and will end on an
         --  arbitrary instance.
         exit when Result = 0;
      end loop;
      return Result;
   end Nth_Index;

   -----------------
   -- Pretty_Fill --
   -----------------

   function Pretty_Fill
     (Paragraph       : String;
      Max_Line_Length : Positive := Default_Max_Line_Length)
      return Unbounded_String
   is
      --  Since we will be changing the lines afterward, this is not compatible
      --  with the use of Line_Prefix.
      Result          : Unbounded_String :=
        Greedy_Fill (Paragraph, Max_Line_Length, Line_Prefix => "");
      Number_Of_Lines : constant Natural := Count_Lines (Result);
      Did_Something   : Boolean;
      Count           : Natural          := 0;
   begin
      --  Compares adjacent lines, starting with the first and second line
      --  and ending with the third to last and second to last lines. If
      --  Badness after moving the last word of the upper line to the next
      --  line is less than or equal to Badness of leaving the paragraph alone,
      --  then the word is moved. This is repeated through the whole paragraph
      --  (Max_Line_Length / 2) * Number_Of_Lines times or until nothing is
      --  changed in a loop of the paragraph.

      loop
         Count         := Count + 1;
         Did_Something := False;
         for Line_Number in 1 .. Number_Of_Lines - 2 loop

            declare
               --  ??? Very inefficient, we keep searching the same pattern
               --  over and over.
               Index_0     : constant Natural  :=
                  Nth_Index (Result, "" & ASCII.LF, Line_Number - 1);
               Index_1     : constant Natural  :=
                  Nth_Index (Result, "" & ASCII.LF, Line_Number);
               Index_2     : constant Natural  :=
                  Nth_Index (Result, "" & ASCII.LF, Line_Number + 1);
               Length_1    : constant Positive := Index_1 - Index_0 - 1;
               Length_2    : constant Positive := Index_2 - Index_1 - 1;
               Word_Start  : constant Natural  :=
                  Index (Result, " ", Index_1, Backward);
               Word_Length : constant Positive := Index_1 - Word_Start;

            begin
               if Can_And_Should_Move_A_Word
                    (Max_Line_Length,
                     Length_1,
                     Length_2,
                     Word_Length)
               then
                  Replace_Element (Result, Word_Start, ASCII.LF);
                  Replace_Element (Result, Index_1, ' ');
                  Did_Something       := True;
               end if;

            end;

         end loop;
         exit when not Did_Something;

      end loop;

      if Number_Of_Lines >= 4 then

         declare
            Index_0     : constant Natural  :=
               Nth_Index (Result, "" & ASCII.LF, Number_Of_Lines - 3);
            Index_1     : constant Natural  :=
               Nth_Index (Result, "" & ASCII.LF, Number_Of_Lines - 2);
            Index_2     : constant Natural  :=
               Nth_Index (Result, "" & ASCII.LF, Number_Of_Lines - 1);
            Index_3     : constant Natural  :=
               Nth_Index (Result, "" & ASCII.LF, Number_Of_Lines);
            Length_1    : constant Positive := Index_1 - Index_0 - 1;
            Length_2    : constant Positive := Index_2 - Index_1 - 1;
            Length_3    : constant Positive := Index_3 - Index_2 - 1;
            Word_Start  : constant Natural  :=
               Index (Result, " ", Index_2, Backward);
            Word_Length : constant Positive := Index_2 - Word_Start;
         begin
            if Length_3 + Word_Length <= Max_Line_Length
              and then abs (Length_2 - Length_1) >
                       abs (Length_2 - Length_1 - Word_Length)
            then
               Replace_Element (Result, Word_Start, ASCII.LF);
               Replace_Element (Result, Index_2, ' ');
               Did_Something       := True;
            end if;
         end;

      elsif Number_Of_Lines = 3 then

         declare
            Index_0     : constant Natural  := 0;
            Index_1     : constant Natural  :=
               Nth_Index (Result, "" & ASCII.LF, Number_Of_Lines - 2);
            Index_2     : constant Natural  :=
               Nth_Index (Result, "" & ASCII.LF, Number_Of_Lines - 1);
            Index_3     : constant Natural  :=
               Nth_Index (Result, "" & ASCII.LF, Number_Of_Lines);
            Length_1    : constant Positive := Index_1 - Index_0 - 1;
            Length_2    : constant Positive := Index_2 - Index_1 - 1;
            Length_3    : constant Positive := Index_3 - Index_2 - 1;
            Word_Start  : constant Natural  :=
               Index (Result, " ", Index_2, Backward);
            Word_Length : constant Positive := Index_2 - Word_Start;
         begin
            if Length_3 + Word_Length <= Max_Line_Length
              and then abs (Length_2 - Length_1) >
                       abs (Length_2 - Length_1 - Word_Length)
            then
               Replace_Element (Result, Word_Start, ASCII.LF);
               Replace_Element (Result, Index_2,    ' ');
               Did_Something       := True;
            end if;
         end;
      end if;

      return Result;
   end Pretty_Fill;

end GNATCOLL.Paragraph_Filling;
