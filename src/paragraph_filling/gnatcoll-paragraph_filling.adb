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
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;            use Ada.Strings.Fixed;
with Ada.Strings;                  use Ada.Strings;
with Ada.Text_IO;                  use Ada.Text_IO;

with GNATCOLL.Paragraph_Filling.Words;
use GNATCOLL.Paragraph_Filling.Words;
with GNATCOLL.Paragraph_Filling.Badnesses;
use GNATCOLL.Paragraph_Filling.Badnesses;

package body GNATCOLL.Paragraph_Filling is

   type Badness_Values is array (Word_Index range <>) of Badness_Value;

   type Word_Indexes is array (Word_Index range <>) of Word_Index;

   package Word_Vectors is new Ada.Containers.Vectors (
      Index_Type => Word_Index,
      Element_Type => Word_Index);

   use Word_Vectors;

   subtype Word_Vector is Word_Vectors.Vector;

   function Greedy_Fill
     (Paragraph       : Paragraph_Filling.Words.Words;
      Max_Line_Length : Positive)
      return            String;
   --  Formats a paragraph with the greedy algorithm (by putting as many words
   --  as possible on each line).

   function Nth_Index
     (Source  : String;
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

   procedure Knuth_Fill
     (Paragraph       : in out Paragraph_Filling.Words.Words;
      Max_Line_Length : Positive := Default_Max_Line_Length);
   --  Fill the paragraph in the best possible way, based on an algorithm
   --  invented by Knuth. This algorithm uses dynamic programming techniques in
   --  order to fill paragraphs so that they have the lowest possible badness
   --  and line count. Badness is calculated by the Line_Badness function in
   --  Paragraph_Filling.Badnesses. For details see the paper, "Breaking
   --  Paragraphs into Lines", by Donald E. Knuth and Michael F. Plass,
   --  Software Practice and Experience, 11 (1981).

   function Count_Occurrences
     (Source  : String;
      Pattern : String)
      return    Natural;
   --  Number of times Pattern occurs in Source

   function Count_Lines (Source : String) return Natural;
   --  Number of lines in Source

   function Minimum_Lines
     (Paragraph       : Paragraph_Filling.Words.Words;
      Max_Line_Length : Positive)
      return            Natural;
   --  Minimum number of lines needed to fill Paragraph

   function Paragraph_Badness
     (Paragraph       : Paragraph_Filling.Words.Words;
      First_Words     : Word_Indexes;
      Max_Line_Length : Positive)
      return            Badness_Value;
   --  This calculates the total badness for the paragraph, as the sum of the
   --  badness for all the lines. First_Words is the first word on each line,
   --  plus a value at the end pointing past the last word in the paragraph.

   procedure Insert_New_Lines
     (Paragraph   : in out Paragraph_Filling.Words.Words;
      First_Words : Word_Vector);
   --  Inserts a new line before the first word of every line, skipping the
   --  first word of the paragraph and adding an extra new line at the end
   --  of the paragraph.

   function Slow_Fill
     (Paragraph       : Paragraph_Filling.Words.Words;
      Max_Line_Length : Positive)
      return            String;
   --  Fill the paragraph in the best possible way, using an extremely slow
   --  algorithm that tries all the possibilities. Used for testing the Knuth
   --  algorithm. This should produce the same result as the the Knuth
   --  algorithm.

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

   function Count_Lines (Source : String) return Natural is
   begin
      return Count_Occurrences (Source, Pattern => "" & ASCII.LF);
   end Count_Lines;

   -----------------------
   -- Count_Occurrences --
   -----------------------

   function Count_Occurrences
     (Source  : String;
      Pattern : String) return Natural
   is
      Result : Natural := 0;
      J      : Natural := 0;
   begin
      loop
         J := Index (Source, Pattern, J + 1);
         if J = 0 then
            return Result;
         end if;
         Result := Result + 1;
      end loop;
   end Count_Occurrences;

   -----------------
   -- Greedy_Fill --
   -----------------

   function Greedy_Fill
     (Paragraph       : Paragraph_Filling.Words.Words;
      Max_Line_Length : Positive) return String
   is
      Result              : Unbounded_String;
      Current_Line_Length : Natural;
   begin
      if Paragraph.Num_Chars /= 0 then
         Current_Line_Length := Word_Length (Paragraph, 1);
         Result              :=
            To_Unbounded_String (Nth_Word (Paragraph, 1));
         --  Go through the rest of the words, and for each one check if it
         --  fits on the current line.
         for Count in 2 .. Paragraph.After_Last_Word - 1 loop
            --  It fits
            if Current_Line_Length + Word_Length (Paragraph, Count) + 1 <=
               Max_Line_Length
            then
               Current_Line_Length := Current_Line_Length + 1;
               Append (Result, ' ');
            --  It doesn't fit. Start a new line
            else
               Current_Line_Length := 0;
               Append (Result, ASCII.LF);
            end if;
            Current_Line_Length := Current_Line_Length +
                                   Word_Length (Paragraph, Count);
            Append
              (Result,
               To_Unbounded_String (Nth_Word (Paragraph, Count)));
         end loop;
         Append (Result, ASCII.LF);
      end if;
      return To_String (Result);
   end Greedy_Fill;

   -----------------
   -- Greedy_Fill --
   -----------------

   function Greedy_Fill
     (Paragraph       : String;
      Max_Line_Length : Positive := Default_Max_Line_Length) return String
   is
      Para : String renames Paragraph;
   begin
      return Greedy_Fill (Index_Paragraph (Para), Max_Line_Length);
   end Greedy_Fill;

   ----------------------
   -- Insert_New_Lines --
   ----------------------

   procedure Insert_New_Lines
     (Paragraph   : in out Paragraph_Filling.Words.Words;
      First_Words : Word_Vector)
   is
   begin
      for Count in 2 .. Word_Index (Length (First_Words)) - 1 loop
         Add_New_Line (Paragraph, Before => Element (First_Words, Count));
      end loop;
      Add_New_Line (Paragraph, Before => Paragraph.After_Last_Word);
   end Insert_New_Lines;

   -------------------
   -- Is_Whitespace --
   -------------------

   function Is_Whitespace (Char : Character) return Boolean is
   begin
      if Char = ' ' or else Char = ASCII.HT or else Char = ASCII.LF then
         return True;
      end if;
      return False;
   end Is_Whitespace;

   ----------------
   -- Knuth_Fill --
   ----------------

   procedure Knuth_Fill
     (Paragraph       : in out Paragraph_Filling.Words.Words;
      Max_Line_Length : Positive := Default_Max_Line_Length)
   is

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
      Insert_New_Lines
        (Paragraph,
         Determine_First_Words
            (Last_Word (Paragraph),
             Calculate_Best (Paragraph, Max_Line_Length)));
   end Knuth_Fill;

   ----------------
   -- Knuth_Fill --
   ----------------

   function Knuth_Fill
     (Paragraph       : String;
      Max_Line_Length : Positive := Default_Max_Line_Length) return String
   is
      Para : Paragraph_Filling.Words.Words := Index_Paragraph (Paragraph);
   begin
      Knuth_Fill (Para, Max_Line_Length);

      --  Assert that slow fill answer is the same as the Knuthian answer, but
      --  only if Slow_Fill will not take too long. The limit of four lines was
      --  chosen based on experimenting with the speed of Slow_Fill.

      if False and then Minimum_Lines (Para, Max_Line_Length) <= 4 then
         pragma Assert
           (Slow_Fill (Paragraph, Max_Line_Length) = To_String (Para));
         null;
      end if;
      return To_String (Para);
   end Knuth_Fill;

   -------------------
   -- Minimum_Lines --
   -------------------

   function Minimum_Lines
     (Paragraph       : Paragraph_Filling.Words.Words;
      Max_Line_Length : Positive) return Natural
   is
   begin
      --  Calculate the minimum number of lines by applying the greedy
      --  algorithm, and then counting the number of lines produces by that
      --  algorithm.

      return Count_Lines (Greedy_Fill (Paragraph, Max_Line_Length));
   end Minimum_Lines;

   --------------------
   -- No_Fill --
   --------------------

   function No_Fill
     (Paragraph       : String;
      Max_Line_Length : Positive := Default_Max_Line_Length) return String
   is
      pragma Unreferenced (Max_Line_Length);
   begin
      return Paragraph;
   end No_Fill;

   ---------------
   -- Nth_Index --
   ---------------

   function Nth_Index
     (Source  : String;
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

   -----------------------
   -- Paragraph_Badness --
   -----------------------

   package Word_IO is new Ada.Text_IO.Integer_IO (Word_Index);

   function Paragraph_Badness
     (Paragraph       : Paragraph_Filling.Words.Words;
      First_Words     : Word_Indexes;
      Max_Line_Length : Positive) return Badness_Value
   is
      pragma Assert (First_Words (First_Words'First) = 1);
      pragma Assert
        (First_Words (First_Words'Last) = Paragraph.After_Last_Word);

      Result : Badness_Value := Zero;
   begin
      for J in First_Words'First .. First_Words'Last - 1 loop
         declare
            X : constant Word_Index := First_Words (J);
            Y : constant Word_Index := First_Words (J + 1) - 1;
            --  X and Y are the first and last words on this line

            This_Line_Badness : constant Badness_Value :=
               Line_Badness (Paragraph, X, Y, Max_Line_Length);
         begin
            --  Single word too long for line.  ???Probably Line_Badness
            --  should return 0 in this case. Not sure how that affects the
            --  Knuth algorithm, but it would mean we can get rid of this
            --  'if'.

            if This_Line_Badness = Infinity and then X = Y then
               null;

            else
               Result := Result + This_Line_Badness;
            end if;

            exit when Result = Infinity; -- Can't get worse
         end;
      end loop;

      if False and then Result < Infinity then -- Debugging output
         Put (Image (Result));
         Put (ASCII.HT & " ==> ");
         for J in First_Words'First .. First_Words'Last loop
            Put (" ");
            Word_IO.Put (First_Words (J), Width => 2);
         end loop;
         New_Line;
      end if;

      return Result;
   end Paragraph_Badness;

   -----------------
   -- Pretty_Fill --
   -----------------

   function Pretty_Fill
     (Paragraph       : String;
      Max_Line_Length : Positive := Default_Max_Line_Length)
      return            String
   is
      Result          : String :=
                          Greedy_Fill (Paragraph, Max_Line_Length);
      Number_Of_Lines : constant Positive :=
                          Ada.Strings.Fixed.Count (Result, "" & ASCII.LF);
      Did_Something   : Boolean;
      Count           : Natural           := 0;
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
                  Result (Word_Start) := ASCII.LF;
                  Result (Index_1)    := ' ';
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
               Result (Word_Start) := ASCII.LF;
               Result (Index_2)    := ' ';
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
               Result (Word_Start) := ASCII.LF;
               Result (Index_2)    := ' ';
               Did_Something       := True;
            end if;
         end;

      end if;

      return Result;
   end Pretty_Fill;

   ---------------
   -- Slow_Fill --
   ---------------

   function Slow_Fill
     (Paragraph       : Paragraph_Filling.Words.Words;
      Max_Line_Length : Positive) return String
   is

      --  If N = the number of words (Paragraph.After_Last_Word-1), then there
      --  are N-1 spaces between words. Each space could be turned into a
      --  new-line, or not. We could try all those possibilities, which would
      --  be 2**(N-1) possibilities.
      --
      --  We make it somewhat faster, by only trying possibilities that have
      --  the right number of lines. If L is the number of lines, then the
      --  number of possibilities is N-choose-L, that is, (N! / (N-L)! * L!).
      --  Example: 50 words formatted into 5 lines --> 2,118,760 possibilities.
      --  That's a lot, but much better than 2**(50-1) = 562,949,953,421,312.

      Para : Paragraph_Filling.Words.Words := Paragraph;

      Num_Lines : constant Word_Index :=
         Word_Index (Minimum_Lines (Para, Max_Line_Length));
      --  Only try possibilities that have exactly this number of lines

      --  Min_Badness is the minimum badness value for all the possibilities
      --  tried, and Min_Badness_First_Words is the first-words-in-lines
      --  corresponding to that minimum badness.

      Min_Badness             : Badness_Value := Infinity;
      Min_Badness_First_Words : Word_Indexes (1 .. Num_Lines + 1);
      --  One extra element for beyond-the-last-word

      procedure Recursive_Check (First_Words : Word_Indexes);
      --  Recursively enumerate all the possibilities. We start with
      --  First_Words containing just 1 (the first word of the first line must
      --  be the first word of the paragraph). We then recursively add all
      --  possible first words for the second line, and so on, until we've got
      --  the right number of first words.

      ---------------------
      -- Recursive_Check --
      ---------------------

      procedure Recursive_Check (First_Words : Word_Indexes) is
         pragma Assert (First_Words'Length in 1 .. Num_Lines + 1);

         function "<=" (X, Y : Badness_Value) return Boolean;

         ----------
         -- "<=" --
         ----------

         function "<=" (X, Y : Badness_Value) return Boolean is
         begin
            return X = Y or else X < Y;
         end "<=";

      begin
         --  We've got the right number of words, so check if the badness is
         --  better than what we found so far.

         if First_Words'Length = Num_Lines + 1 then
            declare
               Badness : constant Badness_Value :=
                  Paragraph_Badness (Para, First_Words, Max_Line_Length);
            begin
               if Badness <= Min_Badness then
                  Min_Badness             := Badness;
                  Min_Badness_First_Words := First_Words;
               end if;
            end;

         --  We've got all but the last one, so recurse just one more time,
         --  appending the past-last word

         elsif First_Words'Length = Num_Lines then
            Recursive_Check (First_Words & Para.After_Last_Word);

         --  Not enough first words, yet. Go through all the words after the
         --  ones we have, leaving room for the rest, and recurse

         else
            pragma Assert (First_Words'Length in 1 .. Num_Lines - 1);

            for J in
                  First_Words (First_Words'Last) + 1 ..
                  Para.After_Last_Word - (Num_Lines - First_Words'Length)
            loop
               Recursive_Check (First_Words & J);
            end loop;
         end if;
      end Recursive_Check;

   --  Start of processing for Slow_Fill

   begin
      Recursive_Check ((1 => 1));

      --  ??? Mimics other code, should be split out into sub-program

      for Count in 2 .. Min_Badness_First_Words'Last loop
         Add_New_Line (Para, Before => Min_Badness_First_Words (Count));
      end loop;
      Add_New_Line (Para, Before => Para.After_Last_Word);

      return To_String (Para);
   end Slow_Fill;

   function Slow_Fill
     (Paragraph       : String;
      Max_Line_Length : Positive) return String
   is
   begin
      return Slow_Fill (Index_Paragraph (Paragraph), Max_Line_Length);
   end Slow_Fill;

end GNATCOLL.Paragraph_Filling;
