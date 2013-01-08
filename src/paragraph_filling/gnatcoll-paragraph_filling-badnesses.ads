------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2013, AdaCore                     --
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

with GNATCOLL.Paragraph_Filling.Words; use GNATCOLL.Paragraph_Filling.Words;

private package GNATCOLL.Paragraph_Filling.Badnesses is

   type Badness_Value is private;
   Zero     : constant Badness_Value;
   Infinity : constant Badness_Value;
   --  It's a non-negative number that represents how bad the formatting of a
   --  line or paragraph is, with Zero meaning perfectly good, and Infinity
   --  representing "too long to fit on a line". The term 'badness' comes from
   --  Knuth's TeX.

   function "+" (X, Y : Badness_Value) return Badness_Value;
   --  Adds two Badnesses. If the result is greater than infinity, it returns
   --  infinity.

   function "<" (X, Y : Badness_Value) return Boolean;
   --  Returns true if X is less than Y. Else returns False

   function "**" (X : Natural; Y : Positive) return Badness_Value;
   --  Raise X to the Y power and returns the result as a Badness_Value. If the
   --  result is greater that infinity, returns infinity.

   function Image (Badness : Badness_Value) return String;

   function Line_Badness
     (W                : Paragraph_Filling.Words.Words;
      X, Y             : Word_Index;
      Max_Line_Length  : Positive;
      Format_Last_Line : Boolean := False) return Badness_Value;
   --  Returns badness as determined by a formula invented by Knuth. This
   --  formula calculates the square of the difference between the Line_Length
   --  (calculated by the Line_Length function in Paragraph_Filling.Words) and
   --  the Max_Line_Length. If the line length is greater than the
   --  Max_Line_Length then the function returns Infinity.
   --
   --  ??? Currently, Format_Last_Line is always defaulted to false. However,
   --  the calls could be changes to allow a user option of whether to include
   --  the last line in badness calculations.

   function Line_Badness
     (Line_Length     : Positive;
      Max_Line_Length : Positive) return Badness_Value;
   --  Returns badness as determined by a formula invented by Knuth.
   --  This formula calculates the square of the difference between the
   --  Line_Length and the Max_Line_Length. If the line length is greater
   --  than the Max_Line_Length then the function returns Infinity.

private

   type Badness_Value is new Natural;
   Zero     : constant Badness_Value := 0;
   Infinity : constant Badness_Value := Badness_Value'Last;

end GNATCOLL.Paragraph_Filling.Badnesses;
