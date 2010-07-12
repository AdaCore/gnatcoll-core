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

with GNATCOLL.Para_Fill.Words; use GNATCOLL.Para_Fill.Words;

private package GNATCOLL.Para_Fill.Badnesses is

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
     (W                : GNATCOLL.Para_Fill.Words.Words;
      X, Y             : Word_Index;
      Max_Line_Length  : Positive;
      Format_Last_Line : Boolean := False) return Badness_Value;
   --  Returns badness as determined by a formula invented by Knuth. This
   --  formula calculates the square of the difference between the Line_Length
   --  (calculated by the Line_Length function in GNATCOLL.Para_Fill.Words) and
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

end GNATCOLL.Para_Fill.Badnesses;
