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

package body GNATCOLL.Paragraph_Filling.Badnesses is

   ----------
   -- "**" --
   ----------

   function "**" (X : Natural; Y : Positive) return Badness_Value is
      pragma Unsuppress (All_Checks);
   begin
      return Badness_Value (Integer'(X ** Y));
   exception
      when Constraint_Error =>
         return Infinity;
   end "**";

   ---------
   -- "+" --
   ---------

   overriding function "+" (X, Y : Badness_Value) return Badness_Value is
      pragma Unsuppress (All_Checks);
   begin
      return Badness_Value (Integer (X) + Integer (Y));
   exception
      when Constraint_Error =>
         return Infinity;
   end "+";

   ---------
   -- "<" --
   ---------

   overriding function "<" (X, Y : Badness_Value) return Boolean is
   begin
      return Integer (X) < Integer (Y);
   end "<";

   -----------
   -- Image --
   -----------

   function Image (Badness : Badness_Value) return String is
   begin
      if Badness = Infinity then
         return "Inf";
      else
         declare
            Result : constant String := Badness_Value'Image (Badness);
         begin
            --  Slicing removes the superfluous space
            return Result (Result'First + 1 .. Result'Last);
         end;
      end if;
   end Image;

   ------------------
   -- Line_Badness --
   ------------------

   function Line_Badness
     (W                : Paragraph_Filling.Words.Words;
      X, Y             : Word_Index;
      Max_Line_Length  : Positive;
      Format_Last_Line : Boolean := False) return Badness_Value
   is
      Distance : constant Integer := Max_Line_Length - Line_Length (W, X, Y);

   begin
      --  Line is too long

      if Distance < 0 then

         --  One word line, meaning nothing can be done to shorten it

         if X = Y then
            return 0;

         --  Not one word line, meaning it can be split into two pieces

         else
            return Infinity;
         end if;

      --  Last line is not bad if unless Format_Last_Line = True

      elsif  Y = Last_Word (W)  and then not Format_Last_Line then
         return 0;

         --  Otherwise, normal line. Return the badness of distance to the end
         --  of the line squared.

      else
         return Distance ** 2;
      end if;
   end Line_Badness;

   ------------------
   -- Line_Badness --
   ------------------

   function Line_Badness
     (Line_Length : Positive; Max_Line_Length : Positive)
      return Badness_Value is
   begin
      if Line_Length > Max_Line_Length then
         return Infinity;

      else
         return (Max_Line_Length - Line_Length) ** 2;
      end if;
   end Line_Badness;

end GNATCOLL.Paragraph_Filling.Badnesses;
