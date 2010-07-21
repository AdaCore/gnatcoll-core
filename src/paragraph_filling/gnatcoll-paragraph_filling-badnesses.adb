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
