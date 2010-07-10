-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                       Copyright (C) 2010, AdaCore                 --
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

--  This software was originally contributed by William A. Duff.

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body GNATCOLL.Para_Fill.Words is

   function Remove_Excess_Characters (Paragraph : String) return String;
   --  Takes out all spaces, tabs, and new line characsters and creates a
   --  string with exactly one space between each word, plus one space at
   --  the end.

   package Word_Vectors is new Ada.Containers.Vectors (
      Index_Type => Word_Index,
      Element_Type => Positive);

   use Word_Vectors;

   subtype Word_Vector is Word_Vectors.Vector;

   function To_Word_Starts (Vector : Word_Vector) return Word_Starts;
   --  Takes a Word_Vector and returns the equivalent Word_Starts.

   ------------------
   -- Add_New_Line --
   ------------------

   procedure Add_New_Line (W : in out Words; Before : Word_Index) is
   begin
      W.Paragraph (W.Starts (Before) - 1) := ASCII.LF;
   end Add_New_Line;

   ---------------------
   -- Index_Paragraph --
   ---------------------

   function Index_Paragraph (Paragraph : String) return Words is
      Result : Word_Vector;
      Fixed_Para : constant String := Remove_Excess_Characters (Paragraph);
   begin
      Append (Result, Fixed_Para'First);

      for Count in Fixed_Para'Range loop
         if Fixed_Para (Count) = ' ' then
            Append (Result, Count + 1);
         end if;
      end loop;

      return
        (Num_Chars => Fixed_Para'Length,
         After_Last_Word => Word_Index (Length (Result)),
         Paragraph => Fixed_Para,
         Starts    => To_Word_Starts (Result));
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
      return W.Paragraph (W.Starts (N) ..  W.Starts (N + 1) - 2);
   end Nth_Word;

   ------------------------------
   -- Remove_Excess_Characters --
   ------------------------------

   function Remove_Excess_Characters (Paragraph : String) return String is
      Result : Unbounded_String;
      Count  : Positive := Paragraph'First;
   begin
      if Paragraph = "" then
         return "";
      end if;

      if not Is_Whitespace (Paragraph (Paragraph'First)) then
         Append (Result, Paragraph (Paragraph'First));
      end if;

      while Count < Paragraph'Last loop
         Count := Count + 1;

         if Is_Whitespace (Paragraph (Count)) then
            while Count <= Paragraph'Length and then
             Is_Whitespace (Paragraph (Count)) loop
               Count := Count + 1;
            end loop;
            Append (Result, ' ');
            Count := Count - 1;
         else
            Append (Result, Paragraph (Count));
         end if;

      end loop;

      if Element (Result, Length (Result)) = ' ' then
         return To_String (Result);
      else
         return To_String (Result) & ' ';
      end if;
   end Remove_Excess_Characters;

   ---------------
   -- To_String --
   ---------------

   function To_String (W : Words) return String is
   begin
      return W.Paragraph;
   end To_String;

   --------------------
   -- To_Word_Starts --
   --------------------

   function To_Word_Starts (Vector : Word_Vector) return Word_Starts is
      Result : Word_Starts (1 .. Word_Index (Length (Vector)));
   begin
      for Count in 1 .. Length (Vector) loop
         Result (Word_Index (Count))  :=
            Word_Vectors.Element (Vector, Word_Index (Count));
      end loop;
      return Result;
   end To_Word_Starts;

   -----------------
   -- Word_Length --
   -----------------

   function Word_Length (W : Words; N : Word_Index) return Positive is
   begin
      return W.Starts (N + 1) - W.Starts (N) - 1;
   end Word_Length;

end GNATCOLL.Para_Fill.Words;
