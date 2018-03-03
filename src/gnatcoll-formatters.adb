------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

package body GNATCOLL.Formatters is

   ----------------------
   -- Columns_Vertical --
   ----------------------

   procedure Columns_Vertical
     (Words     : Strings.XString_Array;
      Width     : Positive;
      Put_Line  : not null access procedure (Line : Strings.XString);
      Pad       : Strings.Char_Type   := Strings.Space;
      Delimiter : Strings.Char_String := (1 => Strings.Space))
   is
      use Strings;

      Rows : Natural;
      Cols : Natural;
      Max  : Natural := 0;
      Idx  : Natural;
      Len  : Natural;
   begin
      --  Prepare initial rough proposal for number of rows

      for W of Words loop
         if W.Length > Max then
            Max := W.Length;
         end if;
      end loop;

      Cols := Width / (Max + Delimiter'Length);
      Rows := Words'Length / Cols;

      if Words'Length rem Cols /= 0 then
         Rows := Rows + 1;
      end if;

      --  Trying to reduce number of rows using max length in each column

      Reduce_Rows : while Rows > 1 loop
         Rows := Rows - 1;
         Cols := Words'Length / Rows;

         if Words'Length rem Rows > 0 then
            Cols := Cols + 1;
         end if;

         Idx := Words'First;
         Len := 0;

         for Col in 1 .. Cols loop
            Max := 0;
            for Row in 1 .. Rows loop
               if Max < Words (Idx).Length then
                  Max := Words (Idx).Length;
               end if;
               Idx := Idx + 1;
               exit when Idx > Words'Last;
            end loop;

            Len := Len + Max + Delimiter'Length;

            if Len > Width then
               Rows := Rows + 1;
               exit Reduce_Rows;
            end if;
            exit when Idx > Words'Last;
         end loop;
      end loop Reduce_Rows;

      Cols := Words'Length / Rows;

      if Words'Length rem Rows > 0 then
         Cols := Cols + 1;
      end if;

      declare
         Cmax  : array (1 .. Cols) of Positive := (others => 1);
         Line  : XString;
         Align : Natural;
      begin
         for J in Words'Range loop
            Idx := (J - Words'First) / Rows + 1;
            if Cmax (Idx) < Words (J).Length then
               Cmax (Idx) := Words (J).Length;
            end if;
         end loop;

         for Row in 1 .. Rows loop
            Line.Clear;
            Align := 0;

            for Col in 1 .. Cols loop
               Idx := (Col - 1) * Rows + Row - 1 + Words'First;

               exit when Idx > Words'Last;

               if not Line.Is_Empty then
                  Line.Append (Align * Pad);
                  Line.Append (Delimiter);
               end if;

               Line.Append (Words (Idx));

               Align := Cmax (Col) - Words (Idx).Length;
            end loop;

            Put_Line (Line);
         end loop;
      end;
   end Columns_Vertical;

   -------------------------------
   -- Columns_Vertical_XStrings --
   -------------------------------

   function Columns_Vertical_XString
     (Words       : Strings.XString_Array;
      Width       : Positive;
      Pad         : Strings.Char_Type   := Strings.Space;
      Delimiter   : Strings.Char_String := (1 => Strings.Space))
      return Strings.XString
   is
      Result : Strings.XString;

      procedure Append_Line (Line : Strings.XString);
      --  Append line to result

      ---------------
      -- Each_Line --
      ---------------

      procedure Append_Line (Line : Strings.XString) is
      begin
         Result.Append (Line);
         Result.Append (End_Of_Line);
      end Append_Line;

      procedure Format_Columns is new Columns_Vertical (Strings);

   begin
      Format_Columns (Words, Width, Append_Line'Access, Pad, Delimiter);
      return Result;
   end Columns_Vertical_XString;

end GNATCOLL.Formatters;
