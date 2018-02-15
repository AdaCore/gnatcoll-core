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

procedure GNATCOLL.Format_Columns_Vertical_Impl
  (Words    : Strings.XString_Array;
   Width    : Positive;
   Spaces   : Natural := 1;
   Put_Line : access procedure (Line : Strings.Character_String))
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

   Cols := Width / (Max + Spaces);
   Rows := Words'Length / Cols;

   if Words'Length rem Cols /= 0 then
      Rows := Rows + 1;
   end if;

   --  Trying to reduce number of rows using max length in each column

   Reduce_Rows : loop
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

         Len := Len + Max + Spaces;

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

            Line.Append (Align * Space);
            Line.Append (Words (Idx));

            Align := Cmax (Col) - Words (Idx).Length + Spaces;
         end loop;

         Put_Line (Line.To_String);
      end loop;
   end;
end GNATCOLL.Format_Columns_Vertical_Impl;
