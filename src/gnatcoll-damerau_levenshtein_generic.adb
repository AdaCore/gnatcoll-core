------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

function GNATCOLL.Damerau_Levenshtein_Generic
  (Left, Right : Array_Type) return Natural
is
   L : Array_Type renames Left;
   R : Array_Type renames Right;
   D : array (L'First - 1 .. L'Last, R'First - 1 .. R'Last) of Natural;
begin
   for I in D'Range (1) loop
      D (I, D'First (2)) := Natural (I);
   end loop;

   for I in D'Range (2) loop
      D (D'First (1), I) := Natural (I);
   end loop;

   for J in R'Range loop
      for I in L'Range loop
         D (I, J) :=
           Natural'Min
             (Natural'Min (D (I - 1, J), D (I, J - 1)) + 1,
              D (I - 1, J - 1) + (if L (I) = R (J) then 0 else 1));

         if J > R'First and then I > L'First
           and then R (J) = L (I - 1) and then R (J - 1) = L (I)
         then
            D (I, J) := Natural'Min (D (I, J), D (I - 2, J - 2) + 1);
         end if;
      end loop;
   end loop;

   return D (L'Last, R'Last);
end GNATCOLL.Damerau_Levenshtein_Generic;
