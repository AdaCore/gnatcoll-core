------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

--  This package implements the Boyer-Moore algorithm for string searching,
--  as described in the book "Algorithms" by T. Cormen (McGrawHill edts)
--
--  This is a very efficient string searching algorithm, where the key being
--  search is preprocessed to speed up further searches. However, unlike some
--  other search algorithms, the text being searched does not need any
--  pre-processing.

package GNATCOLL.Boyer_Moore is

   type Pattern is private;

   Max_Pattern_Length : constant := Integer'Last;
   --  Maximal length for patterns that can be searched.
   --  Changing this means that patterns will simply use more space.

   procedure Compile
     (Motif          : in out Pattern;
      From_String    : String;
      Case_Sensitive : Boolean := True);
   --  Compile the required tables to match From_String anywhere.
   --  Motif needs to be freed when you are done using it.
   --
   --  Note: A case_sensitive search is always more efficient, and should
   --  be used if you don't specifically need a case insensitive search.

   procedure Free (Motif : in out Pattern);
   --  Free the memory occupied by the motif

   function Search (Motif : Pattern; In_String : String) return Integer;
   --  Return the location of the match for Motif in In_String, or -1 if there
   --  is no match;

private
   subtype Offset is Natural range 0 .. Max_Pattern_Length;
   --  This is the maximal offset reported by pattern. This might result in
   --  a slightly less efficient processing for patterns longer than this in
   --  extreme cases, but these are for very rare cases.

   type Occurrence_Array is array (Character) of Offset;
   type Offset_Array is array (Natural range <>) of Offset;
   type Offset_Array_Access is access Offset_Array;
   type String_Access is access String;

   type Pattern is record
      Last_Occurrence : Occurrence_Array;
      Good_Suffix     : Offset_Array_Access;
      Motif           : String_Access;
      Case_Sensitive  : Boolean;
   end record;
end GNATCOLL.Boyer_Moore;
