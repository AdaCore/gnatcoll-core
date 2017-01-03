------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2017, AdaCore                     --
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

--  A symbol table

--  Equal strings are always represented with the same pointer, thus
--  reducing the amount of memory to store multiple instances of the same
--  string, and speeding up comparison (since you only need to compare the
--  pointer, not the string itself).

with Ada.Containers.Hashed_Sets;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

package GNATCOLL.Symbols is

   type Symbol_Table_Record (<>) is tagged private;
   type Symbol_Table_Access is access all Symbol_Table_Record'Class;
   --  A symbol table associating integers with strings.
   --  By default, this is not task safe, so you will need to extend this if
   --  the symbol is to be shared between multiple tasks.

   type Symbol is private;
   No_Symbol    : constant Symbol;
   Empty_String : constant Symbol;

   function Allocate return Symbol_Table_Access;
   --  Allocate a new symbol table

   function Find
     (Table : access Symbol_Table_Record;
      Str   : String) return Symbol;
   --  Return the internal version of Str.
   --  Comparing Symbol is the same as comparing the string itself, but much
   --  faster.

   function Get
      (Sym : Symbol; Empty_If_Null : Boolean := True)
      return Cst_String_Access;
   pragma Inline_Always (Get);
   --  The string associated with the symbol.
   --  The returned string must not be deallocated, it points to internal data.
   --  For No_Symbol, this returns null or the empty string, depending on
   --  Empty_If_Null.

   procedure Free (Table : in out Symbol_Table_Record);
   procedure Free (Table : in out Symbol_Table_Access);
   --  Free the table

   function Hash (S : Symbol) return Ada.Containers.Hash_Type;
   --  Returns a hash for the symbol, in case you need to create your own
   --  hash tables.

   function Debug_Print (S : Symbol) return String;
   --  Return a displaying version of symbol (debugging purposes only)

   procedure Display_Stats (Self : access Symbol_Table_Record);
   --  Display statistics about the table.
   --  This is meant for debug purposes only, and the output might change from
   --  one version to the next.

private

   type Symbol is new Cst_String_Access;

   Cst_Empty_String : aliased constant String := "";

   No_Symbol    : constant Symbol := null;
   Empty_String : constant Symbol := Cst_Empty_String'Access;

   function Hash (Str : Cst_String_Access) return Ada.Containers.Hash_Type;
   function Key_Equal (Key1, Key2 : Cst_String_Access) return Boolean;
   pragma Inline (Hash, Key_Equal);

   package String_Htable is new Ada.Containers.Hashed_Sets
     (Element_Type        => Cst_String_Access,
      Hash                => Hash,
      Equivalent_Elements => Key_Equal,
      "="                 => "=");

   type Symbol_Table_Record is tagged record
      Hash    : String_Htable.Set;

      Calls_To_Find : Natural := 0;
      Total_Size    : Long_Long_Integer := 0;
      Size_Saved    : Long_Long_Integer := 0;
   end record;

end GNATCOLL.Symbols;
