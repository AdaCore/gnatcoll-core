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

--  A symbol table.
--  Similar strings are always represented with the same pointer, thus
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
   No_Symbol        : constant Symbol;
   Empty_String     : constant Symbol;

   function Allocate return Symbol_Table_Access;
   --  Allocate a new symbol table.

   function Find
     (Table : access Symbol_Table_Record;
      Str   : String) return Symbol;
   --  Return the internal version of Str.
   --  Comparing Symbol is the same as comparing the string itself, but much
   --  faster.

   function Get (Sym : Symbol) return Cst_String_Access;
   pragma Inline_Always (Get);
   --  The string associated with the symbol.
   --  The returned string must not be deallocated, it points to internal data

   procedure Free (Table : in out Symbol_Table_Record);
   procedure Free (Table : in out Symbol_Table_Access);
   --  Free the table

   function Hash (S : Symbol) return Ada.Containers.Hash_Type;
   --  Returns a hash for the symbol, in case you need to create your own
   --  htables

   function Debug_Print (S : Symbol) return String;
   --  Return a displaying version of symbol (debugging purposes only)

   procedure Display_Stats (Self : access Symbol_Table_Record);
   --  Display statistics about the table.
   --  This is meant for debug purposes only, and the output might change from
   --  one version to the next.

private
   type Symbol is new Cst_String_Access;

   Cst_Empty_String     : aliased constant String := "";

   No_Symbol        : constant Symbol := null;
   Empty_String     : constant Symbol := Cst_Empty_String'Access;

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
   end record;

end GNATCOLL.Symbols;
