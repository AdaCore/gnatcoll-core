------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                   Copyright (C) 2020-2021, AdaCore                       --
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

--  This package provides WString_Builder and Static_WString_Builder objects
--  that allows to efficiently build Wide_Strings and C compatible wchar **
--  strings.

with GNATCOLL.OS;
with Ada.Strings.UTF_Encoding;

package GNATCOLL.WString_Builders is

   package OS renames GNATCOLL.OS;
   package UTF8 renames Ada.Strings.UTF_Encoding;

   type WString_Builder is limited private;
   --  String_Builder is an efficient unbounded structure to create String
   --  object by aggregation. The structure also maintains a null character at
   --  the end of the String allowing export to C without reallocation.
   --  Instances of WString_Builder should be finalized by calling Deallocate
   --  procedure.

   procedure Append (Self : in out WString_Builder; Str : UTF8.UTF_8_String);
   --  Append Str to Self

   procedure Append (Self : in out WString_Builder; Char : Wide_Character);
   --  Append Char to Self

   procedure Set (Self : in out WString_Builder; Str : UTF8.UTF_8_String);
   --  Reset content of Self to Str

   function Element
      (Self : WString_Builder; N : Positive) return Wide_Character
      with Inline;
   --  Return the Nth character of Self

   function Length (Self : WString_Builder) return Natural
      with Inline;
   --  Return the length of Self (the size does not take into account
   --  the trailing ASCII.NUL character maintained by the structure).

   function As_String (Self : WString_Builder) return Wide_String
      with Inline;
   --  Return an Ada String (without the trailing ASCII.NUL)

   function As_UTF8_String (Self : WString_Builder) return UTF8.UTF_8_String
      with Inline;
   --  Return an Ada String (without the trailing ASCII.NUL)

   function As_C_WString
      (Self          : WString_Builder;
       Null_If_Empty : Boolean := False)
      return OS.C_WString
      with Inline;
   --  Return a wchar* pointing to the beginning of Self content

   procedure Deallocate (Self : in out WString_Builder)
      with Inline;
   --  Free heap memory associated with Self

   type Static_WString_Builder (Size_With_NUL : Natural) is limited private;
   --  Behave the same way as String_Builder except that the maximum
   --  size if known in advance. The structure does not allocate memory
   --  on the heap. Size passed as discriminant should be the maximum size
   --  of the string plus one character for the trailing NUL char.

   procedure Append
      (Self : in out Static_WString_Builder;
       Str  : UTF8.UTF_8_String)
      with Inline;
   --  Append Str to Self

   procedure Append
      (Self : in out Static_WString_Builder;
       Char : Wide_Character)
      with Inline;
   --  Append Char to Self

   procedure Set
      (Self : in out Static_WString_Builder;
       Str  : UTF8.UTF_8_String)
      with Inline;
   --  Reset content of Self to Str

   function Element
      (Self : Static_WString_Builder; N : Positive)
      return Wide_Character
      with Inline;
   --  Return the Nth character of Self

   function Length (Self : Static_WString_Builder) return Natural
      with Inline;
   --  Return the length of Self (the size does not take into account
   --  the trailing NUL character maintained by the structure).

   function As_String (Self : Static_WString_Builder) return Wide_String
      with Inline;
   --  Return an Ada String (without the trailing ASCII.NUL)

   function As_UTF8_String
      (Self : Static_WString_Builder)
      return UTF8.UTF_8_String
      with Inline;
   --  Return an Ada String (without the trailing ASCII.NUL)

   function As_C_WString
      (Self          : Static_WString_Builder;
       Null_If_Empty : Boolean := False)
      return OS.C_WString
      with Inline;
   --  Return a wchar* pointing to the beginning of Self content

private

   type WString_Access is access Wide_String;

   type Static_WString_Builder (Size_With_NUL : Natural) is limited record
      Str      : Wide_String (1 .. Size_With_NUL) :=
        (others => Wide_Character'Val (0));
      Str_Last : Natural := 0;
   end record;

   WString_Builder_Short_Size : constant Natural := 25;

   type WString_Builder is limited record
      Heap_Str  : WString_Access := null;
      Str_Last  : Natural := 0;
      Stack_Str : Wide_String (1 .. WString_Builder_Short_Size + 1);
   end record;
   --  String_Builder record size is set to use 64 bytes on most systems
   --  (size of L1 cache line on most systems). For 43-bytes long or smaller
   --  strings no allocation on the heap will be done (in that case Stack_Str
   --  is used to store the string). For bigger strings Heap_Str is used.
   --  Str_Last is the index either in Stack_Str or Heap_Str of the last
   --  character in the string.

end GNATCOLL.WString_Builders;
