------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                   Copyright (C) 2019-2020, AdaCore                       --
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

with System;
with GNATCOLL.OS;

package GNATCOLL.String_Builders is

   package OS renames GNATCOLL.OS;

   type String_Builder is limited private;
   --  String_Builder is an efficient unbounded structure to create String
   --  object by aggregation. The structure also maintains a null character at
   --  the end of the String allowing export to C without reallocation.
   --  Instances of String_Builder should be finalized by calling Deallocate
   --  procedure.

   procedure Append (Self : in out String_Builder; Str : String);
   --  Append Str to Self

   procedure Append (Self : in out String_Builder; Char : Character);
   --  Append Char to Self

   procedure Set (Self : in out String_Builder; Str : String);
   --  Reset content of Self to Str

   function Element (Self : String_Builder; N : Positive) return Character
      with Inline;
   --  Return the Nth character of Self

   function Length (Self : String_Builder) return Natural
      with Inline;
   --  Return the length of Self (the size does not take into account
   --  the trailing ASCII.NUL character maintained by the structure).

   function As_String (Self : String_Builder) return String
      with Inline;
   --  Return an Ada String (without the trailing ASCII.NUL)

   function As_C_String (Self : String_Builder) return OS.C_String
      with Inline;
   --  Return a char* pointing to the beginning of Self content

   procedure Deallocate (Self : in out String_Builder)
      with Inline;
   --  Free heap memory associated with Self

   type Static_String_Builder (Size_With_NUL : Natural) is limited private;
   --  Behave the same way as String_Builder except that the maximum
   --  size if known in advance. The structure does not allocate memory
   --  on the heap. Size passed as discriminant should be the maximum size
   --  of the string plus one character for the trailing NUL char.

   procedure Append (Self : in out Static_String_Builder; Str : String)
      with Inline;
   --  Append Str to Self

   procedure Append (Self : in out Static_String_Builder; Char : Character)
      with Inline;
   --  Append Char to Self

   procedure Set (Self : in out Static_String_Builder; Str : String)
      with Inline;
   --  Reset content of Self to Str

   function Element
      (Self : Static_String_Builder; N : Positive)
      return Character
      with Inline;
   --  Return the Nth character of Self

   function Length (Self : Static_String_Builder) return Natural
      with Inline;
   --  Return the length of Self (the size does not take into account
   --  the trailing ASCII.NUL character maintained by the structure).

   function As_String (Self : Static_String_Builder) return String
      with Inline;
   --  Return an Ada String (without the trailing ASCII.NUL)

   function As_C_String (Self : Static_String_Builder) return OS.C_String
      with Inline;
   --  Return a char* pointing to the beginning of Self content

private

   type String_Access is access String;
   type CString is new System.Address;

   Empty_String  : constant String := "" & ASCII.NUL;
   Empty_CString : constant CString := CString (Empty_String (1)'Address);

   type Static_String_Builder (Size_With_NUL : Natural) is limited record
      Str      : String (1 .. Size_With_NUL) := (others => ASCII.NUL);
      Str_Last : Natural := 0;
   end record;

   String_Builder_Short_Size : constant Natural := 43;

   type String_Builder is limited record
      Heap_Str  : String_Access := null;
      Str_Last  : Natural := 0;
      Stack_Str : String (1 .. String_Builder_Short_Size + 1);
   end record;
   --  String_Builder record size is set to use 64 bytes on most systems
   --  (size of L1 cache line on most systems). For 43-bytes long or smaller
   --  strings no allocation on the heap will be done (in that case Stack_Str
   --  is used to store the string). For bigger strings Heap_Str is used.
   --  Str_Last is the index either in Stack_Str or Heap_Str of the last
   --  character in the string.

end GNATCOLL.String_Builders;
