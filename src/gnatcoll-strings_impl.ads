------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2017, AdaCore                          --
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

--  This package provides the implementation for XString.
--  Ada provides several kinds of strings:
--  * A String
--    is a fixed-width string, but very fast in general. Functions returning
--    a String must do so on the secondary stack, which might be slow.
--  * A Bounded_String
--    has a known maximal size, but can represent any string smaller than this.
--    It also doesn't do any memory allocation, and therefore is fast. You
--    need one instance of the package for each size of bounded_string.
--  * An Unbounded_String
--    is a string of any size, which automatically allocates more memory when
--    needed. They are very flexible, but not very efficient.
--
--  This package provides a fourth type of string, which tries to combine the
--  advantages of all the above:
--  * Unlimited size of the string, which grows as necessary
--    More flexible than String and Bounded_String.
--  * No memory allocation for small strings (for a certain definition of
--    small, see the SSize parameter below). This means very fast handling of
--    those small strings.
--    Faster than Unbounded_String, both for small strings, as seen above,
--    but also for large strings since one can use the Reserve procedure to
--    preallocate enough space.
--  * More extensive interface.
--    For instance, you can use X(1) to get the first character of the string.
--    The index always starts at 1, just like unbounded strings.
--  * Supports various character types
--  * Faster substrings
--    When using copy-on-write, returning a substring does not require any
--    copy of the data or memory allocation. This makes the operation much
--    faster, in particular for operations that return lots of substrings,
--    like Split.
--  * Easy iteration
--    It is possible to use a "for..of" loop to iterate on all valid indexes
--    or on all characters, with a speed similar to what is done for a String.
--
--  Like Unbounded_String, a XString should not be accessed unprotected from
--  several different tasks. We do not use locks for maximum efficiency. At
--  the same time, it is safe to pass a copy to a different thread. The
--  internal shared buffer is protected against concurrent accesses, via a
--  lock-free data structure.
--
--  When you use this package, you could be seeing memory leaks when your
--  program exits. This is because, like unbounded_string, it uses the
--  pragma Finalize_Storage_Only which means that GNAT can skip the calls to
--  free memory on exit, for performance reasons.

private with Ada.Finalization;
with Ada.Strings;
with GNATCOLL.Atomic;
with System;

package GNATCOLL.Strings_Impl is

   type String_Size is mod 2 ** 32;
   --  Internal size used for string sizes. This matches a Natural as used for
   --  standard Ada strings.

   Big_String_Size : constant := 2 * System.Word_Size + 2 * String_Size'Size;
   type Optimal_String_Size is mod (Big_String_Size / 8);
   for Optimal_String_Size'Size use Character'Size;
   --  Type used to instantiate GNATCOLL.Strings
   --  Ideal size is 11 bytes on 32 bits system or 15 on 64 bits systems),
   --  so that a small string (stored without memory allocation) takes the
   --  same size as a big string (not counting the size of the allocated
   --  memory).

   generic
      type SSize is mod <>;
      --  Number of characters that can be stored in the XString object itself,
      --  without requiring memory allocations.
      --  We pass a type as opposed to an actual number of character because we
      --  need to apply representation clauses based on this type, which cannot
      --  be done if we have the number of characters.
      --  This type must be between 1 and 127.

      type Character_Type is (<>);
      --  The character type to use. You can use Character for compatibility
      --  with an Ada string, or Wide_Character for better Unicode support,
      --  or possibly other types.
      --  Strings have no notion of encoding (like UTF-8 for instance).
      --  Instead they store already decoded characters, which is faster
      --  both to compute the length and move to the next character, but
      --  requires more space.

      type Character_String is array (Positive range <>) of Character_Type;
      --  An array of Char_Type, i.e. a string as can be stored on the
      --  stack.

      Space : Character_Type := Character_Type'Val (Character'Pos (' '));
      --  The space character

      Copy_On_Write : Boolean := GNATCOLL.Atomic.Is_Lock_Free;
      --  Whether we only duplicate strings when they are actually modified.
      --  The alternative is to duplicate them every time a xstring is copied
      --  into another. The latter might be faster in some cases (less
      --  contention in multithreading for instance).

   package Strings is
      pragma Compile_Time_Error
         (Natural (SSize'Last) > 2 ** 7, "SSize too large");

      subtype Char_Type is Character_Type;
      subtype Char_String is Character_String;
      --  Local renamings, so that users of the package can use these types.

      type XString is tagged private
      with
         Constant_Indexing  => Get,
         Iterable           => (First       => First,
                                Next        => Next,
                                Has_Element => Has_Element,
                                Element     => Get);
      pragma Preelaborable_Initialization (XString);

      Null_XString : constant XString;

      type Unconstrained_Char_Array is array (1 .. Natural'Last) of Char_Type;
      type Char_Array is access all Unconstrained_Char_Array;
      pragma Suppress_Initialization (Unconstrained_Char_Array);
      pragma No_Strict_Aliasing (Char_Array);
      for Char_Array'Storage_Size use 0;
      --  Type used to obtain a string access to a given address.
      --  Initialization is suppressed to handle pragma Normalize_Scalars.
      --  No variable of this type can be declared. It is only used via an
      --  access type (The storage size clause ensures we do not allocate
      --  variables of this type).
      --  It is the responsibility of the user to check the proper bounds.

      ----------------
      -- Properties --
      ----------------

      function Length (Self : XString) return Natural;
      --  The number of characters in the string

      function Get (Self : XString; Index : Positive) return Char_Type
         with Inline;
      --  Return the Index-th character of the string.
      --  The index always starts at 1.
      --
      --  raises Ada.Strings.Index_Error if this is not a valid index.
      --  A simpler way to use this function is simply to use indexing:
      --       Self (Index)
      --  as done for a regular Ada string.

      --------------------------
      -- Iteration on indexes --
      --------------------------

      type Index_Range is record
         Low, High : Natural;
      end record
         with Iterable => (First       => First,
                           Next        => Next,
                           Has_Element => Has_Element,
                           Element     => Element);
      function First (Self : Index_Range) return Positive is (Self.Low);
      function Next (Self : Index_Range; Index : Positive) return Positive
         is (Index + 1);
      function Has_Element (Self : Index_Range; Index : Positive)
         return Boolean is (Index <= Self.High);
      function Element
         (Self : Index_Range; Index : Positive) return Positive
         is (Index);

      function Iterate (Self : XString) return Index_Range
         is ((Low => 1, High => Self.Length));
      --  Provide an iterator to get all indexes of the string.
      --  This provides a convenient iterator:
      --     for Index of Self.Iterate loop
      --        C := Self (Index);
      --     end loop;
      --  This loop is about as fast as iterating directly on a
      --  String via a 'Range attribute.

      -----------------------------
      -- Iteration on characters --
      -----------------------------

      function First (Self : XString) return Positive is (1);
      function Next (Self : XString; Index : Positive) return Positive
         is (Index + 1);
      function Has_Element (Self : XString; Index : Positive) return Boolean
         is (Index <= Self.Length);
      --  Standard iteration functions.
      --  Each iteration returns the next character in the string.
      --
      --  Although this is better used as
      --     for C of Str loop
      --        null;
      --     end loop;
      --
      --  See the Iterate function if you need to get the indexes instead

      ----------------------
      -- Building strings --
      ----------------------
      --  No operator "&" is provided, for efficiency reasons. Such an
      --  operator would need to create temporary strings which then
      --  need to be freed almost immediately. Since this becomes a slow
      --  operation, this API does not provide it by default.

      procedure Set (Self : in out XString; Str : Char_String);
      --  Store a string in Self

      procedure Append (Self : in out XString; Str : Char_String);
      procedure Append (Self : in out XString; Char : Char_Type);
      procedure Append (Self : in out XString; Str : XString);
      --  Append to the end of Self.

      function "*" (Count : Natural; Right : Char_Type) return XString;
      function "*" (Count : Natural; Right : Char_String) return XString;
      function "*" (Count : Natural; Right : XString) return XString;
      --  Build a new string that duplicates the Right parameter Count times

      procedure Reserve (Self : in out XString; Size : String_Size);
      --  Make sure Self has enough storage to contain a string of length
      --  Size. This doesn't impact the current value of Self, so if the
      --  current length is greater than Size, nothing is done.
      --  More memory could be allocated, for performance reasons.

      procedure Shrink (Self : in out XString);
      --  Shrinks the memory used by Self to the minimum needed. This will
      --  likely require some memory allocation and copying the characters.

      ---------------
      -- Comparing --
      ---------------
      --   ??? Some operators are commented out because of limitations in
      --   AJIS.

      function "=" (Self : XString; Str : Char_String) return Boolean;
      function "=" (Self, Str : XString) return Boolean;
      --  function "=" (Str : Char_String; Self : XString) return Boolean
      --     is (Self = Str);

      function "<" (Self : XString; Str : Char_String) return Boolean;
      --  function "<" (Str : Char_String; Self : XString) return Boolean;
      function "<" (Self, Str : XString) return Boolean;

      function "<=" (Self : XString; Str : Char_String) return Boolean;
      --  function "<=" (Str : Char_String; Self : XString) return Boolean;
      function "<=" (Self, Str : XString) return Boolean;

      function ">" (Self : XString; Str : Char_String) return Boolean
         is (not (Self <= Str));
      --  function ">" (Str : Char_String; Self : XString) return Boolean
      --     is (Self <= Str);
      function ">" (Self, Str : XString) return Boolean
         is (Str <= Self);

      function ">=" (Self : XString; Str : Char_String) return Boolean
         is (not (Self < Str));
      --  function ">=" (Str : Char_String; Self : XString) return Boolean
      --     is (Self < Str);
      function ">=" (Self, Str : XString) return Boolean
         is (Str < Self);
      --  Compare strings

      ----------------
      -- Converting --
      ----------------

      procedure Get_String
         (Self : XString;
          S    : out Char_Array;
          L    : out Natural)
         with Inline;
      --  Returns a pointer to the internal string data.
      --  Do not modify the characters in this string, since it could be
      --  shared among multiple strings.
      --  S is only valid as long as Self is not accessed or modified.

      function To_String (Self : XString) return Char_String;
      --  This functions returns the internal string.
      --  As much as possible, you should use Get_String instead, which is
      --  much more efficient. This function requires returning data whose
      --  size is not known statically to the compiler, thus requires using
      --  the secondary stack and copying the string. This can have significant
      --  performance impact when the string is big.

      -------------
      -- Testing --
      -------------

      function Starts_With
         (Self : XString; Prefix : Char_String) return Boolean;
      function Starts_With (Self : XString; Prefix : XString) return Boolean;
      --  Whether Self starts with the specific prefix.

      function Ends_With
         (Self : XString; Suffix : Char_String) return Boolean;
      function Ends_With (Self : XString; Suffix : XString) return Boolean;
      --  Whether Self ends with the specific suffix.

      ----------------
      -- Substrings --
      ----------------
      --  The following subprograms return a substring of Self, based on
      --  various criteria.
      --
      --  When using copy-on-write, these subprograms will share the storage
      --  of Self, and thus will not require new memory allocation. This
      --  makes them fast.
      --
      --  All returned substrings always start at index 1, even if you took
      --  a slice from another index on.

      procedure Slice (Self  : in out XString; Low, High : Positive);
      function Slice (Self : XString; Low, High : Positive) return XString;
      --  Return a substring of Self.
      --  The first character of Self is always at index 1, so this function
      --  returns a slice from the Low-th character of Self to the High-th
      --  character of Self.
      --
      --  raises Ada.Strings.Index_Error if any of the indexes is invalid.

      procedure Trim
         (Self  : in out XString;
          Side  : Ada.Strings.Trim_End := Ada.Strings.Both;
          Chars : Char_Type := Space);
      function Trim
         (Self  : XString;
          Side  : Ada.Strings.Trim_End := Ada.Strings.Both;
          Chars : Char_Type := Space) return XString;
      --  Remove characters on either end of the string.
      --  All characters equal to Chars are removed from either ends.

      function Head (Self : XString; Count : Natural) return XString;
      --  Return the first Count characters of Self.
      --  If Self is smaller, it is returned as is.

      function Tail (Self : XString; Count : Natural) return XString;
      --  Return the last Count characters of Self.
      --  If Self is smaller, it is returned as is.

   private

      Max_Small_Length : constant String_Size := String_Size (SSize'Last);
      --  Number of bytes in the small_string buffer, as decided by the user.

      type Big_String_Data (Copy_On_Write : Boolean) is limited record
         case Copy_On_Write is
            when False =>
               Bytes1   : Unconstrained_Char_Array;
            when True =>
               Refcount : aliased GNATCOLL.Atomic.Atomic_Counter;
               Bytes2   : Unconstrained_Char_Array;
         end case;
      end record with Unchecked_Union;
      type Big_String_Data_Access is access all Big_String_Data;
      pragma Suppress_Initialization (Big_String_Data);
      pragma No_Strict_Aliasing (Big_String_Data_Access);
      --  Unsafe: this is the data used by big strings to store the actual
      --  byte sequence. When we use refcounting, we need to have an explicit
      --  refcount, which is not needed otherwise.

      type Small_String is record
         Is_Big  : Boolean;
         Size    : SSize;
         Data    : Char_String (1 .. Natural (Max_Small_Length));
      end record;
      for Small_String use record
         Is_Big  at 0 range 0 .. 0;
         Size    at 0 range 1 .. 7;
      end record;
      pragma Suppress_Initialization (Small_String);
      --  Hard-code the fact that we can represent the small size on 7 bits
      --  (the pragma Compile_Time_Error ensures this is the case). Would be
      --  nice if we could use "15" of "7" for larger small string, but we
      --  would need static constants for this, and generic formal are not
      --  (so that compilers can implement shared generics).

      subtype Half_Capacity_Size is String_Size range 0 .. 2 ** 31 - 1;

      type Big_String is record
         Is_Big        : Boolean;
         Half_Capacity : Half_Capacity_Size;
         Size          : String_Size;

         Data          : aliased Big_String_Data_Access;
         --  This field must be aligned on multiple of word_size, so can't
         --  be last.

         First         : Positive;
         --  Index of the first character in data.
         --  This is used to share the data between substrings. When we
         --  do not use copy-on-write, part of the buffer might becomes
         --  useless but this is faster than reallocating and copying.

         --  On 64-bits platforms, we have 32 bits unused here.
      end record;
      for Big_String use record
         Is_Big        at 0  range 0 .. 0;
         Half_Capacity at 0  range 1 .. 31;
         Size          at 4  range 0 .. 31;
         Data          at 8  range 0 .. System.Word_Size - 1;
         First         at 8 + System.Word_Size / 8 range 0 .. 31;
      end record;
      for Big_String'Size use Big_String_Size;
      pragma Suppress_Initialization (Big_String);
      --  Capacity is always an even number, and we store half of it, so that
      --  it leaves one bit for the flag.

      type String_Data (Is_Big : Boolean := False) is record
         case Is_Big is
            when False => Small : Small_String;
            when True  => Big   : Big_String;
         end case;
      end record
      with Unchecked_Union;

      type XString is new Ada.Finalization.Controlled with record
         Data   : String_Data := (Is_Big => False, Small => <>);
      end record;
      overriding procedure Adjust (Self : in out XString);
      overriding procedure Finalize (Self : in out XString);

      pragma Finalize_Storage_Only (XString);
      --  Finalization is only required for freeing storage

      --  pragma Stream_Convert (...);
      --  ??? provide stream routines without dragging in Ada.Streams

      Null_XString : constant XString :=
         (Ada.Finalization.Controlled with
          Data => (Is_Big => False, Small => <>));

   end Strings;

   --  Unbounded strings have:
   --     index         Index_Non_Blank    Count
   --     Find_Token    Translate          Replace_Slice
   --     Insert        Overwrite          Delete
   --     Move          Less_Case_Insensitive
   --     Hash          Hash_Case_Insensitive
   --     Equal_Case_Insensitive
   --     Ada.Strings.UTF_Encoding

   --  C++ has:
   --     iterators     size  and length   max_size         resize
   --     capacity      reserve            clear            empty
   --     shrink_to_fit                    at               operator+=
   --     append        push_bask          assign           insert
   --     erase         replace            swap             pop_back
   --     find          rfind              find_first_of    find_last_of
   --     find_first_not_of                find_last_not_of substr
   --     compare

   --  Python adds:
   --                "in"       count      isalpha      isprintable lower
   --                format                isdecimal    isspace     lstrip
   --    splitlines  "*"        expandtabs isdigit      istitle     partition
   --    strip       capitalize find       isidentifier isupper     replace
   --    search      casefold   index      islower      join        swapcase
   --    split       center     isalnum    isnumeric    ljust       title
   --    upper       zfill

   --  Support for ASCII, UTF8, UTF16 and UTF32
   --  Support for small string optimization. Perhaps the user should be able
   --    to specify the size of the internal buffer, so that we can avoid
   --    allocations when string is less than 1000 bytes, but fallback on
   --    malloc otherwise.
   --  Allocate a multiple of page_size for maximum efficiency
   --  Should we have refcounting ? C++ forbids it now because the move
   --    constructor basically makes them useless most of the time.
   --    But in Ada, we only have Adjust, which is called fairly often, so
   --    this would save time. At the cost of some locking (or CAS) so less
   --    scalable with multiple threads (but Ada unbounded_string is already
   --    not thread safe).
   --    Likely disable COW for small strings.
   --  Support for Ada2012 iterators
   --  Should substrings be represented efficiently by sharing the same
   --    buffer ?
   --  Make thread safe (although one given instance only used in one thread)

end GNATCOLL.Strings_Impl;
