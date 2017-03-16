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

--  Comparing with other string types
--  =================================

--  Ada provides several kinds of strings:
--  * A String
--    is a fixed-width string, but very fast in general. Functions returning
--    a String must do so on the secondary stack, which might be slow.
--  * A Bounded_String
--    has a known maximal size, but can represent any string smaller than this.
--    It also doesn't do any memory allocation, and therefore is fast. You
--    need one instance of the package for each size of bounded_string. This
--    is the type to use if your coding standard restricts memory allocations.
--  * An Unbounded_String
--    is a string of any size, which automatically allocates more memory when
--    needed. They are very flexible, but not very efficient.

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
--    See the encodings section below.
--  * Faster substrings
--    When using copy-on-write, returning a substring does not require any
--    copy of the data or memory allocation. This makes the operation much
--    faster, in particular for operations that return lots of substrings,
--    like Split.
--  * Easy iteration
--    It is possible to use a "for..of" loop to iterate on all valid indexes
--    or on all characters, with a speed similar to what is done for a String.

--  Task safety
--  ===========

--  Like Unbounded_String, a XString should not be accessed unprotected from
--  several different tasks. We do not use locks for maximum efficiency.

--  However, it is safe to pass a copy of a string to another thread, even when
--  they share some data internally. The sharing is an implementation detail,
--  and therefore properly encapsulated in this package.  This package does not
--  use locks internally, but atomic operations.

--  So the following is invalid:
--
--             Thread 1        |       Thread 2
--       S.Set ("...");        |
--       S.Append ("...");     |  S.Append ("...");  --  invalid
--       S.Append ("...");     |  S.To_String;       --  also invalid
--
--  But the following is valid:
--
--             Thread 1        |      Thread 2
--       S.Set ("...");        |
--                             |  lock; S2 := S; unlock;
--                             |
--       S.Append ("...");     |  S2.Append ("...");
--            at this point, they no longer share data, in fact

--  Memory management
--  =================

--  A major design decision for this package is that it does as little memory
--  allocations as possible. Therefore for small strings, it does none. When
--  the string grows, it ends up allocating a buffer, whose size will be
--  increased when the string keeps growing.  The growth strategy attempts a
--  balance between speed and memory usage, so it will allocate more memory
--  than strictly needed for the string, in case it eventually grows.  See the
--  Shrink subprogram if you need to restrict memory usage.

--  When this package needs to return a copy of a string, or a substring,
--  it has two possible strategies:

--      * Either it tries to share the already allocated memory. As explained
--        above, this is done in a thread-safe manner, and thus has a small
--        performance penalty. On the other hand, it doesn't need to copy
--        characters around, so it might be faster.
--        As soon as you modify a string, the buffer can no longer be shared,
--        and a copy is created (thus the term "copy on write").

--      * Or, if you disabled copy on write, it systematically allocates new
--        memory when you do a copy or take a substring. This removes the
--        need for atomic operations, and might be more efficient in heavily
--        multi-threaded applications.

--  We recommend doing actual performance measurement to decide which strategy
--  to use.

--  When you use this package, you could be seeing memory leaks when your
--  program exits. This is because, like unbounded_string, it uses the pragma
--  Finalize_Storage_Only which means that GNAT can skip the calls to free
--  memory on exit, for performance reasons. It should not have memory leaks
--  the rest of the time, though.

--  Indexing
--  =========

--  As opposed to what is done for standard Ada strings, all indexing always
--  start at index 1. Even if you take a substring from indices 5 to 6, for
--  instance, the resulting substring's first character is at index 1.

--  This is both by design (for a lot of users, it is confusing to remember to
--  use the proper indexes 'First and 'Last with standard strings), but also is
--  needed because the internal buffer can be shared.  For instance, when you
--  take a substring as above, the internal buffer is shared (so we are really
--  looking at characters 5 through 6 in this shared buffer). But as soon as
--  you modify the substring, for instance by appending some data, this package
--  needs to reallocate memory. In this case, it will move characters around,
--  and we will be looking at actual characters 1 through 2 in the internal
--  buffer.

--  Always using "1" as the user-visible index for the first character ensures
--  that any internal reallocation or move of data is transparent to the user.

--  Unicode, character encodings,...
--  ================================

--  When you need to manipulate characters outside of the ASCII character set
--  (for instance accented letters, or Chinese symbols), things get more
--  complicated.

--  The unicode body assigned unique code points to all possible characters
--  in use on Earth. These code points are 16 bits numbers. Ada provides
--  various types to deal with code points:

--     * A Character only represents code points 0 through 255.
--       It is typically used to represent the Latin1 subset of unicode,
--       i.e. western Europe characters.
--
--     * A Wide_Character can represent mode Unicode code points, but
--       requires twice as much memory to represent
--
--     * A Wide_Wide_Character can represent all characters, but is even
--       larger.

--  There exist other character sets than Unicode, which usually predate it.
--  They associate different character with codepoints. For instance, in the
--  Latin1 charset, the code point 192 is "a grave"). But in latin5, it is a
--  Russian letter. In Latin1, that Russian character cannot be represented.

--  So to represent a character on the screen, we have to know its code point,
--  but also its character set. The simplest, in terms of programming, it to
--  always convert the input string from a known charset (say latin5) to
--  unicode internally, so that we can always compare codepoints easily in the
--  code, without the need for conversions all over the place.

--  These code points (integers) need to be converted to strings so that we can
--  display them, store them in files, input them,... Here, there also exists
--  various ways to do that, called encodings:
--
--      * Historical charset always have codepoints between 0 and 255 (and
--        of course these are only a subset of all characters one might use
--        in the world). So we simply use a series of bytes to represent them.
--        In Ada, we would use a String for that purpose. But then, as stated
--        above, this means that to compare two strings we have to know that
--        they use the same charset.
--
--      * Unicode defines a UTF-8 encoding. It can represent any codepoint,
--        including greater than 255, but uses a variable number of bytes for
--        code points. This is efficient in terms of storage (most of the
--        time the characters only use a single byte), but costly in terms
--        of manipulation since we have to make sure not to cut the string
--        between two bytes of a multi-byte character.
--
--      * Unicode also defines UTF-16, which has two variants, depending on
--        whether the most significant byte goes first or not. Code points
--        are represented on two bytes, though sometimes they will need a
--        bit more for some rare codepoints.
--
--      * Unicode finally defined UTF-32 (also with two variants), where
--        codepoints are always represented as 4 bytes.

--  As seen above, this is a very complex topic, and manipulating strings with
--  different encodings, or comparing strings with different charsets becomes
--  complex and costly in terms of performance.

--  As a result, the choice taken in this package is to always store decoded
--  strings (i.e. we don't use utf-8, utf-16 or anything else, but just store
--  an array of code points). The Character_Type formal parameter can be used
--  to chose the range of code points you want to be able to represent.

--  Likewise, we always assume these are Unicode code points.

--  So the workflow is the following:
--
--     * Get an input string (from the user, a database,...), with a
--       known charset and encoding.
--       We call such a string a byte sequence since it cannot be
--       interpreted correctly without information on the charset and
--       encoding.
--
--     * Decode this byte sequence into an XString
--
--     * Manipulate the XString using any of the subprograms in this API.
--
--     * If you need to output the string (to the user, into a file,...)
--       encode it with an appropriate charset and encoding.

private with Ada.Finalization;
with Ada.Containers;
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
   --  Ideal size is 19 bytes on 32 bits system or 23 on 64 bits systems),
   --  so that a small string (stored without memory allocation) takes the
   --  same size as a big string (not counting the size of the allocated
   --  memory).

   function Default_Growth
      (Current, Min_Size : String_Size) return String_Size;
   --  The default growth strategy. This decides how much memory we should
   --  allocate/reallocate for the internal buffer, based on the amount of
   --  memory we already use (Current), and the minimal number characters
   --  we need to store in the string.
   --  Current and Min_Size are given in 'characters' not in 'bytes', since
   --  a character could potentially take several bytes.

   generic
      type SSize is mod <>;
      --  Number of characters that can be stored in the XString object itself,
      --  without requiring memory allocations.
      --  We pass a type as opposed to an actual number of character because we
      --  need to apply representation clauses based on this type, which cannot
      --  be done if we have the number of characters.
      --  This type must be a range between 1 and 127.

      type Character_Type is (<>);
      --  The character type to use. You can use Character for compatibility
      --  with an Ada string, or Wide_Character for better Unicode support,
      --  or possibly other types.

      type Character_String is array (Positive range <>) of Character_Type;
      --  An array of Char_Type, i.e. a string as can be stored on the
      --  stack.

      Space : Character_Type := Character_Type'Val (Character'Pos (' '));
      --  The space character

      with function To_Lower
         (Item : Character_Type) return Character_Type is <>;
      with function To_Upper
         (Item : Character_Type) return Character_Type is <>;
      --  character-specific functions.
      --  In general, you do not need to specify those if you have 'use'd
      --  the package Ada.Characters.Handling or Ada.Wide_Characters.Handling.

      Copy_On_Write : Boolean := GNATCOLL.Atomic.Is_Lock_Free;
      --  Whether we only duplicate strings when they are actually modified.
      --  The alternative is to duplicate them every time a xstring is copied
      --  into another. The latter might be faster in some cases (less
      --  contention in multithreading for instance).

      with function Growth_Strategy
         (Current, Min_Size : String_Size) return String_Size
         is Default_Growth;
      --  See the comment for Default_Growth

   package Strings is
      pragma Compile_Time_Error
         (Natural (SSize'Last) > 2 ** 7, "SSize too large");

      subtype Char_Type is Character_Type;
      subtype Char_String is Character_String;
      --  Local renamings, so that users of the package can use these types.

      type XString is tagged private
      with
         Constant_Indexing  => Get,
         Variable_Indexing  => Reference,
         Iterable           => (First       => First,
                                Next        => Next,
                                Has_Element => Has_Element,
                                Element     => Get);
      pragma Preelaborable_Initialization (XString);

      Null_XString : constant XString;

      type Unconstrained_Char_Array is
         array (1 .. Natural'Last) of aliased Char_Type;
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

      type XString_Array is array (Natural range <>) of XString;

      ----------------
      -- Properties --
      ----------------

      function Length (Self : XString) return Natural;
      --  The number of characters in the string

      function Is_Empty (Self : XString) return Boolean
         is (Self.Length = 0);
      --  Whether the string is empty.

      function Get (Self : XString; Index : Positive) return Char_Type
         with Inline;
      --  Return the Index-th character of the string.
      --  The index always starts at 1.
      --
      --  raises Ada.Strings.Index_Error if this is not a valid index.
      --  A simpler way to use this function is simply to use indexing:
      --       Self (Index)
      --  as done for a regular Ada string.

      type Character_Reference (Char : not null access Char_Type)
         is limited private
         with Implicit_Dereference => Char;
      --  A type through which we can modify a character of a string.
      --  It is made limited to make it harder to keep such a reference and
      --  pass it as parameter, for instance.
      --  Such a reference becomes invalid as soon as the contents of the
      --  string is modified, and could potentially reference freed memory.

      function Reference
         (Self  : aliased in out XString;
          Index : Positive) return Character_Reference
         with Inline;
      --  Returns a reference to a specific character in the string.
      --  It is possible to change the contents of the string via this
      --  function. It is meant to be used implicitly as in:
      --      Self (Index) := 'A';
      --
      --  This makes Self unshareable, so that if you later do:
      --      S2 := Self;
      --  then S2 will have to make a copy of the string even when using
      --  copy-on-write.

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

      function To_XString (Str : Char_String) return XString;
      --  Same as creating a temporary function and Set-ing its value.
      --  This is less efficient that Set and results in more copies.

      procedure Append (Self : in out XString; Str : Char_String);
      procedure Append (Self : in out XString; Char : Char_Type);
      procedure Append (Self : in out XString; Str : XString);
      --  Append to the end of Self.

      function "*" (Count : Natural; Right : Char_Type) return XString;
      function "*" (Count : Natural; Right : Char_String) return XString;
      function "*" (Count : Natural; Right : XString) return XString;
      --  Build a new string that duplicates the Right parameter Count times

      procedure Reserve (Self : in out XString; Capacity : String_Size);
      --  Make sure Self has enough storage to contain a string of length
      --  Size. This doesn't impact the current value of Self, so if the
      --  current length is greater than Size, nothing is done.
      --  More memory could be allocated, for performance reasons.

      procedure Shrink (Self : in out XString);
      --  Shrinks the memory used by Self to the minimum needed. This will
      --  likely require some memory allocation and copying the characters.

      procedure Swap (Self, Str : in out XString);
      --  Swap the contents of the two strings.
      --  This is more efficient than using an intermediate variable, and
      --  is often useful in various algorithms.

      ------------------------
      -- Justifying strings --
      ------------------------

      procedure Center
         (Self  : in out XString;
          Width : Positive;
          Pad   : Char_Type := Space);
      function Center
         (Self  : XString;
          Width : Positive;
          Pad   : Char_Type := Space) return XString;
      --  Center Self, and surround it with Pad characters, such that the
      --  total width is Width.
      --  If Self is longer than Width, it is unmodified (so the result
      --  could be longer than Width, use Head if you want to make sure
      --  this isn't the case).
      --  The function is not efficient since it needs to allocate memory
      --  and copy the characters.

      procedure Left_Justify
         (Self  : in out XString;
          Width : Positive;
          Pad   : Char_Type := Space);
      function Left_Justify
         (Self  : XString;
          Width : Positive;
          Pad   : Char_Type := Space) return XString;
      --  Add Pad characters at the end of Self, so that the resulting
      --  string is of length Width.
      --  If Self is longer than Width, it is returned as is.
      --  The function is not efficient since it needs to allocate memory
      --  and copy the characters.

      procedure Right_Justify
         (Self  : in out XString;
          Width : Positive;
          Pad   : Char_Type := Space);
      function Right_Justify
         (Self  : XString;
          Width : Positive;
          Pad   : Char_Type := Space) return XString;
      --  Add Pad characters at the beginning of Self, so that the resulting
      --  string is of length Width.
      --  If Self is longer than Width, it is returned as is.
      --  The function is not efficient since it needs to allocate memory
      --  and copy the characters.

      ---------------
      -- Comparing --
      ---------------
      --   ??? Some operators are commented out because of limitations in
      --   AJIS.

      function "=" (Left : XString;      Right : Char_String) return Boolean;
      function "=" (Left : XString;      Right : XString) return Boolean;
      function "=" (Left : Char_String;  Right : XString) return Boolean
         is (Right = Left);

      function "<" (Left : XString;      Right : Char_String) return Boolean;
      function "<" (Left : Char_String;  Right : XString) return Boolean;
      function "<" (Left : XString;      Right : XString) return Boolean;

      function "<=" (Left : XString;     Right : Char_String) return Boolean;
      function "<=" (Left : Char_String; Right : XString) return Boolean;
      function "<=" (Left : XString;     Right : XString) return Boolean;

      function ">" (Left : XString;      Right : Char_String) return Boolean
         is (not (Left <= Right));
      function ">" (Left : Char_String;  Right : XString) return Boolean
         is (not (Left <= Right));
      function ">" (Left : XString;      Right : XString) return Boolean
         is (not (Left <= Right));

      function ">=" (Left : XString;     Right : Char_String) return Boolean
         is (not (Left < Right));
      function ">=" (Left : Char_String; Right : XString) return Boolean
         is (not (Left < Right));
      function ">=" (Left : XString;     Right : XString) return Boolean
         is (not (Left < Right));
      --  Compare strings

      subtype Compare_Result is Integer range -1 .. 1;
      function Compare
         (Left : XString;     Right : Char_String) return Compare_Result;
      function Compare
         (Left : XString;     Right : XString) return Compare_Result
         with Inline;
      function Compare
         (Left : Char_String; Right : XString) return Compare_Result
         is (-Compare (Right, Left));
      --  Compare two strings.
      --  If they are equal, returns 0.
      --  If Left is before Right in lexicographical order, return -1.
      --  If Left is after Right in lexicographical order, return 1.
      --  The standard operators above are not defined in terms of Compare
      --  because the compiler is sometimes able to generate more efficient
      --  code for them.

      function Compare_Case_Insensitive
         (Left : XString;     Right : Char_String) return Compare_Result;
      function Compare_Case_Insensitive
         (Left : XString;     Right : XString) return Compare_Result
         with Inline;
      function Compare_Case_Insensitive
         (Left : Char_String; Right : XString) return Compare_Result
         is (-Compare_Case_Insensitive (Right, Left));
      --  Same as above, but ignore casing differences

      function Equal_Case_Insensitive
         (Left : XString;     Right : Char_String) return Boolean
         is (Compare_Case_Insensitive (Left, Right) = 0);
      function Equal_Case_Insensitive
         (Left : XString;     Right : XString) return Boolean
         is (Compare_Case_Insensitive (Left, Right) = 0);
      function Equal_Case_Insensitive
         (Left : Char_String; Right : XString) return Boolean
         is (Compare_Case_Insensitive (Left, Right) = 0);

      function Less_Case_Insensitive
         (Left : XString;     Right : Char_String) return Boolean
         is (Compare_Case_Insensitive (Left, Right) = -1);
      function Less_Case_Insensitive
         (Left : XString;     Right : XString) return Boolean
         is (Compare_Case_Insensitive (Left, Right) = -1);
      function Less_Case_Insensitive
         (Left : Char_String; Right : XString) return Boolean
         is (Compare_Case_Insensitive (Left, Right) = -1);

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
      -- Hashing --
      -------------

      function Hash (Self : XString) return Ada.Containers.Hash_Type;
      --  Return a hash value suitable for the standard containers map.
      --  This is not a cryptographica hash.

      function Hash_Case_Insensitive
         (Self : XString) return Ada.Containers.Hash_Type;
      --  Same as above, but ignore casing

      ------------
      -- Casing --
      ------------

      procedure To_Upper (Self : in out XString);
      function To_Upper (Self : XString) return XString;
      --  Convert all characters of Self to upper case, using the formal
      --  parameter To_Upper.

      procedure To_Lower (Self : in out XString);
      function To_Lower (Self : XString) return XString;
      --  Convert all characters of Self to lower case, using the formal
      --  parameter To_Lower.

      procedure Capitalize (Self : in out XString);
      --  Make sure the first letter of Self is upper cased.
      --  All other characters are lower cased.

      procedure Title (Self : in out XString);
      --  The first letter and all letters after a space are upper cased,
      --  and all other characters are lower cased.

      function Is_Upper (Self : XString) return Boolean;
      function Is_Lower (Self : XString) return Boolean;
      --  True if all characters in Self are upper or lower cased

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

      ---------------
      -- Searching --
      ---------------
      --  These functions do not use advanced algorithms like Boyer-Moore,
      --  so do not take take advantage of the pattern to optimize the
      --  search.
      --  See also GNATCOLL.Boyer_Moore for more advanced algorithms

      function Count
         (Self : XString;
          Char : Char_Type;
          Low  : Positive := 1;
          High : Natural := Natural'Last) return Natural;
      function Count
         (Self : XString;
          Str  : Char_String;
          Low  : Positive := 1;
          High : Natural := Natural'Last) return Natural;
      --  Return the number of non-overlapping occurrences of Char or Str.
      --  If Str is the empty string, returns Natural'Last (infinite).
      --  The search is done in the substring Low..High (by default the
      --  whole string).
      --  Index_Error is raised if Low is not a valid index (unless Self
      --  is the empty string).

      function Find
         (Self : XString;
          Char : Char_Type;
          Low  : Positive := 1;
          High : Natural := Natural'Last) return Natural;
      function Find
         (Self : XString;
          Str  : Char_String;
          Low  : Positive := 1;
          High : Natural := Natural'Last) return Natural;
      function Right_Find
         (Self : XString;
          Char : Char_Type;
          Low  : Positive := 1;
          High : Natural := Natural'Last) return Natural;
      function Right_Find
         (Self : XString;
          Str  : Char_String;
          Low  : Positive := 1;
          High : Natural := Natural'Last) return Natural;
      --  Return the index of the first occurrence of Char or Str,
      --  in the substring Self(Low..High).
      --  Index_Error is raised if Low is not a valid index (unless Self
      --  is the empty string).
      --
      --  The Right_Find functions start searching from the right of
      --  Self.
      --
      --  0 is returned when no match was found or Str is the empty string.

      ----------------
      -- Substrings --
      ----------------
      --  The following subprograms return a substring of Self, based on
      --  various criteria.
      --
      --  When using copy-on-write, these subprograms will share the storage
      --  of Self, and thus will not require new memory allocation. This
      --  makes them fast.
      --  When not using copy-on-write, however, they require copy of the
      --  characters and memory allocations. The functions are even more
      --  expensive, since they require additional copies, so we recommend
      --  using the procedures instead.
      --
      --  All returned substrings always start at index 1, even if you took
      --  a slice from another index on.

      procedure Slice
         (Self  : in out XString; Low : Positive; High : Natural);
      function Slice
         (Self : XString; Low : Positive; High : Natural) return XString;
      procedure Slice
         (Self : XString;
          Low  : Positive;
          High : Natural;
          Into : in out XString);
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

      function Split
         (Self       : XString;
          Sep        : Char_Type;
          Max_Split  : Positive := Positive'Last;
          Omit_Empty : Boolean := False) return XString_Array;
      procedure Split
         (Self       : XString;
          Sep        : Char_Type;
          Omit_Empty : Boolean := False;
          Into       : out XString_Array;
          Last       : out Natural);
      function Split
         (Self       : XString;
          Sep        : Char_String;
          Max_Split  : Positive := Positive'Last;
          Omit_Empty : Boolean := False) return XString_Array;
      procedure Split
         (Self       : XString;
          Sep        : Char_String;
          Omit_Empty : Boolean := False;
          Into       : out XString_Array;
          Last       : out Natural);
      --  Split self into chunks, on every occurrence of Sep.
      --
      --  The procedure is faster since it does fewer copies of the strings,
      --  in particular when not using Copy-On-Write. Only elements from
      --  Into'First .. Last have been set or modified. Others are left
      --  untouched.
      --
      --  If Max_Split is specified, at most that many substrings are
      --  returned, and the last one extends till the end of Self.
      --  The procedure uses the size of Into has the maximum number of
      --  splits that are allowed.
      --  Specifying a Max_Split is more efficient, since otherwise these
      --  subprograms need to count the number of times splitting is
      --  necessary.
      --
      --  If Omit_Empty is true, then none of the returned substring will
      --  be the empty string.
      --
      --  For instance, if Self = "1,,2,3,,4", then:
      --     * Sep=',' =>   ["1", "2", "", "3", "", "4"]
      --     * Sep=',' and Omit_Empty=True => ["1", "2", "3", "4"]
      --     * Sep=',' and Max_Split=3  => ["1", "2", "3,,4"]
      --  As another example, if you need to split on consecutive whitespaces
      --  you can use  Omit_Empty=True  and Sep=' ', for instance:
      --      "  2   3  4"  => ["2", "3", "4"]
      --
      --  Splitting an empty string will return an empty array.
      --  Splitting on an empty Sep has the same effect.

      function Right_Split
         (Self       : XString;
          Sep        : Char_Type;
          Max_Split  : Positive := Positive'Last;
          Omit_Empty : Boolean := False) return XString_Array;
      procedure Right_Split
         (Self       : XString;
          Sep        : Char_Type;
          Omit_Empty : Boolean := False;
          Into       : out XString_Array;
          Last       : out Natural);
      function Right_Split
         (Self       : XString;
          Sep        : Char_String;
          Max_Split  : Positive := Positive'Last;
          Omit_Empty : Boolean := False) return XString_Array;
      procedure Right_Split
         (Self       : XString;
          Sep        : Char_String;
          Omit_Empty : Boolean := False;
          Into       : out XString_Array;
          Last       : out Natural);
      --  Same as Split, but starting from the right.
      --  The substrings are returned in the reverse order, from right to
      --  left in Self.

      procedure Join
         (Self      : out XString;
          Sep       : Char_String;
          Items     : XString_Array);
      function Join
         (Sep       : Char_String;
          Items     : XString_Array) return XString;
      procedure Join
         (Self      : out XString;
          Sep       : Char_Type;
          Items     : XString_Array);
      function Join
         (Sep       : Char_Type;
          Items     : XString_Array) return XString;
      --  Return a string that contains all elements from Items, separated
      --  by Self.
      --  The function versions are less efficient (more so when not using
      --  copy-on-write).

      ---------------
      -- Modifying --
      ---------------

      procedure Replace
         (Self : in out XString; Index : Positive; Char : Char_Type);
      --  Replace a specific character in the string.
      --  Index_Error is raised if the index is invalid.

      procedure Replace
         (Self      : in out XString;
          Low       : Positive;
          High      : Natural;
          By        : Char_String);
      procedure Replace_Slice
         (Self      : in out XString;
          Low       : Positive;
          High      : Natural;
          By        : XString) with Inline;
      --  Replace the substring Low..High with By.
      --  Low must be a valid index (but High might be larger than the
      --  string's length).
      --  If High < Low, this is the equivalent of inserting the new
      --  string at position Low.

      procedure Insert
         (Self      : in out XString;
          Before    : Positive;
          New_Item  : Char_String) with Inline;
      procedure Insert
         (Self      : in out XString;
          Before    : Positive;
          New_Item  : XString) with Inline;
      --  Insert the new item at the given position in Self.

      procedure Overwrite
         (Self      : in out XString;
          Position  : Positive;
          New_Item  : Char_String) with Inline;
      procedure Overwrite
         (Self      : in out XString;
          Position  : Positive;
          New_Item  : XString) with Inline;
      --  Replace the substring at the given Position with the new
      --  item. If Self is longer, characters after are preserved.

      procedure Delete
         (Self      : in out XString;
          Low       : Positive;
          High      : Natural)
         with Inline;
      --  Delete the substring Low..High.
      --  Both indexes must be valid.

      procedure Clear (Self : in out XString);
      --  Reset the contents of Self, and frees all allocated memory.
      --  You do not need to call this procedure in general, since memory
      --  is handled automatically. For instance, when Self goes out of
      --  scope, memory is freed.
      --  In general, it is more efficient to call Set on the string without
      --  calling Clear first, since GNATCOLL will be able to reuse already
      --  allocated memory in such a case.

   private

      Max_Small_Length : constant String_Size := String_Size (SSize'Last);
      --  Number of bytes in the small_string buffer, as decided by the user.

      type Character_Reference (Char : not null access Char_Type)
         is null record;

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

      pragma Stream_Convert (XString, To_XString, To_String);
      --  provide stream routines without dragging in Ada.Streams

      Null_XString : constant XString :=
         (Ada.Finalization.Controlled with
          Data => (Is_Big => False, Small => <>));

   end Strings;

   --  Unbounded strings have:
   --     Index_Non_Blank  Find_Token    Translate
   --     Ada.Strings.UTF_Encoding
   --  Can we reorganize to share code between various instances that
   --  use the same Char_Type and Char_String ?

   --  C++ has:
   --    rfind              find_first_of    find_last_of
   --    find_first_not_of  find_last_not_of

   --  Python adds:
   --    "in"        isalpha      isprintable
   --    format      isdecimal    isspace     partition
   --    splitlines  expandtabs   isdigit     istitle
   --    zfill       isidentifier isalnum     swapcase
   --    casefold    isnumeric

end GNATCOLL.Strings_Impl;
