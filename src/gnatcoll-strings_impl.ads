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
--    Faster than Unbounded_String
--  * More extensive interface.
--    For instance, you can use X(1) to get the first character of the string.
--    The index always starts at 1, just like unbounded strings.
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

   type Optimal_String_Size is
      mod (System.Word_Size + 2 * String_Size'Size) / 8;
   for Optimal_String_Size'Size use 8;
   --  Type used to instantiate GNATCOLL.Strings
   --  Ideal size is 11 bytes on 32 bits system or 15 on 64 bits systems),
   --  so that a small string (stored without memory allocation) takes the
   --  same size as a big string (not counting the size of the allocated
   --  memory).

   type Unconstrained_String is new String (1 .. Natural'Last);
   type Unconstrained_String_Access is access all Unconstrained_String;
   pragma Suppress_Initialization (Unconstrained_String);
   pragma No_Strict_Aliasing (Unconstrained_String_Access);
   for Unconstrained_String_Access'Storage_Size use 0;
   --  Type used to obtain a string access to a given address.
   --  Initialization is suppressed to handle pragma Normalize_Scalars.
   --  No variable of this type can be declared. It is only used via an access
   --  type (The storage size clause ensures we do not allocate variables of
   --  this type).
   --  It is the responsibility of the user to check the proper bounds.

   generic
      type SSize is mod <>;
      --  Number of characters that can be stored in the XString object itself,
      --  without requiring memory allocations.
      --  We pass a type as opposed to an actual number of character because we
      --  need to apply representation clauses based on this type, which cannot
      --  be done if we have the number of characters.
      --  This type must be between 1 and 127.

      Copy_On_Write : Boolean := GNATCOLL.Atomic.Is_Lock_Free;
      --  Whether we only duplicate strings when they are actually modified.
      --  The alternative is to duplicate them every time a xstring is copied
      --  into another. The latter might be faster in some cases (less
      --  contention in multithreading for instance).

   package Strings is
      pragma Compile_Time_Error
         (Natural (SSize'Last) > 2 ** 7, "SSize too large");

      type XString is tagged private with Constant_Indexing  => Get;
      pragma Preelaborable_Initialization (XString);

      Null_XString : constant XString;

      procedure Set (Self : in out XString; Str : String);
      --  Store a string in Self

      procedure Append (Self : in out XString; Str : String);
      procedure Append (Self : in out XString; Char : Character);
      --  Append to the end of Self

      function "=" (Self : XString; Str : String) return Boolean;
      function "=" (Self, Str : XString) return Boolean;
      --  Compare strings

      function Length (Self : XString) return Natural;
      --  The number of characters in the string

      subtype Unconstrained_String_Access is
         GNATCOLL.Strings_Impl.Unconstrained_String_Access;
      procedure Get_String
         (Self : XString;
          S    : out Unconstrained_String_Access;
          L    : out Natural)
         with Inline;
      --  Returns a pointer to the internal string data.
      --  Do not modify the characters in this string, since it could be
      --  shared among multiple strings.
      --  S is only valid as long as Self is not accessed or modified.

      function To_String (Self : XString) return String;
      --  This functions returns the internal string.
      --  As much as possible, you should use Get_String instead, which is
      --  much more efficient. This function requires returning data whose
      --  size is not known statically to the compiler, thus requires using
      --  the secondary stack and copying the string. This can have significant
      --  performance impact when the string is big.

      function Get (Self : XString; Index : Positive) return Character
         with Inline;
      --  Return the Index-th character of the string.
      --  The index always starts at 1.
      --
      --  raises Ada.Strings.Index_Error if this is not a valid index.
      --  A simpler way to use this function is simply to use indexing:
      --       Self (Index)
      --  as done for a regular Ada string.

      procedure Trim
         (Self : in out XString;
          Side : Ada.Strings.Trim_End := Ada.Strings.Both);
      --  Remove space charactes on either end of the string

   private

      Max_Small_Length : constant String_Size := String_Size (SSize'Last);
      --  Number of bytes in the small_string buffer, as decided by the user.

      type Big_String_Data (Copy_On_Write : Boolean) is limited record
         case Copy_On_Write is
            when False =>
               Bytes1   : Unconstrained_String;
            when True =>
               Refcount : aliased GNATCOLL.Atomic.Atomic_Counter;
               Bytes2   : Unconstrained_String;
         end case;
      end record with Unchecked_Union;
      type Big_String_Data_Access is access all Big_String_Data;
      pragma No_Strict_Aliasing (Big_String_Data_Access);
      --  Unsafe: this is the data used by big strings to store the actual
      --  byte sequence. When we use refcounting, we need to have an explicit
      --  refcount, which is not needed otherwise.

      type Small_String is record
         Is_Big  : Boolean;
         Size    : SSize;
         Data    : String (1 .. Natural (Max_Small_Length));
      end record;
      for Small_String use record
         Is_Big  at 0 range 0 .. 0;
         Size    at 0 range 1 .. 7;
      end record;
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
      end record;
      for Big_String use record
         Is_Big        at 0 range 0 .. 0;
         Half_Capacity at 0 range 1 .. 31;
         Size          at 4 range 0 .. 31;
      end record;
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
   --    startswith  "in"       count      isalpha      isprintable lower
   --    endswith    format     encode     isdecimal    isspace     lstrip
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
