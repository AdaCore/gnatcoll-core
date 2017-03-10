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

with Ada.Strings;                  use Ada.Strings;
with Ada.Unchecked_Conversion;
with GNATCOLL.Atomic;              use GNATCOLL.Atomic;
with GNATCOLL.Refcount;
with System.Memory;                use System.Memory;

pragma Warnings (Off, ".*is an internal GNAT unit");
with System.String_Hash;
pragma Warnings (On, ".*is an internal GNAT unit");

package body GNATCOLL.Strings_Impl is

   Page_Size : constant := 4096;
   --  Memory page size

   package body Strings is
      function Convert is new Ada.Unchecked_Conversion
         (System.Address, Char_Array);
      function Convert is new Ada.Unchecked_Conversion
         (System.Address, Big_String_Data_Access);
      function Convert is new Ada.Unchecked_Conversion
         (Big_String_Data_Access, System.Address);

      Bytes_Per_Char : constant size_t := Char_Type'Size / Character'Size;
      --  Number of bytes for each character in the string.

      Extra_Header_Size : constant System.Memory.size_t :=
         (if Copy_On_Write
          then System.Memory.size_t (GNATCOLL.Atomic.Atomic_Counter'Size / 8)
          else 0);
      --  Extra bytes needed for Big_String_Data in addition to the
      --  byte data stored in Unconstrained_String.

      procedure Store_Capacity (Self : in out XString; Capacity : String_Size)
         with Inline;
      function Get_Capacity (Self : XString) return String_Size
         is (2 * Self.Data.Big.Half_Capacity);
      --  Returns the current capacity of a large string

      procedure Store_Size (Self : in out XString; Size : Natural)
         with Inline;
      --  Store the size of Self.

      function Compute_Capacity
         (Current, Min_Size : String_Size) return String_Size
         with Inline;
      --  Compute the new capacity for a big_string, so that the string has
      --  space for at least Min_Size characters.

      procedure Clone
         (Self   : in out XString;
          Data   : Big_String_Data_Access)
         with Pre => Self.Data.Small.Is_Big, Inline;
      --  Set the big string data, copying from Data.
      --  We copy the data from the parameter and not from Self.Data.Big.Data
      --  because the latter might already have been set to null at that
      --  point.
      --  New memory is allocated.

      procedure Make_Writable_Thread_Safe (Self : in out XString) with Inline;
      procedure Make_Writable_Thread_Unsafe (Self : in out XString)
         with Inline;
      Make_Writable : constant not null
         access procedure (Self : in out XString) :=
            (if GNATCOLL.Refcount.Application_Uses_Tasks
             then Make_Writable_Thread_Safe'Access
             else Make_Writable_Thread_Unsafe'Access);
      --  Make sure we can modify Self (not a shared string)
      --  Two versions are provided: the Unsafe version is faster, but will
      --  fail when a string is read from a thread and written in another one,
      --  as in the following scenario:
      --            thread 1                   |               thread 2
      --    S.Set ("some long long long str"); |
      --    Append (S, "some long long str");  |
      --    --  stops after testing refcount   |
      --                                       | S2 := S;
      --                                       | --  buffer is now shared
      --                                       | Put_Line (S2.To_String);
      --    --  modifies shared buffer         |
      --                                       | Put_Line (S2.To_String);
      --                                       | --  different output

      procedure Convert_To_Big_String
         (Self : in out XString;
          Size : String_Size)
         with Inline, Pre => not Self.Data.Small.Is_Big;
      --  Convert to a big string, by copying the small string data.
      --  We never convert back to a small string afterwards, to benefit from
      --  the memory we already allocated.
      --  This procedure does not copy the actual string, only allocates
      --  memory.
      --  Sets the size of the string

      --------------------
      -- Store_Capacity --
      --------------------

      procedure Store_Capacity
         (Self : in out XString; Capacity : String_Size) is
      begin
         Self.Data.Big.Half_Capacity := Capacity / 2;
      end Store_Capacity;

      -----------------------
      --  Compute_Capacity --
      -----------------------

      function Compute_Capacity
         (Current, Min_Size : String_Size) return String_Size
      is
         --  Compute minimum new size.
         --  1.5 is often considered the best strategy, between efficiency
         --  and memory usage.
         New_Size : constant String_Size :=
            String_Size'Max (Current * 3 / 2, Min_Size);
      begin
         if New_Size > Page_Size then
            --  Round up to the nearest page size, since this is what
            --  the system allocates anyway. This will always lead an even
            --  number.
            return (New_Size / Page_Size + 1) * Page_Size;
         else
            --  Must be an even number
            return New_Size + (New_Size and 1);
         end if;
      end Compute_Capacity;

      ----------------
      -- Store_Size --
      ----------------

      procedure Store_Size (Self : in out XString; Size : Natural) is
      begin
         if Self.Data.Big.Is_Big then
            Self.Data.Big.Size := String_Size (Size);
         else
            Self.Data.Small.Size := SSize (Size);
         end if;
      end Store_Size;

      ---------------------------------
      -- Make_Writable_Thread_Unsafe --
      ---------------------------------

      procedure Make_Writable_Thread_Unsafe (Self : in out XString) is
      begin
         if not Copy_On_Write or else not Self.Data.Small.Is_Big then
            null;   --  nothing to do
         elsif Self.Data.Big.Data.Refcount = 1 then
            null;
         else
            Decrement (Self.Data.Big.Data.Refcount);
            Clone (Self, Self.Data.Big.Data);
         end if;
      end Make_Writable_Thread_Unsafe;

      -------------------------------
      -- Make_Writable_Thread_Safe --
      -------------------------------

      procedure Make_Writable_Thread_Safe (Self : in out XString) is
         Tmp   : Big_String_Data_Access;
      begin
         if not Copy_On_Write or else not Self.Data.Small.Is_Big then
            null;   --  nothing to do
         else
            --  ??? We do not need an atomic sync_bool_compare_and_swap,
            --  since a string is not shared among threads (although the
            --  internal storage might be).
            Tmp := Self.Data.Big.Data;
            Self.Data.Big.Data := null;

            --  Now we know that Self.Data.Big.Data is null, and Tmp is
            --  set to the previous value. We still own a reference to
            --  that previous value, so it won't be freed by another
            --  thread.
            --  If another thread tries to do an assignment now, it will
            --  end up with a null buffer. But that is only possible if
            --  the other thread is accessing a shared string, which is
            --  not supported (a thread reading the string while we are
            --  modifying it).

            if Decrement (Tmp.Refcount) then
               --  We were the only user, so it is safe to keep the string
               Unsafe_Increment (Tmp.Refcount);
               Self.Data.Big.Data := Tmp;
            else
               --  Other threads were still sharing the data. We have to
               --  make a copy
               Clone (Self, Tmp);
            end if;

         end if;
      end Make_Writable_Thread_Safe;

      -----------
      -- Clone --
      -----------

      procedure Clone
         (Self   : in out XString;
          Data   : Big_String_Data_Access)
      is
         Size  : constant Integer := Integer (Self.Data.Big.Size);
         First : constant Natural := Natural (Self.Data.Big.First);
         Cap  : constant String_Size :=
            Compute_Capacity (0, Min_Size => Self.Data.Big.Size);
         Result : constant Big_String_Data_Access := Convert
            (System.Memory.Alloc
               (size_t (Cap) * Bytes_Per_Char + Extra_Header_Size));
      begin
         if Copy_On_Write then
            Result.Refcount := 1;
            Result.Bytes2 (1 .. Size) :=
               Data.Bytes2 (First .. First - 1 + Size);
         else
            Result.Bytes1 (1 .. Size) :=
               Data.Bytes1 (First .. First - 1 + Size);
         end if;

         Self.Data.Big.First := 1;
         Store_Capacity (Self, Cap);

         Self.Data.Big.Data := Result;
      end Clone;

      ------------
      -- Adjust --
      ------------

      overriding procedure Adjust (Self : in out XString) is
      begin
         if not Self.Data.Small.Is_Big then
            null;   --  nothing to do
         elsif Copy_On_Write then
            Increment (Self.Data.Big.Data.Refcount);
         else
            --  We do not need atomic operations here. We are still in
            --  the thread that did the assignment, and there is no
            --  shared data in this mode.
            Clone (Self, Self.Data.Big.Data);
         end if;
      end Adjust;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Self : in out XString) is
         Tmp : Big_String_Data_Access;
      begin
         --  nothing to do for small strings
         if Self.Data.Small.Is_Big then
            Tmp := Self.Data.Big.Data;
            if Tmp /= null then
               Self.Data.Big.Data := null;

               if not Copy_On_Write or else Decrement (Tmp.Refcount) then
                  System.Memory.Free (Convert (Tmp));
               end if;
            end if;
         end if;
      end Finalize;

      -----------
      -- Clear --
      -----------

      procedure Clear (Self : in out XString) is
      begin
         Finalize (Self);
         Self.Data.Small.Is_Big := False;
         Self.Data.Small.Size := 0;
      end Clear;

      ---------------------------
      -- Convert_To_Big_String --
      ---------------------------

      procedure Convert_To_Big_String
         (Self : in out XString;
          Size : String_Size)
      is
         New_Size  : constant String_Size := Compute_Capacity (0, Size);
      begin
         Store_Capacity (Self, New_Size);
         Self.Data.Big.Is_Big := True;
         Self.Data.Big.Data := Convert
            (System.Memory.Alloc
               (size_t (New_Size) * Bytes_Per_Char + Extra_Header_Size));
         Self.Data.Big.Size := Size;
         Self.Data.Big.First := 1;

         if Copy_On_Write then
            Self.Data.Big.Data.Refcount := 1;
         end if;
      end Convert_To_Big_String;

      -------------
      -- Reserve --
      -------------

      procedure Reserve (Self : in out XString; Capacity : String_Size) is
         Current_Cap : String_Size;
         First       : Natural;
         New_Size    : String_Size;
         Old_Size    : Natural;
      begin
         if Self.Data.Small.Is_Big then
            --  We are about to modify the string
            if Copy_On_Write then
               Make_Writable (Self);
            end if;

            Current_Cap := Get_Capacity (Self);
            First := Self.Data.Big.First;

            --  Do we have enough space at the end already (i.e
            --  the capacity we really need extends from First, not
            --  from character 1).

            if Current_Cap >= String_Size (First) - 1 + Capacity then
               --  nothing to do, we have enough space
               null;

            else
               --  We'll have to make space. The simplest is first to move all
               --  characters back to First=1, which might free enough space at
               --  the end of the string.

               if First > 1 then
                  Old_Size := Natural (Self.Data.Big.Size);
                  if Copy_On_Write then
                     Self.Data.Big.Data.Bytes2 (1 .. Old_Size) :=
                        Self.Data.Big.Data.Bytes2
                           (First .. First - 1 + Old_Size);
                  else
                     Self.Data.Big.Data.Bytes1 (1 .. Old_Size) :=
                        Self.Data.Big.Data.Bytes1
                           (First .. First - 1 + Old_Size);
                  end if;

                  Self.Data.Big.First := 1;
               end if;

               --  Do we have enough space now ?

               if Current_Cap < Capacity then
                  New_Size := Compute_Capacity (Current_Cap, Capacity);
                  Store_Capacity (Self, New_Size);
                  Self.Data.Big.Data := Convert
                     (System.Memory.Realloc
                       (Convert (Self.Data.Big.Data),
                        size_t (New_Size) * Bytes_Per_Char
                        + Extra_Header_Size));
               end if;
            end if;

         else
            --  If we'll need a large string
            if Capacity > Max_Small_Length then
               declare
                  Current : constant Natural :=
                     Natural (Self.Data.Small.Size);
                  Old : constant Char_String :=
                     Self.Data.Small.Data (1 .. Current);
               begin
                  Convert_To_Big_String (Self, Capacity);
                  pragma Assert (Self.Data.Big.First = 1);
                  Self.Data.Big.Size := String_Size (Current);

                  if Copy_On_Write then
                     Self.Data.Big.Data.Bytes2 (1 .. Current) :=
                        Convert (Old'Address) (1 .. Current);
                  else
                     Self.Data.Big.Data.Bytes1 (1 .. Current) :=
                        Convert (Old'Address) (1 .. Current);
                  end if;
               end;
            end if;
         end if;
      end Reserve;

      ------------
      -- Shrink --
      ------------

      procedure Shrink (Self : in out XString) is
         New_Size : String_Size;
      begin
         if not Self.Data.Small.Is_Big then
            --  Nothing to do
            null;
         else
            --  ??? Should we try to revert to a small string
            Make_Writable (Self);

            New_Size := Compute_Capacity (0, Self.Data.Big.Size);
            Store_Capacity (Self, New_Size);
            Self.Data.Big.Data := Convert
               (System.Memory.Realloc
                 (Convert (Self.Data.Big.Data),
                  size_t (New_Size) * Bytes_Per_Char + Extra_Header_Size));
         end if;
      end Shrink;

      ---------
      -- Set --
      ---------

      procedure Set
         (Self : in out XString;
          Str  : Char_String)
      is
      begin
         Store_Size (Self, 0);
         Self.Reserve (Capacity => Str'Length);

         if not Self.Data.Small.Is_Big then
            Self.Data.Small.Size := Str'Length;
            Self.Data.Small.Data (1 .. Str'Length) := Str;
         else
            Self.Data.Big.Size := String_Size (Str'Length);
            Self.Data.Big.First := 1;

            if Copy_On_Write then
               Self.Data.Big.Data.Bytes2 (1 .. Str'Length) :=
                  Convert (Str'Address) (1 .. Str'Length);
            else
               Self.Data.Big.Data.Bytes1 (1 .. Str'Length) :=
                  Convert (Str'Address) (1 .. Str'Length);
            end if;
         end if;
      end Set;

      ------------
      -- Append --
      ------------

      procedure Append
        (Self : in out XString;
         Str  : Char_String)
      is
         Current  : constant Natural := Self.Length;
         New_Size : constant Natural := Current + Str'Length;
         F        : Natural;
      begin
         --  Make sure we have enough space, possibly by moving
         --  characters back to position 1, or by converting to
         --  a big string, or resizing the current buffer.

         Self.Reserve (Capacity => String_Size (New_Size));

         if not Self.Data.Small.Is_Big then
            Self.Data.Small.Data (Current + 1 .. New_Size) := Str;
            Self.Data.Small.Size := SSize (New_Size);

         else
            F := Natural (Self.Data.Big.First) + Current;
            Self.Data.Big.Size := String_Size (New_Size);

            if Copy_On_Write then
               Self.Data.Big.Data.Bytes2 (F .. F - 1 + Str'Length) :=
                  Convert (Str'Address) (1 .. Str'Length);
            else
               Self.Data.Big.Data.Bytes1 (F .. F - 1 + Str'Length) :=
                  Convert (Str'Address) (1 .. Str'Length);
            end if;
         end if;
      end Append;

      ------------
      -- Append --
      ------------

      procedure Append
        (Self : in out XString;
         Char : Char_Type)
      is
         Current : constant Natural := Self.Length;
         F : Natural;
      begin
         Self.Reserve (Capacity => String_Size (Current + 1));

         if not Self.Data.Small.Is_Big then
            Self.Data.Small.Data (Current + 1) := Char;
            Self.Data.Small.Size := SSize (Current + 1);
         else
            F := Natural (Self.Data.Big.First) + Current;
            Self.Data.Big.Size := String_Size (Current + 1);

            if Copy_On_Write then
               Self.Data.Big.Data.Bytes2 (F) := Char;
            else
               Self.Data.Big.Data.Bytes1 (F) := Char;
            end if;
         end if;
      end Append;

      ------------
      -- Append --
      ------------

      procedure Append (Self : in out XString; Str : XString) is
         B : Char_Array;
         L : Natural;
      begin
         Get_String (Str, B, L);
         Self.Append (Char_String (B (1 .. L)));
      end Append;

      ---------
      -- "*" --
      ---------

      function "*" (Count : Natural; Right : Char_Type) return XString is
         Result : XString;
      begin
         Result.Reserve (Capacity => String_Size (Count));

         for C in 1 .. Count loop
            Result.Append (Right);
         end loop;

         return Result;
      end "*";

      ---------
      -- "*" --
      ---------

      function "*" (Count : Natural; Right : Char_String) return XString is
         Result : XString;
      begin
         Result.Reserve (Capacity => String_Size (Count * Right'Length));

         for C in 1 .. Count loop
            Result.Append (Right);
         end loop;

         return Result;
      end "*";

      ---------
      -- "*" --
      ---------

      function "*" (Count : Natural; Right : XString) return XString is
         Result : XString;
      begin
         Result.Reserve (Capacity => String_Size (Count * Right.Length));

         for C in 1 .. Count loop
            Result.Append (Right);
         end loop;

         return Result;
      end "*";

      ------------
      -- Length --
      ------------

      function Length (Self : XString) return Natural is
      begin
         if not Self.Data.Small.Is_Big then
            return Natural (Self.Data.Small.Size);
         else
            return Natural (Self.Data.Big.Size);
         end if;
      end Length;

      ----------------
      -- Get_String --
      ----------------

      procedure Get_String
         (Self : XString;
          S    : out Char_Array;
          L    : out Natural) is
      begin
         if not Self.Data.Small.Is_Big then
            L := Natural (Self.Data.Small.Size);
            S := Convert (Self.Data.Small.Data'Address);

         --  For a big string, we need to take into account First. Yet,
         --  everything should behave for the user as if the first character
         --  was always at index 1.

         elsif Copy_On_Write then
            L := Natural (Self.Data.Big.Size);
            S := Convert
               (Self.Data.Big.Data.Bytes2
                  (Natural (Self.Data.Big.First))'Address);
         else
            L := Natural (Self.Data.Big.Size);
            S := Convert
               (Self.Data.Big.Data.Bytes1
                  (Natural (Self.Data.Big.First))'Address);
         end if;
      end Get_String;

      ----------------
      -- To_XString --
      ----------------

      function To_XString (Str : Char_String) return XString is
         R : XString;
      begin
         R.Set (Str);
         return R;
      end To_XString;

      ---------------
      -- To_String --
      ---------------

      function To_String (Self : XString) return Char_String is
         B : Char_Array;
         L : Natural;
      begin
         Get_String (Self, B, L);
         return Char_String (B (1 .. L));
      end To_String;

      ---------
      -- "=" --
      ---------

      function "=" (Left : XString; Right : Char_String) return Boolean is
         B : Char_Array;
         L : Natural;
      begin
         Get_String (Left, B, L);
         return Char_String (B (1 .. L)) = Right;
      end "=";

      ---------
      -- "=" --
      ---------

      function "=" (Left, Right : XString) return Boolean is
         B1, B2 : Char_Array;
         L1, L2 : Natural;
      begin
         Get_String (Left,  B1, L1);
         Get_String (Right, B2, L2);

         --  ??? Should we check the pointers and "First"

         return L1 = L2 and then B1 (1 .. L1) = B2 (1 .. L2);
      end "=";

      ---------
      -- "<" --
      ---------

      function "<" (Left : XString; Right : Char_String) return Boolean is
         B : Char_Array;
         L : Natural;
      begin
         Get_String (Left, B, L);
         return Char_String (B (1 .. L)) < Right;
      end "<";

      ---------
      -- "<" --
      ---------

      function "<" (Left : Char_String; Right : XString) return Boolean is
         B : Char_Array;
         L : Natural;
      begin
         Get_String (Right, B, L);
         return Left < Char_String (B (1 .. L));
      end "<";

      ---------
      -- "<" --
      ---------

      function "<" (Left, Right : XString) return Boolean is
         B, B2 : Char_Array;
         L, L2 : Natural;
      begin
         Get_String (Left, B, L);
         Get_String (Right, B2, L2);
         return B (1 .. L) < B2 (1 .. L2);
      end "<";

      ----------
      -- "<=" --
      ----------

      function "<=" (Left : XString; Right : Char_String) return Boolean is
         B : Char_Array;
         L : Natural;
      begin
         Get_String (Left, B, L);
         return Char_String (B (1 .. L)) <= Right;
      end "<=";

      ----------
      -- "<=" --
      ----------

      function "<=" (Left : Char_String; Right : XString) return Boolean is
         B : Char_Array;
         L : Natural;
      begin
         Get_String (Right, B, L);
         return Left <= Char_String (B (1 .. L));
      end "<=";

      ----------
      -- "<=" --
      ----------

      function "<=" (Left, Right : XString) return Boolean is
         B, B2 : Char_Array;
         L, L2 : Natural;
      begin
         Get_String (Left,  B, L);
         Get_String (Right, B2, L2);
         return B (1 .. L) <= B2 (1 .. L2);
      end "<=";

      -------------
      -- Compare --
      -------------

      function Compare
         (Left : XString; Right : Char_String) return Compare_Result
      is
         S : Char_Array;
         L : Natural;
         C2 : Char_Type;
      begin
         Get_String (Left, S, L);

         for C in 1 .. Integer'Min (L, Right'Length) loop
            C2 := Right (Right'First + C - 1);
            if S (C) < C2 then
               return -1;
            elsif S (C) > C2 then
               return 1;
            end if;
         end loop;

         if L = Right'Length then
            return 0;
         elsif L < Right'Length then
            return -1;
         else
            return 1;
         end if;
      end Compare;

      -------------
      -- Compare --
      -------------

      function Compare
         (Left : XString; Right : XString) return Compare_Result
      is
         S : Char_Array;
         L : Natural;
      begin
         Get_String (Right, S, L);
         return Compare (Left, Char_String (S (1 .. L)));
      end Compare;

      ------------------------------
      -- Compare_Case_Insensitive --
      ------------------------------

      function Compare_Case_Insensitive
         (Left : XString; Right : Char_String) return Compare_Result
      is
         S : Char_Array;
         L : Natural;
         C2, C3 : Char_Type;
      begin
         Get_String (Left, S, L);

         for C in 1 .. Integer'Min (L, Right'Length) loop
            C3 := To_Lower (S (C));
            C2 := To_Lower (Right (Right'First + C - 1));
            if C3 < C2 then
               return -1;
            elsif C3 > C2 then
               return 1;
            end if;
         end loop;

         if L = Right'Length then
            return 0;
         elsif L < Right'Length then
            return -1;
         else
            return 1;
         end if;
      end Compare_Case_Insensitive;

      ------------------------------
      -- Compare_Case_Insensitive --
      ------------------------------

      function Compare_Case_Insensitive
         (Left : XString; Right : XString) return Compare_Result
      is
         S : Char_Array;
         L : Natural;
      begin
         Get_String (Right, S, L);
         return Compare_Case_Insensitive (Left, Char_String (S (1 .. L)));
      end Compare_Case_Insensitive;

      ---------
      -- Get --
      ---------

      function Get (Self : XString; Index : Positive) return Char_Type is
         B : Char_Array;
         L : Natural;
      begin
         Get_String (Self, B, L);
         if Index <= L then
            return B (Index);
         else
            raise Ada.Strings.Index_Error with "Invalid index" & Index'Img
               & " (greater than" & L'Img & ")";
         end if;
      end Get;

      -----------
      -- Slice --
      -----------

      procedure Slice
         (Self   : in out XString;
          Low    : Positive;
          High   : Natural)
      is
         New_Size : constant Natural := High - Low + 1;
      begin
         if not Self.Data.Small.Is_Big then
            if Low > Natural (Self.Data.Small.Size)
               or else High > Natural (Self.Data.Small.Size)
            then
               raise Ada.Strings.Index_Error;
            end if;

            Self.Data.Small.Data (1 .. New_Size) :=
               Self.Data.Small.Data (Low .. High);
            Self.Data.Small.Size := SSize (New_Size);

         else
            if String_Size (Low) > Self.Data.Big.Size
               or else String_Size (High) > Self.Data.Big.Size
            then
               raise Ada.Strings.Index_Error;
            end if;

            --  Keep the same data (no need for change in refcount
            --  or to duplicate)
            Self.Data.Big.First := Low + Self.Data.Big.First - 1;
            Self.Data.Big.Size := String_Size (New_Size);
         end if;
      end Slice;

      -----------
      -- Slice --
      -----------

      procedure Slice
         (Self : XString;
          Low  : Positive;
          High : Natural;
          Into : in out XString)
      is
         Len   : constant Natural := Self.Length;
         Size  : String_Size;
         Is_Same : Boolean;
      begin
         --  We can't use Set, since we want to share the buffer when
         --  possible.

         if Low > Len then
            raise Ada.Strings.Index_Error with Low'Img & ">" & Len'Img;
         end if;

         if High > Len then
            raise Ada.Strings.Index_Error with High'Img & ">" & Len'Img;
         end if;

         --  We should not call Reserve: this would call Make_Writable,
         --  and thus potentially requires a copy of the buffer. Instead,
         --  we want to reuse the buffer if possible.
         --  But Into might already have some data, so we must avoid leaks

         Size := String_Size (High - Low + 1);

         if not Self.Data.Big.Is_Big then
            --  Taking a slice of a small string always results in small
            if Into.Data.Big.Is_Big then
               Finalize (Into);
               Into.Data.Big.Is_Big := False;
            end if;

            Into.Data.Small.Data (1 .. Integer (Size)) :=
               Self.Data.Small.Data (Low .. High);
            Into.Data.Small.Size := SSize (Size);

         elsif Copy_On_Write then
            Is_Same := Into.Data.Big.Is_Big
               and then Into.Data.Big.Data = Self.Data.Big.Data;

            --  Stop holding a shared buffer, if we were
            if not Is_Same then
               Finalize (Into);
            end if;

            Into.Data.Big :=
               (Is_Big        => True,
                Data          => Self.Data.Big.Data,
                Half_Capacity => Self.Data.Big.Half_Capacity,
                Size          => Size,
                First         => Low + Self.Data.Big.First - 1);

            if not Is_Same then
               Increment (Into.Data.Big.Data.Refcount);  --  buffer is shared
            end if;

         else
            --  If Into and Self are the same object (the only case where
            --  their Data is the same), keep that buffer and change the
            --  slice we use.

            if Into.Data.Big.Is_Big
               and then Into.Data.Big.Data = Self.Data.Big.Data
            then
               Into.Data.Big.First := Low + Self.Data.Big.First - 1;
               Into.Data.Big.Size := Size;

            else
               --  Try and reuse memory if we can. This memory is unique
               --  to Into, so we can safely alter it.

               if not Into.Data.Big.Is_Big then
                  if Size <= Max_Small_Length then
                     Into.Data.Small.Data (1 .. Natural (Size)) :=
                        Char_String (Self.Data.Big.Data.Bytes1
                          (Low + Self.Data.Big.First - 1
                           ..  High + Self.Data.Big.First - 1));
                     Into.Data.Small.Size := SSize (Size);
                     return;
                  else
                     Into.Data.Small.Size := 0;
                     Convert_To_Big_String (Into, Size);
                  end if;

               else
                  Into.Data.Big.Size := 0;
                  Reserve (Into, Capacity => Size);
               end if;

               Into.Data.Big.Size := Size;
               Into.Data.Big.Data.Bytes1
                  (Into.Data.Big.First
                   ..  Into.Data.Big.First + Natural (Size) - 1) :=
                  Self.Data.Big.Data.Bytes1
                     (Low + Self.Data.Big.First - 1
                      ..  High + Self.Data.Big.First - 1);
            end if;
         end if;
      end Slice;

      -----------
      -- Slice --
      -----------

      function Slice
         (Self : XString;
          Low  : Positive;
          High : Natural) return XString
      is
         Result : XString;
      begin
         Slice (Self, Low, High, Into => Result);
         return Result;
      end Slice;

      ----------
      -- Trim --
      ----------

      procedure Trim
         (Self  : in out XString;
          Side  : Ada.Strings.Trim_End := Ada.Strings.Both;
          Chars : Char_Type := Space)
      is
         S    : Char_Array;
         L    : Natural;
         F    : Natural := 1;
      begin
         Get_String (Self, S, L);

         if Side = Ada.Strings.Both
            or else Side = Ada.Strings.Right
         then
            while L >= 1 and then S (L) = Chars loop
               L := L  - 1;
            end loop;
         end if;

         if Side = Ada.Strings.Both
            or else Side = Ada.Strings.Left
         then
            while F <= L and then S (F) = Chars loop
               F := F + 1;
            end loop;
         end if;

         Self.Slice (F, L);
      end Trim;

      ----------
      -- Trim --
      ----------

      function Trim
         (Self  : XString;
          Side  : Ada.Strings.Trim_End := Ada.Strings.Both;
          Chars : Char_Type := Space) return XString
      is
         S    : Char_Array;
         L    : Natural;
         F    : Natural := 1;
      begin
         Get_String (Self, S, L);

         if Side = Ada.Strings.Both
            or else Side = Ada.Strings.Right
         then
            while L >= 1 and then S (L) = Chars loop
               L := L  - 1;
            end loop;
         end if;

         if Side = Ada.Strings.Both
            or else Side = Ada.Strings.Left
         then
            while F <= L and then S (F) = Chars loop
               F := F + 1;
            end loop;
         end if;

         return Self.Slice (F, L);
      end Trim;

      -----------------
      -- Starts_With --
      -----------------

      function Starts_With
         (Self : XString; Prefix : Char_String) return Boolean
      is
         S    : Char_Array;
         L    : Natural;
      begin
         Get_String (Self, S, L);
         return L >= Prefix'Length
            and then Char_String (S (1 .. Prefix'Length)) = Prefix;
      end Starts_With;

      -----------------
      -- Starts_With --
      -----------------

      function Starts_With
         (Self : XString; Prefix : XString) return Boolean
      is
         S, S2    : Char_Array;
         L, L2    : Natural;
      begin
         Get_String (Self, S, L);
         Get_String (Prefix, S2, L2);
         return L >= L2 and then S (1 .. L2) = S2 (1 .. L2);
      end Starts_With;

      ---------------
      -- Ends_With --
      ---------------

      function Ends_With
         (Self : XString; Suffix : Char_String) return Boolean
      is
         S : Char_Array;
         L : Natural;
      begin
         Get_String (Self, S, L);
         return L >= Suffix'Length
            and then Char_String (S (L - Suffix'Length + 1 .. L)) = Suffix;
      end Ends_With;

      ---------------
      -- Ends_With --
      ---------------

      function Ends_With (Self : XString; Suffix : XString) return Boolean is
         S, S2 : Char_Array;
         L, L2 : Natural;
      begin
         Get_String (Self, S, L);
         Get_String (Suffix, S2, L2);
         return L >= L2 and then S (L - L2 + 1 .. L) = S2 (1 .. L2);
      end Ends_With;

      ----------
      -- Head --
      ----------

      function Head (Self : XString; Count : Natural) return XString is
         L : constant Natural := Self.Length;
      begin
         return Self.Slice (1, Natural'Min (Count, L));
      end Head;

      ----------
      -- Tail --
      ----------

      function Tail (Self : XString; Count : Natural) return XString is
         L : constant Natural := Self.Length;
      begin
         return Self.Slice (Natural'Max (1, L - Count + 1), L);
      end Tail;

      -------------
      -- Replace --
      -------------

      procedure Replace
         (Self : in out XString; Index : Positive; Char : Char_Type)
      is
         S : Char_Array;
         L : Natural;
      begin
         if Self.Data.Big.Is_Big then
            Make_Writable (Self);
         end if;

         Get_String (Self, S, L);
         if Index > L then
            raise Ada.Strings.Index_Error with Index'Img & ">" & L'Img;
         end if;

         S (Index) := Char;
      end Replace;

      -------------
      -- Replace --
      -------------

      procedure Replace
         (Self      : in out XString;
          Low       : Positive;
          High      : Natural;
          By        : Char_String)
      is
         S : Char_Array;
         L, L2 : Natural;
         New_L : Natural;
      begin
         L := Self.Length;
         if Low > L then
            raise Ada.Strings.Index_Error with Low'Img & ">" & L'Img;
         end if;

         if High >= L then
            New_L := Low - 1 + By'Length;
         else
            New_L := Low - 1 + By'Length + (L - High);
         end if;

         --  This makes the string writable
         Self.Reserve (String_Size (New_L));

         --  Couldn't get the string before, since we might have reset it
         Get_String (Self, S, L2);

         if High < L then
            S (Low + By'Length .. Low + By'Length + L - High - 1) :=
               S (High + 1 .. L);
         end if;

         if By'Length /= 0 then
            S (Low .. Low + By'Length - 1) :=
               Convert (By'Address) (1 .. By'Length);
         end if;

         if Self.Data.Small.Is_Big then
            Self.Data.Big.Size := String_Size (New_L);
         else
            Self.Data.Small.Size := SSize (New_L);
         end if;
      end Replace;

      -------------------
      -- Replace_Slice --
      -------------------

      procedure Replace_Slice
         (Self      : in out XString;
          Low       : Positive;
          High      : Natural;
          By        : XString)
      is
         By_Length : constant Natural := By.Length;
         S, S2 : Char_Array;
         L, L2 : Natural;
         New_L : Natural;
      begin
         --  First make strings unique, in case Self and By share a buffer.
         --  Unfortunately, just calling Make_Writable first would require
         --  one malloc here, then a second one to reserve the correct size.
         --  So instead we have to duplicate part of the code for Replace.

         L := Self.Length;
         if Low > L then
            raise Ada.Strings.Index_Error with Low'Img & ">" & L'Img;
         end if;

         if High >= L then
            New_L := Low - 1 + By_Length;
         else
            New_L := Low - 1 + By_Length + (L - High);
         end if;

         --  This makes the string writable, and ensure we no longer share
         --  the buffer.
         Self.Reserve (String_Size (New_L));

         --  Couldn't get the string before, since we might have reset it
         Get_String (Self, S, L2);
         Get_String (By,   S2, L2);

         if High < L then
            S (Low + By_Length .. Low + By_Length + L - High - 1) :=
               S (High + 1 .. L);
         end if;

         if By_Length /= 0 then
            S (Low .. Low + By_Length - 1) := S2 (1 .. L2);
         end if;

         if Self.Data.Small.Is_Big then
            Self.Data.Big.Size := String_Size (New_L);
         else
            Self.Data.Small.Size := SSize (New_L);
         end if;
      end Replace_Slice;

      ------------
      -- Insert --
      ------------

      procedure Insert
         (Self      : in out XString;
          Before    : Positive;
          New_Item  : Char_String) is
      begin
         Self.Replace (Low => Before, High => Before - 1, By => New_Item);
      end Insert;

      ------------
      -- Insert --
      ------------

      procedure Insert
         (Self      : in out XString;
          Before    : Positive;
          New_Item  : XString) is
      begin
         Self.Replace_Slice
            (Low => Before, High => Before - 1, By => New_Item);
      end Insert;

      ---------------
      -- Overwrite --
      ---------------

      procedure Overwrite
         (Self      : in out XString;
          Position  : Positive;
          New_Item  : Char_String) is
      begin
         Self.Replace
            (Low  => Position,
             High => Position + New_Item'Length - 1,
             By   => New_Item);
      end Overwrite;

      ---------------
      -- Overwrite --
      ---------------

      procedure Overwrite
         (Self      : in out XString;
          Position  : Positive;
          New_Item  : XString) is
      begin
         Self.Replace_Slice
            (Low  => Position,
             High => Position + New_Item.Length - 1,
             By   => New_Item);
      end Overwrite;

      ------------
      -- Delete --
      ------------

      procedure Delete
         (Self      : in out XString;
          Low       : Positive;
          High      : Natural) is
      begin
         Self.Replace (Low, High, Char_String'(1 .. 0 => Char_Type'First));
      end Delete;

      ----------
      -- Hash --
      ----------

      function Hash (Self : XString) return Ada.Containers.Hash_Type is
         function H is new System.String_Hash.Hash
            (Char_Type, Char_String, Ada.Containers.Hash_Type);
         S : Char_Array;
         L : Natural;
      begin
         Get_String (Self, S, L);
         return H (Char_String (S (1 .. L)));
      end Hash;

      ---------------------------
      -- Hash_Case_Insensitive --
      ---------------------------

      function Hash_Case_Insensitive
         (Self : XString) return Ada.Containers.Hash_Type
      is
         function H is new System.String_Hash.Hash
            (Char_Type, Char_String, Ada.Containers.Hash_Type);
         S : Char_Array;
         L : Natural;
      begin
         Get_String (Self, S, L);

         declare
            S2 : Char_String := Char_String (S (1 .. L));
         begin
            for C in 1 .. L loop
               S2 (C) := To_Lower (S2 (C));
            end loop;
            return H (S2);
         end;
      end Hash_Case_Insensitive;

      ----------
      -- Swap --
      ----------

      procedure Swap (Self, Str : in out XString) is
         D : constant String_Data := Str.Data;
      begin
         Str.Data := Self.Data;
         Self.Data := D;
      end Swap;

      ------------
      -- Center --
      ------------

      procedure Center
         (Self  : in out XString;
          Width : Positive;
          Pad   : Char_Type := Space)
      is
         Len : constant Natural := Self.Length;
         S   : Char_Array;
         L   : Natural;
         F   : Positive;
      begin
         if Len < Width then
            Self.Reserve (String_Size (Width));

            Get_String (Self, S, L);
            F := (Width - Len + 1) / 2;

            S (F + 1 .. F + L) := S (1 .. L);

            for C in 1 .. F loop
               S (C) := Pad;
            end loop;

            for C in F + L + 1 .. Width loop
               S (C) := Pad;
            end loop;

            Store_Size (Self, Width);
         end if;
      end Center;

      ------------
      -- Center --
      ------------

      function Center
         (Self  : XString;
          Width : Positive;
          Pad   : Char_Type := Space) return XString
      is
         Len    : constant Natural := Self.Length;
         Result : XString;
         F      : Positive;
         S, S2  : Char_Array;
         L, L2  : Natural;
      begin
         if Len >= Width then
            return Self;
         else
            Result.Reserve (String_Size (Width));

            Get_String (Self, S, L);
            Get_String (Result, S2, L2);

            F := (Width - Len + 1) / 2;

            for C in 1 .. F loop
               S2 (C) := Pad;
            end loop;

            S2 (F + 1 .. F + L) := S (1 .. L);

            for C in F + L + 1 .. Width loop
               S2 (C) := Pad;
            end loop;

            Store_Size (Result, Width);
            return Result;
         end if;
      end Center;

      ------------------
      -- Left_Justify --
      ------------------

      procedure Left_Justify
         (Self  : in out XString;
          Width : Positive;
          Pad   : Char_Type := Space)
      is
         Len : constant Natural := Self.Length;
         S   : Char_Array;
         L   : Natural;
      begin
         if Len < Width then
            Self.Reserve (String_Size (Width));
            Get_String (Self, S, L);

            for C in Len + 1 .. Width loop
               S (C) := Pad;
            end loop;

            Store_Size (Self, Width);
         end if;
      end Left_Justify;

      ------------------
      -- Left_Justify --
      ------------------

      function Left_Justify
         (Self  : XString;
          Width : Positive;
          Pad   : Char_Type := Space) return XString
      is
         --  A simpler implementation is:
         --      Result : XString := Self;
         --      Result.Left_Justify (Width, Pad);
         --  But when not using copy-on-write this results in onre
         --  extra copy of the string.

         Len : constant Natural := Self.Length;
         S   : Char_Array;
         L   : Natural;
         Result : XString;
      begin
         if Len >= Width then
            return Self;
         else
            Result := Self;
            Result.Reserve (String_Size (Width));

            Get_String (Result, S, L);
            for C in Len + 1 .. Width loop
               S (C) := Pad;
            end loop;

            Store_Size (Result, Width);
            return Result;
         end if;
      end Left_Justify;

      -------------------
      -- Right_Justify --
      -------------------

      procedure Right_Justify
         (Self  : in out XString;
          Width : Positive;
          Pad   : Char_Type := Space)
      is
         Len : constant Natural := Self.Length;
         S   : Char_Array;
         L   : Natural;
      begin
         if Len < Width then
            Self.Reserve (String_Size (Width));
            Get_String (Self, S, L);

            S (Width - Len + 1 .. Width) := S (1 .. Len);
            for C in 1 .. Width - Len loop
               S (C) := Pad;
            end loop;

            Store_Size (Self, Width);
         end if;
      end Right_Justify;

      -------------------
      -- Right_Justify --
      -------------------

      function Right_Justify
         (Self  : XString;
          Width : Positive;
          Pad   : Char_Type := Space) return XString
      is
         Len    : constant Natural := Self.Length;
         S, S2  : Char_Array;
         L, L2  : Natural;
         Result : XString;
      begin
         if Len >= Width then
            return Self;
         else
            Result.Reserve (String_Size (Width));

            Get_String (Result, S, L);
            Get_String (Self,   S2, L2);

            S (Width - Len + 1 .. Width) := S2 (1 .. Len);
            for C in 1 .. Width - L2 loop
               S (C) := Pad;
            end loop;

            Store_Size (Result, Width);
            return Result;
         end if;
      end Right_Justify;

      -----------
      -- Count --
      -----------

      function Count
         (Self : XString;
          Char : Char_Type;
          Low  : Positive := 1;
          High : Natural := Natural'Last) return Natural
      is
         S        : Char_Array;
         L        : Natural;
         Result   : Natural := 0;
      begin
         Get_String (Self, S, L);

         if L = 0 then
            return 0;
         end if;

         if Low > L then
            raise Ada.Strings.Index_Error with Low'Img & " >" & L'Img;
         end if;

         L := Natural'Min (High, L);

         for C in Low .. L loop
            if S (C) = Char then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Count;

      -----------
      -- Count --
      -----------

      function Count
         (Self : XString;
          Str  : Char_String;
          Low  : Positive := 1;
          High : Natural := Natural'Last) return Natural
      is
         SL    : constant Integer := Str'Length - 1;
         S     : Char_Array;
         L     : Natural;
         Num   : Natural := 0;
         Index : Natural := Low;
      begin
         Get_String (Self, S, L);

         if L = 0 then
            return 0;
         end if;

         if SL = -1 then
            return Natural'Last;
         end if;

         if Low > L then
            raise Ada.Strings.Index_Error with Low'Img & " >" & L'Img;
         end if;

         L := Natural'Min (High, L);

         while Index <= L - SL loop
            if Char_String (S (Index .. Index + SL)) = Str then
               Num := Num + 1;
               Index := Index + SL + 1;
            else
               Index := Index + 1;
            end if;
         end loop;

         return Num;
      end Count;

      ----------
      -- Find --
      ----------

      function Find
         (Self : XString;
          Char : Char_Type;
          Low  : Positive := 1;
          High : Natural := Natural'Last) return Natural
      is
         S        : Char_Array;
         L        : Natural;
      begin
         Get_String (Self, S, L);

         if Low > L then
            raise Ada.Strings.Index_Error with Low'Img & " >" & L'Img;
         end if;

         L := Natural'Min (High, L);

         for C in Low .. L loop
            if S (C) = Char then
               return C;
            end if;
         end loop;
         return 0;
      end Find;

      ----------
      -- Find --
      ----------

      function Find
         (Self : XString;
          Str  : Char_String;
          Low  : Positive := 1;
          High : Natural := Natural'Last) return Natural
      is
         SL    : constant Integer := Str'Length - 1;
         S     : Char_Array;
         L     : Natural;
         Index : Natural := Low;
      begin
         Get_String (Self, S, L);

         if L = 0 or else SL = -1 then
            return 0;
         end if;

         if Low > L then
            raise Ada.Strings.Index_Error with Low'Img & " >" & L'Img;
         end if;

         L := Natural'Min (High, L);

         while Index <= L - SL loop
            if Char_String (S (Index .. Index + SL)) = Str then
               return Index;
            end if;

            Index := Index + 1;
         end loop;

         return 0;
      end Find;

      ----------------
      -- Right_Find --
      ----------------

      function Right_Find
         (Self : XString;
          Char : Char_Type;
          Low  : Positive := 1;
          High : Natural := Natural'Last) return Natural
      is
         S        : Char_Array;
         L        : Natural;
      begin
         Get_String (Self, S, L);

         if Low > L then
            raise Ada.Strings.Index_Error with Low'Img & " >" & L'Img;
         end if;

         L := Natural'Min (High, L);

         for C in reverse Low .. L loop
            if S (C) = Char then
               return C;
            end if;
         end loop;
         return 0;
      end Right_Find;

      ----------------
      -- Right_Find --
      ----------------

      function Right_Find
         (Self : XString;
          Str  : Char_String;
          Low  : Positive := 1;
          High : Natural := Natural'Last) return Natural
      is
         SL    : constant Integer := Str'Length - 1;
         S     : Char_Array;
         L     : Natural;
         Index : Natural;
      begin
         Get_String (Self, S, L);

         if L = 0 or else SL = -1 then
            return 0;
         end if;

         if Low > L then
            raise Ada.Strings.Index_Error with Low'Img & " >" & L'Img;
         end if;

         L := Natural'Min (High, L);
         Index := L - SL;

         while Index >= 1 loop
            if Char_String (S (Index .. Index + SL)) = Str then
               return Index;
            end if;

            Index := Index - 1;
         end loop;

         return 0;
      end Right_Find;

      -----------
      -- Split --
      -----------

      procedure Split
         (Self       : XString;
          Sep        : Char_Type;
          Omit_Empty : Boolean := False;
          Into       : out XString_Array;
          Last       : out Natural)
      is
         S      : Char_Array;
         L      : Natural;
         Index  : Positive;
         F      : Positive;  --  Start of current chunk
      begin
         Get_String (Self, S, L);

         if Into'Length = 1 then
            if L = 0 and then Omit_Empty then
               Last := Into'First - 1;
            else
               Last := Into'First;
               Into (Last) := Self;
            end if;
         else
            Last := Into'First - 1;
            Index := 1;
            F := 1;

            while Index <= L loop
               --  For efficiency, we do not use Find here, since that would
               --  do a lot of extra testing that we do not need
               if S (Index) = Sep then
                  if not Omit_Empty or else F <= Index - 1 then
                     Last := Last + 1;
                     Slice (Self, F, Index - 1, Into => Into (Last));
                  end if;

                  F := Index + 1;
                  exit when Last = Into'Last - 1;
               end if;
               Index := Index + 1;
            end loop;

            if F > L then
               if not Omit_Empty then
                  Last := Last + 1;
                  Into (Last).Clear;
               end if;
            else
               Last := Last + 1;
               Slice (Self, F, L, Into => Into (Last));
            end if;
         end if;
      end Split;

      -----------
      -- Split --
      -----------

      function Split
         (Self       : XString;
          Sep        : Char_Type;
          Max_Split  : Positive := Positive'Last;
          Omit_Empty : Boolean := False) return XString_Array
      is
         L        : constant Natural := Self.Length;
         Max_Size : constant Natural :=
            (if Max_Split /= Natural'Last
             then Integer'Min (Max_Split, L)
             else Self.Count (Sep) + 1);
         Result   : XString_Array (1 .. Max_Size);
         Last     : Natural;
      begin
         Split (Self, Sep, Omit_Empty, Result, Last);
         return Result (Result'First .. Last);
      end Split;

      -----------
      -- Split --
      -----------

      function Split
         (Self       : XString;
          Sep        : Char_String;
          Max_Split  : Positive := Positive'Last;
          Omit_Empty : Boolean := False) return XString_Array
      is
         L        : constant Natural := Self.Length;
         Max_Size : constant Natural :=
            (if Sep'Length = 0
             then 1  --  Won't be used anyway
             elsif Max_Split /= Natural'Last
             then Integer'Min (Max_Split, L)
             else Self.Count (Sep) + 1);
         Result   : XString_Array (1 .. Max_Size);
         Last     : Natural;
      begin
         Split (Self, Sep, Omit_Empty, Result, Last);
         return Result (Result'First .. Last);
      end Split;

      -----------
      -- Split --
      -----------

      procedure Split
         (Self       : XString;
          Sep        : Char_String;
          Omit_Empty : Boolean := False;
          Into       : out XString_Array;
          Last       : out Natural)
      is
         SL     : constant Integer := Sep'Length - 1;
         S      : Char_Array;
         L      : Natural;
         Index  : Positive;
         F      : Positive;  --  Start of current chunk
      begin
         Get_String (Self, S, L);

         if L = 0 or else SL = -1 then
            Last := Into'First - 1;
            return;
         end if;

         if Into'Length = 1 then
            if L = 0 and then Omit_Empty then
               Last := Into'First - 1;
            else
               Last := Into'First;
               Into (Last) := Self;
            end if;
         else
            Last := Into'First - 1;
            Index := 1;
            F := 1;

            while Index <= L - SL loop
               --  For efficiency, we do not use Find here, since that would
               --  do a lot of extra testing that we do not need
               if Char_String (S (Index .. Index + SL)) = Sep then
                  if not Omit_Empty or else F <= Index - 1 then
                     Last := Last + 1;
                     Slice (Self, F, Index - 1, Into => Into (Last));
                  end if;

                  F := Index + SL + 1;
                  exit when Last = Into'Last - 1;

                  Index := F;
               else
                  Index := Index + 1;
               end if;
            end loop;

            if F > L then
               if not Omit_Empty then
                  Last := Last + 1;
                  Into (Last).Clear;
               end if;
            else
               Last := Last + 1;
               Slice (Self, F, L, Into => Into (Last));
            end if;
         end if;
      end Split;

      -----------------
      -- Right_Split --
      -----------------

      procedure Right_Split
         (Self       : XString;
          Sep        : Char_Type;
          Omit_Empty : Boolean := False;
          Into       : out XString_Array;
          Last       : out Natural)
      is
         S      : Char_Array;
         L      : Natural;
         Index  : Integer;
         F      : Natural;  --  End of current chunk
      begin
         Get_String (Self, S, L);

         if Into'Length = 1 then
            if L = 0 and then Omit_Empty then
               Last := Into'First - 1;
            else
               Last := Into'First;
               Into (Last) := Self;
            end if;
         else
            Last := Into'First - 1;
            Index := L;
            F := L;

            while Index >= 1 loop
               if S (Index) = Sep then
                  if not Omit_Empty or else F > Index then
                     Last := Last + 1;

                     if Index >= L then
                        Into (Last).Clear;
                     else
                        Slice (Self, Index + 1, F, Into => Into (Last));
                     end if;
                  end if;

                  F := Index - 1;
                  exit when Last = Into'Last - 1;
               end if;
               Index := Index - 1;
            end loop;

            if F < 1 then
               if not Omit_Empty then
                  Last := Last + 1;
                  Into (Last).Clear;
               end if;
            else
               Last := Last + 1;
               Slice (Self, 1, F, Into => Into (Last));
            end if;
         end if;
      end Right_Split;

      -----------------
      -- Right_Split --
      -----------------

      function Right_Split
         (Self       : XString;
          Sep        : Char_Type;
          Max_Split  : Positive := Positive'Last;
          Omit_Empty : Boolean := False) return XString_Array
      is
         L        : constant Natural := Self.Length;
         Max_Size : constant Natural :=
            (if Max_Split /= Natural'Last
             then Integer'Min (Max_Split, L)
             else Self.Count (Sep) + 1);
         Result   : XString_Array (1 .. Max_Size);
         Last     : Natural;
      begin
         Right_Split (Self, Sep, Omit_Empty, Result, Last);
         return Result (Result'First .. Last);
      end Right_Split;

      -----------------
      -- Right_Split --
      -----------------

      procedure Right_Split
         (Self       : XString;
          Sep        : Char_String;
          Omit_Empty : Boolean := False;
          Into       : out XString_Array;
          Last       : out Natural)
      is
         SL     : constant Integer := Sep'Length - 1;
         S      : Char_Array;
         L      : Natural;
         Index  : Integer;
         F      : Natural;  --  End of current chunk
      begin
         Get_String (Self, S, L);

         if L = 0 or else SL = -1 then
            Last := Into'First - 1;
            return;
         end if;

         if Into'Length = 1 then
            if L = 0 and then Omit_Empty then
               Last := Into'First - 1;
            else
               Last := Into'First;
               Into (Last) := Self;
            end if;
         else
            Last := Into'First - 1;
            Index := L - SL;
            F := L;

            while Index >= 1 loop
               if Char_String (S (Index .. Index + SL)) = Sep then
                  if not Omit_Empty or else Index + SL + 1 <= F then
                     Last := Last + 1;

                     if Index + SL + 1 > L then
                        Into (Last).Clear;
                     else
                        Slice (Self, Index + SL + 1, F, Into => Into (Last));
                     end if;
                  end if;

                  F := Index - 1;
                  exit when Last = Into'Last - 1;
                  Index := Index - SL;
               end if;
               Index := Index - 1;
            end loop;

            if F < 1 then
               if not Omit_Empty then
                  Last := Last + 1;
                  Into (Last).Clear;
               end if;
            else
               Last := Last + 1;
               Slice (Self, 1, F, Into => Into (Last));
            end if;
         end if;
      end Right_Split;

      -----------------
      -- Right_Split --
      -----------------

      function Right_Split
         (Self       : XString;
          Sep        : Char_String;
          Max_Split  : Positive := Positive'Last;
          Omit_Empty : Boolean := False) return XString_Array
      is
         L        : constant Natural := Self.Length;
         Max_Size : constant Natural :=
            (if Sep'Length = 0
             then 1  --  Won't be used anyway
             elsif Max_Split /= Natural'Last
             then Integer'Min (Max_Split, L)
             else Self.Count (Sep) + 1);
         Result   : XString_Array (1 .. Max_Size);
         Last     : Natural;
      begin
         Right_Split (Self, Sep, Omit_Empty, Result, Last);
         return Result (Result'First .. Last);
      end Right_Split;

      ----------
      -- Join --
      ----------

      function Join
         (Sep       : Char_Type;
          Items     : XString_Array) return XString
      is
         Result : XString;
      begin
         Join (Result, Sep, Items);
         return Result;
      end Join;

      ----------
      -- Join --
      ----------

      procedure Join
         (Self  : out XString;
          Sep   : Char_Type;
          Items : XString_Array)
      is
         Size   : Integer;
      begin
         if Items'Length = 0 then
            Self.Clear;
         else
            Size := Items'Length - 1;
            for It of Items loop
               Size := Size + It.Length;
            end loop;

            Store_Size (Self, 0);  --  Reset the string
            Self.Reserve (String_Size (Size));

            for It in Items'Range loop
               Self.Append (Items (It));
               if It /= Items'Last then
                  Self.Append (Sep);
               end if;
            end loop;
         end if;
      end Join;

      ----------
      -- Join --
      ----------

      function Join
         (Sep       : Char_String;
          Items     : XString_Array) return XString
      is
         Result : XString;
      begin
         Join (Result, Sep, Items);
         return Result;
      end Join;

      ----------
      -- Join --
      ----------

      procedure Join
         (Self  : out XString;
          Sep   : Char_String;
          Items : XString_Array)
      is
         Size   : Natural;
      begin
         if Items'Length = 0 then
            Self.Clear;
         else
            Size := Sep'Length * (Items'Length - 1);
            for It of Items loop
               Size := Size + It.Length;
            end loop;

            Store_Size (Self, 0);  --  Reset the string
            Self.Reserve (String_Size (Size));

            for It in Items'Range loop
               Self.Append (Items (It));
               if It /= Items'Last then
                  Self.Append (Sep);
               end if;
            end loop;
         end if;
      end Join;

      --------------
      -- To_Upper --
      --------------

      procedure To_Upper (Self : in out XString) is
         S   : Char_Array;
         L   : Natural;
      begin
         Make_Writable (Self);
         Get_String (Self, S, L);
         for Idx in 1 .. L loop
            S (Idx) := To_Upper (S (Idx));
         end loop;
      end To_Upper;

      --------------
      -- To_Upper --
      --------------

      function To_Upper (Self : XString) return XString is
         R : XString := Self;
      begin
         To_Upper (R);
         return R;
      end To_Upper;

      --------------
      -- To_Lower --
      --------------

      procedure To_Lower (Self : in out XString) is
         S   : Char_Array;
         L   : Natural;
      begin
         Make_Writable (Self);
         Get_String (Self, S, L);
         for Idx in 1 .. L loop
            S (Idx) := To_Lower (S (Idx));
         end loop;
      end To_Lower;

      --------------
      -- To_Lower --
      --------------

      function To_Lower (Self : XString) return XString is
         R : XString := Self;
      begin
         To_Lower (R);
         return R;
      end To_Lower;

      ----------------
      -- Capitalize --
      ----------------

      procedure Capitalize (Self : in out XString) is
         S   : Char_Array;
         L   : Natural;
      begin
         Make_Writable (Self);
         Get_String (Self, S, L);
         S (1) := To_Upper (S (1));
      end Capitalize;

      -----------
      -- Title --
      -----------

      procedure Title (Self : in out XString) is
         S   : Char_Array;
         L   : Natural;
         Idx : Natural;
      begin
         Make_Writable (Self);
         Get_String (Self, S, L);
         S (1) := To_Upper (S (1));

         Idx := 2;
         while Idx < L loop

            if S (Idx) = Space then
               S (Idx + 1) := To_Upper (S (Idx + 1));
               Idx := Idx + 2;
            else
               Idx := Idx + 1;
            end if;
         end loop;
      end Title;

      --------------
      -- Is_Upper --
      --------------

      function Is_Upper (Self : XString) return Boolean is
      begin
         for C of Self loop
            if C /= To_Upper (C) then
               return False;
            end if;
         end loop;
         return True;
      end Is_Upper;

      --------------
      -- Is_Lower --
      --------------

      function Is_Lower (Self : XString) return Boolean is
      begin
         for C of Self loop
            if C /= To_Lower (C) then
               return False;
            end if;
         end loop;
         return True;
      end Is_Lower;

   end Strings;

end GNATCOLL.Strings_Impl;
