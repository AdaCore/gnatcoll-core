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

      procedure Resize (Self : in out XString; Size : String_Size)
         with Pre => Self.Data.Small.Is_Big;
      --  Resize the Big buffer in Self so that it fits at least
      --  Size characters. The buffer is never shrunk.
      --  Sets the size of the string

      procedure Store_Capacity (Self : in out XString; Capacity : String_Size)
         with Inline;
      function Get_Capacity (Self : XString) return String_Size
         is (2 * Self.Data.Big.Half_Capacity);
      --  Returns the current capacity of a large string

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

      ------------
      -- Resize --
      ------------

      procedure Resize (Self : in out XString; Size : String_Size) is
         Current  : constant String_Size := Get_Capacity (Self);
         New_Size : String_Size;
         Old_Size : Natural;
         First    : Natural;
      begin
         if Current < String_Size (Self.Data.Big.First) - 1 + Size then
            --  We'll have to make space. The simplest is first to move all
            --  characters back to First=1, which might free enough space at
            --  the end of the string.

            First := Self.Data.Big.First;
            if First > 1 then
               Old_Size := Natural (Self.Data.Big.Size);
               if Copy_On_Write then
                  Self.Data.Big.Data.Bytes2 (1 .. Old_Size) :=
                     Self.Data.Big.Data.Bytes2 (First .. First - 1 + Old_Size);
               else
                  Self.Data.Big.Data.Bytes1 (1 .. Old_Size) :=
                     Self.Data.Big.Data.Bytes1 (First .. First - 1 + Old_Size);
               end if;

               Self.Data.Big.First := 1;
            end if;

            --  Do we now have enough space ?

            if Current < Size then
               New_Size := Compute_Capacity (Current, Size);
               Store_Capacity (Self, New_Size);
               Self.Data.Big.Data := Convert
                  (System.Memory.Realloc
                    (Convert (Self.Data.Big.Data),
                     size_t (New_Size) * Bytes_Per_Char + Extra_Header_Size));
            end if;
         end if;

         Self.Data.Big.Size := Size;
      end Resize;

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
         if not Self.Data.Small.Is_Big then
            null;   --  nothing to do
         else
            Tmp := Self.Data.Big.Data;
            if Tmp /= null then
               Self.Data.Big.Data := null;

               if not Copy_On_Write or else Decrement (Tmp.Refcount) then
                  System.Memory.Free (Convert (Tmp));
               end if;
            end if;
         end if;
      end Finalize;

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

      procedure Reserve (Self : in out XString; Size : String_Size) is
      begin
         if Size <= Max_Small_Length then
            --  Nothing to do, a small string can fit
            null;
         elsif not Self.Data.Small.Is_Big then
            Convert_To_Big_String (Self, Size);
         else
            Make_Writable (Self);
            Resize (Self, Size);
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
         Small     : constant Boolean := not Self.Data.Small.Is_Big;
      begin
         --  If we were already using a big_string, continue to do so
         --  since the memory is already allocated anyway.
         if Small and then Str'Length <= Max_Small_Length then
            Self.Data.Small.Is_Big := False;
            Self.Data.Small.Size := Str'Length;
            Self.Data.Small.Data (1 .. Str'Length) := Str;
         else
            if Small then
               Convert_To_Big_String (Self, Str'Length);
            else
               Make_Writable (Self);
               Resize (Self, Str'Length);
            end if;

            if Copy_On_Write then
               Self.Data.Big.Data.Bytes2 (1 .. Str'Length) :=
                  Convert (Str'Address) (1 .. Str'Length);
            else
               Self.Data.Big.Data.Bytes1 (1 .. Str'Length) :=
                  Convert (Str'Address) (1 .. Str'Length);
            end if;

            Self.Data.Big.First := 1;
         end if;
      end Set;

      ------------
      -- Append --
      ------------

      procedure Append
        (Self : in out XString;
         Str  : Char_String)
      is
         Small     : constant Boolean := not Self.Data.Small.Is_Big;
         New_Size : String_Size;
         Current  : String_Size;
         F        : Natural;
      begin
         if Small then
            Current := String_Size (Self.Data.Small.Size);
            New_Size := Current + Str'Length;

            if New_Size <= Max_Small_Length then
               Self.Data.Small.Data
                  (Natural (Current + 1) .. Natural (New_Size)) := Str;
               Self.Data.Small.Size := SSize (New_Size);
               return;
            end if;

            declare
               Old : constant Char_String :=
                  Self.Data.Small.Data (1 .. Natural (Current));
            begin
               Convert_To_Big_String (Self, New_Size);

               pragma Assert (Self.Data.Big.First = 1);

               if Copy_On_Write then
                  Self.Data.Big.Data.Bytes2 (1 .. Natural (Current)) :=
                     Convert (Old'Address) (1 .. Natural (Current));
               else
                  Self.Data.Big.Data.Bytes1 (1 .. Natural (Current)) :=
                     Convert (Old'Address) (1 .. Natural (Current));
               end if;
            end;

            F := 1;

         else
            Current := Self.Data.Big.Size;
            New_Size := Current + Str'Length;
            Make_Writable (Self);
            Resize (Self, New_Size);
            F := Natural (Self.Data.Big.First);
         end if;

         if Copy_On_Write then
            Self.Data.Big.Data.Bytes2
               (Natural (Current) + F .. F - 1 + Natural (New_Size)) :=
                 Convert (Str'Address) (1 .. Str'Length);
         else
            Self.Data.Big.Data.Bytes1
               (Natural (Current) + F .. F - 1 + Natural (New_Size)) :=
                 Convert (Str'Address) (1 .. Str'Length);
         end if;
      end Append;

      ------------
      -- Append --
      ------------

      procedure Append
        (Self : in out XString;
         Char : Char_Type)
      is
         Small    : constant Boolean := not Self.Data.Small.Is_Big;
         New_Size : String_Size;
         Current  : String_Size;
         F        : Natural;
      begin
         if Small then
            Current := String_Size (Self.Data.Small.Size);
            New_Size := Current + 1;
            if New_Size <= Max_Small_Length then
               Self.Data.Small.Size := SSize (New_Size);
               Self.Data.Small.Data (Natural (New_Size)) := Char;
            else
               declare
                  Old : constant Char_String :=
                     Self.Data.Small.Data (1 .. Natural (Current));
               begin
                  Convert_To_Big_String (Self, New_Size);

                  if Copy_On_Write then
                     Self.Data.Big.Data.Bytes2 (1 .. Natural (Current)) :=
                        Convert (Old'Address) (1 .. Natural (Current));
                     Self.Data.Big.Data.Bytes2 (Natural (New_Size)) := Char;
                  else
                     Self.Data.Big.Data.Bytes1 (1 .. Natural (Current)) :=
                        Convert (Old'Address) (1 .. Natural (Current));
                     Self.Data.Big.Data.Bytes1 (Natural (New_Size)) := Char;
                  end if;
               end;
            end if;

         else
            Current := Self.Data.Big.Size;
            New_Size := Current + 1;

            Make_Writable (Self);
            Resize (Self, New_Size);

            F := Natural (Self.Data.Big.First);

            if Copy_On_Write then
               Self.Data.Big.Data.Bytes2 (F - 1 + Natural (New_Size)) := Char;
            else
               Self.Data.Big.Data.Bytes1 (F - 1 + Natural (New_Size)) := Char;
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
         Result.Reserve (Size => String_Size (Count));

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
         Result.Reserve (Size => String_Size (Count * Right'Length));

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
         Result.Reserve (Size => String_Size (Count * Right.Length));

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

      function "=" (Self : XString; Str : Char_String) return Boolean is
         B : Char_Array;
         L : Natural;
      begin
         Get_String (Self, B, L);
         return Char_String (B (1 .. L)) = Str;
      end "=";

      ---------
      -- "=" --
      ---------

      function "=" (Self, Str : XString) return Boolean is
         B1, B2 : Char_Array;
         L1, L2 : Natural;
      begin
         Get_String (Self, B1, L1);
         Get_String (Str,  B2, L2);
         return L1 = L2 and then B1 (1 .. L1) = B2 (1 .. L2);
      end "=";

      ---------
      -- "<" --
      ---------

      function "<" (Self : XString; Str : Char_String) return Boolean is
         B : Char_Array;
         L : Natural;
      begin
         Get_String (Self, B, L);
         return Char_String (B (1 .. L)) < Str;
      end "<";

      ---------
      -- "<" --
      ---------

--      function "<" (Str : Char_String; Self : XString) return Boolean is
--         B : Char_Array;
--         L : Natural;
--      begin
--         Get_String (Self, B, L);
--         return Str < Char_String (B (1 .. L));
--      end "<";

      ---------
      -- "<" --
      ---------

      function "<" (Self, Str : XString) return Boolean is
         B, B2 : Char_Array;
         L, L2 : Natural;
      begin
         Get_String (Self, B, L);
         Get_String (Str, B2, L2);
         return B (1 .. L) < B2 (1 .. L2);
      end "<";

      ----------
      -- "<=" --
      ----------

      function "<=" (Self : XString; Str : Char_String) return Boolean is
         B : Char_Array;
         L : Natural;
      begin
         Get_String (Self, B, L);
         return Char_String (B (1 .. L)) <= Str;
      end "<=";

      ----------
      -- "<=" --
      ----------

--      function "<=" (Str : Char_String; Self : XString) return Boolean is
--         B : Char_Array;
--         L : Natural;
--      begin
--         Get_String (Self, B, L);
--         return Str <= Char_String (B (1 .. L));
--      end "<=";

      ----------
      -- "<=" --
      ----------

      function "<=" (Self, Str : XString) return Boolean is
         B, B2 : Char_Array;
         L, L2 : Natural;
      begin
         Get_String (Self, B, L);
         Get_String (Str, B2, L2);
         return B (1 .. L) <= B2 (1 .. L2);
      end "<=";

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
          Low, High : Positive)
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

      function Slice (Self : XString; Low, High : Positive) return XString is
         Result : XString;
         Len    : constant Natural := Self.Length;
      begin
         --  We can't use Set, since we want to share the buffer when
         --  possible.

         if Low > Len then
            raise Ada.Strings.Index_Error with Low'Img & ">" & Len'Img;
         end if;

         if High > Len then
            raise Ada.Strings.Index_Error with High'Img & ">" & Len'Img;
         end if;

         if not Self.Data.Small.Is_Big then
            Result.Set (Self.Data.Small.Data (Low .. High));

         elsif Copy_On_Write then
            Result := Self;   --  share data and increment refcount

            --  User indexing is from 1, but this matches First internally
            Result.Data.Big.First := Low + Self.Data.Big.First - 1;
            Result.Data.Big.Size := String_Size (High - Low + 1);

         else
            Convert_To_Big_String (Result, String_Size (High - Low + 1));
            Result.Data.Big.Data.Bytes1 (1 .. High - Low + 1) :=
               Self.Data.Big.Data.Bytes1
                  (Low + Self.Data.Big.First - 1 ..
                     High + Self.Data.Big.First - 1);
         end if;

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

   end Strings;

end GNATCOLL.Strings_Impl;
