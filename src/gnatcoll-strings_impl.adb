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

   function Convert is new Ada.Unchecked_Conversion
      (System.Address, Unconstrained_String_Access);

   package body Strings is
      function Convert is new Ada.Unchecked_Conversion
         (System.Address, Big_String_Data_Access);
      function Convert is new Ada.Unchecked_Conversion
         (Big_String_Data_Access, System.Address);

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

      function Clone
         (Self   : XString;
          Data   : Big_String_Data_Access)
         return Big_String_Data_Access
         with Pre => Self.Data.Small.Is_Big, Inline;
      --  Create a clone of the big string data, allocating new memory

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
      begin
         if Current < Size then
            New_Size := Compute_Capacity (Current, Size);
            Store_Capacity (Self, New_Size);
            Self.Data.Big.Data := Convert
               (System.Memory.Realloc
                 (Convert (Self.Data.Big.Data),
                  size_t (New_Size) + Extra_Header_Size));
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
            Self.Data.Big.Data := Clone (Self, Self.Data.Big.Data);
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
               Self.Data.Big.Data := Clone (Self, Tmp);
            end if;
         end if;
      end Make_Writable_Thread_Safe;

      -----------
      -- Clone --
      -----------

      function Clone
         (Self   : XString;
          Data   : Big_String_Data_Access)
         return Big_String_Data_Access
      is
         Size   : constant Integer := Integer (Self.Data.Big.Size);
         Result : constant Big_String_Data_Access := Convert
            (System.Memory.Alloc
               (size_t (Get_Capacity (Self)) + Extra_Header_Size));
      begin
         if Copy_On_Write then
            Result.Refcount := 1;
            Result.Bytes2 (1 .. Size) := Data.Bytes2 (1 .. Size);
         else
            Result.Bytes1 (1 .. Size) := Data.Bytes1 (1 .. Size);
         end if;
         return Result;
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
            Self.Data.Big.Data := Clone (Self, Self.Data.Big.Data);
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
            (System.Memory.Alloc (size_t (New_Size) + Extra_Header_Size));
         Self.Data.Big.Size := Size;

         if Copy_On_Write then
            Self.Data.Big.Data.Refcount := 1;
         end if;
      end Convert_To_Big_String;

      ---------
      -- Set --
      ---------

      procedure Set
         (Self : in out XString;
          Str  : String)
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
                  Convert (Str'Address) (Str'Range);
            else
               Self.Data.Big.Data.Bytes1 (1 .. Str'Length) :=
                  Convert (Str'Address) (Str'Range);
            end if;
         end if;
      end Set;

      ------------
      -- Append --
      ------------

      procedure Append
        (Self : in out XString;
         Str  : String)
      is
         Small     : constant Boolean := not Self.Data.Small.Is_Big;
         New_Size : String_Size;
         Current  : String_Size;
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
               Old : constant String :=
                  Self.Data.Small.Data (1 .. Natural (Current));
            begin
               Convert_To_Big_String (Self, New_Size);

               if Copy_On_Write then
                  Self.Data.Big.Data.Bytes2 (1 .. Natural (Current)) :=
                     Convert (Old'Address) (1 .. Natural (Current));
               else
                  Self.Data.Big.Data.Bytes1 (1 .. Natural (Current)) :=
                     Convert (Old'Address) (1 .. Natural (Current));
               end if;
            end;

         else
            Current := Self.Data.Big.Size;
            New_Size := Current + Str'Length;
            Make_Writable (Self);
            Resize (Self, New_Size);
         end if;

         if Copy_On_Write then
            Self.Data.Big.Data.Bytes2
               (Natural (Current + 1) .. Natural (New_Size)) :=
               Convert (Str'Address) (1 .. Str'Length);
         else
            Self.Data.Big.Data.Bytes1
               (Natural (Current + 1) .. Natural (New_Size)) :=
               Convert (Str'Address) (1 .. Str'Length);
         end if;
      end Append;

      ------------
      -- Append --
      ------------

      procedure Append
        (Self : in out XString;
         Char : Character)
      is
         Small    : constant Boolean := not Self.Data.Small.Is_Big;
         New_Size : String_Size;
         Current  : String_Size;
      begin
         if Small then
            Current := String_Size (Self.Data.Small.Size);
            New_Size := Current + 1;
            if New_Size <= Max_Small_Length then
               Self.Data.Small.Size := SSize (New_Size);
               Self.Data.Small.Data (Natural (New_Size)) := Char;
            else
               declare
                  Old : constant String :=
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

            if Copy_On_Write then
               Self.Data.Big.Data.Bytes2 (Natural (New_Size)) := Char;
            else
               Self.Data.Big.Data.Bytes1 (Natural (New_Size)) := Char;
            end if;
         end if;
      end Append;

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
          S    : out Unconstrained_String_Access;
          L    : out Natural) is
      begin
         if not Self.Data.Small.Is_Big then
            L := Natural (Self.Data.Small.Size);
            S := Convert (Self.Data.Small.Data'Address);
         elsif Copy_On_Write then
            L := Natural (Self.Data.Big.Size);
            S := Convert (Self.Data.Big.Data.Bytes2'Address);
         else
            L := Natural (Self.Data.Big.Size);
            S := Convert (Self.Data.Big.Data.Bytes1'Address);
         end if;
      end Get_String;

      ---------------
      -- To_String --
      ---------------

      function To_String (Self : XString) return String is
         B : Unconstrained_String_Access;
         L : Natural;
      begin
         Get_String (Self, B, L);
         return String (B (1 .. L));
      end To_String;

      ---------
      -- "=" --
      ---------

      function "=" (Self : XString; Str : String) return Boolean is
         B : Unconstrained_String_Access;
         L : Natural;
      begin
         Get_String (Self, B, L);
         return String (B (1 .. L)) = Str;
      end "=";

      ---------
      -- "=" --
      ---------

      function "=" (Self, Str : XString) return Boolean is
         B1, B2 : Unconstrained_String_Access;
         L1, L2 : Natural;
      begin
         Get_String (Self, B1, L1);
         Get_String (Str,  B2, L2);
         return L1 = L2 and then B1 (1 .. L1) = B2 (1 .. L2);
      end "=";

      ---------
      -- Get --
      ---------

      function Get (Self : XString; Index : Positive) return Character is
         B : Unconstrained_String_Access;
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

      ----------
      -- Trim --
      ----------

      procedure Trim
         (Self : in out XString;
          Side : Ada.Strings.Trim_End := Ada.Strings.Both)
      is
         S    : Unconstrained_String_Access;
         L    : Natural;
      begin
         Get_String (Self, S, L);

         if Side = Ada.Strings.Both
            or else Side = Ada.Strings.Right
         then
            while L >= 1 and then S (L) = ' ' loop
               L := L  - 1;
            end loop;
         end if;

         if not Self.Data.Small.Is_Big then
            Self.Data.Small.Size := SSize (L);
         else
            Self.Data.Big.Size := String_Size (L);
         end if;
      end Trim;

   end Strings;

end GNATCOLL.Strings_Impl;
