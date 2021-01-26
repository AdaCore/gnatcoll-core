------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2021, AdaCore                          --
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

with Ada.Unchecked_Deallocation;
with System.Storage_Elements;

package body GNATCOLL.String_List_Builders is

   procedure Free is new Ada.Unchecked_Deallocation
     (String, String_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Element_Addrs, Element_Addrs_Access);

   procedure Allocate (Self     : in out String_List_Builder;
                       Elements : Natural;
                       Chars    : Natural);

   function S_First
      (Self : String_List_Builder; Index : Positive) return Integer
   with Inline => True;

   function S_First
      (Self : String_List_Builder; Index : Positive) return Integer
   is
      use System.Storage_Elements;
   begin
      return Integer (Self.Addrs (Index) - Self.Str (1)'Address) + 1;
   end S_First;

   function S_Last
      (Self : String_List_Builder; Index : Positive) return Integer
   with Inline => True;

   function S_Last
      (Self : String_List_Builder; Index : Positive) return Integer
   is
      use System.Storage_Elements;
   begin
      if Index = Self.Addrs_Last then
         return Self.Str_Last - 1;
      else
         return Integer (Self.Addrs (Index + 1) - Self.Str (1)'Address) - 1;
      end if;
   end S_Last;

   function S_Len (Self : String_List_Builder; Index : Positive) return Integer
   with Inline => True;

   function S_Len
      (Self : String_List_Builder; Index : Positive) return Integer
   is
   begin
      return S_Last (Self, Index) - S_First (Self, Index) + 1;
   end S_Len;

   --------------
   -- Allocate --
   --------------

   procedure Allocate (Self     : in out String_List_Builder;
                       Elements : Natural;
                       Chars    : Natural)
   is
      use type System.Address;
      use System.Storage_Elements;

      Idx_Max : Natural :=
         (if Self.Addrs = null then 8 else Self.Addrs'Length);
      Str_Max : Natural :=
         (if Self.Str = null then 128 else Self.Str'Length);
      Offset : constant System.Address :=
         (if Self.Str = null then System.Null_Address else
          Self.Str (1)'Address);
      Str_Reallocated : Boolean := False;

   begin
      --  We always need to ensure that we have at least one index more
      --  allocated
      while Self.Addrs_Last + Elements + 1 > Idx_Max loop
         Idx_Max := Idx_Max * 2;
      end loop;

      --  a double null character. This will ease export to C structures
      --  use for example to represent environment.
      while Self.Str_Last + Chars + 2 > Str_Max loop
         Str_Max := Str_Max * 2;
      end loop;

      --  Perform reallocations
      if Self.Str = null or else Str_Max > Self.Str'Length then
         declare
            New_Str : constant String_Access := new String (1 .. Str_Max);
         begin
            if Self.Str /= null then
               --  Copy previous content if necessary
               New_Str (1 .. Self.Str_Last) := Self.Str (1 .. Self.Str_Last);
               Free (Self.Str);
            end if;
            New_Str (Self.Str_Last + 1) := ASCII.NUL;
            New_Str (Self.Str_Last + 2) := ASCII.NUL;
            Self.Str := New_Str;
            Str_Reallocated := True;
         end;
      end if;

      if Self.Addrs = null or else Idx_Max > Self.Addrs'Length or else
         Str_Reallocated
      then
         declare
            New_Idx : constant Element_Addrs_Access :=
               new Element_Addrs (1 .. Idx_Max);
         begin
            for Index in 1 .. Self.Addrs_Last loop
               New_Idx (Index) := Self.Str (1)'Address +
                  (Self.Addrs (Index) - Offset);
            end loop;

            if Self.Addrs /= null then
               Free (Self.Addrs);
            end if;

            --  Self.Indexes null implies Last_Index set to 0
            New_Idx (Self.Addrs_Last + 1) := System.Null_Address;
            Self.Addrs := New_Idx;
         end;
      end if;
   end Allocate;

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out String_List_Builder; Item : String)
   is
      --  Length of C string counting last ASCII.NUL char
      C_Item_Length : constant Integer := Item'Length + 1;

   begin
      --  Allocate space for one element and a total size of C_Item_Length.
      Allocate (Self, 1, C_Item_Length);

      --  Insert address of new element.
      Self.Addrs_Last := Self.Addrs_Last + 1;
      Self.Addrs (Self.Addrs_Last) := Self.Str (Self.Str_Last + 1)'Address;

      --  Ensure array of address endds with a null address
      --  (for char** export).
      Self.Addrs (Self.Addrs_Last + 1) := System.Null_Address;

      --  Insert content of new element
      Self.Str (Self.Str_Last + 1 .. Self.Str_Last + Item'Length) := Item;
      Self.Str (Self.Str_Last + C_Item_Length) := ASCII.NUL;

      --  Ensure we have two more ASCII.NUL characters (useful when exporting
      --  as a C block of char*).
      Self.Str (Self.Str_Last + C_Item_Length + 1) := ASCII.NUL;
      Self.Str (Self.Str_Last + C_Item_Length + 2) := ASCII.NUL;

      --  Update Self.Str last pointer.
      Self.Str_Last := Self.Str_Last + C_Item_Length;

   end Append;

   procedure Append
      (Self  : in out String_List_Builder;
       Other : String_List_Builder)
   is
   begin
      --  Preallocate all needed space
      Allocate (Self, Length (Other), Other.Str_Last);

      --  And then copy items from Other to Self.
      for Idx in 1 .. Length (Other) loop
         Append (Self, Element (Other, Idx));
      end loop;
   end Append;

   -----------------
   -- As_C_String --
   -----------------

   function As_C_String (Self : String_List_Builder) return OS.C_String
   is
   begin
      if Self.Str /= null then
         return OS.C_String (Self.Str (1)'Address);
      else
         return OS.Null_C_String;
      end if;
   end As_C_String;

   -----------------------
   -- As_C_String_Array --
   -----------------------

   function As_C_String_Array
      (Self : String_List_Builder)
      return OS.C_String_Array
   is
   begin
      if Self.Addrs /= null then
         return OS.C_String_Array (Self.Addrs (1)'Address);
      else
         return OS.Null_C_String_Array;
      end if;
   end As_C_String_Array;

   -------------
   -- As_List --
   -------------

   function As_List (Self : String_List_Builder) return GS.String_List
   is
      Result : GS.String_List (1 .. Self.Addrs_Last);
   begin
      for Index in 1 .. Self.Addrs_Last loop
         Result (Index) := new String'(Element (Self, Index));
      end loop;
      return Result;
   end As_List;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (Self : in out String_List_Builder)
   is
   begin
      if Self.Str /= null then
         Free (Self.Str);
      end if;

      if Self.Addrs /= null then
         Free (Self.Addrs);
      end if;

      Self.Addrs_Last := 0;
      Self.Str_Last  := 0;
   end Deallocate;

   ------------
   -- Delete --
   ------------

   procedure Delete (Self : in out String_List_Builder; Index : Positive)
   is
      use System.Storage_Elements;
   begin
      if Index < Self.Addrs_Last then
         declare
            Start_Addr : constant System.Address := Self.Addrs (Index);
            End_Addr   : constant System.Address := Self.Addrs (Index + 1) - 1;
            Len : constant Integer := S_Len (Self, Index) + 1;
         begin
            Self.Str (S_First (Self, Index) ..
                      S_Last (Self, Length (Self)) - Len + 1) :=
               Self.Str (S_First (Self, Index + 1) ..
                         S_Last (Self, Length (Self)) + 1);

            Self.Str_Last := Self.Str_Last - Len;

            for J in Index +  1 .. Self.Addrs_Last loop
               Self.Addrs (J) :=
                  Self.Addrs (J + 1) - (End_Addr - Start_Addr + 1);
            end loop;

            Self.Str (Self.Str_Last + 1) := ASCII.NUL;
            Self.Str (Self.Str_Last + 2) := ASCII.NUL;

            Self.Addrs_Last := Self.Addrs_Last - 1;
            Self.Addrs (Self.Addrs_Last + 1) := System.Null_Address;
         end;

      elsif Index = Self.Addrs_Last then
         --  If this is the last element just decrease the last counters
         Self.Str_Last := S_First (Self, Index) - 1;
         Self.Str (Self.Str_Last + 1) := ASCII.NUL;
         Self.Str (Self.Str_Last + 2) := ASCII.NUL;
         Self.Addrs_Last := Self.Addrs_Last - 1;
         Self.Addrs (Self.Addrs_Last + 1) := System.Null_Address;
      else
         raise Constraint_Error;
      end if;
   end Delete;

   -------------
   -- Element --
   -------------

   function Element
      (Self  : String_List_Builder;
       Index : Positive)
      return String
   is
   begin
      if Index <= Self.Addrs_Last then
         return Self.Str (S_First (Self, Index) .. S_Last (Self, Index));
      else
         raise Constraint_Error;
      end if;
   end Element;

   function Element
      (Self  : String_List_Builder;
       Index : Positive)
      return OS.C_String
   is
   begin
      if Index <= Self.Addrs_Last then
         return OS.C_String (Self.Addrs (Index));
      else
         raise Constraint_Error;
      end if;
   end Element;

   ------------
   -- Length --
   ------------

   function Length (Self : String_List_Builder) return Natural
   is
   begin
      return Self.Addrs_Last;
   end Length;

end GNATCOLL.String_List_Builders;
