------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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
with Ada.Strings.UTF_Encoding.Wide_Strings;

package body GNATCOLL.WString_Builders is

   package UTF renames Ada.Strings.UTF_Encoding.Wide_Strings;

   WNUL : constant Wide_Character := Wide_Character'Val (0);

   Minimal_Heap_Size : constant Natural := 64;

   procedure Free is new Ada.Unchecked_Deallocation
     (Wide_String, WString_Access);

   procedure Allocate (Self : in out WString_Builder; Chars : Natural);

   --------------
   -- Allocate --
   --------------

   procedure Allocate (Self : in out WString_Builder; Chars : Natural)
   is
      Str_Max : Natural := (if Self.Heap_Str = null then Minimal_Heap_Size
                            else Self.Heap_Str'Length);
   begin

      --  Ensure we have room for total length + 1 (for WNUL)
      while Self.Str_Last + Chars + 1 > Str_Max loop
         Str_Max := Str_Max * 2;
      end loop;

      --  Perform reallocations
      if Self.Heap_Str = null or else Str_Max > Self.Heap_Str'Length then
         declare
            New_Str : constant WString_Access :=
               new Wide_String (1 .. Str_Max);
         begin
            if Self.Heap_Str /= null then
               --  Copy previous content if necessary
               New_Str (1 .. Self.Str_Last + 1) :=
                  Self.Heap_Str (1 .. Self.Str_Last + 1);
               Free (Self.Heap_Str);
            elsif Self.Str_Last > 0 then
               New_Str (1 .. Self.Str_Last + 1) :=
                  Self.Stack_Str (1 .. Self.Str_Last + 1);
            end if;
            Self.Heap_Str := New_Str;
         end;
      end if;
   end Allocate;

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out WString_Builder; Str : UTF8.UTF_8_String) is
      WStr : constant Wide_String := UTF.Decode (Str);
      New_Last : constant Natural := Self.Str_Last + WStr'Length;
   begin
      if WStr'Length = 0 then
         return;
      end if;

      if New_Last > WString_Builder_Short_Size then
         Allocate (Self, WStr'Length);
         Self.Heap_Str (Self.Str_Last + 1 .. New_Last) := WStr;
         Self.Heap_Str (New_Last + 1) := WNUL;
      else
         Self.Stack_Str (Self.Str_Last + 1 .. New_Last) := WStr;
         Self.Stack_Str (New_Last + 1) := WNUL;
      end if;
      Self.Str_Last := New_Last;

   end Append;

   procedure Append
      (Self : in out Static_WString_Builder;
       Str  : UTF8.UTF_8_String)
   is
      WStr : constant Wide_String := UTF.Decode (Str);
      New_Last : constant Natural := Self.Str_Last + WStr'Length;
   begin
      if WStr'Length = 0 then
         return;
      end if;

      if New_Last > Self.Size_With_NUL - 1 then
         raise Constraint_Error;
      end if;

      Self.Str (Self.Str_Last + 1 .. New_Last) := WStr;
      Self.Str_Last := New_Last;
      Self.Str (Self.Str_Last + 1) := WNUL;
   end Append;

   procedure Append (Self : in out WString_Builder; Char : Wide_Character) is
   begin
      if Self.Str_Last + 1 > WString_Builder_Short_Size then
         Allocate (Self, 1);
         Self.Str_Last := Self.Str_Last + 1;
         Self.Heap_Str (Self.Str_Last) := Char;
         Self.Heap_Str (Self.Str_Last + 1) := WNUL;
      else
         Self.Str_Last := Self.Str_Last + 1;
         Self.Stack_Str (Self.Str_Last) := Char;
         Self.Stack_Str (Self.Str_Last + 1) := WNUL;
      end if;
   end Append;

   procedure Append
      (Self : in out Static_WString_Builder;
       Char : Wide_Character)
   is
      New_Last : constant Natural := Self.Str_Last + 1;
   begin
      if New_Last > Self.Size_With_NUL - 1 then
         raise Constraint_Error;
      end if;
      Self.Str_Last := New_Last;
      Self.Str (Self.Str_Last) := Char;
      Self.Str (Self.Str_Last + 1) := WNUL;
   end Append;

   ------------------
   -- As_C_WString --
   ------------------

   function As_C_WString
      (Self          : WString_Builder;
       Null_If_Empty : Boolean := False)
      return OS.C_WString
   is
   begin
      if Self.Str_Last = 0 then
         if Null_If_Empty then
            return OS.Null_C_WString;
         else
            return OS.Empty_C_WString;
         end if;
      elsif Self.Str_Last > WString_Builder_Short_Size then
         return OS.C_WString (Self.Heap_Str (1)'Address);
      else
         return OS.C_WString (Self.Stack_Str (1)'Address);
      end if;
   end As_C_WString;

   function As_C_WString
      (Self          : Static_WString_Builder;
       Null_If_Empty : Boolean := False)
      return OS.C_WString
   is
   begin
      if Self.Str_Last = 0 then
         if Null_If_Empty then
            return OS.Null_C_WString;
         else
            return OS.Empty_C_WString;
         end if;
      else
         return OS.C_WString (Self.Str (1)'Address);
      end if;
   end As_C_WString;

   ---------------
   -- As_String --
   ---------------

   function As_String (Self : WString_Builder) return Wide_String is
   begin
      if Self.Str_Last > WString_Builder_Short_Size then
         return Self.Heap_Str.all (1 .. Self.Str_Last);
      else
         return Self.Stack_Str (1 .. Self.Str_Last);
      end if;
   end As_String;

   function As_String (Self : Static_WString_Builder) return Wide_String is
   begin
      return Self.Str (1 .. Self.Str_Last);
   end As_String;

   --------------------
   -- As_UTF8_String --
   --------------------

   function As_UTF8_String (Self : WString_Builder) return UTF8.UTF_8_String is
   begin
      return UTF.Encode (As_String (Self));
   end As_UTF8_String;

   function As_UTF8_String
      (Self : Static_WString_Builder) return UTF8.UTF_8_String
   is
   begin
      return UTF.Encode (As_String (Self));
   end As_UTF8_String;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (Self : in out WString_Builder) is
   begin
      if Self.Heap_Str /= null then
         Free (Self.Heap_Str);
         Self.Str_Last := 0;
      end if;
   end Deallocate;

   -------------
   -- Element --
   -------------

   function Element
      (Self : WString_Builder;
       N    : Positive)
      return Wide_Character
   is
   begin
      if N > Self.Str_Last then
         raise Constraint_Error;
      elsif Self.Str_Last > WString_Builder_Short_Size then
         return Self.Heap_Str.all (N);
      else
         return Self.Stack_Str (N);
      end if;
   end Element;

   function Element
      (Self : Static_WString_Builder; N : Positive)
      return Wide_Character
   is
   begin
      if N > Self.Str_Last then
         raise Constraint_Error;
      else
         return Self.Str (N);
      end if;
   end Element;

   ------------
   -- Length --
   ------------

   function Length (Self : WString_Builder) return Natural is
   begin
      return Self.Str_Last;
   end Length;

   function Length (Self : Static_WString_Builder) return Natural is
   begin
      return Self.Str_Last;
   end Length;

   ---------
   -- Set --
   ---------

   procedure Set (Self : in out WString_Builder; Str : UTF8.UTF_8_String) is
   begin
      Self.Str_Last := 0;
      Append (Self, Str);
   end Set;

   procedure Set
      (Self : in out Static_WString_Builder;
       Str  : UTF8.UTF_8_String)
   is
   begin
      Self.Str_Last := 0;
      Append (Self, Str);
   end Set;

end GNATCOLL.WString_Builders;
