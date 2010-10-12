-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                    Copyright (C) 2008-2010, AdaCore               --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- As a special exception, if other files instantiate generics  from --
-- this unit, or you link this  unit with other files to produce  an --
-- executable, this unit does not by itself cause the resulting exe- --
-- cutable  to be covered by  the  GNU General  Public License. This --
-- exception does not however  invalidate any other reasons why  the --
-- executable  file  might  be  covered  by  the  GNU General Public --
-- License.                                                          --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with GNAT.Strings;               use GNAT.Strings;

package body GNATCOLL.Utils is

   ----------
   -- Free --
   ----------

   procedure Free (List : in out GNAT.Strings.String_List) is
   begin
      for L in List'Range loop
         Free (List (L));
      end loop;
   end Free;

   -----------
   -- Equal --
   -----------

   function Equal (S1, S2 : String; Case_Sensitive : Boolean) return Boolean is
      J1 : Natural;
      J2 : Natural;
   begin
      if Case_Sensitive then
         return S1 = S2;

      else
         if S1'Length /= S2'Length then
            return False;
         end if;

         J1 := S1'First;
         J2 := S2'First;

         while J1 <= S1'Last loop
            if To_Lower (S1 (J1)) /= To_Lower (S2 (J2)) then
               return False;
            end if;

            J1 := J1 + 1;
            J2 := J2 + 1;
         end loop;

         return True;
      end if;
   end Equal;

   ----------------------------
   -- Case_Insensitive_Equal --
   ----------------------------

   function Case_Insensitive_Equal (S1, S2 : String) return Boolean is
   begin
      return Equal (S1, S2, Case_Sensitive => False);
   end Case_Insensitive_Equal;

   -----------
   -- Image --
   -----------

   function Image
     (Value      : Integer;
      Min_Width  : Integer;
      Force_Sign : Boolean := False;
      Padding    : Character := '0') return String
   is
      S : constant String := Integer'Image (Value);
      Buf : String (1 .. Integer'Max (S'Length, Min_Width + 1)) :=
        (others => Padding);
      First : Integer := 2;
   begin
      Buf (Buf'Last - S'Length + 2 .. Buf'Last) := S (2 .. S'Last);
      if Value < 0 then
         First := 1;
         Buf (1) := '-';
      elsif Force_Sign then
         First := 1;
         Buf (1) := '+';
      end if;
      return Buf (First .. Buf'Last);
   end Image;

   -----------
   -- Split --
   -----------

   function Split
     (Str : String; On : Character) return GNAT.Strings.String_List_Access
   is
      First : Integer := Str'First;
      Count : Natural := 1;
      Result : GNAT.Strings.String_List_Access;
   begin
      for C in Str'Range loop
         if Str (C) = On then
            Count := Count + 1;
         end if;
      end loop;

      Result := new GNAT.Strings.String_List (1 .. Count);
      Count := 1;

      for C in Str'Range loop
         if Str (C) = On then
            Result (Count) := new String'(Str (First .. C - 1));
            First := C + 1;
            Count := Count + 1;
         end if;
      end loop;

      Result (Count) := new String'(Str (First .. Str'Last));
      return Result;
   end Split;

   -----------
   -- Split --
   -----------

   function Split
     (Str : String; On : Character) return Unbounded_String_Array
   is
      First : Integer := Str'First;
      Count : Natural := 1;

      use Ada.Strings.Unbounded;
   begin
      for C in Str'Range loop
         if Str (C) = On then
            Count := Count + 1;
         end if;
      end loop;

      declare
         Result : Unbounded_String_Array (1 .. Count);
      begin
         Count := 1;

         for C in Str'Range loop
            if Str (C) = On then
               Result (Count) := To_Unbounded_String (Str (First .. C - 1));
               First := C + 1;
               Count := Count + 1;
            end if;
         end loop;

         Result (Count) := To_Unbounded_String (Str (First .. Str'Last));
         return Result;
      end;
   end Split;

   ----------------
   -- Capitalize --
   ----------------

   function Capitalize (Name : String) return String is
      Result : String (Name'Range);
      J : Integer := Result'First;
   begin
      for N in Name'Range loop
         if Name (N) = '+' then
            Result (J) := 'p';
            J := J + 1;
         elsif Name (N) = '?' then
            Result (J) := 'U';
            J := J + 1;
         elsif Name (N) = '_'
           and then N > Name'First
           and then Name (N - 1) = '_'
         then
            null;
         elsif Name (N) >= ' ' and then Name (N) <= '/' then
            Result (J) := '_';
            J := J + 1;
         elsif J = Result'First
           or else Result (J - 1) = '_'
         then
            Result (J) := To_Upper (Name (N));
            J := J + 1;

         else
            Result (J) := To_Lower (Name (N));
            J := J + 1;
         end if;
      end loop;
      return Result (Result'First .. J - 1);
   end Capitalize;

   ---------------
   -- Ends_With --
   ---------------

   function Ends_With (Str : String; Suffix : String) return Boolean is
      pragma Suppress (All_Checks);
   begin
      --  This version is slightly faster than checking
      --     return Tail (File_Name, Suffix'Length) = Suffix;
      --  which needs a function returning a string.

      if Str'Length < Suffix'Length then
         return False;
      end if;

      --  Do the loop in reverse, since it likely that Suffix starts with '.'
      --  In the GPS case, it is also often the case that suffix starts with
      --  '.ad' for Ada extensions
      for J in reverse Suffix'Range loop
         if Str (Str'Last + J - Suffix'Last) /= Suffix (J) then
            return False;
         end if;
      end loop;

      return True;
   end Ends_With;

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With (Str : String; Suffix : String) return Boolean is
      pragma Suppress (All_Checks);
   begin
      if Str'Length < Suffix'Length then
         return False;
      end if;

      return Str (Str'First .. Str'First + Suffix'Length - 1) = Suffix;
   end Starts_With;

end GNATCOLL.Utils;
