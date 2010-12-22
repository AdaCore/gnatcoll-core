-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                    Copyright (C) 2008-2010, AdaCore               --
--                                                                   --
-- THis is free software;  you can redistribute it and/or modify  it --
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
with Interfaces.C.Strings;
with GNAT.Case_Util;
with GNAT.OS_Lib;
with GNAT.Strings;               use GNAT.Strings;

package body GNATCOLL.Utils is

   OpenVMS_Host : Boolean := False;

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

   ----------------------------
   -- Is_Directory_Separator --
   ----------------------------

   function Is_Directory_Separator (C : Character) return Boolean is
   begin
      --  In addition to the default directory_separator allow the '/' to
      --  act as separator since this is allowed in MS-DOS, Windows 95/NT,
      --  and OS2 ports. On VMS, the situation is more complicated because
      --  there are two characters to check for.

      return C = GNAT.OS_Lib.Directory_Separator
        or else C = '/'
        or else (OpenVMS_Host and then (C = ']' or else C = ':'));
   end Is_Directory_Separator;

   ----------------------
   -- Set_OpenVMS_Host --
   ----------------------

   procedure Set_OpenVMS_Host (Setting : Boolean := True) is
   begin
      OpenVMS_Host := Setting;
   end Set_OpenVMS_Host;

   -------------------------
   -- Executable_Location --
   -------------------------

   function Executable_Location return String is
      use Interfaces.C.Strings;

      type chars_ptr_ptr is access all chars_ptr;
      Argv : chars_ptr_ptr;
      pragma Import (C, Argv, "gnat_argv");

      Exec_Name : constant String := Value (Argv.all);

      function Get_Install_Dir (S : String) return String;
      --  S is the executable name preceeded by the absolute or relative
      --  path, e.g. "c:\usr\bin\gcc.exe" or "..\bin\gcc". Returns the absolute
      --  or relative directory where "bin" lies (in the example "C:\usr"
      --  or ".."). If the executable is not a "bin" directory, return "".

      ---------------------
      -- Get_Install_Dir --
      ---------------------

      function Get_Install_Dir (S : String) return String is
         Exec      : String  := GNAT.OS_Lib.Normalize_Pathname
            (S, Resolve_Links => False);
         Path_Last : Integer := 0;

      begin
         for J in reverse Exec'Range loop
            if Is_Directory_Separator (Exec (J)) then
               Path_Last := J - 1;
               exit;
            end if;
         end loop;

         if Path_Last >= Exec'First + 2 then
            GNAT.Case_Util.To_Lower (Exec (Path_Last - 2 .. Path_Last));
         end if;

         --  If we are not in a bin/ directory

         if Path_Last < Exec'First + 2
           or else Exec (Path_Last - 2 .. Path_Last) /= "bin"
           or else (Path_Last - 3 >= Exec'First
                    and then not Is_Directory_Separator (Exec (Path_Last - 3)))
         then
            return Exec (Exec'First .. Path_Last)
               & GNAT.OS_Lib.Directory_Separator;

         else
            --  Skip bin/, but keep the last directory separator
            return Exec (Exec'First .. Path_Last - 3);
         end if;
      end Get_Install_Dir;

   --  Beginning of Executable_Location

   begin
      --  First determine if a path prefix was placed in front of the
      --  executable name.

      for J in reverse Exec_Name'Range loop
         if Is_Directory_Separator (Exec_Name (J)) then
            return Get_Install_Dir (Exec_Name);
         end if;
      end loop;

      --  If you are here, the user has typed the executable name with no
      --  directory prefix.

      declare
         Ex : String_Access := GNAT.OS_Lib.Locate_Exec_On_Path (Exec_Name);
         Dir : constant String := Get_Install_Dir (Ex.all);
      begin
         Free (Ex);
         return Dir;
      end;
   end Executable_Location;

end GNATCOLL.Utils;
