------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Calendar;               use Ada.Calendar;
with Ada.Calendar.Time_Zones;    use Ada.Calendar.Time_Zones;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Command_Line;
with GNAT.Calendar;              use GNAT.Calendar;
with GNAT.Calendar.Time_IO;      use GNAT.Calendar.Time_IO;
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

   -------------------
   -- Is_Whitespace --
   -------------------

   function Is_Whitespace (Char : Character) return Boolean is
   begin
      if Char = ' '
        or else Char = ASCII.HT
        or else Char = ASCII.LF
        or else Char = ASCII.CR
      then
         return True;
      end if;
      return False;
   end Is_Whitespace;

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

   -------------
   -- Replace --
   -------------

   procedure Replace
     (S           : in out Ada.Strings.Unbounded.Unbounded_String;
      Pattern     : String;
      Replacement : String)
   is
      use Ada.Strings.Unbounded;
      Ind : Natural := Index_Non_Blank (S);
   begin
      while Ind < Length (S) loop
         Ind := Index (S, Pattern, Ind);

         exit when Ind = 0;

         S := Replace_Slice (S, Ind, Ind + Pattern'Length - 1, Replacement);
         Ind := Ind + Replacement'Length;
      end loop;
   end Replace;

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
      Exec_Name : constant String := Ada.Command_Line.Command_Name;

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
            (S, Resolve_Links => True);
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
      --  There is a potential issue here (see K112-046) where GNAT.OS_Lib
      --  will in fact return any non-executable file found in the PATH,
      --  whereas shells only consider executable files. As a result, the
      --  user might end up with a wrong directory, not matching the one
      --  found by the shell.

      declare
         Ex  : String_Access := GNAT.OS_Lib.Locate_Exec_On_Path (Exec_Name);
         Dir : constant String := Get_Install_Dir (Ex.all);
      begin
         Free (Ex);
         return Dir;
      end;
   end Executable_Location;

   -----------------
   -- Skip_Blanks --
   -----------------

   procedure Skip_Blanks (Str : String; Index : in out Natural) is
   begin
      while Index <= Str'Last
        and then Is_Whitespace (Str (Index))
      loop
         Index := Index + 1;
      end loop;
   end Skip_Blanks;

   --------------------------
   -- Skip_Blanks_Backward --
   --------------------------

   procedure Skip_Blanks_Backward (Str : String; Index : in out Natural) is
   begin
      while Index >= Str'First
        and then Is_Whitespace (Str (Index))
      loop
         Index := Index - 1;
      end loop;
   end Skip_Blanks_Backward;

   ---------------
   -- Find_Char --
   ---------------

   function Find_Char (Str : String; Char : Character) return Natural is
      Last : Natural := Str'First;
   begin
      while Last <= Str'Last and then Str (Last) /= Char loop
         Last := Last + 1;
      end loop;
      return Last;
   end Find_Char;

   ---------
   -- EOL --
   ---------

   function EOL (Str : String) return Natural is
   begin
      return Find_Char (Str, ASCII.LF);
   end EOL;

   ----------
   -- Join --
   ----------

   function Join
     (Str : String; List : GNAT.Strings.String_List) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for L in List'Range loop
         if List (L) /= null then
            if Result /= Null_Unbounded_String then
               Append (Result, Str);
            end if;

            Append (Result, List (L).all);
         end if;
      end loop;
      return To_String (Result);
   end Join;

   ---------------------
   -- Strip_Character --
   ---------------------

   function Strip_Character (Text : String; C : Character) return String is
      pragma Suppress (All_Checks);

      To       : String (1 .. Text'Length);
      Index_To : Positive := 1;

   begin
      for Index in Text'Range loop
         if Text (Index) /= C then
            To (Index_To) := Text (Index);
            Index_To := Index_To + 1;
         end if;
      end loop;

      return To (1 .. Index_To - 1);
   end Strip_Character;

   --------------
   -- Strip_CR --
   --------------

   function Strip_CR (Text : String) return String is
   begin
      return Strip_Character (Text, ASCII.CR);
   end Strip_CR;

   ------------------------
   -- Get_Command_Output --
   ------------------------

   function Get_Command_Output
     (Command : access GNAT.Expect.Process_Descriptor'Class) return String
   is
      use GNAT.Expect;

      Output : String_Access := new String (1 .. 1024);
      --  Buffer used to accumulate standard output from the launched
      --  command, expanded as necessary during execution.

      Last : Integer := 0;
      --  Index of the last used character within Output

      Status     : Integer;

   begin
      declare
         Result : Expect_Match;
         pragma Unreferenced (Result);

      begin
         --  This loop runs until the call to Expect raises Process_Died

         loop
            Expect (Command.all, Result, ".+");

            declare
               NOutput : String_Access;
               S       : constant String := Expect_Out (Command.all);
               pragma Assert (S'Length > 0);

            begin
               --  Expand buffer if we need more space. Note here that we add
               --  S'Length to ensure that S will fit in the new buffer size.

               if Last + S'Length > Output'Last then
                  NOutput := new String (1 .. 2 * Output'Last + S'Length);
                  NOutput (Output'Range) := Output.all;
                  Free (Output);

                  --  Here if current buffer size is OK

               else
                  NOutput := Output;
               end if;

               NOutput (Last + 1 .. Last + S'Length) := S;
               Last := Last + S'Length;
               Output := NOutput;
            end;
         end loop;

      exception
         when Process_Died =>
            Close (Command.all, Status);
      end;

      if Last = 0 then
         Free (Output);
         return "";
      end if;

      declare
         S : constant String := Output (1 .. Last);
      begin
         Free (Output);
         return S;
      end;
   end Get_Command_Output;

   ----------------
   -- Time_Value --
   ----------------

   function Time_Value (Str : String) return Ada.Calendar.Time is
      First  : Integer := Str'First;
      Last   : Integer := Str'Last;
      TZ     : Integer := Integer'Last;
      Result : Ada.Calendar.Time;
   begin
      --  Do we have the name of the day at the beginning of the string, as in
      --     Tue, 19 Dec 2006 13:59:04 +0000

      if Str'Length > 4 and then Str (First + 3) = ',' then
         First := First + 5;
      end if;

      --  Do we have a timezone ? Postgres outputs these with the
      --  following suffix: "+02" or "-02". This only applies when the time is
      --  also given, ie the field is long enough (8 chars for time + at least
      --  8 for date (01/02/02)

      if Str'Length > 16
        and then (Str (Str'Last - 2) = '+'
                  or else Str (Str'Last - 2) = '-')
      then
         Last := Str'Last - 3;
         TZ   := Integer'Value (Str (Str'Last - 2 .. Str'Last));
      end if;

      --  Postgres includes subseconds in the display. We should ignore them,
      --  they are not needed in our context

      for S in reverse First .. Last loop
         if Str (S) = '.' then
            Last := S - 1;
            exit;
         end if;
      end loop;

      Result := GNAT.Calendar.Time_IO.Value (Str (First .. Last));

      --  Take into account the timezone specified in the time

      if TZ /= Integer'Last then
         Result := Result - Duration (TZ * 3600);
      end if;

      --  GNAT.Calendar.Time_IO has added an time offset for the local
      --  timezone, as opposed to what Ada.Calendar.Formatting.Time_Of does.
      --  Therefore, we remove that extra offset. This also ensures that Image
      --  behaves the same whether the time was created through
      --  Accounts.Value (ie GNAT.Calendar.Time_Of) or through
      --  GNATCOLL.Email.Utils.To_Time (ie Ada.Calendar.Formatting.Time_Of).
      --  The drawback is that this requires a system call every time we
      --  convert a string to a time, and then every time we convert a time to
      --  a string. A more efficient approach could have been that
      --  GNATCOLL.Email.Utils.To_Time would use GNAT.Calendar, but GNATCOLL
      --  really should output UTC date I think.

      return Result
        + Duration (Ada.Calendar.Time_Zones.UTC_Time_Offset (Result)) * 60.0;

   exception
      when Constraint_Error =>
         return GNAT.Calendar.No_Time;
   end Time_Value;

end GNATCOLL.Utils;
