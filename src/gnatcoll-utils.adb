------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
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

with Ada.Calendar.Formatting;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Strings.Fixed;          use Ada.Strings;
with Ada.Strings.Maps;           use Ada.Strings.Maps;
with GNAT.Calendar.Time_IO;
with GNAT.Case_Util;
with GNAT.OS_Lib;
with GNAT.Strings;               use GNAT.Strings;

package body GNATCOLL.Utils is

   use Ada.Calendar.Time_Zones;

   OpenVMS_Host : Boolean := False;

   function Count_For_Split
     (Str              : String;
      On               : Character;
      Omit_Empty_Lines : Boolean := True)
      return Natural;
   --  Returns the number of strings that will occur after splitting Str on On.

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

   function Replace
     (S : String; Pattern : String; Replacement : String) return String
   is
      Idx : Natural;
   begin
      Idx := Fixed.Index (S, Pattern);

      if Idx = 0 then
         return S;
      else
         return S (S'First .. Idx - 1) & Replacement
                & Replace
                    (S           => S (Idx + Pattern'Length .. S'Last),
                     Pattern     => Pattern,
                     Replacement => Replacement);
      end if;
   end Replace;

   ---------------------
   -- Count_For_Split --
   ---------------------

   function Count_For_Split
     (Str              : String;
      On               : Character;
      Omit_Empty_Lines : Boolean := True)
      return Natural
   is
      Count : Natural := 0;
      C      : Integer;
   begin
      if Str = "" then
         return 0;
      end if;

      C := Str'First;
      while C <= Str'Last loop
         if Str (C) = On then
            Count := Count + 1;

            if Omit_Empty_Lines then
               while C <= Str'Last and then Str (C) = On loop
                  C := C + 1;
               end loop;
            else
               C := C + 1;
            end if;
         else
            C := C + 1;
         end if;
      end loop;

      if Str (Str'Last) /= On then
         Count := Count + 1;
      end if;
      return Count;
   end Count_For_Split;

   -----------
   -- Split --
   -----------

   function Split
     (Str              : String;
      On               : Character;
      Omit_Empty_Lines : Boolean := True)
      return GNAT.Strings.String_List_Access
   is
      Total  : constant Natural := Count_For_Split (Str, On, Omit_Empty_Lines);
      Result : constant GNAT.Strings.String_List_Access :=
        new GNAT.Strings.String_List (1 .. Total);
      First  : Integer := Str'First;
      Count  : Natural := 1;
      C      : Integer := Str'First;
   begin
      if Total = 0 then
         return Result;
      end if;

      while C <= Str'Last loop
         if Str (C) = On then
            Result (Count) := new String'(Str (First .. C - 1));

            if Omit_Empty_Lines then
               while C <= Str'Last and then Str (C) = On loop
                  C := C + 1;
               end loop;
            else
               C := C + 1;
            end if;

            First := C;
            Count := Count + 1;

         else
            C := C + 1;
         end if;
      end loop;

      if First <= Str'Last then
         Result (Count) := new String'(Str (First .. Str'Last));
      end if;

      return Result;
   end Split;

   -----------
   -- Split --
   -----------

   function Split
     (Str              : String;
      On               : Character;
      Omit_Empty_Lines : Boolean := True) return Unbounded_String_Array
   is
      use Ada.Strings.Unbounded;
      Total  : constant Natural := Count_For_Split (Str, On, Omit_Empty_Lines);
      Result : Unbounded_String_Array (1 .. Total);
      First  : Integer := Str'First;
      Count  : Natural := 1;
      C      : Integer := Str'First;

   begin
      if Total = 0 then
         return Result;
      end if;

      while C <= Str'Last loop
         if Str (C) = On then
            Result (Count) := To_Unbounded_String (Str (First .. C - 1));
            Count := Count + 1;

            if Omit_Empty_Lines then
               while C <= Str'Last and then Str (C) = On loop
                  C := C + 1;
               end loop;
            else
               C := C + 1;
            end if;

            First := C;

         else
            C := C + 1;
         end if;
      end loop;

      if First <= Str'Last then
         Result (Count) := To_Unbounded_String (Str (First .. Str'Last));
      end if;

      return Result;
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

      --  When no timezone is specified by the user, UTC is assumed.
      TZ      : Duration := 0.0;
      TZ_Mark : constant Character_Set := To_Set ("+-");

   begin
      if Str = "" then
         return No_Time;
      end if;

      --  Do we have the name of the day at the beginning of the string, as in
      --     Tue, 19 Dec 2006 13:59:04+00

      if Str'Length > 4 and then Str (First + 3) = ',' then
         First := First + 5;
      end if;

      --  Check for presence of time zone information in the various formats
      --  specified by ISO8601. This only applies when the time is also given,
      --  i.e. the value is long enough (8 chars for time + at least 8 for
      --  date (01/02/02).

      if Str'Length > 16 then
         if Is_In (Str (Last - 2), TZ_Mark) then
            --  [+-]HH

            TZ   := -Duration'Value (Str (Last - 2 .. Last)) * 3600;
            Last := Last - 3;

         elsif Is_In (Str (Last - 4), TZ_Mark) then
            --  [+-]HHMM

            TZ := -Duration'Value (Str (Last - 4 .. Last - 2)) * 3600
              - Duration'Value (Str (Last - 4) & Str (Last - 1 .. Last)) * 60;
            Last := Last - 5;

         elsif Is_In (Str (Last - 5), TZ_Mark)
           and then Str (Last - 2) = ':'
         then
            --  [+-]HH:MM

            TZ := -Duration'Value (Str (Last - 5 .. Last - 3)) * 3600
              - Duration'Value (Str (Last - 5) & Str (Last - 1 .. Last)) * 60;
            Last := Last - 6;

         end if;
      end if;

      --  Special case: UTC time zone speficied as 'Z'

      if Str'Length > 1 and then Str (Last) = 'Z' then
         Last := Last - 1;
         TZ := 0.0;   --  date is given as UTC
      end if;

      --  Ignore fraction of second

      for S in reverse First .. Last loop
         if Str (S) = '.' then
            Last := S - 1;
            exit;
         end if;
      end loop;

      --  In ISO format, the separator between date and time is 'T', whereas
      --  GNAT.Calendar.Time_IO expects as space.

      declare
         S2       : String := Str (First .. Last);
         Local    : Ada.Calendar.Time;
         Local_TZ : Duration;
      begin
         for S in S2'Range loop
            if S2 (S) = 'T' then
               S2 (S) := ' ';
               exit;
            end if;
         end loop;

         Local := GNAT.Calendar.Time_IO.Value (S2);

         --  GNAT.Calendar.Time_IO.Value uses Ada.Calendars.Time_Of, which
         --  for GNAT assumes the input date is in the local time zone.
         --  Local_TZ is used to compensated that offset.

         Local_TZ := Duration
           (Ada.Calendar.Time_Zones.UTC_Time_Offset (Local)) * 60.0;

         --  Time zone offset (in seconds) for the local timezone
         return Local + TZ + Local_TZ;
      end;

   exception
      when Constraint_Error =>
         return No_Time;
   end Time_Value;

   --------------
   -- Truncate --
   --------------

   function Truncate
     (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0) return Time
   is
      Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number;
      Dum1  : Day_Duration;
      Dum2  : Boolean;
   begin
      Formatting.Split
        (Date, Year, Month, Day, Dum1, Leap_Second => Dum2,
         Time_Zone => Time_Zone);
      return Formatting.Time_Of (Year, Month, Day, Time_Zone => Time_Zone);
   end Truncate;

   ----------------
   -- Line_Start --
   ----------------

   function Line_Start (Str : String; P : Natural) return Natural is
      Index : Natural := Natural'Min (Str'Last, P);
   begin
      if P <= Str'First then
         return P;
      end if;

      if Str (Index) = ASCII.LF then
         Index := Index - 1;

         if Str (Index) = ASCII.LF then
            return Index + 1;
         elsif Str (Index) = ASCII.CR then
            if Index > Str'First then
               Index := Index - 1;

               if Str (Index) = ASCII.LF then
                  return Index + 1;
               end if;
            else
               return Str'First;
            end if;
         end if;

      elsif Str (Index) = ASCII.CR then
         Index := Index - 1;

         if Str (Index) = ASCII.LF then
            return Index + 1;
         end if;
      end if;

      for J in reverse Str'First .. Index loop
         if Str (J) = ASCII.LF or else Str (J) = ASCII.CR then
            if J < Str'Last then
               return J + 1;
            else
               return Str'Last;
            end if;
         end if;
      end loop;

      return Str'First;
   end Line_Start;

   --------------
   -- Line_End --
   --------------

   function Line_End (Str : String; P : Natural) return Natural is
   begin
      for J in P .. Str'Last loop
         if Str (J) = ASCII.LF or else Str (J) = ASCII.CR then
            return J - 1;
         end if;
      end loop;

      return Str'Last;
   end Line_End;

   ---------------
   -- Next_Line --
   ---------------

   function Next_Line (Str : String; P : Natural) return Natural is
   begin
      for J in P .. Str'Last - 1 loop
         if Str (J) = ASCII.LF then
            return J + 1;
         end if;
      end loop;

      return Str'Last;
   end Next_Line;

   -------------------
   -- Previous_Line --
   -------------------

   function Previous_Line (Str : String; P : Natural) return Natural is
      Index : constant Natural := Line_Start (Str, P);
   begin
      if Index > Str'First then
         return Line_Start (Str, Index - 1);
      else
         return Str'First;
      end if;
   end Previous_Line;

   ----------------
   -- Skip_Lines --
   ----------------

   procedure Skip_Lines
     (Str           : String;
      Lines         : Integer;
      Index         : in out Natural;
      Lines_Skipped : out Natural)
   is
      Index_Saved : Natural;
   begin
      Lines_Skipped := 0;

      if Lines >= 0 then
         while Lines_Skipped < Lines loop
            Index := Next_Line (Str, Index);

            if Index = Str'Last then
               Index := Line_Start (Str, Index);
               exit;
            end if;

            Lines_Skipped := Lines_Skipped + 1;
         end loop;
      else
         Index_Saved := Line_Start (Str, Index);

         while Lines_Skipped < -Lines loop
            Index := Previous_Line (Str, Index);

            exit when Index = Index_Saved;

            Lines_Skipped := Lines_Skipped + 1;
         end loop;
      end if;
   end Skip_Lines;

   -------------------
   -- Is_Blank_Line --
   -------------------

   function Is_Blank_Line
     (Str : String; Index : Natural := 0) return Boolean
   is
      It : Natural := Index;
   begin
      if It = 0 then
         It := Str'First;
      end if;

      if It >= Str'First then
         while It <= Str'Last
           and then Str (It) /= ASCII.CR
           and then Str (It) /= ASCII.LF
         loop
            if Str (It) /= ' '
              and then Str (It) /= ASCII.HT
            then
               return False;
            end if;

            It := It + 1;
         end loop;
      end if;

      return True;
   end Is_Blank_Line;

   --------------------
   -- Skip_To_String --
   --------------------

   procedure Skip_To_String
     (Str       : String;
      Index     : in out Natural;
      Substring : String)
   is
      L : constant Natural := Substring'Length - 1;
   begin
      while Index + L <= Str'Last
        and then Str (Index .. Index + L) /= Substring
      loop
         Index := Index + 1;
      end loop;
   end Skip_To_String;

   -----------------------
   -- Forward_UTF8_Char --
   -----------------------

   function Forward_UTF8_Char
     (Str : String; Index : Integer) return Integer
   is
      type Unicode_Char is mod 2**32;
      C : constant Unicode_Char := Character'Pos (Str (Index));
   begin
      --  Compute the length of the encoding given what was in the first byte
      if C < 128 then
         return Index + 1;
      elsif (C and 16#E0#) = 16#C0# then
         return Index + 2;
      elsif (C and 16#F0#) = 16#E0# then
         return Index + 3;
      elsif (C and 16#F8#) = 16#F0# then
         return Index + 4;
      elsif (C and 16#FC#) = 16#F8# then
         return Index + 5;
      elsif (C and 16#FE#) = 16#FC# then
         return Index + 6;
      else
         --  Invalid encoding
         return Index + 1;
      end if;
   end Forward_UTF8_Char;

   --------------------
   -- Skip_To_Column --
   --------------------

   procedure Skip_To_Column
     (Str           : String;
      Columns       : Integer := 0;
      Index         : in out Integer;
      Tab_Width     : Integer := 8)
   is
      Current_Col   : Integer := 1;
   begin
      if Str = "" then
         return;
      end if;

      while Current_Col < Columns
        and then Natural (Index) <= Str'Last
        and then Str (Natural (Index)) /= ASCII.LF
      loop
         if Natural (Index) < Str'Last
           and then Str (Natural (Index)) = ASCII.HT
         then
            Current_Col := Current_Col
              + (Tab_Width - (Current_Col - 1) mod Tab_Width);
         else
            Current_Col := Current_Col + 1;
         end if;

         Index := Forward_UTF8_Char (Str, Natural (Index));
      end loop;
   end Skip_To_Column;

end GNATCOLL.Utils;
