------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2006-2017, AdaCore                     --
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

pragma Ada_2012;

with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Hash_Case_Insensitive;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with Interfaces;              use Interfaces;
with System.WCh_Con;          use System.WCh_Con;
with GNAT.Decode_String;

pragma Warnings (Off);
--  Ada.Strings.Unbounded.Aux is an internal GNAT unit
with Ada.Strings.Unbounded.Aux;
pragma Warnings (On);

package body GNATCOLL.Email.Utils is

   U_Charset_US_ASCII : constant Unbounded_String :=
                          To_Unbounded_String (Charset_US_ASCII);

   package Decode_Shift_JIS is new GNAT.Decode_String (WCEM_Shift_JIS);
   package Decode_EUC is new GNAT.Decode_String (WCEM_EUC);
   package Decode_UTF8 is new GNAT.Decode_String (WCEM_UTF8);

   type Next_Char_Acc is
     access procedure (S : String; Index : in out Natural);
   --  Procedure moving Index from one character to the next in S,
   --  taking multi-byte encodings into account.

   procedure Single_Byte_Next_Char (S : String; Index : in out Natural);
   --  Default version for single-byte charsets, simply incrementing Index

   ---------------------------
   -- Single_Byte_Next_Char --
   ---------------------------

   procedure Single_Byte_Next_Char (S : String; Index : in out Natural) is
      pragma Unreferenced (S);
   begin
      Index := Index + 1;
   end Single_Byte_Next_Char;

   function Next_Char_For_Charset (Charset : String) return Next_Char_Acc is
      (if Charset = Charset_Shift_JIS
         then Decode_Shift_JIS.Next_Wide_Character'Access
       elsif Charset = Charset_EUC
         then Decode_EUC.Next_Wide_Character'Access
       elsif Charset = Charset_UTF_8
         then Decode_UTF8.Next_Wide_Character'Access
       else
         Single_Byte_Next_Char'Access);
   --  Next_Char procedure for the named Charset

   procedure Next_Char_Ignore_Invalid
     (NC    : Next_Char_Acc;
      S     : String;
      Index : in out Natural);
   pragma Inline (Next_Char_Ignore_Invalid);
   --  Call NC (S, Index), but if an exception is raised (e.g. due to
   --  an invalid encoding in S, fall back to incrementing Index by 1.

   ------------------------------
   -- Next_Char_Ignore_Invalid --
   ------------------------------

   procedure Next_Char_Ignore_Invalid
     (NC    : Next_Char_Acc;
      S     : String;
      Index : in out Natural)
   is
      Orig_Index : constant Natural := Index;
   begin
      NC (S, Index);
   exception
      when others =>
         Index := Orig_Index + 1;
   end Next_Char_Ignore_Invalid;

   function Needs_Quoting
      (Char   : Character;
       Where  : Region;
       Is_EOL : Boolean) return Boolean;
   --  Return True if C needs to be quoted when appearing in Region, False
   --  otherwise. Is_EOL indicates whether Char is last of its line.

   function Needs_Quoting
     (U      : Unbounded_String;
      Where  : Region;
      Is_EOL : Boolean) return Boolean;
   --  True if any non-whitespace character in U needs to be quoted per
   --  the above function. Is_EOL indicates whehter the last character of U is
   --  last of its line.

   procedure Read_Integer
     (S : String; Index : in out Integer; Value : out Integer);
   --  return the integer starting at Index, and moves Index after the integer

   procedure Skip_Comment (S : String; Index : in out Integer);
   --  Skip the comment, if any, that starts at Index.
   --  In RFC 2822, comments are between parenthesis, and can be nested

   procedure Skip_Quoted_String (S : String; Index : in out Integer);
   --  Skip a quoted string, taking properly into account the backslashes
   --  Index should point after the opening quote.

   procedure Parse_And_Skip_Address
     (From_C  : in out Charset_String_List.Cursor;
      From    : in out Integer;
      Address : out Email_Address);
   --  Parse the first email address at (From_C, From), and leaves them after
   --  it, so that if there are more addresses in From_C they can all be parsed
   --  easily.

   procedure Parse_And_Skip_Address
     (Str           : String;
      From          : in out Integer;
      Buffer        : in out Unbounded_String;
      Buffer_Has_At : in out Boolean;
      In_Quote      : in out Boolean;
      Comment       : in out Unbounded_String;
      Address       : in out Email_Address;
      Found         : out Boolean);
   --  Internal version of Parse_And_Skip_Address, which applies to a
   --  us-ascii string. It maintains internal data.
   --  In_Quote indicates whether we are initially within an open quote ("),
   --  and on exit whether we are still processing a quoted string.

   procedure Post_Process_Address
     (Address         : in out Email_Address;
      Buffer, Comment : Unbounded_String;
      Buffer_Has_At   : Boolean);
   --  Complete the data in Address, given Buffer and Comment that were
   --  generated by Parse_And_Skip_Address. This procedure should be called
   --  after Parse_And_Skip_Address, before returning an address to the user.

   Special_Chars : constant array (Character) of Boolean :=
     ('[' | ']' | '\' | '(' | ')' | '<' | '>' | '@' | ',' => True,
      ':' | ';' | '"' | '.' => True,
      others => False);

   Quoted_Chars : constant array (Character) of Boolean :=
     ('[' | ']' | '\' | '(' | ')' | '"' => True,
      others => False);

   Qp_Convert : constant array (Character) of Short_Integer :=
     ('0' => 0,  '1' => 1,  '2' => 2,  '3' => 3,  '4' => 4,
      '5' => 5,  '6' => 6,  '7' => 7,  '8' => 8,  '9' => 9,
      'A' => 10, 'B' => 11, 'C' => 12, 'D' => 13, 'E' => 14, 'F' => 15,
      'a' => 10, 'b' => 11, 'c' => 12, 'd' => 13, 'e' => 14, 'f' => 15,
      others => -1);

   type Byte is mod 256;
   Base64_Convert : constant array (Character) of Byte :=
     ('+' => 62, '/' => 63, '0' => 52, '1' => 53, '2' => 54, '3' => 55,
      '4' => 56, '5' => 57, '6' => 58, '7' => 59, '8' => 60, '9' => 61,
      'A' => 0,  'B' => 1,  'C' => 2,  'D' => 3,  'E' => 4,  'F' => 5,
      'G' => 6,  'H' => 7,  'I' => 8,  'J' => 9,  'K' => 10, 'L' => 11,
      'M' => 12, 'N' => 13, 'O' => 14, 'P' => 15, 'Q' => 16, 'R' => 17,
      'S' => 18, 'T' => 19, 'U' => 20, 'V' => 21, 'W' => 22, 'X' => 23,
      'Y' => 24, 'Z' => 25, 'a' => 26, 'b' => 27, 'c' => 28, 'd' => 29,
      'e' => 30, 'f' => 31, 'g' => 32, 'h' => 33, 'i' => 34, 'j' => 35,
      'k' => 36, 'l' => 37, 'm' => 38, 'n' => 39, 'o' => 40, 'p' => 41,
      'q' => 42, 'r' => 43, 's' => 44, 't' => 45, 'u' => 46, 'v' => 47,
      'w' => 48, 'x' => 49, 'y' => 50, 'z' => 51, others => -1);

   type Mod64 is mod 2 ** 6;
   To_Base64 : constant array (Mod64) of Character :=
     (00 => 'A',  1 => 'B',  2 => 'C',  3 => 'D',  4 => 'E',  5 => 'F',
       6 => 'G',  7 => 'H',  8 => 'I',  9 => 'J', 10 => 'K', 11 => 'L',
      12 => 'M', 13 => 'N', 14 => 'O', 15 => 'P', 16 => 'Q', 17 => 'R',
      18 => 'S', 19 => 'T', 20 => 'U', 21 => 'V', 22 => 'W', 23 => 'X',
      24 => 'Y', 25 => 'Z', 26 => 'a', 27 => 'b', 28 => 'c', 29 => 'd',
      30 => 'e', 31 => 'f', 32 => 'g', 33 => 'h', 34 => 'i', 35 => 'j',
      36 => 'k', 37 => 'l', 38 => 'm', 39 => 'n', 40 => 'o', 41 => 'p',
      42 => 'q', 43 => 'r', 44 => 's', 45 => 't', 46 => 'u', 47 => 'v',
      48 => 'w', 49 => 'x', 50 => 'y', 51 => 'z', 52 => '0', 53 => '1',
      54 => '2', 55 => '3', 56 => '4', 57 => '5', 58 => '6', 59 => '7',
      60 => '8', 61 => '9', 62 => '+', 63 => '/');

   Hex_Chars : constant array (0 .. 15) of Character := "0123456789ABCDEF";

   ------------------------
   -- Skip_Quoted_String --
   ------------------------

   procedure Skip_Quoted_String (S : String; Index : in out Integer) is
   begin
      while Index <= S'Last loop
         if S (Index) = '"' then
            Index := Index + 1;
            return;
         elsif S (Index) = '\' then
            Index := Index + 2;
         else
            Index := Index + 1;
         end if;
      end loop;
      --  There is no closing '"'
      Index := S'Last + 1;
   end Skip_Quoted_String;

   ------------------
   -- Skip_Comment --
   ------------------

   procedure Skip_Comment (S : String; Index : in out Integer) is
      Par   : Natural := 1;
   begin
      if S (Index) = '(' then
         Index := Index + 1;

         while Index <= S'Last loop
            if S (Index) = ')' then
               Par := Par - 1;
               if Par = 0 then
                  Index := Index + 1;
                  return;
               else
                  Index := Index + 1;
               end if;
            elsif S (Index) = '(' then
               Par := Par + 1;
               Index := Index + 1;
            elsif S (Index) = '\' then
               Index := Index + 2;
            else
               Index := Index + 1;
            end if;
         end loop;

         --   No closing ')'
         Index := S'Last + 1;
      end if;
   end Skip_Comment;

   ------------------
   -- Read_Integer --
   ------------------

   procedure Read_Integer
     (S : String; Index : in out Integer; Value : out Integer)
   is
      Start : constant Integer := Index;
   begin
      if S (Index) = '-' or else S (Index) = '+' then
         Index := Index + 1;
      end if;

      while Index <= S'Last
        and then S (Index) in '0' .. '9'
      loop
         Index := Index + 1;
      end loop;

      Value := Integer'Value (S (Start .. Index - 1));
   end Read_Integer;

   -------------
   -- To_Time --
   -------------

   function To_Time
     (Date   : String;
      Format : Time_Format := Time_RFC2822) return Ada.Calendar.Time
   is
      Index   : Integer := Date'First;
      Index2  : Integer;
      Year    : Year_Number  := Year_Number'First;
      Month   : Month_Number := Month_Number'First;
      Day     : Day_Number   := Day_Number'First;
      Seconds : Day_Duration := 0.0;
      TZ      : Time_Offset  := 0;

      Time_Error : exception;

      procedure Read_Day;
      procedure Read_Month;
      procedure Read_Year;
      procedure Read_Time;
      --  Read the day of month or the year
      procedure Read_Time_Zone;

      procedure Read_Day is
      begin
         Read_Integer (Date (Index .. Date'Last), Index, Value => Index2);
         Day := Day_Number (Index2);
         Skip_Whitespaces (Date (Index .. Date'Last), Index);
      end Read_Day;

      procedure Read_Month is
         pragma Warnings (Off);
         type Month_Name is
                (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
         pragma Warnings (On);
      begin
         Month :=
           Month_Name'Pos (Month_Name'Value (Date (Index .. Index + 2))) + 1;

         --  Some mailers print the month in full, not just the first three
         --  chars. Although this isn't part of the RFC 2822, we still want to
         --  handle these
         Index := Index + 3;
         while Index <= Date'Last
           and then not Is_Whitespace (Date (Index))
         loop
            Index := Index + 1;
         end loop;
         Skip_Whitespaces (Date (Index .. Date'Last), Index);
      exception
         when others =>
            --  This really means the date format is incorrect!
            null;
      end Read_Month;

      procedure Read_Year is
      begin
         Read_Integer (Date (Index .. Date'Last), Index, Value => Index2);
         if Index2 < 0 then
            raise Time_Error;
         elsif Index2 <= 49 then
            Year := Year_Number (2000 + Index2);
         elsif Index2 <= 99 then
            Year := Year_Number (1900 + Index2);
         else
            Year := Year_Number (Index2);
         end if;
         Skip_Whitespaces (Date (Index .. Date'Last), Index);
      end Read_Year;

      procedure Read_Time is
      begin
         Read_Integer (Date (Index .. Date'Last), Index, Value => Index2);
         Seconds := Seconds + Day_Duration (Index2) * 3600.0;
         if Date (Index) /= ':' then
            raise Time_Error;
         end if;
         Index := Index + 1;
         Read_Integer (Date (Index .. Date'Last), Index, Value => Index2);
         Seconds := Seconds + Day_Duration (Index2) * 60.0;
         if Date (Index) = ':' then
            Index := Index + 1;
            Read_Integer (Date (Index .. Date'Last), Index, Value => Index2);
            Seconds := Seconds + Day_Duration (Index2);
         end if;
         Skip_Whitespaces (Date (Index .. Date'Last), Index);
      end Read_Time;

      procedure Read_Time_Zone is
         TZ_Local : Integer;
         type Named_TZ is (AST, ADT, EST, EDT, CST, CDT, MST, MDT, PST, PDT);
         Named_TZ_Offset : constant array (Named_TZ) of Time_Offset :=
           (AST => -240,
            ADT => -180,
            EST => -300,
            EDT => -240,
            CST => -360,
            CDT => -300,
            MST => -420,
            MDT => -360,
            PST => -480,
            PDT => -420);
      begin
         --  Timezone (we might have none in badly formed dates)
         if Index < Date'Last then
            if Date (Index) = '-' or else Date (Index) = '+' or else
               Date (Index) in '0' .. '9'
            then
               Read_Integer (Date (Index .. Date'Last), Index,
                             Value => TZ_Local);
               TZ := Time_Offset ((TZ_Local / 100) * 60 + TZ_Local mod 100);
            else

               --  The timezone table does not include the military time zones
               --  defined in RFC822, other than Z. According to RFC1123, the
               --  description in RFC822 gets the signs wrong, so we can't rely
               --  on any such time zones. RFC1123 recommends that numeric
               --  timezone indicators be used instead of timezone names.

               if Date (Index .. Index + 1) = "UT"
                 or else Date (Index .. Index + 2) = "UTC"
                 or else Date (Index) = 'Z'
               then
                  TZ := 0;
               else
                  TZ := Named_TZ_Offset
                    (Named_TZ'Value (Date (Index .. Index + 2)));
               end if;
            end if;
         end if;

      exception
         when Constraint_Error =>
            --  Invalid time zone, just ignore
            null;
      end Read_Time_Zone;

   begin
      --  RFC 2822 format is
      --     [day-of-week] ","] day month-name year FWS time-of-day FWS tz
      --     year  := 4 * DIGIT | 2 * DIGIT
      --     day   := 1*2DIGIT
      --     time-of-day := hour ":" minute [":" second]
      --     tz    := (("+" | "-") 4DIGIT) | "UT" | "GMT" | ...
      --
      --  Envelope format is
      --     Tue Jan 24 14:48:49 2006 +0100

      Skip_Whitespaces (Date (Index .. Date'Last), Index);

      --  Day of week is optional, skip it

      case Format is
         when Time_RFC2822 =>
            Index2 := Next_Occurrence (Date (Index .. Date'Last), ',');
            if Index2 <= Date'Last then
               Index := Index2 + 1;
               Skip_Whitespaces (Date (Index .. Date'Last), Index);
            end if;

            Read_Day;
            Read_Month;
            Read_Year;
            Read_Time;

         when Time_Envelope =>
            Index := Index + 3;
            Skip_Whitespaces (Date (Index .. Date'Last), Index);

            Read_Month;
            Read_Day;
            Read_Time;
            Read_Year;
      end case;

      Read_Time_Zone;

      return Time_Of (Year, Month, Day, Seconds, Time_Zone => TZ);

   exception
      when Time_Error =>
         return No_Time;
      when Constraint_Error =>
         return No_Time;
   end To_Time;

   -----------------
   -- Format_Time --
   -----------------

   function Format_Time (Date : Ada.Calendar.Time) return String
   is
      Result : Unbounded_String;
      Y      : Year_Number;
      M      : Month_Number;
      D      : Day_Number;
      H      : Hour_Number;
      Min    : Minute_Number;
      S      : Second_Number;
      SS     : Second_Duration;
   begin
      Split
        (Date, Y, M, D, H, Min, S, SS, Time_Zone => 0);

      Result := To_Unbounded_String
        (Image (Integer (H), Min_Width => 2) & ":");
      Append (Result, Image (Integer (Min), Min_Width => 2) & ":");
      Append (Result, Image (Integer (S), Min_Width => 2));
      return To_String (Result);
   end Format_Time;

   -----------------
   -- Format_Date --
   -----------------

   Day_Names : constant array (Day_Name) of String (1 .. 3) :=
     (Monday    => "Mon",
      Tuesday   => "Tue",
      Wednesday => "Wed",
      Thursday  => "Thu",
      Friday    => "Fri",
      Saturday  => "Sat",
      Sunday    => "Sun");

   Month_Names : constant array (1 .. 12) of String (1 .. 3) :=
     (1  => "Jan", 2  => "Feb", 3  => "Mar",
      4  => "Apr", 5  => "May", 6  => "Jun",
      7  => "Jul", 8  => "Aug", 9  => "Sep",
      10 => "Oct", 11 => "Nov", 12 => "Dec");

   function Format_Date
     (Date         : Ada.Calendar.Time;
      Use_GMT      : Boolean := False;
      From_Line    : Boolean := False;
      No_TZ        : Boolean := False;
      Show_Time    : Boolean := True;
      Show_Seconds : Boolean := True;
      Show_Day     : Boolean := True) return String
   is
      Result : Unbounded_String;
      Y      : Year_Number;
      M      : Month_Number;
      D      : Day_Number;
      H      : Hour_Number;
      Min    : Minute_Number;
      S      : Second_Number;
      SS     : Second_Duration;
      TZ     : Time_Offset := 0;
      RFC_TZ : Integer;
      Unknown_TZ : Boolean := False;

   begin
      if not (Use_GMT or else No_TZ) then
         begin
            --  Number of minutes difference for the timezone

            TZ := UTC_Time_Offset (Date);

         exception
            when Unknown_Zone_Error =>
               Unknown_TZ := True;
         end;
      end if;

      --  We cannot use GNAT.Calendar.Time_IO for week days, since we always
      --  want the english names, not the locale's version.

      Split (Date, Y, M, D, H, Min, S, SS, Time_Zone => TZ);

      if Show_Day then
         --  Note: we can't just call Day_Of_Week (Date), since this gives the
         --  day of week for Date *in the local time zone*, and in No_TZ or
         --  Use_GMT mode we want the day of week for Date
         --  *in the UT time zone*. So, we conjure up another date whose year,
         --  month, and day number in month (and therefore day of week) in
         --  local time are the same as those of Date in GMT (namely, Y, M,
         --  and D).

         Result :=
           To_Unbounded_String
             (Day_Names (Day_Of_Week (Ada.Calendar.Time_Of (Y, M, D))));

         if From_Line then
            Append (Result, " ");
         else
            Append (Result, ", ");
         end if;
      end if;

      if not From_Line then
         Append (Result, Image (Integer (D), Min_Width => 2) & " ");
      end if;

      Append (Result, Month_Names (M) & ' ');

      if From_Line then
         Append (Result, Image (Integer (D), Min_Width => 2) & " ");
      else
         Append (Result, Image (Integer (Y), Min_Width => 2) & " ");
      end if;

      if Show_Time then
         Append (Result, Image (Integer (H), Min_Width => 2) & ":");
         Append (Result, Image (Integer (Min), Min_Width => 2));
         if Show_Seconds then
            Append (Result, ":" & Image (Integer (S), Min_Width => 2));
         end if;
      end if;

      if From_Line then
         Append (Result, " " & Image (Integer (Y), Min_Width => 2));
      end if;

      if not No_TZ then
         if Use_GMT then
            Append (Result, " GMT");
         elsif Unknown_TZ then
            Append (Result, " -0000");
         else
            RFC_TZ := Integer ((TZ / 60) * 100 + TZ mod 60);
            Append (Result, " " & Image (RFC_TZ, Min_Width => 4,
                                         Force_Sign => True));
         end if;
      end if;

      return To_String (Result);
   end Format_Date;

   -------------------
   -- Parse_Address --
   -------------------

   function Parse_Address (Email : String) return Email_Address is
      Index         : Integer := Email'First;
      Result        : Email_Address;
      Buffer        : Unbounded_String;
      Buffer_Has_At : Boolean := False;
      Comment       : Unbounded_String;
      Found         : Boolean;
      In_Quote      : Boolean := False;
   begin
      Parse_And_Skip_Address
        (Str           => Email,
         From          => Index,
         Buffer        => Buffer,
         Buffer_Has_At => Buffer_Has_At,
         In_Quote      => In_Quote,
         Comment       => Comment,
         Address       => Result,
         Found         => Found);
      Post_Process_Address
        (Address         => Result,
         Buffer          => Buffer,
         Comment         => Comment,
         Buffer_Has_At   => Buffer_Has_At);
      return Result;
   end Parse_Address;

   ----------------------------
   -- Parse_And_Skip_Address --
   ----------------------------

   procedure Parse_And_Skip_Address
     (Str           : String;
      From          : in out Integer;
      Buffer        : in out Unbounded_String;
      Buffer_Has_At : in out Boolean;
      In_Quote      : in out Boolean;
      Comment       : in out Unbounded_String;
      Address       : in out Email_Address;
      Found         : out Boolean)
   is
      Index   : Integer;
   begin
      if In_Quote then
         Index := From;
         Skip_Quoted_String (Str (Index .. Str'Last), Index);
         Address.Real_Name := Trim
           (To_Unbounded_String
              (Str (From + 1 .. Index - 2)), Ada.Strings.Both);
         From := Index + 1;
         In_Quote := False;
      end if;

      --  Skip spaces

      while From <= Str'Last
        and then (Str (From) = ASCII.LF
                  or else Str (From) = ASCII.CR
                  or else Str (From) = ASCII.HT
                  or else Str (From) = ' ')
      loop
         From := From + 1;
      end loop;

      --  Only parse the contents of us-ascii strings. The rest cannot
      --  contain email addresses nor comments anyway.

      while From <= Str'Last loop
         if From <= Str'Last then
            if Str (From) = '(' then
               --  A comment. Ignored in general, but if we do not have a
               --  real name, it is likely to be contained in this
               --  comment, which is what some old mailers used to do:
               --     report@gnat.com (Report)

               Index := From;
               Skip_Comment (Str (From .. Str'Last), Index);
               Append (Comment, Str (From + 1 .. Index - 2));
               From := Index;

            elsif Str (From) = '<' then
               --  The email address

               Index := From;
               while Index <= Str'Last
                 and then Str (Index) /= '>'
               loop
                  Index := Index + 1;
               end loop;

               Address.Address := To_Unbounded_String
                 (Str (From + 1 .. Index - 1));
               From := Index + 1;

            --  ',' is the standard separator in mail messages, but ';' is
            --  often used by users when manually typing a list of addresses
            elsif Str (From) = ','
              or else Str (From) = ';'
              or else Str (From) = ASCII.LF
              or else Str (From) = ASCII.CR
              or else Str (From) = ASCII.HT
              or else (Buffer_Has_At and then Str (From) = ' ')
            then
               --  End of current address
               From := From + 1;
               Found := True;
               return;

            elsif Str (From) = '"' then
               Index := From + 1;
               Skip_Quoted_String (Str (Index .. Str'Last), Index);

               if Index > Str'Last then
                  In_Quote := True;
               end if;

               Address.Real_Name := Trim
                 (To_Unbounded_String
                    (Str (From + 1 .. Index - 2)), Ada.Strings.Both);

               if Index <= Str'Last and then Str (Index) = ' ' then
                  From := Index + 1;
               else
                  From := Index;
               end if;

            else
               if Str (From) = '@' then
                  Buffer_Has_At := True;
               end if;

               Append (Buffer, Str (From));
               From := From + 1;
            end if;
         end if;
      end loop;

      Found := False;
   end Parse_And_Skip_Address;

   ----------------------------
   -- Parse_And_Skip_Address --
   ----------------------------

   procedure Parse_And_Skip_Address
     (From_C  : in out Charset_String_List.Cursor;
      From    : in out Integer;
      Address : out Email_Address)
   is
      use Charset_String_List;

      Buffer  : Unbounded_String;
      Comment : Unbounded_String;
      Buffer_Has_At : Boolean := False;

      In_Quote : Boolean := False;
      --  Quotes are not necessarily encoded, and we could have for instance:
      --     " =?iso-2022-jp?b?...?= " <foo@nowhere.jp>
      --  which is made of several strings: one for the opening quote, one for
      --  the encoded name, and a last one that includes the quote and the
      --  email address.

      Continue : Boolean;

      procedure Analyze (CS : Charset_String);
      --  Analyze a given element of the list. Done in nested procedure to
      --  avoid a copy of each element of Email

      procedure Analyze (CS : Charset_String) is
         Tmp     : Unbounded_String;
         Found   : Boolean := False;
      begin
         --  Only parse the contents of us-ascii strings. The rest cannot
         --  contain email addresses nor comments anyway

         if CS.Charset = Charset_US_ASCII then
            Parse_And_Skip_Address
              (Str           => To_String (CS.Contents),
               From          => From,
               Buffer        => Buffer,
               Buffer_Has_At => Buffer_Has_At,
               In_Quote      => In_Quote,
               Comment       => Comment,
               Address       => Address,
               Found         => Found);
         else
            --  Reencode, to preserve names in international charsets
            Encode (Str     => To_String (CS.Contents),
                    Charset => To_String (CS.Charset),
                    Where   => Addr_Header,
                    Result  => Tmp);
            Append (Buffer, Tmp);
         end if;

         Continue := not Found;
      end Analyze;

   --  Start of processing for Parse_And_Skip_Address

   begin
      Address := Null_Address;
      while Has_Element (From_C) loop
         Query_Element (From_C, Analyze'Unrestricted_Access);
         exit when not Continue;
         Next (From_C);
         From := 1;
      end loop;

      Post_Process_Address (Address, Buffer, Comment, Buffer_Has_At);
   end Parse_And_Skip_Address;

   --------------------------
   -- Post_Process_Address --
   --------------------------

   procedure Post_Process_Address
     (Address         : in out Email_Address;
      Buffer, Comment : Unbounded_String;
      Buffer_Has_At   : Boolean)
   is
      pragma Unreferenced (Buffer_Has_At);
   begin
      if Address.Address = Null_Unbounded_String then
         --  Ideally, we should test whether Buffer contains a @ string. But
         --  there are degenerate cases where we have an email address on its
         --  own with no @ sign, and we want to handle them for backward
         --  compatibility...
         Address.Address := Trim (Buffer, Ada.Strings.Both);

      else
         if Address.Real_Name = Null_Unbounded_String then
            if Buffer = Null_Unbounded_String then
               Address.Real_Name := Trim (Comment, Ada.Strings.Both);
            else
               Address.Real_Name := Trim (Buffer, Ada.Strings.Both);
            end if;
         end if;
      end if;
   end Post_Process_Address;

   ----------------
   -- To_Address --
   ----------------

   function To_Address
     (Address   : String;
      Real_Name : String := "") return Email_Address
   is
   begin
      return (Address   => To_Unbounded_String (Address),
              Real_Name => To_Unbounded_String (Real_Name));
   end To_Address;

   -------------------
   -- Get_Addresses --
   -------------------

   function Get_Addresses (Str : String) return Address_Set.Set is
      use Charset_String_List;
      L : Charset_String_List.List;
   begin
      Append (L, (Contents => To_Unbounded_String (Str),
                  Charset  => To_Unbounded_String (Charset_US_ASCII)));
      return Get_Addresses (L);
   end Get_Addresses;

   function Get_Addresses
     (Str : Charset_String_List.List) return Address_Set.Set
   is
      use Charset_String_List, Address_Set;
      C      : Charset_String_List.Cursor := First (Str);
      From   : Integer := 1;
      Result : Address_Set.Set;
      Addr   : Email_Address;
   begin
      while Has_Element (C) loop
         Parse_And_Skip_Address (C, From, Addr);
         if Addr /= Null_Address then
            Include (Result, Addr);
         end if;
      end loop;
      return Result;
   end Get_Addresses;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Addresses    : Address_Set.Set;
      Separator    : String := ", ";
      Address_Only : Boolean := False) return String
   is
      use Address_Set;
      Tmp : Unbounded_String;
      C   : Address_Set.Cursor := First (Addresses);
   begin
      while Has_Element (C) loop
         if Tmp /= Null_Unbounded_String then
            Append (Tmp, Separator);
         end if;

         if Address_Only then
            Append (Tmp, Element (C).Address);
         else
            Append (Tmp, Format_Address (Element (C)));
         end if;

         Next (C);
      end loop;
      return To_String (Tmp);
   end To_String;

   --------------------
   -- Format_Address --
   --------------------

   function Format_Address
     (Email   : Email_Address;
      Charset : String := Charset_US_ASCII) return Charset_String_List.List
   is
      L : Charset_String_List.List;

   begin
      --  If Charset is US-ASCII, we can't rely on RFC 2047 encoding to
      --  protect any special characters, so fall back to legacy formatting
      --  routine, which will do backslash-escaping as needed. If nothing
      --  needs quoting, don't bother to go trough RFC 2047 either.

      if Charset = Charset_US_ASCII
           or else not Needs_Quoting
                         (Email.Real_Name,
                          Is_EOL => False, Where => Addr_Header)
      then
         L.Append ((Contents =>
                      To_Unbounded_String
                        (Legacy_Format_Address
                           (Real    => To_String (Email.Real_Name),
                            Address => To_String (Email.Address))),
                    Charset  => U_Charset_US_ASCII));

      --  Case where we have a non-ASCII charset specified

      else
         --  Here we have a non-default Charset specified: RFC 2047 encoding
         --  will also take care of escaping special characters.

         L.Append ((Contents => Email.Real_Name,
                    Charset  => To_Unbounded_String (Charset)));

         --  Actual address must not be encoded in any way: add a separate
         --  US ASCII section.

         L.Append (Charset_String'
                     (Contents => " <" & Email.Address & ">",
                      Charset  => U_Charset_US_ASCII));
      end if;

      return L;
   end Format_Address;

   --------------------
   -- Format_Address --
   --------------------

   function Format_Address
     (Email   : Email_Address;
      Charset : String := Charset_US_ASCII) return Unbounded_String
   is
      Res : Unbounded_String;
   begin
      To_String (Format_Address (Email, Charset), Res);
      return Res;
   end Format_Address;

   ---------------------------
   -- Legacy_Format_Address --
   ---------------------------

   function Legacy_Format_Address
     (Real    : String;
      Address : String) return String
   is
      Has_Special : Boolean := False;
      --  True if Real contains any special character that needs to be
      --  escaped in an RFC 2822 address header.

   begin
      if Real = "" then
         return Address;

      else
         for C in Real'Range loop
            if Special_Chars (Real (C)) then
               Has_Special := True;
               exit;
            end if;
         end loop;

         if Has_Special then
            return '"' & Quote (Real) & """ <" & Address & '>';
         else
            return Quote (Real) & " <" & Address & '>';
         end if;
      end if;
   end Legacy_Format_Address;

   -----------
   -- Quote --
   -----------

   function Quote (Str : String) return String is
      Result : String (Str'First .. Str'Last * 2);
      Index  : Integer := Result'First;
   begin
      for C in Str'Range loop
         if Quoted_Chars (Str (C)) then
            Result (Index) := '\';
            Index := Index + 1;
         end if;
         Result (Index) := Str (C);
         Index := Index + 1;
      end loop;
      return Result (Result'First .. Index - 1);
   end Quote;

   -------------
   -- Unquote --
   -------------

   function Unquote (Str : String) return String is
      Result : String (Str'Range);
      Index  : Integer := Result'First;
      C      : Integer := Str'First;
   begin
      while C <= Str'Last loop
         if Str (C) = '\' and then C < Str'Last then
            Result (Index) := Str (C + 1);
            C := C + 1;
         else
            Result (Index) := Str (C);
         end if;
         C     := C + 1;
         Index := Index + 1;
      end loop;
      return Result (Result'First .. Index - 1);
   end Unquote;

   ----------
   -- Hash --
   ----------

   function Hash (Addr : Email_Address) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash_Case_Insensitive (To_String (Addr.Address));
   end Hash;

   --------------------
   -- Get_Recipients --
   --------------------

   function Get_Recipients
     (Msg          : Message'Class;
      Include_From : Boolean := False) return Address_Set.Set
   is
      use Address_Set;
      Iter   : Header_Iterator := Get_Headers (Msg);
      H      : Header;
      Result : Address_Set.Set;
   begin
      loop
         Next (Iter, H => H);
         exit when H = Null_Header;
         if Get_Name (H) = "to"
           or else Get_Name (H) = "cc"
           or else Get_Name (H) = "resent-to"
           or else Get_Name (H) = "resent-cc"
           or else (Include_From and then Get_Name (H) = "from")
         then
            --  ??? Should avoid extra copy here
            Union (Result, Get_Recipients (H));
         end if;
      end loop;
      return Result;
   end Get_Recipients;

   --------------------
   -- Get_Recipients --
   --------------------

   function Get_Recipients (H : Header'Class) return Address_Set.Set is
   begin
      if H.Contents = null then
         return Address_Set.Empty_Set;
      else
         return Get_Addresses (H.Contents.Value);
      end if;
   end Get_Recipients;

   -------------
   -- Flatten --
   -------------

   procedure Flatten
     (List   : Charset_String_List.List;
      Result : out Unbounded_String)
   is
      use Charset_String_List;
      C      : Charset_String_List.Cursor := First (List);
   begin
      Result := Null_Unbounded_String;
      while Has_Element (C) loop
         Append (Result, Element (C).Contents);
         Next (C);
      end loop;
   end Flatten;

   ---------------
   -- To_String --
   ---------------

   procedure To_String
     (List   : Charset_String_List.List;
      Result : out Unbounded_String;
      Where  : Any_Header := Other_Header)
   is
      use Charset_String_List;
      C   : Charset_String_List.Cursor := First (List);
      Tmp : Unbounded_String;
   begin
      Result := Null_Unbounded_String;
      while Has_Element (C) loop
         Encode
           (Str     => To_String (Element (C).Contents),
            Charset => To_String (Element (C).Charset),
            Where   => Where,
            Result  => Tmp);

         Append (Result, Tmp);
         Next (C);
      end loop;
   end To_String;

   -------------------------
   -- Domain_From_Address --
   -------------------------

   function Domain_From_Address (Email : String) return String is
   begin
      for E in Email'First .. Email'Last - 1 loop
         if Email (E) = '@' then
            return Email (E + 1 .. Email'Last);
         end if;
      end loop;
      return "";
   end Domain_From_Address;

   function Domain_From_Address (Email : Email_Address) return String is
   begin
      return Domain_From_Address (To_String (Email.Address));
   end Domain_From_Address;

   -----------------------
   -- Login_From_Address --
   ------------------------

   function Login_From_Address (Email : String) return String is
   begin
      for E in Email'First .. Email'Last loop
         if Email (E) = '@' then
            return Email (Email'First .. E - 1);
         end if;
      end loop;
      return Email;
   end Login_From_Address;

   function Login_From_Address (Email : Email_Address) return String is
   begin
      return Login_From_Address (To_String (Email.Address));
   end Login_From_Address;

   -------------------
   -- Needs_Quoting --
   -------------------

   function Needs_Quoting
      (Char   : Character;
       Where  : Region;
       Is_EOL : Boolean) return Boolean
   is
   begin
      if Char = ' ' or else Char = ASCII.HT then
         return Is_EOL or else Where in Any_Header;

      elsif Char = '='
              or else Char = '?'
              or else Character'Pos (Char) not in 32 .. 126
      then
         return True;

      else
         return Where = Addr_Header and then Special_Chars (Char);
      end if;
   end Needs_Quoting;

   function Needs_Quoting
     (U      : Unbounded_String;
      Where  : Region;
      Is_EOL : Boolean) return Boolean
   is
      use Ada.Strings.Unbounded.Aux;

      Str  : Big_String_Access;
      Last : Integer;
      EOL  : Boolean;
   begin
      Get_String (U, Str, Last);
      for J in Str'First .. Last loop
         EOL := Is_EOL and then J = Last;

         --  No need to quote whitespace unless at EOL

         if (Str (J) = ' ' or else Str (J) = ASCII.HT) and then not EOL then
            null;

         elsif Needs_Quoting (Str (J), Where, EOL) then
            return True;
         end if;
      end loop;
      return False;
   end Needs_Quoting;

   -----------------------------
   -- Quoted_Printable_Encode --
   -----------------------------

   procedure Quoted_Printable_Encode
     (Str           : String;
      Charset       : String;
      Max_Block_Len : Integer := Integer'Last;
      Where         : Region := Text;
      Result        : out Unbounded_String)
   is
      Block_Prefix    : constant String :=
        (if Where in Any_Header then "=?" & Charset & "?q?" else "");
      Block_Suffix    : constant String :=
        (if Where in Any_Header then "?=" else "");
      Block_Separator : constant String :=
        (if Where in Any_Header then " " else "=" & ASCII.LF);
      --  In Text, use a soft line break

      Current_Len : Natural := 0;
      Max         : constant Natural :=
        Integer'Min (Max_Block_Len, (if Where in Any_Header then 75 else 76))
          - Block_Prefix'Length
          - Block_Suffix'Length
          - (Block_Separator'Length - 1);
      --  Note: Block_Separator may produce a printable character, so must be
      --  counted against the limit.

      function Quote (S : String) return String;
      --  Encode all characters in S

      procedure Append (Substring : String; Splittable : Boolean);
      --  Append Substring to Result, taking into account the max line length.
      --  If Splittable is false, Substring cannot be cut

      -----------
      -- Quote --
      -----------

      function Quote (S : String) return String is
         P      : Integer;
         Result : String (1 .. 3 * S'Length);
         Last   : Integer := 0;
      begin
         for J in S'Range loop
            if S (J) = ' ' and then Where in Any_Header then
               Last := Last + 1;
               Result (Last) := '_';
            else
               Last := Last + 3;
               P := Character'Pos (S (J));
               Result (Last - 2 .. Last) :=
                 ('=', Hex_Chars (P / 16), Hex_Chars (P mod 16));
            end if;
         end loop;
         return Result (1 .. Last);
      end Quote;

      ------------
      -- Append --
      ------------

      procedure Append (Substring : String; Splittable : Boolean) is
         S : Integer := Substring'First;
      begin
         if Substring'Length = 0 then
            return;
         end if;
         if Splittable then
            while Substring'Last - S + 1 > Max - Current_Len loop
               if Current_Len = 0 then
                  Append (Result, Block_Prefix);
               end if;

               Append
                 (Result,
                  Substring (S .. S + Max - Current_Len - 1));
               Append (Result, Block_Suffix & Block_Separator);

               S           := S + Max - Current_Len;
               Current_Len := 0;  --  We just started a new line
            end loop;

            if Current_Len = 0 then
               Append (Result, Block_Prefix);
            end if;

            Append (Result, Substring (S .. Substring'Last));
            Current_Len := Current_Len + Substring'Last - S + 1;

         else
            if Current_Len + Substring'Length > Max then
               if Current_Len /= 0 then
                  Append (Result, Block_Suffix & Block_Separator);
               end if;
               Current_Len := 0;
               Append (Result, Block_Prefix);
               Append (Result, Substring);
               Current_Len := Substring'Length;
            else
               if Current_Len = 0 then
                  Append (Result, Block_Prefix);
               end if;
               Append (Result, Substring);
               Current_Len := Current_Len + Substring'Length;
            end if;
         end if;
      end Append;

      Start, Next, Last : Integer;
      --  Start of current encoded sequence,
      --  start of next encoded sequence,
      --  last element of previous encoded sequence.

      procedure Passthrough;
      --  Output previous span of unencoded characters, i.e.
      --  from Last + 1 to Start - 1.

      -----------------
      -- Passthrough --
      -----------------

      procedure Passthrough is
      begin
         Append (Str (Last + 1 .. Start - 1), Splittable => True);
      end Passthrough;

      Next_Char : constant Next_Char_Acc :=
        (if Where in Any_Header
         then Next_Char_For_Charset (Charset)
         else Single_Byte_Next_Char'Access);

   --  Start of processing for Quoted_Printable_Encode

   begin
      Result := Null_Unbounded_String;

      Next := Str'First;
      Last := Next - 1;
      loop
         Start := Next;
         exit when Start > Str'Last;

         --  Find end of possibly multibyte sequence starting at Start

         Next := Start;
         Next_Char_Ignore_Invalid (Next_Char, Str, Next);

         --  We encode single characters if needed, and always encode
         --  all multibyte characters.

         if Last > Start + 1
              or else
            Needs_Quoting (Str (Start), Where, Is_EOL => Start = Str'Last)
         then
            Passthrough;
            Last := Next - 1;
            Append (Quote (Str (Start .. Last)), Splittable => False);
         end if;
      end loop;
      Passthrough;

      if Current_Len /= 0 then
         Append (Result, Block_Suffix);
      end if;
   end Quoted_Printable_Encode;

   -----------------------------
   -- Quoted_Printable_Decode --
   -----------------------------

   procedure Quoted_Printable_Decode
     (Str    : String;
      Result : out Unbounded_String;
      Where  : Region := Text)
   is
      Start  : Integer := -1;
      S      : Integer;

      function Is_Hex (Char : Character) return Boolean;
      --  Return true if Char is an hexa character

      ------------
      -- Is_Hex --
      ------------

      function Is_Hex (Char : Character) return Boolean is
      begin
         return Qp_Convert (Char) >= 0;
      end Is_Hex;

   --  Start of processing for Quoted_Printable_Decode

   begin
      S := Str'First;
      Result := Null_Unbounded_String;

      while S <= Str'Last loop
         if Str (S) = '_' and then Where in Any_Header then
            --  Encoded SPACE
            if Start /= -1 then
               Append (Result, Str (Start .. S - 1));
               Start := -1;
            end if;
            Append (Result, ' ');

         elsif Str (S) /= '=' then
            --  Regular character
            if Start = -1 then
               Start := S;
            end if;

         elsif Str (S) = '='
           and then S + 1 <= Str'Last
           and then Str (S + 1) = ASCII.LF
         then
            --  Soft line break
            if Start /= -1 then
               Append (Result, Str (Start .. S - 1));
               Start := -1;
            end if;
            S := S + 1;

         elsif S + 2 <= Str'Last
           and then Is_Hex (Str (S + 1))
           and then Is_Hex (Str (S + 2))
         then
            --  Valid quote sequence
            if Start /= -1 then
               Append (Result, Str (Start .. S - 1));
               Start := -1;
            end if;
            Append
              (Result,
               Character'Val
                 (Qp_Convert (Str (S + 1)) * 16 + Qp_Convert (Str (S + 2))));
            S := S + 2;

         else
            --  Invalid quote sequence. Leave it as is
            if Start /= -1 then
               Append (Result, Str (Start .. S - 1));
               Start := -1;
            end if;
         end if;

         S := S + 1;
      end loop;

      if Start /= -1 then
         Append (Result, Str (Start .. Str'Last));
      end if;
   end Quoted_Printable_Decode;

   -------------------
   -- Base64_Encode --
   -------------------

   procedure Base64_Encode
     (Str             : String;
      Charset         : String;
      Max_Block_Len   : Integer := Integer'Last;
      Where           : Region := Text;
      Result          : out Unbounded_String)
   is
      Block_Prefix    : constant String :=
        (if Where in Any_Header then "=?" & Charset & "?b?" else "");
      Block_Suffix    : constant String :=
        (if Where in Any_Header then "?=" else "");
      Block_Separator : constant String :=
        (if Where in Any_Header then " " else (1 => ASCII.LF));
      --  In Text, use a soft line break as line separator

      Max : constant Natural :=
        4 * Integer'Max
              (1,
               (Integer'Min (Max_Block_Len,
                             (if Where in Any_Header then 75 else 76))
                  - Block_Prefix'Length
                  - Block_Suffix'Length) / 4);
      --  Maximum length of encoded data within an encoded block (must be
      --  a non-null multiple of 4). Note: block separator does not contain
      --  any printable character, so does not count against the limit.

      Encoded_Len : constant Integer := (Str'Length + 2) / 3 * 4;
      --  Each group of 3 input bytes yields 4 output bytes, including a
      --  trailing incomplete group.

      Blocks : constant Integer := (Encoded_Len + Max - 1) / Max;

      Encoded_Block_Len : constant Integer :=
        Block_Prefix'Length + Max + Block_Suffix'Length
        + Block_Separator'Length;
      --  Allocation size for each block

      Len : constant Integer :=
          Blocks * Encoded_Block_Len;
      --  Pre-allocation length. This is sufficient to accomodate the entire
      --  encoded data, split into blocks, except if some blocks need to be
      --  flushed early (while incomplete) in order to avoid incorrect
      --  splitting of multi-byte sequences.

      Output   : Ada.Strings.Unbounded.String_Access := new String (1 .. Len);
      Index    : Integer := Output'First;
      Left     : Unsigned_16 := 0;
      Leftbits : Natural := 0;
      Ch       : Mod64;
      Current  : Integer := 0;

      procedure Append (Ch : Character);
      --  Append a new character to the output, splitting lines as necessary

      ------------
      -- Append --
      ------------

      procedure Append (Ch : Character) is
         New_Output : Ada.Strings.Unbounded.String_Access;
      begin
         if Current = 0 then
            --  Make sure that the string has sufficient space for the
            --  full new encoded block

            if Output'Last - Index + 1 < Encoded_Block_Len then
               New_Output :=
                 new String (1 .. Output'Length + Encoded_Block_Len);
               New_Output (1 .. Index - 1) := Output (1 .. Index - 1);
               Free (Output);
               Output := New_Output;
            end if;

            Output (Index .. Index + Block_Prefix'Length - 1) :=
              Block_Prefix;
            Index := Index + Block_Prefix'Length;
         end if;

         Output (Index) := Ch;
         Index          := Index + 1;
         Current        := Current + 1;

         if Current = Max then
            --  Append suffix and separator
            Output (Index .. Index
                               + Block_Suffix'Length
                               + Block_Separator'Length - 1) :=
              Block_Suffix & Block_Separator;
            Index := Index + Block_Suffix'Length + Block_Separator'Length;

            Current := 0;
         end if;
      end Append;

      procedure Encode_Append (Str : String);
      --  Encode Str and append result to output, splitting if necessary.
      --  If Where is Any_Header, then never split Str across two different
      --  blocks.

      procedure Flush;
      --  Flush pending bits, outputting padding characters if necessary

      procedure Encode_Append (Str : String) is
         Out_Chars : constant Integer := (Leftbits + Str'Length * 8 + 5) / 6;
      begin
         --  Force flushing now if in header mode and encoded sequence would
         --  exceed maximum length.

         if Where in Any_Header and then Current + Out_Chars > Max then
            Flush;
         end if;

         for J in Str'Range loop
            Left     := Shift_Left (Left, 8) or Character'Pos (Str (J));
            Leftbits := Leftbits + 8;

            while Leftbits >= 6 loop
               Ch       :=
                 Mod64 ((Shift_Right (Left, Leftbits - 6)) and 16#3f#);
               Leftbits := Leftbits - 6;
               Append (To_Base64 (Ch));
            end loop;
         end loop;
      end Encode_Append;

      -----------
      -- Flush --
      -----------

      procedure Flush is
      begin
         case Leftbits is
            when 0 =>
               null;

            when 2 =>
               Ch := Mod64 (Shift_Left (Left and 3, 4));
               Append (To_Base64 (Ch));
               Append ('=');
               Append ('=');

            when 4 =>
               Ch := Mod64 (Shift_Left (Left and 16#F#, 2));
               Append (To_Base64 (Ch));
               Append ('=');

            when others =>
               raise Program_Error with "invalid Base64 encoder state";
         end case;

         Left := 0;
         Leftbits := 0;
      end Flush;

      Start, Next : Integer;
      --  Start of current encoded sequence,
      --  start of next encoded sequence,
      --  last element of previous encoded sequence.

      Next_Char : constant Next_Char_Acc :=
        (if Where in Any_Header
         then Next_Char_For_Charset (Charset)
         else Single_Byte_Next_Char'Access);
      --  In message bodies, multi-byte encodings can be
      --  split across multiple lines; in headers, they can't
      --  be split across multiple encoded words.

   --  Start of processing for Base64_Encode

   begin
      Next := Str'First;
      loop
         Start := Next;
         exit when Start > Str'Last;

         --  Find end of possibly multibyte sequence starting at Start

         Next := Start;
         Next_Char_Ignore_Invalid (Next_Char, Str, Next);

         Encode_Append (Str (Start .. Next - 1));
      end loop;
      Flush;

      if Current = 0 then
         --  Remove last separator
         Index := Index - Block_Separator'Length;
      elsif Current > 0 then
         Output (Index .. Index + Block_Suffix'Length - 1) :=
           Block_Suffix;
         Index := Index + Block_Suffix'Length;
      end if;

      if Output'Last /= Index - 1 then
         Set_Unbounded_String (Result, Output (1 .. Index - 1));
      else
         Set_Unbounded_String (Result, Output.all);
      end if;

      Free (Output);
   end Base64_Encode;

   -------------------
   -- Base64_Decode --
   -------------------

   procedure Base64_Decode
     (Str    : String;
      Result : out Unbounded_String)
   is
      type Phase_Type is mod 4;
      --  We must use the heap rather than the stack here. In some
      --  cases, we'll be able to avoid a copy of the string anyway,
      --  and in case where this code is run in a multi-threaded
      --  application, the stack size is generally too small anyway
      Output   : Ada.Strings.Unbounded.String_Access :=
         new String (1 .. Str'Length * 6 / 8);
      Index    : Integer := Output'First;

      Phase    : Phase_Type := 0;
      D, Dlast : Byte := 0;

   begin
      for S in Str'First .. Str'Last loop
         if Str (S) = ASCII.LF
           or else Str (S) = ASCII.CR
         then
            Phase := 0;
            Dlast := 0;

         else
            D := Base64_Convert (Str (S));
            if D /= -1 then
               case Phase is
                  when 0 =>
                     Phase := Phase + 1;
                  when 1 =>
                     Output (Index) := Character'Val
                       (Dlast * 4 or ((D and 16#30#) / 16));
                     Index := Index + 1;
                     Phase := Phase + 1;
                  when 2 =>
                     Output (Index) := Character'Val
                       (((Dlast and 16#F#) * 16) or ((D and 16#3C#) / 4));
                     Index := Index + 1;
                     Phase := Phase + 1;
                  when 3 =>
                     Output (Index) := Character'Val
                       (((Dlast and 16#3#) * 64) or D);
                     Index := Index + 1;
                     Phase := 0;
               end case;

               Dlast := D;
            end if;
         end if;
      end loop;

      if Index - 1 = Output'Last then
         Set_Unbounded_String (Result, Output.all);
      else
         Set_Unbounded_String (Result, Output (Output'First .. Index - 1));
      end if;

      Free (Output);
   end Base64_Decode;

   ------------
   -- Encode --
   ------------

   procedure Encode
     (Str     : String;
      Charset : String := Charset_US_ASCII;
      Where   : Region := Text;
      Result  : out Unbounded_String)
   is
      Encoding          : Encoding_Type;
      Set               : constant String := To_Lower (Charset);

   begin
      --  Preferred encoding are the same as in Python

      if Set = Charset_US_ASCII then
         Encoding := Encoding_7bit;
      elsif Set = Charset_ISO_8859_1
        or else Set = "latin_1" or else Set = "latin-1"
        or else Set = Charset_ISO_8859_2
        or else Set = "latin_2" or else Set = "latin-2"
        or else Set = Charset_ISO_8859_3
        or else Set = "latin_3" or else Set = "latin-3"
        or else Set = Charset_ISO_8859_4
        or else Set = "latin_4" or else Set = "latin-4"
        or else Set = Charset_ISO_8859_9
        or else Set = "latin_5" or else Set = "latin-5"
        or else Set = Charset_ISO_8859_10
        or else Set = "latin_6" or else Set = "latin-6"
        or else Set = Charset_ISO_8859_13
        or else Set = "latin_7" or else Set = "latin-7"
        or else Set = Charset_ISO_8859_14
        or else Set = "latin_8" or else Set = "latin-8"
        or else Set = Charset_ISO_8859_15
        or else Set = "latin_9" or else Set = "latin-9"
        or else Set = Charset_Windows_1252
        or else Set = "viscii"
        or else Set = Charset_UTF_8 or else Set = "utf8"
      then
         Encoding := Encoding_QP;
      else
         Encoding := Encoding_Base64;
      end if;

      case Encoding is
         when Encoding_Base64 =>
            Base64_Encode
              (Str, Charset => Set, Where => Where, Result => Result);
         when Encoding_QP =>
            Quoted_Printable_Encode
              (Str, Charset => Set, Where => Where, Result => Result);
         when others =>
            Result := To_Unbounded_String (Str);
      end case;
   end Encode;

   -------------------
   -- Decode_Header --
   -------------------

   procedure Decode_Header
     (Str             : String;
      Default_Charset : String := Charset_US_ASCII;
      Result          : out Charset_String_List.List;
      Where           : Any_Header := Other_Header)
   is
      use Charset_String_List;

      Start    : Integer;
      Index    : Integer;
      Index2   : Integer;
      Section  : Charset_String;
      Encoding : Encoding_Type;
      S        : Integer;

      procedure Append (Section : Charset_String);
      --  Add Section to the result, merging with previous section if needed.
      --  If Section.Charset is empty, use Default_Charset, or Charset_US_ASCII
      --  if possible.

      ------------
      -- Append --
      ------------

      procedure Append (Section : Charset_String) is
         NSection : Charset_String := Section;
      begin
         if NSection.Charset = Null_Unbounded_String then
            declare
               Raw_Str  : Ada.Strings.Unbounded.Aux.Big_String_Access;
               Raw_Last : Integer;
            begin
               Ada.Strings.Unbounded.Aux.Get_String
                  (NSection.Contents, Raw_Str, Raw_Last);
               for J in Raw_Str'First .. Raw_Last loop
                  if Character'Pos (Raw_Str (J)) not in 32 .. 126 then
                     NSection.Charset := To_Unbounded_String (Default_Charset);
                     exit;
                  end if;
               end loop;
               if NSection.Charset = Null_Unbounded_String then
                  NSection.Charset := U_Charset_US_ASCII;
               end if;
            end;
         end if;

         --  Now append the new section to the sequence

         if Is_Empty (Result) then
            Append (Result, NSection);

         else
            --  An empty section between two encoded ones must be ignored

            if NSection.Charset /= Default_Charset
              and then Element (Last (Result)).Charset = Default_Charset
            then
               declare
                  Previous : constant Unbounded_String :=
                    Element (Last (Result)).Contents;
               begin
                  if Index_Non_Blank (Previous) < 1 then
                     Delete_Last (Result);
                  end if;
               end;
            end if;

            --  Try to merge Section with previous one, if possible

            if not Is_Empty (Result)
               and then NSection.Charset = Element (Last (Result)).Charset
            then
               Replace_Element
                 (Result, Last (Result),
                  (Contents =>
                     Element (Last (Result)).Contents & NSection.Contents,
                   Charset  => NSection.Charset));
            else
               Append (Result, NSection);
            end if;
         end if;
      end Append;

   --  Start of processing for Decode_Header

   begin
      Result := Charset_String_List.Empty_List;
      S      := Str'First;
      Start  := Str'First;
      while S < Str'Last loop
         if Str (S) = '='
           and then S < Str'Last
           and then Str (S + 1) = '?'
         then
            Index := Next_Occurrence (Str (S + 2 .. Str'Last), '?');
            if Index < Str'Last then
               Section.Charset :=
                 To_Unbounded_String (To_Lower (Str (S + 2 .. Index - 1)));

               case To_Lower (Str (Index + 1)) is
                  when 'q'    => Encoding := Encoding_QP;
                  when 'b'    => Encoding := Encoding_Base64;
                  when others => Encoding := Encoding_7bit;
               end case;

               if Encoding /= Encoding_7bit
                 and then Index + 2 < Str'Last
               then
                  if Str (Index + 2) = '?' then
                     --  So far we have the prefix =?<charset>?<encoding>?

                     Index2 := Index + 3;
                     Index := Next_Occurrence (Str (Index2 .. Str'Last), '?');
                     if Index < Str'Last and then Str (Index + 1) = '=' then
                        case Encoding is
                           when Encoding_QP   =>
                              Quoted_Printable_Decode
                                (Str (Index2 .. Index - 1),
                                 Where  => Where,
                                 Result => Section.Contents);
                           when Encoding_Base64 =>
                              Base64_Decode
                                (Str (Index2 .. Index - 1),
                                 Result => Section.Contents);
                           when others => null;
                        end case;

                        --  Deal with non-encoded-word part: charset is
                        --  set to Default_Charset, unless the string has
                        --  no character which need to be encoded, in which
                        --  case use US-ASCII instead.

                        if Start <= S - 1 then
                           declare
                              Raw_Section     : String
                                renames Str (Start .. S - 1);
                              --  Part of Str that is not an encoded-word
                           begin
                              Append
                                ((Contents =>
                                    To_Unbounded_String (Raw_Section),
                                  Charset  => Null_Unbounded_String));
                           end;
                        end if;

                        Append (Section);
                        S := Index + 2;
                        Start := S;
                     else
                        S := Index2;
                     end if;
                  else
                     S := Index + 1;
                  end if;
               else
                  S := Index + 1;
               end if;
            end if;
         end if;
         S := S + 1;
      end loop;

      if Start <= Str'Last then
         Append
           ((Contents => To_Unbounded_String (Str (Start .. Str'Last)),
             Charset  => Null_Unbounded_String));
      end if;
   end Decode_Header;

   -------------------
   -- Get_Main_Type --
   -------------------

   function Get_Main_Type (MIME_Type : String) return String is
   begin
      for M in MIME_Type'Range loop
         if MIME_Type (M) = '/' then
            return MIME_Type (MIME_Type'First .. M - 1);
         end if;
      end loop;
      return MIME_Type;
   end Get_Main_Type;

   ------------------
   -- Get_Sub_Type --
   ------------------

   function Get_Sub_Type (MIME_Type : String) return String is
   begin
      for M in MIME_Type'Range loop
         if MIME_Type (M) = '/' then
            if M + 1 <= MIME_Type'Last then
               return MIME_Type (M + 1 .. MIME_Type'Last);
            else
               return "";
            end if;
         end if;
      end loop;
      return MIME_Type;
   end Get_Sub_Type;

end GNATCOLL.Email.Utils;
