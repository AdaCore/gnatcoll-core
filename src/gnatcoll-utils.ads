------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2008-2014, AdaCore                     --
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

--  Various utility subprograms used in GNATCOLL, and that can easily be reused
--  elsewhere

with Ada.Calendar;
with Ada.Strings.Unbounded;
with GNAT.Expect;
with GNAT.Strings;

package GNATCOLL.Utils is

   type Cst_String_Access is access constant String;

   No_Time : constant Ada.Calendar.Time;
   --  A constant to indicate uninitialized time. Recent versions of GNAT
   --  provide this constant in the GNAT.Calendar package, but the constant is
   --  still defined here for compatibility with older compilers.
   --  Both constants must have the same value, though, because the user's code
   --  might be using one or the other indiscriminately.

   procedure Free (List : in out GNAT.Strings.String_List);
   --  Free the memory used by List.
   --  ??? This should be moved to GNAT.Strings itself in fact

   function Equal (S1, S2 : String; Case_Sensitive : Boolean) return Boolean;
   function Case_Insensitive_Equal (S1, S2 : String) return Boolean;
   pragma Inline (Equal, Case_Insensitive_Equal);
   --  Compare two strings

   function Image
     (Value      : Integer;
      Min_Width  : Integer;
      Force_Sign : Boolean := False;
      Padding    : Character := '0') return String;
   --  Return Value as a string, using at least Width digits (padded with
   --  leading characters Padding if necessary); negative values will always
   --  have a leading minus sign; positive values will have a leading plus sign
   --  if Force_Sign is True.
   --  If you set Min_Width to 1, the result is similar to 'Image, without the
   --  leading space for positive numbers.

   procedure Replace
     (S           : in out Ada.Strings.Unbounded.Unbounded_String;
      Pattern     : String;
      Replacement : String);
   --  Return S, with all occurrences of Pattern replaced with Replacement

   function Split
     (Str              : String;
      On               : Character;
      Omit_Empty_Lines : Boolean := True)
      return GNAT.Strings.String_List_Access;
   --  Split the string on the given character.
   --  The result depends on the value of Omit_Empty_Lines. For instance, the
   --  string    "a" & ASCII.LF & ASCII.LF & "b"   will be split as:
   --       ["a", "b"]  if Omit_Empty_Lines is true
   --       ["a", "", "b"] otherwise
   --
   --  Result must be freed by caller.
   --  See also Split below

   type Unbounded_String_Array is array (Natural range <>) of
     Ada.Strings.Unbounded.Unbounded_String;

   function Split
     (Str              : String;
      On               : Character;
      Omit_Empty_Lines : Boolean := True) return Unbounded_String_Array;
   --  Same as Split above, returning an Unbounded_String_Array that does not
   --  need to be freed.

   function Capitalize (Name : String) return String;
   --  Capitalize a string, ie put in upper case the first character and all
   --  characters following '_'

   function Is_Whitespace (Char : Character) return Boolean;
   --  Returns True if Char is a space, new line, or tab; otherwise returns
   --  False.

   function Starts_With (Str : String; Suffix : String) return Boolean;
   function Ends_With (Str : String; Suffix : String) return Boolean;
   --  Return True if Str starts or ends with Suffix

   procedure Skip_Blanks (Str : String; Index : in out Natural);
   procedure Skip_Blanks_Backward (Str : String; Index : in out Natural);
   --  If Str(Index) is a white space, new line, or tab, then skip it and all
   --  following ones. On exit, Index points to the first non white space
   --  character, or after Str'Last.
   --  Skip_Blanks_Backward moves Index backward instead, and will leave it
   --  before Str'First if no non-whitespace was found.

   function Find_Char (Str : String; Char : Character) return Natural;
   --  Return the first occurrence of Char after Str'First (use substrings for
   --  later occurrences).

   function Join (Str : String; List : GNAT.Strings.String_List) return String;
   --  Return a string that is the concatenation of the list elements,
   --  separated by Str: (List(1) & Str & List(2) & Str & ...)
   --  null elements in list are skipped

   function EOL (Str : String) return Natural;
   pragma Inline (EOL);
   --  Return the first end-of-line after Str'First (use substrings for later
   --  lines). The result is either Str'Last+1 or pointing to the first
   --  ASCII.LF found.

   function Line_Start (Str : String; P : Natural) return Natural;
   --  Return the start of the line pointed by P

   function Line_End (Str : String; P : Natural) return Natural;
   --  Return the end of the line pointed by P

   procedure Skip_Lines
     (Str           : String;
      Lines         : Integer;
      Index         : in out Natural;
      Lines_Skipped : out Natural);
   --  Skip Lines forward or backward. Index is set to the beginning of a line.
   --  Lines_Skipped is the number of lines that have actually been skipped.
   --  Use with Skip_To_Column to go to a specific position in a buffer.

   procedure Skip_To_Column
     (Str       : String;
      Columns   : Integer := 0;
      Index     : in out Integer;
      Tab_Width : Integer := 8);
   --  Assuming Index points to the begining of a line (as is the case after
   --  Skip_Lines for instance), jump to the specific column on that line.
   --  This procedure handles tabulations (ie Columns are columns visible to
   --  the user, after tab expansion).

   function Forward_UTF8_Char
     (Str   : String;
      Index : Integer) return Integer;
   --  Moves Index one character forward, taking into account UTF8 encoding.

   function Next_Line (Str : String; P : Natural) return Natural;
   --  Return the start of the next line or Buffer'Last if the end of the
   --  buffer is reached.

   function Previous_Line (Str : String; P : Natural) return Natural;
   --  Return the start of the previous line or Buffer'First if P already
   --  points to the first line of Buffer.

   function Is_Blank_Line
     (Str : String; Index : Natural := 0) return Boolean;
   --  Return True if the line pointed by Index only contains blank characters
   --  (' ', HT, LF, CR). By default, if Index is 0, then the line considered
   --  is the first line of the buffer.

   procedure Skip_To_String
     (Str      : String;
      Index     : in out Natural;
      Substring : String);
   --  Skip every character until an occurence of Substring is found.
   --  Index is set to the first character of the occurence.

   function Strip_Character (Text : String; C : Character) return String;
   --  Return a version of Text after stripping all C's from the string

   function Strip_CR (Text : String) return String;
   pragma Inline (Strip_CR);
   --  Return a version of Text after stripping all the CR from the string.
   --  This function is used on Windows or when the Strip_CR preference is
   --  enabled (for systems that share dos files).
   --  CR/LF sequences are replaced by LF chars.

   ------------
   -- Expect --
   ------------

   function Get_Command_Output
     (Command : access GNAT.Expect.Process_Descriptor'Class) return String;
   --  Runs Command until it finishes, and return its output.
   --  This automatically closes the process cleanly.

   ------------------
   -- File systems --
   ------------------

   function Executable_Location return String;
   --  Return the name of the parent directory where the executable is stored
   --  (so if you are running "prefix"/bin/gps, you would get "prefix").
   --  A special case is done for "bin" directories, which are skipped.
   --  The returned directory always ends up with a directory separator.

   function Is_Directory_Separator (C : Character) return Boolean;
   --  Returns True if C is a directory separator

   procedure Set_OpenVMS_Host (Setting : Boolean := True);
   --  Set whether the host is an OpenVMS host

   -----------
   -- Dates --
   -----------

   function Time_Value (Str : String) return Ada.Calendar.Time;
   --  Check the validity of Str as a string representing a date
   --  using the same formats as in GNAT.Calendar.Time_IO.Value. In addition,
   --  it also supports timezones (as output for instance by PostgreSQL)
   --     1970-01-01 12:00:00+01
   --
   --  Return the Date if the format is valid, No_Time otherwise
   --  All the above can start with the day spelled out, as in "thu, "
   --  The returned date is in UTC format, and should be manipulated through
   --  the functions in Ada.Calendar.*, not the functions in GNAT.Calendar.*
   --  which expect a local time.
   --  The input date is assumed to be in UTC, unless a timezone is specified
   --  with a final "[+-]\d\d".

private

   No_Time : constant Ada.Calendar.Time := Ada.Calendar.Time_Of
     (Ada.Calendar.Year_Number'First,
      Ada.Calendar.Month_Number'First,
      Ada.Calendar.Day_Number'First);

end GNATCOLL.Utils;
