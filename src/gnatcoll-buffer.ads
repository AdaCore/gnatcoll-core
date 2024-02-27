------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
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

--  The unit provide an efficient API to access files character-by-character
--  efficiently. This is specially usefull when implementing a parser or a
--  scanner that needs to scan the complete content.
--
--  Depending on the nature of the input, the implementation use windows,
--  complete buffers or mapped memory.
--
--  Example: a program that counts commas in a file
--
--  declare
--    R : Reader := Open ("./a_file.txt");
--    C : Character;
--    Number_Of_Commas : Integer := 0;
--  begin
--    while R.Next (C) loop
--      if C = ',' then
--        Number_Of_Commas := Number_Of_Commas + 1;
--      end if;
--      R.Release;
--    end loop;
--  end;
--
--  Some performance gain can also be achieved by using a local index
--  to access the internal buffer
--
--  declare
--    R : Reader := Open ("./a_file.txt");
--    C : Character;
--    Offset : Integer := R.Window_Offset;
--    Number_Of_Commas : Integer := 0;
--  begin
--    while R.Next (C, Offset) loop
--      if C = ',' then
--        Number_Of_Commas := Number_Of_Commas + 1;
--      end if;
--      R.Release;
--    end loop;
--    R.Set_Window_Offset (Offset);
--  end;

with Ada.Strings.UTF_Encoding;
with Ada.Finalization;
with GNATCOLL.OS.FS;
with GNATCOLL.Mmap;

package GNATCOLL.Buffer is

   package UTF8 renames Ada.Strings.UTF_Encoding;
   package FS renames GNATCOLL.OS.FS;
   package Mmap renames GNATCOLL.Mmap;

   use all type FS.File_Descriptor;

   type Reader is tagged limited private;
   --  Object used to read character-by-character a file, stream or string

   function Open (FD : FS.File_Descriptor) return Reader;
   --  Open a Reader base on a file descriptor.

   function Open (Path : UTF8.UTF_8_String) return Reader;
   --  Open a Reader on the content of file located at Path.

   function Open_String (Str : UTF8.UTF_8_String) return Reader;
   --  Create Reader object based on UTF-8 string

   function Next (Self : in out Reader'Class; C : out Character) return Boolean
      with Inline_Always => True;
   --  Increment buffer position and get the next character. If no character
   --  is available return False. Otherwise return True and set C to the next
   --  character value.

   function Current_Char (Self : Reader'Class) return Character
      with Inline_Always => True;
   --  Get the current character in the buffer. Call to that function is only
   --  valid if Is_End_Of_Data returns False.

   function Is_End_Of_Data (Self : Reader'Class) return Boolean
      with Inline_Always => True;
   --  Return True if the end of the buffer has been reached.

   function Check (Self : in out Reader'Class; Str : String) return Boolean
      with Inline_Always => True;
   --  Check whether the next characters in the buffer are equal to Str. If
   --  Str is found Check return True and buffer position updated to the
   --  latest character of Str. If False is returned then buffer position is
   --  not updated.

   function Token
      (Self : Reader'Class; First, Last : Long_Long_Integer) return String
      with Inline_Always => True;
   --  Return slice of the buffer between First and Last.

   function Current_Position (Self : Reader'Class) return Long_Long_Integer
      with Inline_Always => True;
   --  Return absolute position in the stream

   procedure Current_Text_Position
      (Self   : Reader'Class;
       Line   : out Integer;
       Column : out Integer);
   --  Get Current text position. For large file (>2GB) or for file opened from
   --  a file descriptor, line is not tracked. Instead the function set Line
   --  and Column to 0.

   procedure Current_Text_Position
      (Self   : Reader'Class;
       Offset : Integer;
       Line   : out Integer;
       Column : out Integer);
   --  Get text position at a given offset

   procedure Release (Self : in out Reader'Class)
      with Inline_Always => True;
   --  Release buffer before the current position. Effect is not immediate.

   function Window_Offset (Self : Reader'Class) return Integer
      with Inline_Always => True;
   --  Get the current offset in the internal buffer

   procedure Set_Window_Offset (Self : in out Reader'Class; Offset : Integer)
      with Inline_Always => True;
   --  Set the curent offset in the internal buffer

   function Next
      (Self   : in out Reader'Class;
       C      : out Character;
       Offset : in out Integer)
      return Boolean with Inline_Always => True;
   --  Same as previous Next function except that Offset is updated instead of
   --  the internal position. This version of Next can be used to speed the
   --  iteration on the stream by using a local variable for the offset.

   procedure Finalize (Self : in out Reader);
   --  Finalize Reader object

private

   type String_Access is access all String;

   type Reader is new Ada.Finalization.Limited_Controlled with record
      --  Relative position in the internal buffer
      Current       : Integer            := 0;
      --  First position in the internal buffer that should be hold
      First         : Integer            := 0;
      --  Last position in the internal buffer
      Last          : Integer            := 0;

      --  File descriptor to the stream (not used if input was a string)
      FD            : FS.File_Descriptor := FS.Invalid_FD;

      --  Structures used to hold the buffer. Once initialized only use
      --  the Buffer attribute to access the internal buffer.
      File          : Mmap.Mapped_File   := Mmap.Invalid_Mapped_File;
      Buffer        : Mmap.Str_Access    := null;
      Buffer_Str    : String_Access      := null;

      --  Absolute offset in the stream of the first character in the buffer
      Offset        : Long_Long_Integer  := 0;

      --  If True Close automatically the file descriptor on finalization
      Auto_Close_FD : Boolean            := True;

      --  If True, this means the end of the stream was reached
      EOF           : Boolean            := False;

      --  If True track line numbers.
      Track_Lines   : Boolean            := True;
   end record;

end GNATCOLL.Buffer;
