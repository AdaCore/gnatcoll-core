------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2013-2013, AdaCore                     --
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

--  An interface to libiconv.
--
--  For historical reasons, international text is often encoded using a
--  language or country dependent character encoding. With the advent of the
--  internet and the frequent exchange of text across countries - even the
--  viewing of a web page from a foreign country is a "text exchange" in this
--  context -, conversions between these encodings have become important. They
--  have also become a problem, because many characters which are present in
--  one encoding are absent in many other encodings. To solve this mess, the
--  Unicode encoding has been created. It is a super-encoding of all others and
--  is therefore the default encoding for new text formats like XML.
--
--  Still, many computers still operate in locale with a traditional (limited)
--  character encoding. Some programs, like mailers and web browsers, must be
--  able to convert between a given text encoding and the user's encoding.
--  Other programs internally store strings in Unicode, to facilitate internal
--  processing, and need to convert between internal string representation
--  (Unicode) and external string representation (a traditional encoding) when
--  they are doing I/O. Libiconv is a conversion library for both kinds of
--  applications.
--
--  If support for libiconv was not compiled into gnatcoll, these subprograms
--  are still available, but will return their input unmodified.

with System;

package GNATCOLL.Iconv is

   type Iconv_T is private;
   --  A conversion descriptor between two encodings.
   --  A conversion description cannot be used in multiple threads
   --  simultaneously.

   function Has_Iconv return Boolean;
   --  Whether support for iconv was compiled into GNATCOLL.
   --  If it returns False, all the subprograms will return their input
   --  unchanged.

   procedure Set_Locale;
   --  Sets the C library's notion of natural language formatting style for
   --  particular sets of routines. Each such style is called a `locale` and
   --  is invoked using an appropriate name passed as a C string.
   --  This procedure sets the locale argument to return hte current locale
   --  (the default is the "C" locale).
   --  This call is needed to get a working charset detection in Iconv_Open.

   ASCII       : constant String := "ASCII";
   ISO_8859_1  : constant String := "ISO-8859-1";
   ISO_8859_2  : constant String := "ISO-8859-2";
   ISO_8859_3  : constant String := "ISO-8859-3";
   ISO_8859_4  : constant String := "ISO-8859-4";
   ISO_8859_5  : constant String := "ISO-8859-5";
   ISO_8859_7  : constant String := "ISO-8859-7";
   ISO_8859_9  : constant String := "ISO-8859-9";
   ISO_8859_10 : constant String := "ISO-8859-10";
   ISO_8859_13 : constant String := "ISO-8859-13";
   ISO_8859_14 : constant String := "ISO-8859-14";
   ISO_8859_15 : constant String := "ISO-8859-15";
   ISO_8859_16 : constant String := "ISO-8859-16";
   KOI8_R  : constant String := "KOI8-R";
   --  Some charsets seemingly supported by most implementations of iconv,
   --  for European languages.

   UTF8    : constant String := "UTF-8";
   UTF16   : constant String := "UTF-16";
   UTF16BE : constant String := "UTF-16BE";
   UTF16LE : constant String := "UTF-16LE";
   UTF32   : constant String := "UTF-32";
   UTF32BE : constant String := "UTF-32BE";
   UTF32LE : constant String := "UTF-32LE";
   --  Some charsets seemingly supported by most implementations of iconv,
   --  for Unicode.

   Locale : constant String := "";
   --  The locale charset

   function Iconv_Open
      (To_Code   : String := UTF8;
       From_Code : String := Locale;
       Transliteration : Boolean := False;
       Ignore          : Boolean := False)
      return Iconv_T;
   --  Allocate a conversion descriptor suitable for converting byte sequences
   --  from character encoding From_Code to character encoding To_Code.
   --  The values permitted for From_Code and To_Code and the supported
   --  combination are system dependent.
   --  The empty encoding name "" is equivalent to the locale dependent
   --  character encoding.
   --  If Transliteration is True, a character that cannot be represented in
   --  the target set might be approximated through one or several characters
   --  that look similar to the original character.
   --  If Ignore is True, characters that cannot be represented in the target
   --  character set will be silently discarded.
   --  This subprogram might raise Unsupported_Conversion.

   Unsupported_Conversion : exception;
   --  Raised when the conversion from From_Code to To_Code is not supported
   --  by the implementation.

   type Iconv_Result is (
      Invalid_Multibyte_Sequence,
      Success,
      Incomplete_Multibyte_Sequence,
      Full_Buffer);

   procedure Iconv
      (State          : Iconv_T;
       Inbuf          : String;
       Input_Index    : in out Positive;
       Outbuf         : in out String;
       Output_Index   : in out Positive;
       Result         : out Iconv_Result);
   --  Converts the multibyte sequence starting at Inbuf(Input_Index) into a
   --  multibyte sequence starting at Outbuf(Output_Index). This procedure
   --  will not try to write past the end of Outbuf.
   --
   --  This function converts of multibyte character at a time, and for each
   --  character conversion it increments the indexes as needed. It also
   --  updates the conversion state in State (for those cases where the
   --  conversion is stateful, this procedure might read a number of input
   --  characters without producing output bytes -- such input is called a
   --  shift sequence).
   --
   --  On exit, Result is set to one of:
   --    * Invalid_Multibyte_Sequence: Input_Index is left pointing to the
   --      beginning of the invalid sequence.
   --    * Success: the input sequence has been entirely converted.
   --    * Incomplete_Multibyte_Sequence: an incomplete sequence is
   --      encountered and the input terminates after it. Input_Index is left
   --      pointing to the beginning of the incomplete sequence.
   --    * Full_Buffer: the output buffer has no more room for the next
   --      converted character.

   procedure Reset (State : Iconv_T);
   --  Resets the conversion state to the initial state.

   procedure Reset
      (State        : Iconv_T;
       Outbuf       : in out String;
       Output_Index : in out Positive;
       Result       : out Iconv_Result);
   --  Attempts to reset the conversion state to the initial state, and store
   --  a corresponding shift sequence in Outbuf(Output_Index..).
   --  The result might be one of Success or Full_Buffer.

   procedure Iconv_Close (State : Iconv_T);
   --  Close the context and free the memory.

   function Iconv (State : Iconv_T; Input : String) return String;
   --  Converts Input.
   --  This function is a convenience for the Iconv procedure, but gives less
   --  control, and for big strings will require more memory. As opposed to
   --  the procedure, it raises exceptions in case of error (either
   --  Invalid_Sequence_Error or Incomplete_Sequence_Error).

   Invalid_Sequence_Error : exception;
   Incomplete_Sequence_Error : exception;

   function Iconv
      (Input           : String;
       To_Code         : String := UTF8;
       From_Code       : String := Locale;
       Transliteration : Boolean := False;
       Ignore          : Boolean := False) return String;
   --  A convenience function that wraps all the above (open, iconv, close)
   --  Might raise Unsupported_Conversion, Invalid_Sequence_Error or
   --  Incomplete_Sequence_Error.

private
   type Iconv_T is new System.Address;

   pragma Import (C, Set_Locale, "gnatcoll_iconv_set_locale");

end GNATCOLL.Iconv;
