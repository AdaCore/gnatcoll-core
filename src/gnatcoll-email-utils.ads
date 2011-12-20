------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

--  This package contains various utility routines related to handling of
--  email messages

with Ada.Calendar;
with Ada.Containers.Hashed_Sets;

package GNATCOLL.Email.Utils is

   -----------
   -- Dates --
   -----------

   type Time_Format is (Time_RFC2822, Time_Envelope);
   --  The time formats supported by this package.
   --  Time_RFC2822 is the one used in the Date: header.
   --  Time_Enveloppe is the one used in the From_ header

   function To_Time
     (Date   : String;
      Format : Time_Format := Time_RFC2822) return Ada.Calendar.Time;
   --  Interprets the Date as a date/time, and returns it. The time is UTC.
   --  If Date doesn't match Format, No_Time is returned.

   function Format_Date
     (Date         : Ada.Calendar.Time;
      Date_Is_GMT  : Boolean := False;
      Use_GMT      : Boolean := False;
      From_Line    : Boolean := False;
      No_TZ        : Boolean := False;
      Show_Time    : Boolean := True;
      Show_Seconds : Boolean := True;
      Show_Day     : Boolean := True) return String;
   --  Format the date as a RFC 2822 string, eg:
   --     Fri, 09 Nov 2001 01:08:47 -0000
   --  By default, the input date is considered as local date, rather than GMT.
   --  You can change this by changing the Date_Is_GMT parameter.
   --  If Use_GMT is true and no timezone is used, the timezone is output as
   --  "GMT" rather than a numeric zone. This is needed for some protocols.
   --  Date is always interpreted as in the UTC timezone.
   --  The format used will be that of the Unix From_ line if From_Line is
   --  true. In this case, the format is:
   --     Tue Jan 24 14:48:49 2006 +0100
   --  If No_TZ is true, then the date is automatically converted to UTC, and
   --  the timezone indication is not displayed.
   --  If Show_Seconds is false, then seconds will not be displayed (this can
   --  be used to save space, but the output format is not compatible with
   --  RFC 2822).
   --  If Show_Day is false, the day of week is not displayed. The output is
   --  also not compatible with RFC 2822.

   function Format_Time
     (Date      : Ada.Calendar.Time) return String;
   --  Format the time in Date as a RFC2822 string, eg:
   --       01:08:47 -0000

   ---------------
   -- Addresses --
   ---------------

   type Email_Address is record
      Real_Name : Unbounded_String;
      Address   : Unbounded_String;
   end record;
   Null_Address : constant Email_Address;

   function "=" (Addr1, Addr2 : Email_Address) return Boolean;
   --  Whether Addr1 and Addr2 have the same address, even if real name differs

   function Hash (Addr : Email_Address) return Ada.Containers.Hash_Type;
   package Address_Set is new Ada.Containers.Hashed_Sets
     (Email_Address, Hash, "=");

   function Quote (Str : String) return String;
   --  Return a string which is a quoted version of Str: backslashes have been
   --  replaced by \\, and double-quotes by \".

   function Unquote (Str : String) return String;
   --  Return an unquoted version of Str. Extra Backslashes are removed

   function Parse_Address (Email : String) return Email_Address;
   --  Split an email address as read from a message header into its
   --  constituents

   function Get_Addresses (Str : String) return Address_Set.Set;
   function Get_Addresses
     (Str : Charset_String_List.List) return Address_Set.Set;
   --  Return the list of addresses in Str.
   --  The second version properly preserves real names from extended charsets.

   function To_String
     (Addresses    : Address_Set.Set;
      Separator    : String := ", ";
      Address_Only : Boolean := False) return String;
   --  Return the list of addresses as a string compatible with RFC 2822.
   --  Parsing this field with Get_Addresses would return the same set of
   --  addresses of Separator has its default value.
   --  if Address_Only is true, then the real names are never shown in the
   --  string.

   function Get_Recipients
     (Msg : Message'Class; Include_From : Boolean := False)
      return Address_Set.Set;
   function Get_Recipients (H   : Header'Class) return Address_Set.Set;
   --  Return the list of all recipients of the message. This takes into
   --  account all occurrences of all relevant headers.
   --  In the first case, Include_From indicates whether the sender of the
   --  message should also be returned.

   --  ??? The 2nd function should be renamed to Get_Addresses since it applies
   --  to all headers containing addresses, not only those designating
   --  message recipients.

   function Format_Address (Addr : Email_Address) return String;
   function Format_Address (Real : String; Address : String) return String;
   --  Format an email address into a proper format for RFC2822

   function Domain_From_Address (Email : String) return String;
   function Domain_From_Address (Email : Email_Address) return String;
   --  Return the domain name for the given Email address. In the first case,
   --  Email must only contain the address, not the real name.
   --  If no domain is specified, the empty string is returned. Look at
   --  GNAT.Socket.Host_Name to fall back on the current host.

   function Login_From_Address (Email : String) return String;
   function Login_From_Address (Email : Email_Address) return String;
   --  Return the login name from the given email address, ie the part before
   --  the '@'. In the first case, Email must only contain the address, not the
   --  real name.

   ----------
   -- Mime --
   ----------

   function Get_Main_Type (MIME_Type : String) return String;
   --  Return the main type component of the MIME_Type, for instance "text"
   --  when the type is "text/plain";

   function Get_Sub_Type (MIME_Type : String) return String;
   --  Return the sub type component of the MIME_Type, for instance "plain"
   --  when the type is "text/plain";

   ---------------
   -- Encodings --
   ---------------

   procedure Quoted_Printable_Encode
     (Str                : String;
      Block_Prefix       : String := "";
      Block_Suffix       : String := "";
      Max_Block_Len      : Integer := 76;
      Quote_White_Spaces : Boolean;
      Header             : Boolean := False;
      Result             : out Unbounded_String);
   --  Encode Str in quoted-printable format, as per RFC 2045.
   --  This should be used for ascii-like charsets, like all iso-8859-*
   --  charsets, ie when most of the characters are already in the ASCII
   --  charset (0 through 127).
   --  The output is split in blocks if Max_Block_Len is specified. Each block
   --  has a maximal length, including its prefix and suffix. This is to
   --  conform to the format to use for mail headers.
   --  Quote_White_Spaces is a flag which controls whether to encode embedded
   --  spaces and tabs; when true it encodes such embedded whitespace, and when
   --  false it leaves them unencoded. Note that spaces and tabs appearing at
   --  the end of lines are always encoded.

   procedure Quoted_Printable_Decode
     (Str    : String;
      Result : out Unbounded_String;
      Header : Boolean := False);
   --  Decode Str as a quoted-printable encoded string as per RFC 2045.
   --     The returned value may contain non - ASCII characters, their
   --  interpretation is left to the called (ie the charset is unknown).
   --  If the optional argument header is present and true, underscore will be
   --  decoded as space. This is used to decode "Q" encoded headers as
   --  described in RFC 2047: "MIME (Multipurpose Internet Mail Extensions)
   --  Part Three: Message Header Extensions for Non-ASCII Text".

   procedure Base64_Encode
     (Str           : String;
      Block_Prefix  : String := "";
      Block_Suffix  : String := "";
      Max_Block_Len : Integer := Integer'Last;
      Result        : out Unbounded_String);
   --  Encode Str in base64 format, as defined by RFC 2045.
   --  This should be used for charsets that have little similarity with
   --  ASCII, for instance asian charsets.

   procedure Base64_Decode
     (Str    : String;
      Result : out Unbounded_String);
   --  Decode Str from a base64 encoding, as defined by RFC 2045

   procedure Encode
     (Str     : String;
      Charset : String := Charset_US_ASCII;
      Header  : Boolean := False;
      Result  : out Unbounded_String);
   --  Encode Str in the best encoding to use for Charset. The encoding depends
   --  on how close charset is to ASCII.
   --  If Header is true, then several encoded blocks will be created as
   --  required by RFC 2045 (separated by spaces). In addition, the charset is
   --  included as part of the encoded field, as suitable for mail headers.

   procedure Decode_Header
     (Str             : String;
      Default_Charset : String := Charset_US_ASCII;
      Result          : out Charset_String_List.List);
   --  Decode Str. It might contain several mime-encoded sections, with
   --  different charsets. Each section is returned separately. For each
   --  section, Contents must be interpreted in the context of that charset.
   --  When several adjacent sections have the same encoding, they are merged
   --  for ease of use.
   --  When no charset is specified for a section, the default charset is
   --  assumed.
   --  This function can also be used for headers, when each section starts
   --  with =?charset?q?....?=.

   --------------
   -- Charsets --
   --------------

   procedure Flatten
     (List   : Charset_String_List.List;
      Result : out Unbounded_String);
   --  Return a flatten version of List, where all sections are concatenated.
   --  It will not be possible to go back to List afterward, since the sections
   --  are not MIME-encoded, only their contents is taken into account.
   --  This should never be used for display to the user, only for internal
   --  manipulation when the exact charset of each section is irrelevant.

   procedure To_String
     (List   : Charset_String_List.List;
      Result : out Unbounded_String);
   --  Return a single string representing list, where all sections is
   --  properly encoded and surrounded by =?charset? markers.

private
   Null_Address : constant Email_Address :=
     (Null_Unbounded_String, Null_Unbounded_String);
end GNATCOLL.Email.Utils;
