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

--  This package and its children provide routines to manipulate mailboxes and
--  email messages

with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with GNATCOLL.VFS;

package GNATCOLL.Email is

   ----------------------
   -- Charset sections --
   ----------------------

   type Charset_String is record
      Contents : Unbounded_String;
      Charset  : Unbounded_String;
   end record;
   Null_Charset_String : constant Charset_String;
   --  This type represents a string and its charset. Contents must be
   --  interpreted relatively to Charset, ie characters above 127 must be
   --  read from that charset. For instance character 161 is an inverted
   --  exclamation mark in iso-8859-1, but a latin letter A with ogonek in
   --  iso-8859-2.

   package Charset_String_List is new Ada.Containers.Doubly_Linked_Lists
     (Charset_String);

   Charset_US_ASCII     : constant String := "us-ascii";
   Charset_ISO_8859_1   : constant String := "iso-8859-1";
   Charset_ISO_8859_2   : constant String := "iso-8859-2";
   Charset_ISO_8859_3   : constant String := "iso-8859-3";
   Charset_ISO_8859_4   : constant String := "iso-8859-4";
   Charset_ISO_8859_9   : constant String := "iso-8859-9";
   Charset_ISO_8859_10  : constant String := "iso-8859-10";
   Charset_ISO_8859_13  : constant String := "iso-8859-13";
   Charset_ISO_8859_14  : constant String := "iso-8859-14";
   Charset_ISO_8859_15  : constant String := "iso-8859-15";
   Charset_Windows_1252 : constant String := "windows-1252";

   -------------
   -- Headers --
   -------------

   type Header is tagged private;
   Null_Header : constant Header;

   Default_Max_Header_Line_Length : constant := 76;
   --  Default maximal length that headers should use

   Content_Description       : constant String := "Content-Description";
   Content_Disposition       : constant String := "Content-Disposition";
   Content_Transfer_Encoding : constant String := "Content-Transfer-Encoding";
   Content_Type              : constant String := "Content-Type";
   MIME_Version              : constant String := "MIME-Version";
   Message_ID                : constant String := "Message-ID";
   CC                        : constant String := "CC";
   --  The standard MIME headers for mail messages.
   --  For Content_Disposition, see RFC 2183 at
   --     http://www.faqs.org/rfcs/rfc2183.html

   Text_Plain                : constant String := "text/plain";
   Text_Html                 : constant String := "text/html";
   Application_Octet_Stream  : constant String := "application/octet-stream";
   Message_RFC822            : constant String := "message/rfc822";
   Multipart_Mixed           : constant String := "multipart/mixed";
   Multipart_Alternative     : constant String := "multipart/alternative";
   Multipart_Signed          : constant String := "multipart/signed";
   Multipart_Digest          : constant String := "multipart/digest";
   Image_Jpeg                : constant String := "image/jpeg";
   Image_Gif                 : constant String := "image/gif";
   Text_Xvcard               : constant String := "text/x-vcard";
   --  Some of the standard MIME types

   function Create
     (Name    : String;
      Value   : String;
      Charset : String := Charset_US_ASCII)
      return Header;
   --  Create a new header, with an unparsed string Value. The interpretation
   --  of Value depends on the specific header (it could be a date, some
   --  content type,...).
   --  Charset indicates the charset used for Value. If Value already contains
   --  a Mime-encoded string (such as '=?iso-8859-1?q?p=F4stal?='), the
   --  charset should be left to us-ascii. If Value contains extended
   --  characters from another charset, the latter must be specified. For
   --  instance, you could replace the previous mime-encoded string with:
   --     Value='pôstal'  Charset='iso-8859-1'
   --  The charset influences how the header is encoded when it is displayed in
   --  a message.
   --  The Value, if it was split into several lines, must have been normalized
   --  and the newline characters removed.

   procedure Append
     (H       : in out Header'Class;
      Value   : String;
      Charset : String := Charset_US_ASCII);
   procedure Append
     (H       : in out Header'Class;
      Value   : Charset_String_List.List);
   --  Appends some content to the header's value

   procedure Set_Param
     (H : in out Header'Class; Param_Name : String; Param_Value : String);
   --  Set the value for one of H's parameters. Such parameters are typically
   --  used for the Content-Type header, to store the file name, or the
   --  boundary for instance. They appear as:
   --      Content-Type: text/plain; charset="iso-8859-1"
   --  If such a parameter is already set, it is replaced in-place, ie the
   --  order of parameters is preserved.

   function Get_Param (H : Header'Class; Param_Name : String) return String;
   --  Get the value for one of H's parameters, or "" if there is no such
   --  param.
   --  This automatically handles continuation headers, ie cases where the
   --  value of the parameter was split onto several lines, as in:
   --     filename*0="value1";
   --     filename*1="value2"

   procedure Delete_Param (H : in out Header'Class; Param_Name : String);
   --  Remove in place one of H's parameters.
   --  No error is the parameter doesn't exist

   function Get_Name (H : Header'Class) return String;
   --  Return the name of the header, lower cased

   function Get_Value (H : Header'Class) return Charset_String_List.List;
   --  Return the value of the header

   procedure To_String
     (H                : Header'Class;
      Max_Line_Len     : Positive := Default_Max_Header_Line_Length;
      Show_Header_Name : Boolean := True;
      Result           : out Unbounded_String);
   --  Return the header's value as string. Optionally, the header's name can
   --  be prepended.
   --  Lines will be split as needed to match Max_Line_Len. The first line will
   --  be shorted to take into account the header's name.
   --  The header is MIME encoded if necessary so that it only contains ASCII
   --  characters suitable for sending in an email message.

   function To_Time
     (H : Header'Class) return Ada.Calendar.Time;
   --  Interprets the header's value as a time, and returns it. This mostly
   --  applies to the 'Date:' header. The returned time is UTC.
   --  The format of the header must match the date format described in
   --  RFC 2822. When the format is incorrect, No_Time is returned.

   --------------
   -- Messages --
   --------------

   type Message is tagged private;
   Null_Message : constant Message;

   function New_Message
     (MIME_Type : String := Text_Plain) return Message;
   --  Return a new empty message. The memory will be freed automatically when
   --  the message is no longer used.
   --  The MIME type is the initial type, but it can be changed at any time by
   --  changing the header. The mail will be created as multi-part if
   --  MIME_Type is one of the standard multipart/* types. Otherwise, a single
   --  part message is created, but that will change automatically depending on
   --  the payload you set for the message. If MIME_Type is the empty string,
   --  no Content-Type header is set.

   function Clone_Message (Msg : Message) return Message;
   --  Return a copy of the given message.
   --  ??? In the case of a multipart message, the contents of each
   --  part of the message is not duplicated.  In other words, modifying
   --  the contents of any part of the payload will affect both the
   --  copy and the original.

   function Reply_To
     (Msg            : Message'Class;
      From_Email     : String;
      From_Real_Name : String := "";
      Quote          : Boolean := True;
      Reply_All      : Boolean := True;
      Local_Date     : Ada.Calendar.Time := Ada.Calendar.Clock) return Message;
   --  Create a new message as a reply to Msg. This impacts subjects,
   --  recipients,... If Quote is True, then Msg is quoted in the payload of
   --  the new message.
   --  Headers are set so that the reply will appear in the same thread as Msg
   --  in mailers that support threads.

   procedure Set_Default_Headers
     (Msg            : in out Message'Class;
      From_Email     : String;
      Subject        : String := "No Subject";
      From_Real_Name : String := "";
      Local_Date     : Ada.Calendar.Time := Ada.Calendar.Clock);
   --  Set the standard headers for the message. This is just a convenient
   --  subprogram, since the same can be done by manipulating directly the
   --  headers.

   type Header_Filter is access function (H : Header'Class) return Boolean;
   --  A filter for headers. It is returned True, the header will be displayed,
   --  otherwise it is skipped.

   type Payload_Filter is access function
     (Attachment : Message'Class) return Boolean;
   --  Whether a given payload part should be displayed when a message is
   --  converted to a string. If it returns True, that part is displayed.
   --  When the filter is unspecified to To_String, all payloads are output.
   --  This filter only applies in the case of multipart messages, and only to
   --  the toplevel attachments (ie if an attachment is itself a message with
   --  other attachments, the filter will not be applied for these).

   procedure To_String
     (Msg                  : Message'Class;
      Envelope             : Boolean  := False;
      Header_Max_Line_Len  : Positive := Default_Max_Header_Line_Length;
      Subject_Max_Line_Len : Positive := Default_Max_Header_Line_Length;
      Content_Filter       : Payload_Filter := null;
      Filter               : Header_Filter := null;
      Decode               : Boolean := False;
      Quote_From           : Boolean := False;
      Result               : out Unbounded_String);
   --  Return the message as string. This string is suitable for passing to any
   --  program like sendmail to forward the mail to its recipients.
   --  If Envelope is True, the envelope line, if known, is included.
   --  If Filter is specified, it can be used to filter out which filters
   --  should be displayed.
   --  If Decode is True and this message is MIME-encoded, it is automatically
   --  decoded.
   --  If Quote_From is true, then each line of Msg's payload preceded by a
   --  blank line and starting with "From " will be prepended with ">" in order
   --  to avoid further tools to be confused with the From_ message delimiter.
   --
   --  The message might be modified if for instance a boundary needs to be
   --  created or adjusted for a multipart message.

   procedure Set_Envelope_From (Msg : in out Message'Class; From : String);
   procedure Set_Envelope_From
     (Msg   : in out Message'Class;
      Email : String;
      Local_Date  : Ada.Calendar.Time);
   function Get_Envelope_From (Msg : Message'Class) return String;
   --  Set the "From " line used for the envelope of the message

   function Date_From_Envelope (Msg : Message'Class) return Ada.Calendar.Time;
   --  Return the date read in the envelope of the message. It is recommanded
   --  that you get the date from the 'Date:' header when available instead.

   function Sender_From_Envelope (Msg : Message'Class) return String;
   --  Return the sender part of the envelope. It is recommended that you use
   --  the From: header instead when available

   procedure Add_Header (Msg : in out Message'Class; H : Header'Class);
   --  Set the unparsed block of headers for the message.
   --  If there is already a header with the same name, it isn't overridden.
   --  Instead, two headers with the same name will exist for the message.

   procedure Delete_Headers (Msg : Message'Class; Name : String);
   procedure Delete_Header  (Msg : Message'Class; H : Header'Class);
   --  Delete either all headers with the given name (all if Name is the empty
   --  string), or a specific header.

   procedure Replace_Header (Msg : Message'Class; H : Header'Class);
   --  Replace the first header with the same name by H, and delete all other
   --  headers with the same name. This is different from doing a
   --     Delete_Headers (Msg, Name);
   --     Add_Header (Create (Name, ...));
   --  since Replace_Header will preserve the order of headers.
   --  If no header with the same name is found, H is simply added to the list.

   function Get_Header (Msg : Message'Class; Name : String) return Header;
   --  Return the first header of Msg with the given name. If this header
   --  occurs multiple times, only the first occurrence is returned.
   --  Name is case-insensitive

   function Get_Content_Type (Msg : Message'Class) return String;
   --  Return the MIME content type for the message.
   --  As per RFC 2045, there is always such a content type, even if it wasn't
   --  specified explicitly by the headers. It defaults to text/plain when the
   --  message is not part of the payload of a multipart/report message, to
   --  message/rfc822 otherwise.
   --  This content-type is always lower-cased.

   function Get_Message_Id (Msg : Message) return String;
   --  Return the Message_Id for this message. This returns the empty string if
   --  no such Id is defined. Otherwise, this extracts the Id from that header,
   --  properly keeping only the Id itself, and not the surrounding <..> if
   --  they exist.

   function Get_Date (Msg : Message) return Ada.Calendar.Time;
   --  Return the date the message was sent. This information is taken from the
   --  Date: header if it exists, and if not from the envelope of the message.

   function Size
     (Msg                 : Message;
      Include_Attachments : Boolean) return Long_Integer;
   --  Return the size of the message and all its MIME parts. This size is not
   --  extremely precise (and doesn't reflect the size it would take to convert
   --  it to a string for instance), and for instance doesn't include the size
   --  of the headers.
   --  If Include_Attachments is False, then all but the first text/plain part
   --  will be ignored

   type Encoding_Type is
     (Encoding_7bit,
      Encoding_8bit,
      Encoding_Binary,
      Encoding_QP,
      Encoding_Base64);

   function Get_Encoding_Type (Msg : Message'Class) return Encoding_Type;
   --  Return the encoding used for this message.
   --  As per RFC 2045, there is always such an encoding, and if no header is
   --  specified then Encoding_7bit is assumed.

   type Header_Iterator is private;

   function Get_Headers
     (Msg : Message'Class; Name : String := "") return Header_Iterator;
   --  Iterate over all headers with the given name. If Name is unspecified,
   --  iterates over all headers of the message. Looping over all headers is
   --  done as follows:
   --      Iter := Get_Headers (Msg);
   --      loop
   --         Next (Iter, H);
   --         exit when H = Null_Header;
   --      end loop;

   procedure Next (Iter : in out Header_Iterator; H : out Header);
   --  Move to the next header with the expected name, and returns it.
   --  Null_Header is returned when there are no more matches

   -------------
   -- Payload --
   -------------
   --  A message can either be a single part message, ie it just contains text,
   --  possibly in various charsets or a multi part message, in which case it
   --  can have attached files, contain nested messages, etc.
   --  The content of the message, whether single or multi part, is called the
   --  payload.
   --  Since each part of a multi-part message can itself have its own headers
   --  and be a nested message, the actual payload of a message is represented
   --  as a list of messages.

   function Is_Multipart (Msg : Message'Class) return Boolean;
   --  Whether the message contains several parts, and must be encoded as a
   --  multipart email message. If False, the payload is a simple string.

   Multipart_Error : exception;

   --------------------------
   -- Single part messages --
   --------------------------

   procedure Set_Text_Payload
     (Msg       : Message'Class;
      Payload   : String;
      MIME_Type : String := Text_Plain;
      Charset   : String := Charset_US_ASCII;
      Prepend   : Boolean := False);
   --  Set the payload of the message, as text. No parsing is done.
   --  If the message is a single part message, this is the text of the
   --  message. If the message is a multi-part message, this is set as one of
   --  the parts, with the given MIME type. As a result, it can be called
   --  several times in such a case, each time will create a new part.
   --  The MIME type will changes the Content-Type header.
   --  If MIME_Type is set to the empty string, it is not updated in the
   --  message. This is mostly useful when Msg was parsed through one of the
   --  functions in Email.Parser.
   --  When Msg is a multi-part message, the new part is either appended after
   --  the existing parts, or prepend before, depending on the Prepend
   --  parameter. If Msg is a single part message, then Payload will replace
   --  the current payload if Prepend is False, otherwise the old payload is
   --  preserved and set after the new one.

   procedure Get_Single_Part_Payload
     (Msg     : Message'Class;
      Payload : out Unbounded_String;
      Decode  : Boolean := False);
   --  Return the content of a message when it doesn't contain multiparts.
   --  If this is a multipart message, Multipart_Error is raised.
   --  If Decode is true and this message is MIME-encoded, it is automatically
   --  decoded. You can also decode it later through the subprograms in
   --  email-utils.ads

   -------------------------
   -- Multi part messages --
   -------------------------

   type Payload_Iterator is private;
   function Get_Payload (Msg : Message'Class) return Payload_Iterator;
   --  Return an iterator over the whole content of the message.
   --  If the message is not a multipart message, a single element will ever
   --  be returned, which is Msg itself. This allows for traversing both
   --  single parts and multiparts messages in a single piece of code.
   --  The following code will find all textual contents of Msg:
   --     Iter := Get_Payload (Msg);
   --     loop
   --        Next (Iter, Item => Attachment);
   --        exit when Attachment = Null_Message;
   --        if Get_Main_Type (Get_Content_Type (Attachment)) = "text" then
   --            Get_Single_Part_Payload (Attachment, ....);
   --        end if;
   --     end loop;

   procedure Next (Iter : in out Payload_Iterator; Item : out Message);
   --  Get the next part in the payload of a message. Null_Message is
   --  returned when there are no more parts in the message.

   procedure Delete_Payload
     (Msg : in out Message'Class; Iter : in out Payload_Iterator);
   --  Remove the corresponding payload from the message

   procedure Convert_To_Multipart (Msg : Message'Class);
   --  Convert the message to a multi-part message if it is a single part one
   --  (does nothing otherwise).
   --  The current textual content is set to be the first part of the converted
   --  message.

   procedure Convert_To_Single_Part (Msg : in out Message'Class);
   --  Try to convert Msg to a single part message. This is only doable if
   --  there is a single textual part, or the message is already single part.
   --  If Msg contains a single part
   --  which is in turn a multipart Msg, it gets processed as well.
   --  All other cases will do nothing.

   procedure Set_Preamble (Msg : in out Message'Class; Preamble : String);
   --  Set the preamble of the MIME message.
   --  This text will be inserted before the first boundary, ie the first
   --  attached file.
   --  Normally, in MIME aware mailers, this preamble will not be visible. It
   --  will only be visible by viewing the full text of the message.
   --  If the message was single-part message, it is automatically converted to
   --  a multi-part message.

   procedure Set_Epilogue (Msg :  in out Message'Class; Epilogue : String);
   --  This is similar to the preamble, but appears after the end of the
   --  last document.
   --  If the message was single-part message, it is automatically converted to
   --  a multi-part message

   procedure Add_Payload (Msg : in out Message'Class;
                          Payload : Message;
                          First : Boolean := False);
   --  Add a new part to a multipart message. Msg is first converted to
   --  multipart if necessary. Payload itself is stored in Msg, ie modifying
   --  Payload later on will impact Msg. This procedure cannot be used when
   --  attaching a real mail message, see Attach_Msg instead.
   --  If First is True, then add the new part at the begining.  Otherwise,
   --  add it at the end.

   procedure Attach_Msg
     (Msg         : in out Message'Class;
      Attach      : Message'Class;
      Description : String := "");
   --  Attach an existing mail message to another one (for instance when
   --  forwarding as attachment).

   type Disposition_Type is (Disposition_Attachment, Disposition_Inline);

   procedure Attach
     (Msg                  : in out Message'Class;
      Path                 : GNATCOLL.VFS.Virtual_File;
      MIME_Type            : String           := Application_Octet_Stream;
      Recommended_Filename : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Description          : String           := "";
      Charset              : String           := Charset_US_ASCII;
      Disposition          : Disposition_Type := Disposition_Attachment;
      Encoding             : Encoding_Type    := Encoding_Base64);
   --  Attach a file to the payload. The file is immediately read from the
   --  disk, and encoded as necessary, so this might be an expensive operation
   --  to perform.
   --  Name_Error is raised if the file is not found.

   function Get_Boundary (Msg : Message'Class) return String;
   --  Return the boundary used for Msg to separate its various parts.
   --  The empty string is returned if this isn't a multipart message.

   procedure Set_Boundary
     (Msg : Message'Class; Boundary : String := "");
   --  Set the boundary to use between parts of the message. If the empty
   --  string is passed, a boundary will be added if none already exists, or
   --  if the current one can not be used because some part of the message
   --  already includes it.
   --  The message is automatically converted to a multipart message if you
   --  call this message, since boundaries can not be used with single part
   --  messages.
   --  As per RFC 1521, the boundary can only use the following characters:
   --    0-9 a-z A-Z '()+_,-./:=?
   --  In this implementation, it must include the sequence =_. This is a
   --  sequence that is guaranteed to never appear in quoted-printable or
   --  base64 encoded parts, and this implementation takes advantage of this
   --  to speed up the check that the boundary can be used.
   --  The string =_ will be appended as many times as necessary to Boundary to
   --  make it valid.
   --  In general, you do not need to call this procedure, which is called
   --  automatically when needed.

private
   type Header_Record is record
      Name            : Unbounded_String;
      Value           : Charset_String_List.List;
      Ref_Count       : Natural := 1;
   end record;
   type Header_Access is access Header_Record;
   type Header is new Ada.Finalization.Controlled with record
      Contents : Header_Access;
   end record;

   procedure Adjust   (H : in out Header);
   procedure Finalize (H : in out Header);

   --  Headers are stored in a list, since order might be relevant sometimes,
   --  especially for the 'Received:' headers.
   package Header_List is new Ada.Containers.Doubly_Linked_Lists (Header);

   type Header_Iterator is record
      Cursor : Header_List.Cursor;
      Name   : Unbounded_String;
   end record;

   type Message_Record;
   type Message_Access is access Message_Record;
   type Message is new Ada.Finalization.Controlled with record
      Contents : Message_Access;
   end record;
   --  Smart pointer to message. This provides automatic freeing of the memory,
   --  but allows us to have a list of messages without having access to the
   --  full view of a Message, which itself contains an instance of the list.

   procedure Adjust   (Msg : in out Message);
   procedure Finalize (Msg : in out Message);

   package Message_List is new Ada.Containers.Doubly_Linked_Lists (Message);

   type Payload_Iterator is record
      Cursor : Message_List.Cursor;
      Msg    : Message;
   end record;

   type Message_Payload (Multipart : Boolean := False) is record
      case Multipart is
         when True =>
            Parts         : Message_List.List;
            Preamble      : Unbounded_String;
            Epilogue      : Unbounded_String;
         when False =>
            Text          : Unbounded_String;
      end case;
   end record;
   Null_Payload : constant Message_Payload := (False, Null_Unbounded_String);
   Null_Multipart_Payload : constant Message_Payload :=
     (True, Message_List.Empty_List, Null_Unbounded_String,
      Null_Unbounded_String);

   type Message_Record is record
      Ref_Count     : Natural := 1;
      Envelope_From : Unbounded_String;
      Headers       : Header_List.List;
      Payload       : Message_Payload;
      Is_Nested     : Boolean := False;
   end record;

   Null_Message : constant Message :=
     (Ada.Finalization.Controlled with Contents => null);

   function Next_Occurrence (S : String; Char : Character) return Integer;
   --  Return the index of the next line start, or a number greater than
   --  S'Last if we are on the last line

   function Is_Whitespace (Char : Character) return Boolean;
   pragma Inline (Is_Whitespace);
   --  Whether Char is a whitespace (tab or space)

   procedure Skip_Whitespaces (S : String; Index : in out Integer);
   pragma Inline (Skip_Whitespaces);
   --  Skip any whitespace character, including newlines, starting at Index.
   --  Leaves Index on the first non-whitespace character

   Null_Header : constant Header :=
     (Ada.Finalization.Controlled with null);

   Null_Charset_String : constant Charset_String :=
     (Null_Unbounded_String, Null_Unbounded_String);

end GNATCOLL.Email;
