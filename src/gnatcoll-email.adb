------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2006-2013, AdaCore                     --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Containers;            use Ada.Containers;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;

with Ada.Unchecked_Deallocation;
with GNATCOLL.Email.Utils;      use GNATCOLL.Email.Utils;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNAT.Strings;              use GNAT.Strings;

package body GNATCOLL.Email is
   use Header_List, Charset_String_List, Message_List;

   function Identify_Header (Name : String) return Any_Header;
   --  Determine whether Name is the name of an Addr_Header or Other_Header

   procedure To_String
     (Payload             : Message_Payload;
      Header_Max_Line_Len : Positive;
      Content_Filter      : Payload_Filter := null;
      Msg                 : Message'Class;
      Append_To           : in out Unbounded_String);
   --  Encode the payload in a form suitable to send the message.
   --  If necessary, this creates the "boundary" for the message.

   procedure To_String
     (Headers              : Header_List.List;
      Header_Max_Line_Len  : Positive;
      Subject_Max_Line_Len : Positive;
      Filter               : Header_Filter := null;
      Append_To            : in out Unbounded_String);
   --  Encode the headers in a form suitable to send the message

   procedure Get_Param_Index
     (H          : Header'Class;
      Param_Name : String;
      C          : out Charset_String_List.Cursor;
      Semicolon  : out Integer;
      Name_Start : out Integer;
      Name_End   : out Integer;
      Value_End  : out Integer);
   --  Find the occurrence of a parameter in the value of H

   procedure Replace_Header_Internal
     (Msg : Message'Class; H : Header'Class; Append : Boolean);
   --  Same as Replace_Header, but Append can be used to specify whether the
   --  header should be appended or prepended to the list if it didn't exist
   --  yet.

   function Check_Boundary
     (Msg : Message'Class; Boundary : String) return Boolean;
   --  Whether Boundary can be used for this message

   function Has_Line_Starting_With
     (Text : Unbounded_String; Starts_With : String) return Boolean;
   --  Whether Text has a line that starts with Starts_With

   function Clone_Header (Ref : Header) return Header;
   --  Return a deep copy of the given Header.

   function Clone_Headers (Ref : Header_List.List) return Header_List.List;
   --  Return a deep copy of the given list of headers.

   procedure Set_From_Header
     (Msg            : in out Message'Class;
      From_Email     : String;
      From_Real_Name : String;
      Charset        : String);
   --  Create and set a From: header for Msg using the given email address and
   --  real name. The real name has the indicated Charset.

   ---------
   -- "=" --
   ---------

   function "=" (Addr1, Addr2 : Email_Address) return Boolean is
   begin
      return To_Lower (To_String (Addr1.Address))
           = To_Lower (To_String (Addr2.Address));
   end "=";

   ---------------------
   -- Next_Occurrence --
   ---------------------

   function Next_Occurrence (S : String; Char : Character) return Integer is
   begin
      for Index in S'Range loop
         if S (Index) = Char then
            return Index;
         end if;
      end loop;
      return S'Last + 1;
   end Next_Occurrence;

   ---------------------
   -- Identify_Header --
   ---------------------

   function Identify_Header (Name : String) return Any_Header is
      L_Name : constant String := To_Lower (Name);
   begin

      if L_Name = "from"
           or else L_Name = "sender"
           or else L_Name = "to"
           or else L_Name = "cc"
           or else L_Name = "bcc"
      then
         return Addr_Header;
      else
         return Other_Header;
      end if;
   end Identify_Header;

   -------------------
   -- Is_Whitespace --
   -------------------

   function Is_Whitespace (Char : Character) return Boolean is
   begin
      return Char = ' ' or Char = ASCII.HT;
   end Is_Whitespace;

   ----------------------
   -- Skip_Whitespaces --
   ----------------------

   procedure Skip_Whitespaces (S : String; Index : in out Integer) is
   begin
      while Index <= S'Last
        and then (Is_Whitespace (S (Index)) or else S (Index) = ASCII.LF)
      loop
         Index := Index + 1;
      end loop;
   end Skip_Whitespaces;

   -----------------
   -- New_Message --
   -----------------

   function New_Message
     (MIME_Type : String := Text_Plain) return Message
   is
      Pay : Message_Payload;
      Msg : Message;
   begin
      if Get_Main_Type (MIME_Type) = "multipart" then
         Pay := Null_Multipart_Payload;
      else
         Pay := Null_Payload;
      end if;

      Msg := (Ada.Finalization.Controlled
         with Contents => new Message_Record'
           (Ref_Count     => 1,
            Envelope_From => Null_Unbounded_String,
            Headers       => Header_List.Empty_List,
            Is_Nested     => False,
            Payload       => Pay));

      if MIME_Type /= "" then
         Replace_Header (Msg, Create ("Content-Type", MIME_Type));
      end if;

      return Msg;
   end New_Message;

   ------------------
   -- Clone_Header --
   ------------------

   function Clone_Header (Ref : Header) return Header is
      Copy : constant Header := (Ada.Finalization.Controlled
         with Contents => new Header_Record);
   begin
      Copy.Contents.all := (Name => Ref.Contents.Name,
                            Value => Ref.Contents.Value,
                            Ref_Count => 1);
      return Copy;
   end Clone_Header;

   -------------------
   -- Clone_Headers --
   -------------------

   function Clone_Headers (Ref : Header_List.List) return Header_List.List is
      Copy : Header_List.List;
      Cursor : Header_List.Cursor := First (Ref);
   begin
      while Cursor /= Header_List.No_Element loop
         Append (Copy, Clone_Header (Element (Cursor)));
         Next (Cursor);
      end loop;
      return Copy;
   end Clone_Headers;

   -------------------
   -- Clone_Message --
   -------------------

   function Clone_Message (Msg : Message) return Message is
      New_Msg : Message;
   begin
      New_Msg := (Ada.Finalization.Controlled
         with Contents => new Message_Record);
      New_Msg.Contents.all := (Ref_Count => 1,
                               Envelope_From => Msg.Contents.Envelope_From,
                               Headers => Clone_Headers (Msg.Contents.Headers),
                               Payload => Msg.Contents.Payload,
                               Is_Nested => Msg.Contents.Is_Nested);
      return New_Msg;
   end Clone_Message;

   --------------
   -- Reply_To --
   --------------

   function Reply_To
     (Msg            : Message'Class;
      From_Email     : String;
      From_Real_Name : String := "";
      Quote          : Boolean := True;
      Reply_All      : Boolean := True;
      Reply_Filter   : access function
                                (Recipient : Email_Address) return Boolean
                         := null;
      Local_Date     : Ada.Calendar.Time := Ada.Calendar.Clock;
      Charset        : String := Charset_US_ASCII) return Message
   is
      Reply      : Message := New_Message;
      H, H2, H3  : Header;
      Is_First   : Boolean;
      To_Quote   : Unbounded_String;
      Part_Iter  : Payload_Iterator;
      Payload    : Message;
      Who_Quoted : Unbounded_String;
   begin
      Set_Envelope_From (Reply, From_Email, Local_Date);

      H := Get_Header (Msg, "Subject");

      if H /= Null_Header then
         H2 := Create ("Subject", "Re: ");
         Append (H2, Get_Value (H));
         Replace_Header_Internal (Reply, H2, Append => False);
      end if;

      Replace_Header_Internal
        (Reply, Create ("Date", Format_Date (Local_Date)), Append => False);

      Set_From_Header (Reply, From_Email, From_Real_Name, Charset);

      H := Get_Header (Msg, "From");
      H2 := Create ("To", "");

      if H /= Null_Header then
         Append (H2, Get_Value (H));
         Flatten (H.Contents.Value, Result => Who_Quoted);
         Who_Quoted := Parse_Address (To_String (Who_Quoted)).Address;
      end if;

      Add_Header (Reply, H2);

      if Reply_All then
         H2 := Create ("Cc", "");

         Is_First := True;
         for Recipient of Get_Recipients (Msg) loop
            if Reply_Filter = null or else Reply_Filter (Recipient) then
               if Is_First then
                  Is_First := False;
               else
                  Append (H2, ", ");
               end if;
               Append (H2, Format_Address (Recipient));
            end if;
         end loop;

         if not Is_First then
            Add_Header (Reply, H2);
         end if;
      end if;

      H := Get_Header (Msg, "Message-Id");

      if H /= Null_Header then
         H2 := Create ("In-Reply-To", "");
         Append (H2, Get_Value (H));
         Add_Header (Reply, H2);

         H2 := Create ("References", "");
         H3 := Get_Header (Msg, "References");
         if H3 /= Null_Header then
            Append (H2, Get_Value (H3));
         else
            H3 := Get_Header (Msg, "In-Reply-To");
            if H3 /= Null_Header then
               Append (H2, Get_Value (H3));
            end if;
         end if;
         Append (H2, Get_Value (H));
         Add_Header (Reply, H2);
      end if;

      if Quote then
         if Is_Multipart (Msg) then
            To_Quote := Null_Unbounded_String;
            Part_Iter := Get_Payload (Msg);

            loop
               Next (Part_Iter, Item => Payload);
               exit when Payload = Null_Message;

               if Get_Main_Type (Get_Content_Type (Payload)) = "text" then
                  Get_Single_Part_Payload (Payload, To_Quote, Decode => True);
                  exit;
               end if;
            end loop;

         elsif Get_Main_Type (Get_Content_Type (Msg)) = "text" then
            Get_Single_Part_Payload (Msg, To_Quote, Decode => True);

         else
            To_Quote := Null_Unbounded_String;
         end if;

         if To_Quote /= Null_Unbounded_String then
            if Who_Quoted /= Null_Unbounded_String then
               Append (Who_Quoted, " wrote:" & ASCII.LF);
            end if;

            declare
               StrA : constant String := To_String (To_Quote);
               Start, Eol : Integer;
            begin
               Start := StrA'First;

               while Start <= StrA'Last loop
                  Eol := Integer'Min
                    (StrA'Last,
                     Next_Occurrence (StrA (Start .. StrA'Last), ASCII.LF));
                  Append (Who_Quoted, "> " & StrA (Start .. Eol));
                  Start := Eol + 1;
               end loop;

               Set_Text_Payload (Reply, To_String (Who_Quoted));
            end;
         end if;

      end if;

      return Reply;
   end Reply_To;

   -------------------------
   -- Set_Default_Headers --
   -------------------------

   procedure Set_Default_Headers
     (Msg            : in out Message'Class;
      From_Email     : String;
      Subject        : String := "No Subject";
      From_Real_Name : String := "";
      Local_Date     : Ada.Calendar.Time := Ada.Calendar.Clock;
      Charset        : String := Charset_US_ASCII)
   is
   begin
      Set_Envelope_From (Msg, From_Email, Local_Date);
      Replace_Header_Internal (Msg,
        Create ("Subject", Subject, Charset), Append => False);
      Replace_Header_Internal (Msg,
        Create ("Date", Format_Date (Local_Date)), Append => False);

      Set_From_Header (Msg, From_Email, From_Real_Name, Charset);
   end Set_Default_Headers;

   ---------------------
   -- Set_From_Header --
   ---------------------

   procedure Set_From_Header
     (Msg            : in out Message'Class;
      From_Email     : String;
      From_Real_Name : String;
      Charset        : String)
   is
      From_H : Header;
   begin
      From_H := Create
         ("From",
          Charset_String_List.List'(Format_Address
             (Email =>
                (Real_Name => To_Unbounded_String (From_Real_Name),
                 Address   => To_Unbounded_String (From_Email)),
              Charset => Charset)));
      Replace_Header_Internal (Msg, From_H, Append => False);
   end Set_From_Header;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Msg : in out Message) is
   begin
      if Msg.Contents /= null then
         Msg.Contents.Ref_Count := Msg.Contents.Ref_Count + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Msg : in out Message) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Message_Record, Message_Access);
      Contents : Message_Access := Msg.Contents;
   begin
      Msg.Contents := null;  --  Make Finalize idempotent
      if Contents /= null then
         Contents.Ref_Count := Contents.Ref_Count - 1;
         if Contents.Ref_Count = 0 then
            Unchecked_Free (Contents);
         end if;
      end if;
   end Finalize;

   -----------------------
   -- Set_Envelope_From --
   -----------------------

   procedure Set_Envelope_From (Msg : in out Message'Class; From : String) is
   begin
      Msg.Contents.Envelope_From := To_Unbounded_String (From);
   end Set_Envelope_From;

   -----------------------
   -- Set_Envelope_From --
   -----------------------

   procedure Set_Envelope_From
     (Msg        : in out Message'Class;
      Email      : String;
      Local_Date : Ada.Calendar.Time)
   is
   begin
      Msg.Contents.Envelope_From := To_Unbounded_String
        ("From " & Email & " " & Format_Date (Local_Date, From_Line => True));
   end Set_Envelope_From;

   -----------------------
   -- Get_Envelope_From --
   -----------------------

   function Get_Envelope_From (Msg : Message'Class) return String is
   begin
      return To_String (Msg.Contents.Envelope_From);
   end Get_Envelope_From;

   ------------------------
   -- Date_From_Envelope --
   ------------------------

   function Date_From_Envelope
     (Msg : Message'Class) return Ada.Calendar.Time
   is
      Str   : constant String := To_String (Msg.Contents.Envelope_From);
      Index : Natural := Str'First;
   begin
      if Str = "" then
         return No_Time;
      end if;

      Index := Index + 5;  --  Skips "From "
      Skip_Whitespaces (Str, Index);

      while Index <= Str'Last
        and then not Is_Whitespace (Str (Index))
      loop
         Index := Index + 1;
      end loop;

      return To_Time (Str (Index .. Str'Last), Format => Time_Envelope);
   end Date_From_Envelope;

   --------------------------
   -- Sender_From_Envelope --
   --------------------------

   function Sender_From_Envelope (Msg : Message'Class) return String is
      Str   : constant String := To_String (Msg.Contents.Envelope_From);
      Index : Natural := Str'First + 5; --  Skips "From"
      Stop  : Natural;
   begin
      Skip_Whitespaces (Str, Index);
      Stop := Index;

      while Stop <= Str'Last
        and then not Is_Whitespace (Str (Stop))
      loop
         Stop := Stop + 1;
      end loop;

      return Str (Index .. Stop - 1);
   end Sender_From_Envelope;

   ------------
   -- Create --
   ------------

   function Create
     (Name    : String;
      Value   : String;
      Charset : String := Charset_US_ASCII) return Header
   is
      V : Charset_String_List.List;
   begin
      Decode_Header (Value,
        Default_Charset => Charset,
        Result          => V,
        Where           => Identify_Header (Name));
      return Create (Name, V);
   end Create;

   function Create
     (Name  : String;
      Value : Charset_String_List.List) return Header
   is
   begin
      return (Ada.Finalization.Controlled with
                Contents =>
                  new Header_Record'
                        (Name   => To_Unbounded_String (To_Lower (Name)),
                         Value  => Value,
                         others => <>));
   end Create;

   ------------
   -- Append --
   ------------

   procedure Append
     (H       : in out Header'Class;
      Value   : String;
      Charset : String := Charset_US_ASCII)
   is
      L : Charset_String_List.List;
   begin
      Decode_Header (Value,
        Default_Charset => Charset,
        Result          => L,
        Where           => Identify_Header (To_String (H.Contents.Name)));
      Splice (H.Contents.Value, Charset_String_List.No_Element, L);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (H : in out Header'Class; Value : Charset_String_List.List)
   is
      C : Charset_String_List.Cursor := First (Value);
   begin
      while Has_Element (C) loop
         Append (H.Contents.Value, Element (C));
         Next (C);
      end loop;
   end Append;

   ---------------
   -- To_String --
   ---------------

   procedure To_String
     (H                : Header'Class;
      Max_Line_Len     : Positive := Default_Max_Header_Line_Length;
      Show_Header_Name : Boolean := True;
      Result           : out Unbounded_String)
   is
      Max     : Positive := Max_Line_Len - 2 - Length (H.Contents.Name);
      N       : String := To_String (H.Contents.Name);
      Value   : constant Charset_String_List.List := Get_Value (H);
      Encoded : Unbounded_String;

      Uppercase_Next : Boolean;
      Next           : Natural;
   begin
      To_String (Value, Encoded, Identify_Header (N));

      if Show_Header_Name then
         --  Fix up casing of header name

         if N = "message-id" then
            N := Message_ID;

         elsif N = "cc" then
            N := CC;

         else
            if N'Length >= 5 and then N (N'First .. N'First + 4) = "mime-" then
               N (N'First .. N'First + 3) := "MIME";
               Next := N'First + 5;
            else
               Next := N'First;
            end if;

            Uppercase_Next := True;
            while Next <= N'Last loop
               if Uppercase_Next then
                  N (Next) := To_Upper (N (Next));
                  Uppercase_Next := False;
               end if;

               if N (Next) = '-' then
                  Uppercase_Next := True;
               end if;
               Next := Next + 1;
            end loop;
         end if;
      end if;

      --  Fold continuation lines

      declare
         Str    : String := To_String (Encoded);
         Last   : Natural;
         Index, Index2 : Integer;

         Offset : Integer := 0;
         --  Count of LF characters skipped so far

      begin
         --  Flatten the header on a single line, eliminating newline
         --  characters.

         for J in Str'Range loop
            if Str (J) = ASCII.LF then
               Offset := Offset + 1;
            elsif Offset > 0 then
               Str (J - Offset) := Str (J);
            end if;
         end loop;

         Last := Str'Last - Offset;

         if Show_Header_Name and then Last <= Max then
            if Last = 0 then  --  Empty header
               Result := To_Unbounded_String (N & ": ");
            elsif Element (Encoded, 1) = ' ' then
               Result := To_Unbounded_String (N & ':' & Str (1 .. Last));
            else
               Result := To_Unbounded_String (N & ": " & Str (1 .. Last));
            end if;
            return;

         elsif not Show_Header_Name and then Last <= Max_Line_Len then
            if Offset = 0 then
               Result := Encoded;  --  Save a string copy
            else
               Result := To_Unbounded_String (Str (1 .. Last));
            end if;
            return;
         end if;

         Result := Null_Unbounded_String;
         Index  := Str'First;

         while Index <= Last loop
            --  Only split on spaces. To keep Content-Type headers as much as
            --  possible on a single line, we split on the first blank space
            --  after the theoretical split point.

            Index2 := Integer'Min (Index + Max - 1, Last);
            loop
               Index2 := Index2 + 1;
               exit when Index2 > Last or else Str (Index2) = ' ';
            end loop;

            --  Index2 points right after last non-blank character

            Append (Result, Str (Index .. Index2 - 1));

            --  Do not print a last line containing only white spaces, this
            --  might confuse mailers.

            if Index2 < Last then
               Append (Result, ASCII.LF & ' ');
            end if;

            Index := Index2 + 1;
            Max   := Max_Line_Len;
         end loop;

         if Show_Header_Name then
            if Length (Result) = 0 or else Element (Result, 1) = ' ' then
               Result := N & ':' & Result;
            else
               Result := N & ": " & Result;
            end if;
         end if;
      end;
   end To_String;

   function To_String
     (H                : Header'Class;
      Max_Line_Len     : Positive := Default_Max_Header_Line_Length;
      Show_Header_Name : Boolean := True) return String
   is
      Result : Unbounded_String;
   begin
      To_String (H, Max_Line_Len, Show_Header_Name, Result);
      return To_String (Result);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   procedure To_String
     (Payload             : Message_Payload;
      Header_Max_Line_Len : Positive;
      Content_Filter      : Payload_Filter := null;
      Msg                 : Message'Class;
      Append_To           : in out Unbounded_String)
   is
      C          : Message_List.Cursor;
      Attachment : Message;
   begin
      case Payload.Multipart is
         when True =>
            declare
               Parts : array (1 .. Length (Payload.Parts)) of Boolean :=
                 (others => True);
               Payload_Count : Natural := Parts'Length;
            begin
               --  First check how many payloads needs to be output
               if Content_Filter /= null then
                  C := First (Payload.Parts);
                  Payload_Count := 0;

                  for P in Parts'Range loop
                     Parts (P) := Content_Filter (Element (C));
                     if Parts (P) then
                        Payload_Count := Payload_Count + 1;
                     end if;
                     Next (C);
                  end loop;
               end if;

               --  At least one payload : create a boundary if necessary

               if Payload_Count > 0 then
                  Set_Boundary (Msg);
               end if;

               declare
                  Boundary : constant String := Get_Boundary (Msg);
               begin
                  if Payload.Preamble /= Null_Unbounded_String then
                     Append (Append_To, Payload.Preamble & ASCII.LF);
                  end if;

                  C := First (Payload.Parts);
                  for P in Parts'Range loop
                     if Parts (P) then
                        if P /= Parts'First then
                           Append (Append_To, ASCII.LF);
                        end if;
                        Append (Append_To, "--" & Boundary & ASCII.LF);
                        Attachment := Element (C);
                        To_String (Attachment.Contents.Headers,
                                   Header_Max_Line_Len,
                                   Header_Max_Line_Len,
                                   Append_To => Append_To);
                        To_String (Attachment.Contents.Payload,
                                   Header_Max_Line_Len,
                                   Msg       => Attachment,
                                   Append_To => Append_To);
                     end if;
                     Next (C);
                  end loop;

                  Append
                    (Append_To, ASCII.LF & "--" & Boundary & "--" & ASCII.LF);

                  if Payload.Epilogue /= Null_Unbounded_String then
                     Append (Append_To, ASCII.LF & Payload.Epilogue);
                  end if;
               end;
            end;

         when False =>
            Append (Append_To, Payload.Text);
      end case;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   procedure To_String
     (Headers              : Header_List.List;
      Header_Max_Line_Len  : Positive;
      Subject_Max_Line_Len : Positive;
      Filter               : Header_Filter := null;
      Append_To            : in out Unbounded_String)
   is
      H   : Header_List.Cursor := First (Headers);
      Tmp : Unbounded_String;
   begin
      while Has_Element (H) loop
         if Filter = null or else Filter (Element (H)) then
            if Get_Name (Element (H)) = "subject" then
               To_String (Element (H), Subject_Max_Line_Len, Result => Tmp);
            else
               To_String (Element (H), Header_Max_Line_Len, Result => Tmp);
            end if;

            if Tmp /= Null_Unbounded_String then
               Append (Append_To, Tmp);
               Append (Append_To, ASCII.LF);
            end if;
         end if;

         Next (H);
      end loop;

      Append (Append_To, ASCII.LF);
   end To_String;

   ----------
   -- Size --
   ----------

   function Size
     (Msg                 : Message;
      Include_Attachments : Boolean) return Long_Integer
   is
      Total : Long_Integer := 0;
      C     : Message_List.Cursor;
   begin
      if Is_Multipart (Msg) then
         Total := Total + Long_Integer (Length (Msg.Contents.Payload.Preamble))
           + Long_Integer (Length (Msg.Contents.Payload.Epilogue));
         C := First (Msg.Contents.Payload.Parts);

         while Has_Element (C) loop
            if Include_Attachments then
               Total := Total + Size (Element (C), True);
            elsif Get_Content_Type (Element (C)) = Text_Plain then
               Total := Total + Size (Element (C), True);
               exit;
            end if;

            Next (C);
         end loop;

      else
         Total := Total + Long_Integer (Length (Msg.Contents.Payload.Text));
      end if;

      return Total;
   end Size;

   ---------------
   -- To_String --
   ---------------

   procedure To_String
     (Msg                  : Message'Class;
      Envelope             : Boolean  := False;
      Header_Max_Line_Len  : Positive := Default_Max_Header_Line_Length;
      Subject_Max_Line_Len : Positive := Default_Max_Header_Line_Length;
      Content_Filter       : Payload_Filter := null;
      Filter               : Header_Filter := null;
      Decode               : Boolean := False;
      Quote_From           : Boolean := False;
      Result               : out Unbounded_String)
   is
      Encoded_Payload : Unbounded_String;
      Payload         : Unbounded_String;
      H               : Header;
      Encoding        : Encoding_Type;
      Encoding_Str    : Unbounded_String;
   begin
      Result := Null_Unbounded_String;

      if Envelope then
         Append (Result, Msg.Contents.Envelope_From);
         Append (Result, ASCII.LF);
      end if;

      --  First convert the payload. This way we know how many payloads are
      --  output, and whether a boundary is necessary or not.

      To_String
        (Msg.Contents.Payload, Header_Max_Line_Len,
         Msg            => Msg,
         Content_Filter => Content_Filter,
         Append_To      => Encoded_Payload);

      if Decode then
         H := Get_Header (Msg, Content_Transfer_Encoding);

         if H.Contents = null then
            Encoding := Encoding_7bit;
         else
            Flatten (H.Contents.Value, Result => Encoding_Str);

            declare
               Encode : constant String := To_Lower
                 (Trim (To_String (Encoding_Str), Ada.Strings.Both));
            begin
               if Encode = "base64" then
                  Encoding := Encoding_Base64;
               elsif Encode = "quoted-printable" then
                  Encoding := Encoding_QP;
               else
                  Encoding := Encoding_7bit;
               end if;
            end;
         end if;

         case Encoding is
            when Encoding_Base64 =>
               Base64_Decode (To_String (Encoded_Payload), Payload);
            when Encoding_QP =>
               Quoted_Printable_Decode (To_String (Encoded_Payload), Payload);
            when others =>
               Payload := Encoded_Payload;
         end case;

      else
         Payload := Encoded_Payload;
      end if;

      To_String
        (Msg.Contents.Headers, Header_Max_Line_Len,
         Subject_Max_Line_Len, Filter,
         Append_To => Result);

      if Quote_From then
         declare
            Payload_Str : constant String := To_String (Payload);
            J           : Integer := Payload_Str'First;
            Copy_From   : Natural := Payload_Str'First;
         begin
            while J < Payload_Str'Last loop

               --  Skip until the beginning of a new line

               while J < Payload_Str'Last
                 and then Payload_Str (J) = ASCII.LF
               loop
                  J := J + 1;
               end loop;

               --  If the new line starts with From_

               if J + 4 <= Payload_Str'Length
                 and then Payload_Str (J .. J + 4) = "From "
               then
                  Append (Result, Payload_Str (Copy_From .. J - 1));
                  Append (Result, ">");
                  Copy_From := J;
                  J := J + 5;
               end if;

               --  Skip till end of line

               while J < Payload_Str'Last
                 and then Payload_Str (J) /= ASCII.LF
               loop
                  J := J + 1;
               end loop;
            end loop;

            Append (Result, Payload_Str (Copy_From .. Payload_Str'Last));
         end;
      else
         Append (Result, Payload);
      end if;
   end To_String;

   -------------
   -- To_Time --
   -------------

   function To_Time (H : Header'Class) return Ada.Calendar.Time is
      Tmp  : Unbounded_String;
   begin
      --  For portability, we could use To_String (H.Value), but that is
      --  slower.
      if H.Contents = null then
         return No_Time;
      else
         Flatten (H.Contents.Value, Result => Tmp);
         return To_Time (To_String (Tmp));
      end if;
   end To_Time;

   ----------------
   -- Add_Header --
   ----------------

   procedure Add_Header (Msg : in out Message'Class; H : Header'Class) is
   begin
      Append (Msg.Contents.Headers, Header (H));
   end Add_Header;

   ----------------
   -- Get_Header --
   ----------------

   function Get_Header (Msg : Message'Class; Name : String) return Header is
      Iter : Header_List.Cursor;
      N    : constant String := To_Lower (Name);
   begin
      if Msg.Contents /= null then
         Iter := First (Msg.Contents.Headers);

         while Has_Element (Iter) loop
            if Element (Iter).Contents.Name = N then
               return Element (Iter);
            end if;

            Next (Iter);
         end loop;
      end if;
      return Null_Header;
   end Get_Header;

   --------------------
   -- Delete_Headers --
   --------------------

   procedure Delete_Headers (Msg : Message'Class; Name : String) is
      Iter  : Header_List.Cursor := First (Msg.Contents.Headers);
      Iter2 : Header_List.Cursor;
      N     : constant String := To_Lower (Name);
   begin
      while Has_Element (Iter) loop
         Iter2 := Next (Iter);

         if Name = ""
           or else Element (Iter).Contents.Name = N
         then
            Delete (Msg.Contents.Headers, Iter);
         end if;

         Iter := Iter2;
      end loop;
   end Delete_Headers;

   -------------------
   -- Delete_Header --
   -------------------

   procedure Delete_Header (Msg : Message'Class; H : Header'Class) is
      Iter : Header_List.Cursor := First (Msg.Contents.Headers);
   begin
      while Has_Element (Iter) loop
         if Element (Iter).Contents = H.Contents then
            Delete (Msg.Contents.Headers, Iter);
            return;
         end if;

         Next (Iter);
      end loop;
   end Delete_Header;

   --------------------
   -- Replace_Header --
   --------------------

   procedure Replace_Header (Msg : Message'Class; H : Header'Class) is
   begin
      Replace_Header_Internal (Msg, H, Append => True);
   end Replace_Header;

   -----------------------------
   -- Replace_Header_Internal --
   -----------------------------

   procedure Replace_Header_Internal
     (Msg : Message'Class; H : Header'Class; Append : Boolean)
   is
      Iter     : Header_List.Cursor := First (Msg.Contents.Headers);
      Iter2    : Header_List.Cursor;
      Is_First : Boolean := True;
   begin
      while Has_Element (Iter) loop
         Iter2 := Next (Iter);

         if Element (Iter).Contents.Name = H.Contents.Name then
            if Is_First then
               Replace_Element (Msg.Contents.Headers, Iter, Header (H));
               Is_First := False;
            else
               Delete (Msg.Contents.Headers, Iter);
            end if;
         end if;

         Iter := Iter2;
      end loop;

      if Is_First then
         if Append then
            Header_List.Append (Msg.Contents.Headers, Header (H));
         else
            Prepend (Msg.Contents.Headers, Header (H));
         end if;
      end if;
   end Replace_Header_Internal;

   -----------------
   -- Get_Headers --
   -----------------

   function Get_Headers
     (Msg : Message'Class; Name : String := "") return Header_Iterator
   is
      C : Header_List.Cursor := First (Msg.Contents.Headers);
      N : constant Unbounded_String := To_Unbounded_String (To_Lower (Name));
   begin
      if Name /= "" then
         while Has_Element (C)
           and then Element (C).Contents.Name /= N
         loop
            Next (C);
         end loop;
      end if;

      return (C, N);
   end Get_Headers;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Header_Iterator; H : out Header) is
   begin
      if Has_Element (Iter.Cursor) then
         H := Element (Iter.Cursor);

         Next (Iter.Cursor);

         if Length (Iter.Name) /= 0 then
            while Has_Element (Iter.Cursor)
              and then Element (Iter.Cursor).Contents.Name /= Iter.Name
            loop
               Next (Iter.Cursor);
            end loop;
         end if;

      else
         H := Null_Header;
      end if;
   end Next;

   ----------------------
   -- Set_Text_Payload --
   ----------------------

   procedure Set_Text_Payload
     (Msg         : Message'Class;
      Payload     : String;
      MIME_Type   : String  := Text_Plain;
      Disposition : String  := "";
      Charset     : String  := Charset_US_ASCII;
      Prepend     : Boolean := False)
   is
      Msg2  : Message;
      H_CT  : Header := Create (Content_Type, MIME_Type);
      H_CTE : Header := Null_Header;
   begin
      if Charset /= "" then
         Set_Param (H_CT, "charset", Charset);
         H_CTE := Create (Content_Transfer_Encoding,
                          (if Charset = Charset_US_ASCII
                           then "7bit"
                           else "8bit"));
      end if;

      if Msg.Contents.Payload.Multipart then
         Msg2 := New_Message (MIME_Type => "");

         Replace_Header (Msg2, H_CT);
         if H_CTE /= Null_Header then
            Replace_Header (Msg2, H_CTE);
         end if;

         if Disposition /= "" then
            Add_Header (Msg2, Create (Content_Disposition, Disposition));
         end if;

         Set_Unbounded_String (Msg2.Contents.Payload.Text, Payload);

         if Prepend then
            Message_List.Prepend (Msg.Contents.Payload.Parts, Msg2);
         else
            Message_List.Append (Msg.Contents.Payload.Parts, Msg2);
         end if;

      else
         if MIME_Type /= "" and not Prepend then
            Replace_Header (Msg, H_CT);
            if H_CTE = Null_Header then
               Delete_Headers (Msg, Content_Transfer_Encoding);
            else
               Replace_Header (Msg, H_CTE);
            end if;

            Delete_Headers (Msg, Content_Disposition);
         end if;

         if Prepend then
            Msg.Contents.Payload.Text := Payload & Msg.Contents.Payload.Text;
         else
            --  Do not use Set_Unbounded_String, which has a memory leak in the
            --  GNAT implementation ???
            Set_Unbounded_String (Msg.Contents.Payload.Text, Payload);
         end if;
      end if;
   end Set_Text_Payload;

   -----------------------------
   -- Get_Single_Part_Payload --
   -----------------------------

   procedure Get_Single_Part_Payload
     (Msg     : Message'Class;
      Payload : out Unbounded_String;
      Decode  : Boolean := False)
   is
      H            : Header;
      Encoding     : Encoding_Type;
      Encoding_Str : Unbounded_String;
   begin
      if Msg.Contents.Payload.Multipart then
         raise Multipart_Error;

      elsif Decode then
         H := Get_Header (Msg, Content_Transfer_Encoding);

         if H.Contents = null then
            Encoding := Encoding_7bit;

         else
            Flatten (H.Contents.Value, Result => Encoding_Str);
            declare
               Encode : constant String := To_Lower
                 (Trim (To_String (Encoding_Str), Ada.Strings.Both));
            begin
               if Encode = "base64" then
                  Encoding := Encoding_Base64;
               elsif Encode = "quoted-printable" then
                  Encoding := Encoding_QP;
               else
                  Encoding := Encoding_7bit;
               end if;
            end;
         end if;

         case Encoding is
            when Encoding_Base64 =>
               Base64_Decode (To_String (Msg.Contents.Payload.Text), Payload);
            when Encoding_QP =>
               Quoted_Printable_Decode
                 (To_String (Msg.Contents.Payload.Text), Payload);
            when others =>
               Payload := Msg.Contents.Payload.Text;
         end case;

      else
         Payload := Msg.Contents.Payload.Text;
      end if;
   end Get_Single_Part_Payload;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (H : Header'Class) return String is
   begin
      return To_String (H.Contents.Name);
   end Get_Name;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (H : Header'Class) return Charset_String_List.List is
   begin
      if H.Contents = null then
         return Charset_String_List.Empty_List;
      else
         return H.Contents.Value;
      end if;
   end Get_Value;

   ------------------
   -- Set_Epilogue --
   ------------------

   procedure Set_Epilogue (Msg : in out Message'Class; Epilogue : String) is
   begin
      Convert_To_Multipart (Msg);
      Msg.Contents.Payload.Epilogue := To_Unbounded_String (Epilogue);
   end Set_Epilogue;

   ------------------
   -- Set_Preamble --
   ------------------

   procedure Set_Preamble (Msg :  in out Message'Class; Preamble : String) is
   begin
      Convert_To_Multipart (Msg);
      Msg.Contents.Payload.Preamble := To_Unbounded_String (Preamble);
   end Set_Preamble;

   ------------------
   -- Is_Multipart --
   ------------------

   function Is_Multipart (Msg : Message'Class) return Boolean is
   begin
      return Msg.Contents.Payload.Multipart;
   end Is_Multipart;

   -----------------
   -- Get_Payload --
   -----------------

   function Get_Payload (Msg : Message'Class) return Payload_Iterator is
   begin
      if Msg.Contents.Payload.Multipart then
         return (Cursor => First (Msg.Contents.Payload.Parts),
                 Msg    => Null_Message);
      else
         return (Cursor => Message_List.No_Element,
                 Msg    => Message (Msg));
      end if;
   end Get_Payload;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Payload_Iterator; Item : out Message) is
   begin
      if Has_Element (Iter.Cursor) then
         Item := Element (Iter.Cursor);
         Next (Iter.Cursor);
      elsif Iter.Msg /= Null_Message then
         Item := Iter.Msg;
         Iter.Msg := Null_Message;
      else
         Item := Null_Message;
      end if;
   end Next;

   --------------------
   -- Delete_Payload --
   --------------------

   procedure Delete_Payload
     (Msg : in out Message'Class; Iter : in out Payload_Iterator) is
   begin
      Delete (Msg.Contents.Payload.Parts, Iter.Cursor);
   end Delete_Payload;

   ----------------------
   -- Get_Content_Type --
   ----------------------

   function Get_Content_Type (Msg : Message'Class) return String is
      T : constant String := Get_Type (Get_Header (Msg, Content_Type));
   begin
      if T /= "" then
         return T;

      elsif Msg.Contents.Is_Nested then
         return Message_RFC822;

      else
         return Text_Plain;
      end if;
   end Get_Content_Type;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (H : Header) return String is
      use Ada.Strings;

      H_Ustr : Unbounded_String;
   begin
      if H = Null_Header then
         return "";
      end if;

      Flatten (H.Contents.Value, Result => H_Ustr);
      declare
         H_Str : constant String := Trim (To_String (H_Ustr), Both);
         SC    : Integer;
      begin
         SC := H_Str'First;
         while SC <= H_Str'Last
                 and then
               not (Is_Whitespace (H_Str (SC)) or else H_Str (SC) = ';')
         loop
            SC := SC + 1;
         end loop;

         return To_Lower (H_Str (H_Str'First .. SC - 1));
      end;
   end Get_Type;

   ----------------------------
   -- Convert_To_Single_Part --
   ----------------------------

   procedure Convert_To_Single_Part (Msg : in out Message'Class) is
      Attach : Message;
   begin
      if Msg.Contents.Payload.Multipart then
         if Length (Msg.Contents.Payload.Parts) = 0 then
            Msg.Contents.Payload :=
              (Multipart => False,
               Text      => Null_Unbounded_String);
            Replace_Header (Msg, Create (Content_Type, Text_Plain));

         elsif Length (Msg.Contents.Payload.Parts) = 1 then
            Attach := Element (First (Msg.Contents.Payload.Parts));
            if Is_Multipart (Attach) then
               Msg.Contents.Payload :=
                 (Multipart => True,
                  Parts     => Attach.Contents.Payload.Parts,
                  Preamble  => Attach.Contents.Payload.Preamble,
                  Epilogue  => Attach.Contents.Payload.Epilogue);
               Replace_Header (Msg, Get_Header (Attach, Content_Type));
            else
               Msg.Contents.Payload :=
                 (Multipart => False,
                  Text      => Attach.Contents.Payload.Text);
               Replace_Header (Msg, Get_Header (Attach, Content_Type));

               if Get_Header (Attach, Content_Transfer_Encoding) /=
                 Null_Header
               then
                  Replace_Header
                    (Msg, Get_Header (Attach, Content_Transfer_Encoding));
               end if;
            end if;
         end if;
      end if;
   end Convert_To_Single_Part;

   --------------------------
   -- Convert_To_Multipart --
   --------------------------

   procedure Convert_To_Multipart (Msg : Message'Class) is
      Part : constant Message_List.List := Message_List.Empty_List;
   begin
      if not Msg.Contents.Payload.Multipart then
         declare
            Old : constant String := To_String (Msg.Contents.Payload.Text);
         begin
            Msg.Contents.Payload :=
              (Multipart => True,
               Parts     => Part,
               Preamble  => Null_Unbounded_String,
               Epilogue  => Null_Unbounded_String);

            if Get_Main_Type (Get_Content_Type (Msg)) /= "multipart" then
               declare
                  Boundary : constant String := Get_Boundary (Msg);
               begin
                  if Boundary /= "" then
                     Replace_Header
                       (Msg,
                        Create
                          (Content_Type, "multipart/mixed; boundary="""
                           & Boundary & '"'));
                  else
                     Replace_Header
                       (Msg, Create (Content_Type, "multipart/mixed"));
                  end if;
               end;
               Replace_Header (Msg, Create (MIME_Version, "1.0"));
               Delete_Headers (Msg, Content_Transfer_Encoding);
            end if;

            if Old /= "" then
               Set_Text_Payload (Msg, Old, MIME_Type => Text_Plain);
            end if;
         end;
      end if;
   end Convert_To_Multipart;

   -----------------
   -- Add_Payload --
   -----------------

   procedure Add_Payload (Msg : in out Message'Class;
                          Payload : Message;
                          First : Boolean := False) is
   begin
      Convert_To_Multipart (Msg);
      Payload.Contents.Is_Nested := True;
      if First then
         Prepend (Msg.Contents.Payload.Parts, Payload);
      else
         Append (Msg.Contents.Payload.Parts, Payload);
      end if;

   end Add_Payload;

   ----------------
   -- Attach_Msg --
   ----------------

   procedure Attach_Msg
     (Msg         : in out Message'Class;
      Attach      : Message'Class;
      Description : String := "")
   is
      Attachment : constant Message :=
                     New_Message (MIME_Type => Message_RFC822);
      Tmp        : Unbounded_String;
   begin
      if Description /= "" then
         Replace_Header
           (Attachment, Create (Content_Description, Description));
      end if;

      To_String (Attach, Result => Tmp);
      Set_Text_Payload (Attachment, To_String (Tmp),
                        Charset   => "",
                        MIME_Type => Message_RFC822);
      Replace_Header (Attachment, Create (Content_Disposition, "inline"));

      Add_Payload (Msg, Attachment);
   end Attach_Msg;

   ------------
   -- Attach --
   ------------

   procedure Attach
     (Msg                  : in out Message'Class;
      Path                 : Virtual_File;
      MIME_Type            : String := Application_Octet_Stream;
      Recommended_Filename : Virtual_File := No_File;
      Description          : String := "";
      Charset              : String := Charset_US_ASCII;
      Disposition          : Disposition_Type := Disposition_Attachment;
      Encoding             : Encoding_Type    := Encoding_Base64)
   is
      Attachment : Message := New_Message (MIME_Type => "");
      Str        : GNAT.Strings.String_Access;

   begin
      declare
         F : Unbounded_String;
      begin
         Convert_To_Multipart (Msg);

         if Get_Main_Type (MIME_Type) = "text" then
            Replace_Header
              (Attachment,
               Create
                 (Content_Type,
                  MIME_Type & "; charset=""" & Charset & '"'));
         else
            Replace_Header (Attachment, Create (Content_Type, MIME_Type));
         end if;

         if Description /= "" then
            Replace_Header
              (Attachment, Create (Content_Description, Description));
         end if;

         case Disposition is
            when Disposition_Attachment =>
               if Recommended_Filename = No_File then
                  Replace_Header
                    (Attachment,
                     Create
                       (Content_Disposition,
                        "attachment; filename="""
                        & (+Base_Name (Path)) & '"'));
               else
                  Replace_Header
                    (Attachment,
                     Create
                       (Content_Disposition,
                        "attachment; filename="""
                        & (+Base_Name (Recommended_Filename)) & '"'));
               end if;

            when Disposition_Inline =>
               if Recommended_Filename = No_File then
                  Replace_Header
                    (Attachment,
                     Create
                       (Content_Disposition,
                        "inline; filename="""
                        & (+Base_Name (Path)) & '"'));
               else
                  Replace_Header
                    (Attachment,
                     Create
                       (Content_Disposition,
                        "inline; filename="""
                        & (+Base_Name (Recommended_Filename)) & '"'));
               end if;
         end case;

         Str := Read_File (Path);

         case Encoding is
            when Encoding_Base64 =>
               Base64_Encode (Str => Str.all, Result => F);
               Add_Header (Attachment,
                           Create (Content_Transfer_Encoding, "base64"));
               Set_Unbounded_String
                 (Attachment.Contents.Payload.Text, To_String (F));

            when Encoding_QP =>
               Quoted_Printable_Encode
                 (Str    => Str.all, Where  => Text, Result => F);
               Add_Header
                 (Attachment,
                  Create (Content_Transfer_Encoding, "quoted-printable"));
               Set_Unbounded_String
                 (Attachment.Contents.Payload.Text, To_String (F));

            when Encoding_7bit =>
               Add_Header
                 (Attachment, Create (Content_Transfer_Encoding, "7bit"));
               Set_Unbounded_String
                 (Attachment.Contents.Payload.Text, Str.all);

            when Encoding_8bit =>
               Add_Header
                 (Attachment, Create (Content_Transfer_Encoding, "8bit"));
               Set_Unbounded_String
                 (Attachment.Contents.Payload.Text, Str.all);

            when Encoding_Binary =>
               Add_Header
                 (Attachment, Create (Content_Transfer_Encoding, "binary"));
               Set_Unbounded_String
                 (Attachment.Contents.Payload.Text, Str.all);
         end case;
      end;

      Free (Str);
      Append (Msg.Contents.Payload.Parts, Attachment);
   end Attach;

   ---------------------
   -- Get_Param_Index --
   ---------------------

   procedure Get_Param_Index
     (H          : Header'Class;
      Param_Name : String;
      C          : out Charset_String_List.Cursor;
      Semicolon  : out Integer;
      Name_Start : out Integer;
      Name_End   : out Integer;
      Value_End  : out Integer) is
   begin
      C := First (H.Contents.Value);
      Semicolon  := 0;  --  Initialize variables to avoid GNAT warnings
      Name_Start := 0;
      Name_End   := 0;
      Value_End  := 0;

      while Has_Element (C) loop
         declare
            Str      : constant String := To_String (Element (C).Contents);
            Index    : Natural := Str'First;
            Stop     : Natural;
            Val_Stop : Natural;
         begin
            while Index <= Str'Last loop
               Index := Next_Occurrence (Str (Index .. Str'Last), ';');

               if Index <= Str'Last then
                  Semicolon := Index;
                  Index := Index + 1;
                  Skip_Whitespaces (Str, Index);

                  Stop := Next_Occurrence (Str (Index + 1 .. Str'Last), '=');
                  if Stop < Str'Last then
                     Val_Stop := Next_Occurrence
                       (Str (Stop + 1 .. Str'Last), ';');

                     if To_Lower
                       (Str (Index .. Stop - 1)) = To_Lower (Param_Name)
                     then
                        Name_Start := Index;
                        Name_End   := Stop - 1;
                        Value_End  := Val_Stop - 1;
                        return;
                     end if;

                     Index := Val_Stop;

                  else
                     Index := Stop;
                  end if;
               end if;
            end loop;
         end;

         Next (C);
      end loop;
   end Get_Param_Index;

   ---------------
   -- Set_Param --
   ---------------

   procedure Set_Param
     (H : in out Header'Class; Param_Name : String; Param_Value : String)
   is
      C : Charset_String_List.Cursor := First (H.Contents.Value);
      Semicolon, Name_Start, Name_End, Val_End : Integer;
      Str : constant String := "; " & Param_Name & "=""" & Param_Value & '"';
   begin
      Get_Param_Index
        (H, Param_Name, C, Semicolon, Name_Start, Name_End, Val_End);

      if Has_Element (C) then
         Replace_Element
           (H.Contents.Value,
            C,
            (Contents =>
               Unbounded_Slice (Element (C).Contents, 1, Semicolon - 1)
               & Str
               & Unbounded_Slice
                 (Element (C).Contents,
                  Val_End + 1, Length (Element (C).Contents)),
             Charset  => Element (C).Charset));
      else
         Append
           (H.Contents.Value,
            (Contents => To_Unbounded_String (Str),
             Charset  => To_Unbounded_String (Charset_US_ASCII)));
      end if;
   end Set_Param;

   ---------------
   -- Get_Param --
   ---------------

   function Get_Param (H : Header'Class; Param_Name : String) return String is
      C : Charset_String_List.Cursor;
      Semicolon, Name_Start, Name_End, Val_End : Integer;

      function Get_Val return String;
      --  Return the value, omitting surrounding quotes if any

      function Get_Val return String is
         Str : constant String :=
           Slice (Element (C).Contents, Name_End + 2, Val_End);
      begin
         if Str (Str'First) = '"' then
            return Str (Str'First + 1 .. Str'Last - 1);
         else
            return Str;
         end if;
      end Get_Val;

   begin
      if H.Contents /= null then
         C := First (H.Contents.Value);
         Get_Param_Index
           (H, Param_Name, C, Semicolon, Name_Start, Name_End,
            Val_End);
         if Has_Element (C) then
            return Get_Val;

         else
            --  Support for continuation headers
            --  http://greenbytes.de/tech/webdav/rfc2231.html#rfc.section.3
            --  where a header can be split onto several lines

            declare
               Current : Natural := 0;
               Val     : Unbounded_String;
            begin
               loop
                  Get_Param_Index
                    (H, Param_Name & "*" & Image (Current, Min_Width => 1),
                     C, Semicolon, Name_Start, Name_End,
                     Val_End);
                  exit when not Has_Element (C);

                  Append (Val, Get_Val);

                  Current := Current + 1;
               end loop;

               return To_String (Val);
            end;
         end if;
      end if;

      return "";
   end Get_Param;

   ------------------
   -- Delete_Param --
   ------------------

   procedure Delete_Param (H : in out Header'Class; Param_Name : String) is
      C : Charset_String_List.Cursor := First (H.Contents.Value);
      Semicolon, Name_Start, Name_End, Val_End : Integer;
   begin
      Get_Param_Index
        (H, Param_Name, C, Semicolon, Name_Start, Name_End, Val_End);

      if Has_Element (C) then
         Replace_Element
           (H.Contents.Value,
            C,
            (Contents =>
               Unbounded_Slice (Element (C).Contents, 1, Semicolon - 1)
               & Unbounded_Slice
                 (Element (C).Contents,
                  Val_End + 1, Length (Element (C).Contents)),
             Charset  => Element (C).Charset));
      end if;
   end Delete_Param;

   ------------------
   -- Get_Boundary --
   ------------------

   function Get_Boundary (Msg : Message'Class) return String is
      Content_T : constant Header := Get_Header (Msg, Content_Type);
   begin
      if Content_T = Null_Header then
         return "";
      end if;

      return Get_Param (Content_T, "boundary");
   end Get_Boundary;

   -----------------------
   -- Get_Encoding_Type --
   -----------------------

   function Get_Encoding_Type (Msg : Message'Class) return Encoding_Type is
      H : constant Header := Get_Header (Msg, Content_Transfer_Encoding);
   begin
      if H = Null_Header then
         return Encoding_7bit;

      else
         declare
            Asc : Unbounded_String;
         begin
            Flatten (H.Contents.Value, Result => Asc);
            if Asc = "base64" then
               return Encoding_Base64;
            elsif Asc = "quoted-printable" then
               return Encoding_QP;
            elsif Asc = "binary" then
               return Encoding_Binary;
            elsif Asc = "8bit" then
               return Encoding_8bit;
            else
               return Encoding_7bit;
            end if;
         end;
      end if;
   end Get_Encoding_Type;

   ----------------------------
   -- Has_Line_Starting_With --
   ----------------------------

   function Has_Line_Starting_With
     (Text : Unbounded_String; Starts_With : String) return Boolean
   is
      StrA  : constant String := To_String (Text);
      Index : Natural;
      Eol   : Natural;
   begin
      Index := StrA'First;

      while Index <= StrA'Last loop
         Eol := Next_Occurrence (StrA (Index .. StrA'Last), ASCII.LF);
         if Index + Starts_With'Length - 1 <= StrA'Last
           and then StrA (Index .. Index + Starts_With'Length - 1) =
             Starts_With
         then
            return True;
         end if;

         Index := Eol + 1;
      end loop;
      return False;
   end Has_Line_Starting_With;

   --------------------
   -- Check_Boundary --
   --------------------

   function Check_Boundary
     (Msg : Message'Class; Boundary : String) return Boolean
   is
      Iter  : Payload_Iterator;
      Msg2  : Message;
      Bound : constant String := "--" & Boundary;
   begin
      if Is_Multipart (Msg) then
         if Has_Line_Starting_With (Msg.Contents.Payload.Preamble, Bound)
           or else Has_Line_Starting_With
             (Msg.Contents.Payload.Epilogue, Bound)
         then
            return False;
         end if;

         Iter := Get_Payload (Msg);
         loop
            Next (Iter, Item => Msg2);
            exit when Msg2 = Null_Message;

            case Get_Encoding_Type (Msg2) is
               when Encoding_QP | Encoding_Base64 =>
                  --  No check needs to be done, since the boundary always
                  --  includes the =_ sequence which cannot occur in such
                  --  contexts
                  null;

               when others =>
                  if not Check_Boundary (Msg2, Boundary) then
                     return False;
                  end if;
            end case;
         end loop;

      else
         if Has_Line_Starting_With (Msg.Contents.Payload.Text, Bound) then
            return False;
         end if;
      end if;

      return True;
   end Check_Boundary;

   ------------------
   -- Set_Boundary --
   ------------------

   procedure Set_Boundary (Msg : Message'Class; Boundary : String := "") is
      Candidate : Unbounded_String;
      Valid     : Boolean := False;
      Content_T : Header;
   begin
      Convert_To_Multipart (Msg);
      Content_T := Get_Header (Msg, Content_Type);

      if Boundary = "" then
         --  Try to reuse the current boundary, if any
         Candidate := To_Unbounded_String (Get_Boundary (Msg));
         if Candidate = "" then
            --  Else default on an unlikely one
            Candidate := To_Unbounded_String ("=_=_=____=_=_");
         end if;
      else
         --  Try and use the user's proposal
         Candidate := To_Unbounded_String (Boundary);
         if Index (Candidate, "=_") = 0 then
            --  Add this string so that we never have to check quoted-printable
            --  or base64 content
            Append (Candidate, "=_");
         end if;
      end if;

      while not Valid loop
         Valid := Check_Boundary (Msg, To_String (Candidate));
         if not Valid then
            Append (Candidate, "=_");
         end if;
      end loop;

      if Content_T = Null_Header then
         Content_T := Create (Content_Type, Multipart_Mixed);
      end if;

      Set_Param (Content_T, "boundary", To_String (Candidate));
      Replace_Header (Msg, Content_T);
   end Set_Boundary;

   ------------
   -- Adjust --
   ------------

   procedure Adjust   (H : in out Header) is
   begin
      if H.Contents /= null then
         H.Contents.Ref_Count := H.Contents.Ref_Count + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (H : in out Header) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Header_Record, Header_Access);
   begin
      if H.Contents /= null then
         H.Contents.Ref_Count := H.Contents.Ref_Count - 1;
         if H.Contents.Ref_Count = 0 then
            Unchecked_Free (H.Contents);
         end if;
      end if;
   end Finalize;

   --------------------
   -- Get_Message_Id --
   --------------------

   function Get_Message_Id (Msg : Message) return String is
      use Ada.Strings;

      H           : constant Header := Get_Header (Msg, "Message-ID");
      MsgId_Str   : constant String :=
        (if H = Null_Header
         then ""
         else Trim (To_String (H, Show_Header_Name => False), Both));
      Index       : Integer;

   begin
      --  Note that we remove leading and trailing spaces, so that a Message-Id
      --  that consists only of spaces will be treated as missing.
      --  Lotus Notes is known to generate such bogus message IDs.

      Index := Next_Occurrence (MsgId_Str, '<');
      if Index > MsgId_Str'Last then
         return MsgId_Str;
      else
         return MsgId_Str (Index + 1 ..
                             Next_Occurrence
                               (MsgId_Str (Index .. MsgId_Str'Last), '>') - 1);
      end if;
   end Get_Message_Id;

   --------------
   -- Get_Date --
   --------------

   function Get_Date (Msg : Message) return Ada.Calendar.Time is
      H : constant Header := Get_Header (Msg, "Date");
   begin
      if H /= Null_Header then
         return To_Time (H);
      else
         return Date_From_Envelope (Msg);
      end if;
   end Get_Date;

end GNATCOLL.Email;
