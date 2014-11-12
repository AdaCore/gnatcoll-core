------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2006-2014, AdaCore                     --
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

with GNAT.Case_Util;             use GNAT.Case_Util;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNAT.Strings;               use GNAT.Strings;

package body GNATCOLL.Email.Parser is

   function Preserve_Header (Name : String) return Boolean;
   pragma Inline (Preserve_Header);
   --  Whether the given header should be preserved in the generated message

   procedure Parse_Payload (Msg : in out Message; Unparsed : String);
   --  Parse the payload, as read in Unparsed, into its various components,
   --  and store them in the message appropriately

   ---------------------
   -- Preserve_Header --
   ---------------------

   function Preserve_Header (Name : String) return Boolean is
      N : String := Name;
   begin
      To_Lower (N);

      case N (N'First) is
         when 'c' =>
            return N = "cc" or else N = "content-type";
         when 'd' =>
            return N = "date";
         when 'f' =>
            return N = "from";
         when 'i' =>
            return N = "in-reply-to";
         when 'm' =>
            return N = "message-id" or else N = "mime-version";
         when 'r' =>
            return N = "references" or else N = "reply-to";
         when 's' =>
            return N = "subject";
         when 't' =>
            return N = "to";
         when 'x' =>
            return True;  --  All X-* headers
         when others =>
            return False;
      end case;
   end Preserve_Header;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Str     : String;
      Msg     : out Message) is
   begin
      Full_Parse (Str, Msg, Store_Headers => True, Store_Payload => True,
                  Parse_Payload => True);
   end Parse;

   --------------------------
   -- Parse_Ignore_Headers --
   --------------------------

   procedure Parse_Ignore_Headers (Str : String; Msg : out Message) is
   begin
      Full_Parse (Str, Msg, Store_Headers => False, Store_Payload => True,
                  Parse_Payload => False);
   end Parse_Ignore_Headers;

   ---------------------------
   -- Parse_Minimal_Headers --
   ---------------------------

   procedure Parse_Minimal_Headers (Str : String; Msg : out Message) is
   begin
      Full_Parse (Str, Msg, Store_Headers => True, Store_Payload => True,
                  Parse_Payload => True, Filter => Preserve_Header'Access);
   end Parse_Minimal_Headers;

   ----------------------
   -- Parse_No_Payload --
   ----------------------

   procedure Parse_No_Payload (Str : String; Msg : out Message) is
   begin
      Full_Parse
        (Str, Msg, Store_Headers => True, Store_Payload => True,
         Parse_Payload => False);
   end Parse_No_Payload;

   --------------------------------------
   -- Parse_No_Payload_Minimal_Headers --
   --------------------------------------

   procedure Parse_No_Payload_Minimal_Headers
     (Str : String; Msg : out Message)
   is
   begin
      Full_Parse
        (Str, Msg, Store_Headers => True, Store_Payload => True,
         Parse_Payload => False,
         Filter        => Preserve_Header'Access);
   end Parse_No_Payload_Minimal_Headers;

   ----------------
   -- Full_Parse --
   ----------------

   procedure Full_Parse
     (Str           : String;
      Msg           : out Message;
      Store_Headers : Boolean := True;
      Store_Payload : Boolean := True;
      Parse_Payload : Boolean := True;
      Filter        : Header_Filter := null)
   is
      Index : Integer := Str'First;
      Stop  : constant Integer := Str'Last;
      Colon : Integer;
      Eol   : Integer;
      Next, Eol2 : Integer;
      Is_Continuation : Boolean;
      Value : Unbounded_String;
   begin
      Msg := New_Message (MIME_Type => "");

      --  Do we have an envelope for the message ?
      if Index + 4 < Str'Last
        and then Str (Index .. Index + 4) = "From "
      then
         Eol := Next_Occurrence (Str (Index .. Stop), ASCII.LF);
         Set_Envelope_From (Msg, Str (Index .. Eol - 1));
         Index := Eol + 1;
      end if;

      --  Find the headers block. It is defined as being the set of lines up
      --  to the first line that doesn't match the headers format. This can be
      --  an empty line (and should generally be the case according to
      --  RFC2822), but could be anything else, in which case the extra line
      --  is assumed to belong to the body

      while Index <= Stop loop
         Eol   := Next_Occurrence (Str (Index .. Stop), ASCII.LF);
         Colon := Next_Occurrence (Str (Index .. Eol), ':');
         exit when Colon > Eol;

         --  ??? Header names are characters between 33 and 126 inclusive. We
         --  should check

         --  Check for continuation lines: if the next line starts with a
         --  whitespace but contains other characters than whitespaces, it is
         --  part of the same header. We have this whitespace handling because
         --  of cases where the subject line is followed by the separator line,
         --  itself starting with a space. This is not full RFC2822 of course,
         --  but it is nice to handle this correctly anyway

         if Str (Eol - 1) = ASCII.CR then
            Value := To_Unbounded_String (Str (Colon + 1 .. Eol - 2));
         else
            Value := To_Unbounded_String (Str (Colon + 1 .. Eol - 1));
         end if;

         while Eol < Str'Last and then Is_Whitespace (Str (Eol + 1)) loop
            Next := Eol + 1;
            Is_Continuation := False;
            Eol2 := Next_Occurrence (Str (Next .. Stop), ASCII.LF);
            for F in Next + 1 .. Eol2 - 1 loop
               if not Is_Whitespace (Str (F)) then
                  if Str (Eol2 - 1) = ASCII.CR then
                     Append (Value, ' ' & Str (F .. Eol2 - 2));
                  else
                     Append (Value, ' ' & Str (F .. Eol2 - 1));
                  end if;
                  Is_Continuation := True;
                  exit;
               end if;
            end loop;

            exit when not Is_Continuation;
            Eol  := Eol2;
         end loop;

         if Store_Headers
           and then (Filter = null or else Filter (Str (Index .. Colon - 1)))
         then
            Add_Header
              (Msg,
               Create (Name  => Str (Index .. Colon - 1),
                       Value => To_String (Value)));
         end if;

         Index := Eol + 1;
      end loop;

      --  A blank line is not part of the body, any other line is
      if Index <= Str'Last and then Str (Index) = ASCII.LF then
         Index := Index + 1;
      end if;

      if Store_Payload then
         if not Parse_Payload then
            Set_Text_Payload (Msg, Str (Index .. Str'Last), Charset => "");
         else
            Email.Parser.Parse_Payload (Msg, Str (Index .. Str'Last));
         end if;
      end if;

   exception
      when others =>
         Msg := Null_Message;
   end Full_Parse;

   -------------------
   -- Parse_Payload --
   -------------------

   procedure Parse_Payload (Msg : in out Message; Unparsed : String) is
      Boundary         : constant String := Get_Boundary (Msg);
      Length           : constant Natural := Boundary'Length;
      Index            : Integer := Unparsed'First;
      Tmp              : Integer;
      Is_Last_Boundary : Boolean := False;
      Is_Boundary      : Boolean;
      Start            : Integer := -1;
      Attachment       : Message;
   begin
      if Boundary = "" then
         Set_Text_Payload (Msg, Unparsed, MIME_Type => "");

      else
         while not Is_Last_Boundary
           and then Index + Length < Unparsed'Last
         loop
            if Unparsed (Index) = '-'
              and then Unparsed (Index + 1) = '-'
              and then Unparsed (Index + 2 .. Index + 1 + Length) = Boundary
            then
               Tmp := Index + 2 + Length;

               if Unparsed (Tmp) = '-'
                 and then Unparsed (Tmp + 1) = '-'
               then
                  Is_Last_Boundary := True;
                  Tmp := Tmp + 2;
               end if;

               Is_Boundary := True;
               while Tmp <= Unparsed'Last
                 and then Unparsed (Tmp) /= ASCII.LF
               loop
                  if not Is_Whitespace (Unparsed (Tmp)) then
                     --  Not a boundary after all
                     Is_Boundary := False;
                     Is_Last_Boundary := False;
                     exit;
                  end if;
                  Tmp := Tmp + 1;
               end loop;

               if Is_Boundary then
                  if Start /= -1 then
                     Full_Parse
                       (Str           => Unparsed (Start .. Index - 2),
                        Msg           => Attachment,
                        Store_Headers => True,
                        Store_Payload => True,
                        Parse_Payload => True);
                     if Attachment /= Null_Message then
                        Add_Payload (Msg, Attachment);
                     else
                        --  Should exit with error message I guess
                        null;
                     end if;

                  else
                     Set_Preamble
                       (Msg, Unparsed (Unparsed'First .. Index - 2));
                  end if;

                  Start := Tmp + 1;
                  Is_Last_Boundary := Is_Last_Boundary
                    or else Tmp + Length >= Unparsed'Last;
               end if;

               Index := Next_Occurrence
                 (Unparsed (Tmp .. Unparsed'Last), ASCII.LF) + 1;

            else
               Index := Next_Occurrence
                 (Unparsed (Index .. Unparsed'Last), ASCII.LF) + 1;
            end if;
         end loop;
      end if;

      if Index < Unparsed'Last and then Start /= -1 then
         Set_Epilogue (Msg, Unparsed (Start .. Unparsed'Last));
      end if;
   end Parse_Payload;

   --------------------------
   -- Full_Parse_From_File --
   --------------------------

   procedure Full_Parse_From_File
     (Filename      : Virtual_File;
      Msg           : out Message;
      Store_Headers : Boolean := True;
      Store_Payload : Boolean := True;
      Parse_Payload : Boolean := True;
      Filter        : Header_Filter := null)
   is
      Str  : GNAT.Strings.String_Access;
   begin
      Str := Read_File (Filename);
      Full_Parse (Str.all,
                  Msg, Store_Headers,
                  Store_Payload, Parse_Payload, Filter);
      Free (Str);
   end Full_Parse_From_File;

end GNATCOLL.Email.Parser;
