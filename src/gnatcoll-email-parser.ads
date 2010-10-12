-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2006-2009, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- As a special exception, if other files instantiate generics  from --
-- this unit, or you link this  unit with other files to produce  an --
-- executable, this unit does not by itself cause the resulting exe- --
-- cutable  to be covered by  the  GNU General  Public License. This --
-- exception does not however  invalidate any other reasons why  the --
-- executable  file  might  be  covered  by  the  GNU General Public --
-- License.                                                          --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides a parser that creates an email message from a textual
--  representation of it.

with GNATCOLL.VFS;

package GNATCOLL.Email.Parser is

   procedure Parse (Str : String; Msg : out Message);
   --  Default message parser

   procedure Parse_Ignore_Headers (Str : String; Msg : out Message);
   --  Same as Parse, but the headers are not stored into the final Msg.
   --  This significantly speeds up the parser, and should be used if you don't
   --  need access to headers later on.

   procedure Parse_Minimal_Headers (Str : String; Msg : out Message);
   --  Same as Parse, but only keep a subset of the headers. This removes
   --  headers like 'Received:', which are generally not useful to manipulate
   --  the message.

   procedure Parse_No_Payload (Str : String; Msg : out Message);
   --  Parse the message, but store its body unparsed (ie nested parts are
   --  not analyzed).

   procedure Parse_No_Payload_Minimal_Headers
     (Str : String; Msg : out Message);
   --  Parse the message, but store its body unparsed (ie nested parts are
   --  not analyzed). Ignore headers that are generally not useful to
   --  manipulate a message.

   type Header_Filter is access function (Name : String) return Boolean;

   procedure Full_Parse
     (Str           : String;
      Msg           : out Message;
      Store_Headers : Boolean := True;
      Store_Payload : Boolean := True;
      Parse_Payload : Boolean := True;
      Filter        : Header_Filter := null);
   --  Internal version of Parse. You could implement your own Parse by
   --  calling this one with the appropriate parameters. For instance, you
   --  can choose the list of headers to store.
   --  If Store_Headers is false, then the headers will not be stored in the
   --  final message. Some of them are still taken into account to properly
   --  parse the message (MIME contents,...). This significantly speeds up the
   --  processing since less memory needs to be allocated.
   --  If Filter is specified, only those headers matching Filter will be
   --  stored. If Store_Headers is False, no header is stored.
   --  If Store_Payload is False, then the payload is not analyzed nor parsed,
   --  simply ignored. When the payload is stored, it can additionally be
   --  parsed, ie when it is a multipart message, each of the part is
   --  extracted separately. To save time, they are not MIME-decoded though.

   procedure Full_Parse_From_File
     (Filename      : GNATCOLL.VFS.Virtual_File;
      Msg           : out Message;
      Store_Headers : Boolean := True;
      Store_Payload : Boolean := True;
      Parse_Payload : Boolean := True;
      Filter        : Header_Filter := null);
   --  Same as Full_Parse, but reads the message directly from a file.
   --  Name_Error is raised if the file could not be read.

end GNATCOLL.Email.Parser;
