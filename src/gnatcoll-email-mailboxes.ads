------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2006-2015, AdaCore                     --
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

--  This package handles mailboxes that contain one or more email messages

pragma Ada_05;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with GNATCOLL.Email.Parser;
with GNAT.Strings;
with GNATCOLL.VFS;

package GNATCOLL.Email.Mailboxes is
   --  ??? Would be nice to have a function to write back a message in a
   --  mailbox (with proper message separators).

   ---------------
   -- Mailboxes --
   ---------------

   type Mailbox is abstract tagged limited private;
   --  This type describes a mailbox, which contains several email messages in
   --  some defined format. See the children of this type for the various
   --  supported formats.
   --  This mailbox can be iterated: to get all messages, you would do the
   --  following:
   --      Box : Mbox;
   --
   --      Open (Box, File_Contents)
   --
   --      Curs : Cursor'Class := First (Box);

   --      while Has_Element (Curs) loop
   --         Get_Message (Curs, Box, Msg);
   --         if Msg /= Null_Message then
   --             --  test above is in case of parsing error
   --             ...
   --         Next (Curs, Box);
   --      end loop;

   type Message_Factory is access procedure (Str : String; Msg : out Message);
   --  Builds a message from a string. It should return Null_Message if the
   --  message could not be parsed.
   --  You can provide a different function if you simply want to get the text
   --  of all messages (for instance for a search function), and do not need to
   --  waste time actually parsing the message.

   type Cursor is abstract tagged private;
   --  An iterator over the contents of a mailbox

   function First (Self : Mailbox) return Cursor'Class is abstract;
   --  Return a cursor to iterator over all messages of the mailbox

   procedure Set_Parser
     (Self     : in out Cursor;
      Factory  : Message_Factory := Email.Parser.Parse'Access);
   --  Set the factory used to create the messages parsed from the mailbox.
   --  It can be used to limit which fields should be parsed, whether the body
   --  should be returned,...

   function Has_Element (Self : Cursor) return Boolean is abstract;
   --  True if Self points to a message in the mailbox, False if past the last
   --  message.

   procedure Get_Message
     (Self : in out Cursor;
      Box  : Mailbox'Class;
      Msg  : out Message) is abstract;
   --  Return the current message.
   --  If there is no such message or the message could not be parsed, returns
   --  Null_Message.
   --  The message is generated from the text representing the mailbox by
   --  calling the factory.

   procedure Next (Self : in out Cursor; Box : Mailbox'Class) is abstract;
   --  Moves to the next message in Self

   --------------------
   -- Unix mailboxes --
   --------------------

   type Mbox is new Mailbox with private;
   --  This type describes a mail box in the traditional format used by Unix
   --  systems. Messages are appended one after another, separated by a blank
   --  line and a line starting with "From ".

   overriding function First (Self : Mbox) return Cursor'Class;
   --  Return an instance of Mbox_Cursor

   type Destructor is access procedure (S : in out GNAT.Strings.String_Access);
   --  Free the memory associated with the "Fp" parameter given to Open

   procedure Free_String (Str : in out GNAT.Strings.String_Access);

   procedure Open
     (Self     : in out Mbox;
      Fp       : access String;
      On_Close : Destructor := Free_String'Access);
   --  Initializes the internal data for the mailbox. This procedure must be
   --  called by the various *Open functions below, but doesn't need to be
   --  called by the user.
   --  No copy of Fp is made. On_Close (if defined) is called when the mbox no
   --  longer needs access to Fp. As a result, you can either give control over
   --  Fp to the mailbox (and leave the default value for On_Close), or keep
   --  control of the string, and pass null to On_Close.

   procedure Open
     (Self     : in out Mbox;
      Filename : GNATCOLL.VFS.Virtual_File);
   --  Same as Open, but takes care of opening the file.
   --  If the file could not be open, Name_Error is raised.

   type Mbox_Cursor is new Cursor with private;
   overriding function Has_Element (Self : Mbox_Cursor) return Boolean;
   overriding procedure Next
     (Self : in out Mbox_Cursor;
      Box  : Mailbox'Class);
   overriding procedure Get_Message
     (Self : in out Mbox_Cursor;
      Box  : Mailbox'Class;
      Msg  : out Message);
   --  See inherited documentation

   -------------------------
   -- In-Memory mailboxes --
   -------------------------

   type Stored_Mailbox is new Mailbox with private;
   --  This type represents the contents of a mailbox in memory. All messages
   --  that are part of a file mailbox are read and kept in memory. This
   --  provides a convenient way to keep messages in memory while they are in
   --  use, and in particular provides ways to sort them.
   --  This type is limited since it would be costly to copy instances of a
   --  mailbox otherwise (duplicating all messages in memory).

   procedure Store
     (Self    : out Stored_Mailbox;
      Box     : in out Mailbox'Class;
      Factory : Message_Factory := Email.Parser.Parse'Access);
   procedure Store
     (Self    : out Stored_Mailbox;
      Box     : in out Mailbox'Class;
      Factory : Message_Factory := Email.Parser.Parse'Access;
      From    : Cursor'Class);
   --  Parse a mailbox and store all its messages in memory.
   --  All messages previously in Self are kept.
   --  Box must already have been Open'ed.
   --  The second version allows you to skip messages if needed

   procedure Append (Self : in out Stored_Mailbox; Msg : Message);
   --  Appends a new message to Self. The current sorting order is not
   --  preserved, and you should call Sort_* again after you have added one or
   --  more messages.

   procedure Thread_Messages (Self : in out Stored_Mailbox);
   --  Sort all messages in Self by threads. This preserves the sort order.
   --  This does nothing if Self is already threaded.

   procedure Remove_Threads (Stored : in out Stored_Mailbox);
   --  Removing all threading information from Stored. The mailbox is no
   --  longer sorted as a result.

   function Is_Threaded (Self : Stored_Mailbox) return Boolean;
   --  Whether Self is sorted by threads

   procedure Sort_By_Date (Self : in out Stored_Mailbox);
   --  Sort all messages by Date. This preserves threading information if
   --  available.

   type Stored_Mailbox_Cursor is new Cursor with private;
   --  Iterate over the contents of a mailbox

   overriding function First (Self : Stored_Mailbox) return Cursor'Class;
   function First
     (Self : Stored_Mailbox; Recurse : Boolean)
      return Stored_Mailbox_Cursor'Class;
   --  Starts iteration over all elements in Self, in the order they were
   --  sorted.
   --  If Recurse is False and messages have been sorted by threads, this will
   --  only iterate over the root message of each thread. Use First_In_Thread
   --  to iterate recursively over each thread. Traversal is depth-first.
   --  If Recurse is True, then all messages will eventually be returned.
   --  The iterator becomes invalid when you call one of the Sort_* functions.
   --  The first version of First returns a cursor that iterates not
   --  recursively.

   function First_In_Thread
     (Self : Stored_Mailbox; Parent : Stored_Mailbox_Cursor'Class)
      return Stored_Mailbox_Cursor'Class;
   --  Return the first child of Msg in its thread. If the threads are
   --  organized as:
   --      Msg1                  (thread level 1)
   --        |_ Msg1.1           (thread level 2)
   --             |_ Msg1.1.1    (thread level 3)
   --        |_ Msg1.2           (thread level 2)
   --      Msg2                  (thread level 1);
   --  and Msg1 is passed in argument, then the iterator will return
   --  Msg1.1 and Msg1.2, not Msg1.1.1 nor Msg2.
   --  This function always returns an empty iterator if the mailbox is not
   --  sorted by threads.

   overriding procedure Next
     (Self : in out Stored_Mailbox_Cursor;
      Box  : Mailbox'Class);
   --  See inherited documentation

   overriding procedure Get_Message
     (Self : in out Stored_Mailbox_Cursor;
      Box  : Mailbox'Class;
      Msg  : out Message);
   function Get_Thread_Level (Iter : Stored_Mailbox_Cursor) return Positive;
   --  Return the current message in the mailbox, or Null_Message if there are
   --  no more messages. See the small drawing above for the meaning of
   --  Thread_Level. If the mailbox has not been sorted by threads, the level
   --  is always 1.

   overriding function Has_Element
     (Self : Stored_Mailbox_Cursor) return Boolean;
   --  Whether calling Next on Iter will return a Message

private
   type Mailbox is abstract new Ada.Finalization.Limited_Controlled with record
      null;
   end record;

   type Cursor is abstract tagged record
      Factory : Message_Factory := Email.Parser.Parse'Access;
   end record;

   type Mbox_Cursor is new Cursor with record
      Start, Stop : Integer;
      Max         : Integer;

      Current     : Message;
      --  Cache the current message
   end record;

   procedure Finalize (Self : in out Mailbox);
   pragma Finalize_Storage_Only (Mailbox);

   type Mbox is new Mailbox with record
      Fp                  : GNAT.Strings.String_Access;
      On_Close            : Destructor;
      Previous_Line_Empty : Boolean := True;
   end record;

   overriding procedure Finalize (Self : in out Mbox);
   --  See inherited documentation

   type Abstract_Message_Info is abstract tagged record
      Msg      : Message;
   end record;
   package Message_Info_List is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Abstract_Message_Info'Class);
   type Message_Info is new Abstract_Message_Info with record
      Children : Message_Info_List.List;
   end record;

   type Sort_Order is (Sort_None, Sort_Date);

   type Stored_Mailbox is new Mailbox with record
      Messages  : Message_Info_List.List; --  Contains Message_Info
      Sorted_By : Sort_Order := Sort_None;
      Threaded  : Boolean := False;
   end record;

   package Cursor_List is new Ada.Containers.Doubly_Linked_Lists
     (Message_Info_List.Cursor, Message_Info_List."=");

   type Stored_Mailbox_Cursor is new Cursor with record
      Cursors      : Cursor_List.List;
      Recurse      : Boolean;
      Thread_Level : Integer;
   end record;
   --  If the specified thread level is 0, all messages are returned.
   --  Otherwise, only the messages at the right level.

end GNATCOLL.Email.Mailboxes;
