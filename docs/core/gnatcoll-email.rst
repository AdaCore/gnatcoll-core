************************************
**Email**: Processing email messages
************************************

.. highlight:: ada
.. index:: email

GNATColl provides a set of packages for managing and processing
email messages. Through this packages, you can extract the various messages
contained in an existing mailbox, extract the various components of a message,
editing previously parsed messages, or create new messages from scratch.

This module fully supports MIME-encoded messages, with attachments.

This module currently does not provide a way to send the message through the
SMTP protocol. Rather, it is used to create an in-memory representation of
the message, which you can then convert to a string, and pass this to a
socket. See for instance the `AWS library <http://www.adacore.com/home/gnatpro/add-on_technologies/web_technologies>`_)
which contains the necessary subprograms to connect with an SMTP server.

Message formats
===============

.. index:: GNATCOLL.Email.Utils

The format of mail messages is defined through numerous RFC documents.
GNATColl tries to conform to these as best as possible. Basically,
a message is made of two parts:

*The headers*
  These are various fields that indicate who sent the message, when, to whom,
  and so on

*The payload (aka body)*
  This is the actual contents of the message. It can either be a simple text,
  or made of one or more attachments in various formats. These attachments can
  be HTML text, images, or any binary file. Since email transfer is done through
  various servers, the set of bytes that can be sent is generally limited to
  7 bit characters. Therefore, the attachments are generally encoded through one
  of the encoding defined in the various MIME RFCs, and they need to be decoded
  before the original file can be manipulated again.

GNATColl gives you access to these various components, as will be
seen in the section :ref:`Parsing_messages`.

.. index:: MIME
.. index:: encoding

The package :file:`GNATCOLL.Email.Utils` contains various subprograms to decode
MIME-encoded streams, which you can use independently from the rest of the
packages in the email module.

The headers part of the message contains various pieces of information about
the message. Most of the headers have a well-defined semantics and format.
However, a user is free to add new headers, which will generally start with
`X-` prefix. For those fields where the format is well-defined, they
contain various pieces of information:

*Email addresses*
  The `From`, `TO` or `CC` fields, among others, contain
  list of recipients. These recipients are the usual email addresses. However,
  the format is quite complex, because the full name of the recipient can also
  be specified, along with comments. The package :file:`GNATCOLL.Email.Utils`
  provides various subprograms for parsing email addresses and list of
  recipients.

*Dates*
  The `Date` header indicates when the message was sent. The format of the
  date is also precisely defined in the RFC, and the package
  :file:`GNATCOLL.Email.Utils` provides subprograms for parsing this date (or,
  on the contrary, to create a string from an existing time).

*Text*
  The `Subject` header provides a brief overview of the message. It is
  a simple text header. However, one complication comes from the fact that the
  user might want to use extended characters not in the ASCII subset. In such
  cases, the Subject (or part of it) will be MIME-encoded. The package
  :file:`GNATCOLL.Email.Utils` provides subprograms to decode MIME-encoded strings,
  with the various charsets.

.. _Parsing_messages:

Parsing messages
================

There are two ways a message is represented in memory: initially, it is
a free-form `String`. The usual Ada operations can be used on the string,
of course, but there is no way to extract the various components of the
message. For this, the message must first be parsed into an instance of the
`Message` type.

This type is controlled, which means that the memory will be freed
automatically when the message is no longer needed.

.. index:: GNATCOLL.Email.Parser

The package :file:`GNATCOLL.Email.Parser` provides various subprograms that
parse a message (passed as a string), and create a `Message` out of it.
Parsing a message might be costly in some cases, for instance if a big
attachment needs to be decoded first. In some cases, your application will
not need that information (for instance you might only be looking for a few
of the headers of the message, and not need any information from the body).
This efficiency concern is why there are multiple parsers. Some of them will
ignore parts of the message, and thus be more efficient if you can use them.

.. index:: GNATCOLL.Email

Once a `Message` has been created, the subprograms in
`GNATCOLL.Email`
can be used to access its various parts.
The documentation for these subprograms is found in the file
`gnatcoll-email.ads` directly, and is not duplicated here.

Parsing mailboxes
=================

Most often, a message is not found on its own (unless you are for instance
writing a filter for incoming messages). Instead, the messages are stored
in what is called a mailbox. The latter can contain thousands of such
messages.

There are traditionally multiple formats that have been used for mailboxes.
At this stage, GNATColl only supports one of them, the `mbox`
format. In this format, the messages are concatenated in a single file,
and separated by a newline.

.. index:: GNATCOLL.Email.Mailboxes

The package `GNATCOLL.Email.Mailboxes` provides all the types and
subprograms
to manipulate mailboxes.
Tagged types are used, so that new formats of mailboxes can relatively easily
be added later on, or in your own application.

Here is a small code example that opens an mbox on the disk, and parses each
message it contains::

  declare
    Box  : Mbox;
    Curs : Cursor;
    Msg  : Message;
  begin
    Open (Box, Filename => "my_mbox");
    Curs := Mbox_Cursor (First (Box));
    while Has_Element (Curs) loop
       Get_Message (Curs, Box, Msg);
       if Msg /= Null_Message then
          ...
       end if;
       Next (Curs, Box);
    end loop;
  end;

As you can see, the mailbox needs to be opened first. Then we get an
iterator (called a cursor, to match the Ada2005 containers naming scheme),
and we then parse each message. The `if` test is optional, but
recommended: the message that is returned might be null if the mailbox
was corrupted and the message could not be parsed. There are still chances
that the next message will be readable, so only the current message should
be ignored.

Creating messages
=================

The subprograms in `GNATCOLL.Email` can also be used to create a message
from scratch. Alternatively, if you have already parsed a message, you
can alter it, or easily generate a reply to it (using the `Reply_To`
subprogram. The latter will preset some headers, so that message threading
is preserved in the user's mailers.

